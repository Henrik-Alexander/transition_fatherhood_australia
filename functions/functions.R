## Functions


#### Create directory structure -------------------

gen_folder <- function(foldername = "raw"){
  if(file.exists(foldername)){
    cat("The", foldername, "folder exists already! \n")
  } else {
    dir.create(foldername)
  }
}
  
# Create the folders
folders <- c("code", "raw", "functions", "data", "results", "figures")
lapply(folders, gen_folder)

#### Negate in function -------------------------------------

`%notin%` <- Negate(`%in%`)

#### Tabulate function --------------------------------------

tab <- function(...){
  tmp <- table(..., useNA = "always")
  return(tmp)
}

### TEMPORARILY: Str match funciton -------------------------
str_matches <- function(string, pattern){
  tmp <- sum(str_detect(string, pattern))
  return(tmp)
}

str_hits <- function(string, pattern){
  loc <- str_detect(string, pattern)
  tmp <- string[loc]
  return(tmp)
}

# Create long data of households --------------------------
long_hh_data <- function(dta, variable, new_variable = "sex") {
  vars <- names(dta)[str_detect(names(dta), pattern = variable)]
  tmp <- data.table::as.data.table(dta)
  tmp <- tmp[, c(..vars, "hhrhid")]
  tmp <- data.table::melt(tmp, id.vars = "hhrhid", mueasure.vars = vars,
  variable.name = "hh_nr", value.name = new_variable)
  tmp$hh_nr <- str_remove(tmp$hh_nr, "[a-z]*_")
  return(tmp)
}


# Create long data of respondents --------------------------
long_rep_data <- function(dta, variable, new_variable = "dec_nr") {
  vars <- names(dta)[str_detect(names(dta), pattern = variable)]
  tmp <- data.table::as.data.table(dta)
  tmp <- tmp[, c(..vars, "xwaveid")]
  tmp <- data.table::melt(tmp, id.vars = "xwaveid", mueasure.vars = vars,
  variable.name = "rep_nr", value.name = new_variable)
  tmp$rep_nr <- str_remove(tmp$rep_nr, "[a-z]*")
  return(tmp)
}


### Create the Gompertz function ---------------------------

# Gompertz function
gompertz_function <- function(x, y0, ymax, k, lag){
  result <- y0 + (ymax -y0)*exp(-exp(k*(lag-x)/(ymax-y0) + 1) )
  return(result)
}

### Parametric hazard models --------------------------------


# Create the model function
par_surv <- function(data = fert2, distribution = "exponential", parameters = NA){
  if(is.na(parameters) & distribution %!in% c("weibull", "gompertz")){
    # Run the exponential regression
    tmp <- survreg(Surv(Censoring, Event) ~ cohort,
                   dist = distribution,
                   data = data)
  }else if(is.na(parameters) & distribution == "weibull"){
    # Run the weibull regression
    tmp <- weibreg(Surv(Censoring, Event) ~ strata(cohort), data = data)
  }else if(distribution == "gompertz"){
    # Prepare the gompertz data
    gomp_dat <- fert2 |> group_by(bioage, cohort) |> 
      summarise(Exposures = n(),
                Events    = log(sum(Event)))
    # Run the gompertz regression
    tmp <- nls(Events ~ Gompertz(bioage, y0, ymax, k, lag ),
               data = gomp_dat, 
               start = list(y0 = 0.15, ymax = 7, k = 0.5, lag = 3))
  }else{
    # Run the model
    tmp <- phreg(Surv(Censoring, Event) ~ cohort,
                 dist = distribution,
                 shape = parameters[1],
                 data = data)    
  }
  return(tmp)
}

#### Load country specific functions ------------------------

#source("Functions/Functions_germany.R")

#### Life table function ------------------------------------

lifetable <- function(mx, sex = "M"){
  
# Let's first define our ages
age <- seq_along(mx)
nages <- length(age)

# The length of the age interval
nx <- c(diff(age),1/mx[length(age)])

# Estimate the number of person years lived by those who die
# following the HMD, we will use the Andreev & Kingkade formulas

a0FUN <- function(sex,mx){
  ax <- NA
  if (sex == 'M') {
    ax[1] <- ifelse(mx[1]>=0.08307,0.29915,
                    ifelse(mx[1]<0.023,0.14929 - 1.99545 * mx[1],
                           0.02832 + 3.26021 * mx[1]))
  }
  
  if (sex == 'F') {
    ax[1] <- ifelse(mx[1]>=0.06891,0.31411,
                    ifelse(mx[1]<0.01724,0.14903 - 2.05527 * mx[1],
                           0.04667 + 3.88089 * mx[1]))
  }
  return(ax[1])
}

ax <- nx/2
ax[1] <- a0FUN(sex=sex,mx=mx)
ax[nages] <- 1/mx[nages]

# Estimate the death probabilities: mx to qx conversion
qx <-  nx * mx / (1 + nx * (1 - ax) * mx)
qx[nages] = 1

# Estimate the survival probabilities
px = 1 - qx

# Estimate thelife table survivors with a radix of 100 000
lx = rep(NA,nages)
lx[1] = 100000
for(i in 1:(nages-1)){
  lx[i+1] <- lx[i]*px[i]
}

# Estimate the life table deaths
dx = lx*qx
dx[nages] = lx[nages]

# Estimate the person-years lived in the age interval
Lx = nx * lx - nx * (1 - ax) * dx
Lx[nages] = lx[nages] * ax[nages]

# Aggregate person-years lived above age x
Tx = rev(cumsum(rev(Lx)))

# Estimate remaining life expectancy
ex <- Tx/lx

# putting everything in a dataframe
LT <- data.frame(age=age,
                 n=nx,
                 mx=round(mx,5),
                 qx=round(qx,5),
                 px=round(1 - qx, 5),
                 ax=round(ax,2),
                 lx=round(lx,0),
                 dx=round(dx,0),
                 Lx=round(Lx,0),
                 Tx=round(Tx,0),
                 ex=round(ex,2))

return(LT)
}


#### Population Share ----------------------------------------

pop_share <- function(population){
  share <- population / sum(population, na.rm = TRUE)
  return(share)
}


#### Genesis API -----------------------------------

# Load data from genesis
genesis_api <- function(task = "results", request) {
  
  # select the task
  tmp <- request %>% filter(names == task) 
  
  #  select the task
  task <- tmp[1, 2]
  
  # select the end of the inquire
  ending <- tmp[1, 3]
  
  # create url
  url <- paste0("https://www-genesis.destatis.de/genesisWS/rest/2020/", task, login, ending)
  
  # scrape the data from the website
  resp <- GET(url)
  if (http_type(resp) != "application/json") {
    stop("API did not return json", call. = FALSE)
  }
  
  jsonlite::fromJSON(content(resp, "text"), simplifyVector = FALSE)
}


#### Load the data from germany -----------------------------


# Function to call the data
load_data_DE <- function(data_nr = "12411KJ0018",start = 2015, end = 2015) {
  
  #create the url
  url <- paste0("https://www-genesis.destatis.de/genesisWS/rest/2020/data/timeseries?username=", IHRE_KENNUNG ,"&password=", IHR_PASSWORT, "&name=", data_nr,
                "&area=all&compress=false&transpose=false&contents=BEVSTD&startyear=", start, "&endyear=", end, "&timeslices=&regionalvariable=0018&regionalkey=&regionalkeycode=&classifyingvariable1=Geschlecht&classifyingkey1=weiblich&classifyingkeycode1=GESW&classifyingvariable2=&classifyingkey2=&classifyingkeycode2=&classifyingvariable3=&classifyingkey3=KREISE&classifyingkeycode3=&job=false&stand=&language=de")
  
  resp <- GET(url)
  
  if (http_type(resp) != "application/json") {
    stop("API did not return json", call. = FALSE)
  }
  
  jsonlite::fromJSON(content(resp, "text"), simplifyVector = FALSE)
}


### Load tables Germany -------------------------------------

# Function to call the data
load_table_DE <- function(data_nr = "12411KJ0018",start = 2015, end = 2015) {
  
  #create the url
  url <- paste0("https://www-genesis.destatis.de/genesisWS/rest/2020/data/tablefile?username=", IHRE_KENNUNG ,"&password=", IHR_PASSWORT, "&name=", data_nr,
                "&area=all&compress=false&transpose=false&startyear=", start, "&endyear=", end, "&timeslices=&regionalvariable=0018&regionalkey=&regionalkeycode=&classifyingvariable1=GES&classifyingkey1=&classifyingkeycode1=&classifyingvariable2=&classifyingkey2=&classifyingkeycode2=&classifyingvariable3=&classifyingkey3=KREISE&classifyingkeycode3=&format=csv&job=false&stand=&language=de")
  
  resp <- GET(url)
  
  if (http_type(resp) != "application/json") {
    stop("API did not return json", call. = FALSE)
  }
  
  jsonlite::fromJSON(content(resp, "text"), simplifyVector = FALSE)
}


#### Negate in function -------------------------------------

`%!in%` <- negate(`%in%`)


#### Tabulate function --------------------------------------


tab <- function(...){
  
  tmp <- table(..., useNA = "always")
  return(tmp)
  
}


#### Extract first column name from each dataset ----------
extract_first_column_name <- function(dataset_list) {
  column_names <- vector("character", length(dataset_list))
  
  for (i in seq_along(dataset_list)) {
    column_names[i] <- names(dataset_list[[i]])[1]
  }
  
  return(column_names)
}



# Load the shape data -----------------------------------------------------
load_shape_deu <- function(year){
  
  # Target folder 
  target <- paste0("Raw/", year)
  tmp    <- "U:/Tmp/tmp_shape.zip"
  
  if(file.exists(target)){
    
    cat("File exists already!")
    
  }else{
    
    # Create the folder
    dir.create(target)
    
    # Set the path
    path <- paste0("https://gisco-services.ec.europa.eu/distribution/v2/communes/download/ref-communes-", year, "-01m.shp.zip")
    
    # Download the file
    download.file(url = path, destfile =  tmp)
    
    # Unzip the file
    unzip(tmp, exdir = target)
    
    # Files
    unzip(paste0(target, "/", list.files(target, pattern = ".*_RG_.*_3857.shp.zip")), exdir = target)
    ## Contains:
    ## .shp -> is a shape file
    ## RG   -> regional boundaries
    ## 3857 -> is mercator cps
    
    # Delete the zip files
    sapply(paste0(target, "/", list.files(target, pattern = ".zip$")), file.remove)
  }
}

# Clean shape data ------------------------------------------------------
clean_shape_deu <- function(year){
  
    # Target folder 
    target <- paste0("Raw/", year)
    
    # Load the data
    data <- read_sf(paste0(target, "/", list.files(target, pattern = ".shp$")))
    
    # Clean the data
    data <- data |>
      clean_names() |> 
      filter(cntr_code == "DE" ) |>
      select(ends_with("name"), nsi_code, nuts_code, geometry) |> 
      rename(reg_code = nsi_code,
             region = ends_with("name")) |> 
      group_by(nuts_code) |> 
      summarise(geometry = st_union(geometry))
    
    
    #& str_detect(nuts_code, pattern = "DE[0-9]{3}")
    # Return the data
    return(data)
    
}

### Clean the lau data ------------------------------------------------

# Write a function to clean the data
clean_lau_data <- function(...){
  
  # Clean the names
  tmp <- ... |> clean_names() 
  
  # Make NAs
  tmp <- tmp |> replace_with_na_all(condition = ~.x  == "n.a.")
  
  # Select the first two rows
  tmp <- tmp[, 1:2]
  
  # Rename the columns
  names(tmp) <- c("nuts_code", "lau_code")
  
  # Clean the variables
  tmp <- tmp |> #filter(str_detect(nuts_code, "DE[0-9]{3}")) |> 
    mutate(lau_code = str_sub(lau_code, 1, 5))
  
  # Return the data
  return(tmp)
  
}

#####               END               ########################




