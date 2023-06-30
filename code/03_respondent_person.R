#######################################
# Purpose: Loading the HILDA data     #
# Author: Henrik-Alexander Schubert   #
# Date: 14.06.2023                    #
# E-Mail: schubert@demogr.mpg.de      #
# Pre-Requisites: full repository     #
#######################################

rm(list = ls())

# Load the functions
source("functions/packages.R")
source("functions/functions.R")
source("functions/graphics.R")

# Load the packages
library(tidyverse)
library(lubridate)
library(data.table)

## IMPORTANT!!!
# The household ID changes across waves

### Specify the respondent questionnaire ---------------------------

# Variables to be selected for the analysis
vars <- c("xwaveid", "hhrpid",    # Panel information
  "hhrhid",                       # Household identifier
  "lsrel",                        # Relationship and marriage
  "tchad", "wschave", "tcnr",     # N. children, , N. of non-resident children
  "icn",                          # Fertility intentions
  "hhiage", "hgage", "hgagef",    # Respondents age
  "hgsex",                        # Demographics
  "hhwte", "hhwtrps", "hhwtrp",   # Weigths
  "edagels", "hhidate")                      # Education

### Specify the fertility questionnaires --------------------------

# Specify the waves
fert_waves <- c(5, 8, 11, 15, 19)

# Create a container
fert_dec <- fert <- vector("list", length = length(fert_waves))

# Specify variables
vars_fert <- c("xwaveid", "hhrhid",          # Identifiers
                "tcr", "tcnr",                # Number of children
                "rcyng", "ncyng",              # Age of the youngest child
                "tcn04", "tcn514", "tcn1524", # Number of children at ages
                "tcr04", "tcr514", "tcr1524", # Number of non-resid children
                "dcany", "dyr", "dcperm",     # Deceased children
                "hhidate")                    # Interview date

### RP: Load the respondent person questionaire ------------------------

# Create a wave vector
waves <- letters[1:21] # 21 Waves

# Create a container for the household questionnaires
rp <- vector("list", length = length(waves))

# Loop over waves
for (wave in seq_along(waves)){

  cat("Working on wave", wave, " \n")

# Set the path to the file for the specific wave
path <- paste0("raw/STATA-files/Rperson_", waves[wave], "210c.dta")

# Load the data
dta  <- haven::read_dta(path) |> as.data.frame()

# Remove the beginning of the variables
names(dta) <- str_remove(names(dta), paste0("^", waves[wave]))

# Transform the date
dta$hhidate <- dmy(dta$hhidate)

### Fertility data -------------------------------------
if (wave %in% fert_waves) {

# Select children variables
dta_fert <- dta |> select(contains(vars_fert))

# Reshape to long data
deceased_child <- long_rep_data(dta_fert,
 variable = "dcdyr",
 new_variable = "yob_dec_child")

 # Assign the wave
deceased_child$wave <- dta_fert$wave <- wave

# Assign the wave to the list
fert[[wave]]     <- dta_fert
fert_dec[[wave]] <- deceased_child

}

### Continue with the person data -----------------------------

# Select the variables variables
dta <- dta |> select(contains(vars))

# Assign the wave
dta$wave <- wave

# Assign to list
rp[[wave]] <- dta

}

# Bind the data
rp   <- rbindlist(rp,       fill = TRUE)
fert <- rbindlist(fert,     fill = TRUE)
fert_dec <- rbindlist(fert_dec, fill = TRUE)

# Rename variables
rp <- rp |>
  rename(
    id = xwaveid,
    id_hh = hhrhid,
    age = hgage,
    edu = edagels,
    nch = tchad,
    nch_nonres = tcnr,
    age_june = hhiage,
    wht_rep = hhwtrps,
    wht_rep2 = hhwtrp,
    fert_int = icniz,
    int_date = hhidate
  )

### Create a birth date variable -----------------------------------

# Select the age and birthdate variable
birth_date <- rp[, .(id, wave, birth = int_date %m-% years(age))]

# Get the min and max date
birth_date <- birth_date[, .(min = min(birth), max = max(birth)), by = id]

# Estimate birth date
random_date <- function(var1, var2) {
  tmp <- sample(x = seq(from = var1, to = var2, by = "day"), size = 1)
  return(tmp)
}

# Pivot wider
birth_date$birth_date <- cbind(purrr::map2(
    	                                .x = birth_date$min,
                                      .y = birth_date$max,
                                      .f = random_date))

# Merge the data sets
rp <- merge(rp, birth_date[, .(id, birth_date)], all = TRUE)


# Save the data
save(rp, file = "data/rp_data.Rda")

### Rename the fertility data ----------------------

# Rename variables
fert <- fert[, .(id    = xwaveid,
          id_hh = hhrhid,
          nchild_res = tcr,
          nchild_non_res = tcnr,
          age_res_child = rcyng,
          age_non_res_chi = ncyng,
          nchi_non_res_0_4 = tcn04,
          nchi_non_res_5_14 = tcn514,
          nchi_non_res15_24 = tcn1524,
          nchi_res_0_4 = tcr04,
          nchi_res_5_14 = tcr514,
          nchi_res15_24 = tcr1524,
          dc_permission = dcperm,
          int_date      = hhidate,
          wave)]


# Rename the variables for fertility deceaced
fert_dec <- fert_dec[, .(id = xwaveid, rep_nr, yob_dec_child, wave)]

# Save the data
save(fert,     file = "data/fert_major.Rda")
save(fert_dec, file = "data/fert_deceased.Rda")


### Load the History and status of parents -------------------------

# Specify the waves of the major modules
parent_waves <- c(8, 12, 15, 19)
waves <- letters[parent_waves[1]]

### END #########################################
