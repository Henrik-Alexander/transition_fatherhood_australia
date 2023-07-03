
#######################################
# Purpose: Creating event data        #
# Author: Henrik-Alexander Schubert   #
# Date: 91.07.2023                    #
# E-Mail: schubert@demogr.mpg.de      #
# Pre-Requisites: full repository     #
#######################################

rm(list = ls())

# Load the packages
library(tidyverse)
library(data.table)

# Load the functions
source("functions/functions.R")
source("functions/graphics.R")

### Load the data --------------------------------

#  Load the data
load("data/rp_data.Rda")
load("data/hh_cleaned.Rda")
load("data/hh_long_cleaned.Rda")
load("data/ep_cleaned.Rda")
load("data/fert_deceased.Rda")
load("data/fert_major.Rda")

# 1. Estimate births over consecutive waves --------

# Make missings
rp$nch[rp$nch < 0] <- NA

# Create the lagged variable
bir_rp <- rp[order(id, wave),
 .(lag_nch = shift(nch, n = 1, type = "lag"),
   min_age = shift(age, n = 1, type = "lag"),
   max_age = age,
   min_date = shift(int_date),
   wave, nch),  by = id]

# Create the lagged birth variable
bir_rp <- bir_rp[, .(birth    = nch - lag_nch, wave, lag_nch, min_age,
                     age_diff = max_age - min_age, min_date), by = id]

# Bind cols
rp <- merge(rp, bir_rp, by = c("id", "wave"))

# Function: Sample a birth date for the children using a uniform distribution
sample_date <- function(min, max) {
    res <- lubridate::interval(min, max) / lubridate::days(1)
    loc <- vector("integer", length = length(res))
     for (i in seq_along(res)) {
      if (!is.na(res[i])) {
      loc[i] <- sample(1:res[i], 1)
      } else { 
      loc[i] <- lubridate::NA_Date_
      }
    }
    tmp <- min + loc
return(tmp)
}

# Sample the birth date
rp[, child_birth_date := sample_date(min_date, int_date)]

# Estimate the age at birth
rp[, age_par :=  as.numeric(interval(child_birth_date, birth_date))]

## Problem: negative births
#  Cross check by education
table(rp$birth, rp$edu)
# Negative birth counts are concentrated in lower education strata

# 2. Estimate the person members in hh ---------------------------

# Household background
hh_back <- hh[, .(id_hh, wave, int_date)]
hh_back <- unique(hh_back)

# Merge with interview date
hh_long <- merge(hh_long, hh_back, all.x = TRUE)

# Select the age and birthdate variable
birth_date <- hh_long[, .(pid, wave, birth = int_date %m-% years(age))]

# Get the min and max date
birth_date <- birth_date[, .(min = min(birth), max = max(birth)), by = pid]

# Estimate birth date
random_date <- function(min, max) {
  if (!is.na(min) && !is.na(max)) {
    tmp <-  sample(x = seq(from = min, to = max, by = "day"),
               size = 1)
  } else {
    tmp <- lubridate::NA_Date_
  }
  return(tmp)
}

# Sample the birth dates
# Apply map to dates is a known problem:
# See the discussion: https://github.com/tidyverse/purrr/issues/358
dates <- map2(.x = birth_date$min, .y = birth_date$max, .f = random_date)
birth_date[, birth_date := dates]

# Merge the data sets
hh_long <- merge(hh_long, birth_date[, .(pid, birth_date)], 
                  by = "pid", all = TRUE)

# Get the parent child information across households
parent_child <- unique(ep[, .(id_moth, id_fath, id, yob)])

# Merge with parent and child information
hh_child <- merge(hh_long, parent_child, by.x = "pid", by.y = "id")

# Merge with the parent's information
hh_child <- merge(hh_child, rp[, .(id, birth_date)], 
by.x = "id_fath", by.y = "id", suffixes = c("", "_father"))
hh_child <- merge(hh_child, rp[, .(id, birth_date)], 
by.x = "id_fath", by.y = "id", suffixes = c("_child", "_mother"))

# Reshape to long format
melt(hh_child, id.vars = "pid",
                measure.vars = "birth_date_",
                variable_name = "parent",
                value_name = "birth_date")


# 3. Estimate the relatives outside hh -------------------------

# 1. Get the birth year of each child
fert[, birth_year = year(int_date - years(age_non_res_chi))]

# Load the non-resident children from the fertility modules
num_nr <- fert[, nchild_non_res, by = .(id, wave)]
num_nr[, .(max_nchild_non_res = nchild_non_res, spells = .N), by = id]



# Load the 


# 4. Estimate deceased children ---------------------------------

# PROBLEM: Are there twins?

# Filter the information
fert_dec <- fert_dec[yob_dec_child >= 0, ]

# Detect the twins
twins_dec <- fert_dec[, .(.N), by = .(wave, yob_dec_child, id)]
twins_dec <- twins_dec[N > 1, ]
# Remove duplicates from the twins data
twins_dec <- unique(twins_dec[ , .(N, yob_dec_child, id)])

# Detect the unique observations per person
dec_births <- unique(fert_dec[, .(id, yob_dec_child)])

# Join the data
dec_births <- merge(dec_births, twins_dec, all = TRUE, key = "id")

# Save the date
save(dec_births, file = "data/deceased_births.Rda")


### Combine the different data sets -----------------------------


### 


### Test the data quality ------------------------------


# Are the number in nchild and birth dates the same?

#


### Look at the data -----------------------------------

# Arrange the data
data <- data |>
  group_by(pid) |>
  arrange(int_date)

# Create a birth variable
data <- data |>
  mutate(birth = ifelse(lag(tchad) != tchad & !is.na(tchad) & !is.na(tchad),
   1, 0))

# Filter respondents who have not had a birth in the first wave
data <- data |>
  mutate(spell = row_number()) |> 
  filter(!(spell == 1 & tchad > 0))

# Filter respondents with at least two waves
data <- data |>
  mutate(spells = n()) |>
  filter(spells >= 2)


# Filter first births and event spell
data <- data |>
  filter(tchad == 0 | (birth == 1 & tchad == 1))

# Filter men
data <- data |>
  filter(sex == 1)

# Estimate age at birth
data <- data |>
  mutate(min_age = if_else(birth == 1, lag(hhiage), NA),
         max_age = if_else(birth == 1, hhiage, NA))


# Save the data
save(data, file = "data/birth_histories.Rda")

### END ################################################