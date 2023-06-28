#######################################
# Purpose: Loading the HILDA data     #
# Author: Henrik-Alexander Schubert   #
# Date: 14.06.2023                    #
# E-Mail: schubert@demogr.mpg.de      #
# Pre-Requisites: full repository     #
#######################################

rm(list = ls())

# Load the functions
source("Functions/packages.R")
source("Functions/functions.R")
source("Functions/graphics.R")

# Load the packages
library(tidyverse)

## IMPORTANT!!!
# The household ID changes across waves

# Variables to be selected for the analysis
vars <- c("xwaveid", "hhrpid",    # Panel information
  "hhrhid",                       # Household identifier
  "lsrel",                        # Relationship and marriage
  "tchad", "wschave", "tcnr",     # N. children, , N. of non-resident children
  "icn",                          # Fertility intentions
  "hhiage", "hgage", "hgagef",    # Respondents age
  "hgsex",                        # Demographics
  "hhwte", "hhwtrps", "hhwtrp",   # Weigths
  "edagels")                      # Education


### RP: Load the respondent person questionaire ------------------------

# Create a wave vector
waves <- letters[1:21] # 21 Waves

# Create a container for the household questionnaires
rp <- vector("list", length = length(waves))

# Loop over waves
for (wave in seq_along(
  waves)){

  cat("Working on wave", wave, " \n")

# Load the first wave
path <- paste0("raw/STATA-files/Rperson_", waves[wave], "210c.dta")

# Load the data
dta  <- haven::read_dta(path) |> as.data.frame()

# Remove the beginning of the variables
names(dta) <- str_remove(names(dta), paste0("^", waves[wave]))

# Children variables
dta <- dta |> select(contains(vars))

# Assign the wave
dta$wave <- wave

# Assign to list
rp[[wave]] <- dta

}


# Bind the data
rp <- rbindlist(rp, fill = TRUE)

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
    fert_int = icniz
  )

# Save the data
save(rp, file = "data/rp_data.Rda")

### END #########################################
