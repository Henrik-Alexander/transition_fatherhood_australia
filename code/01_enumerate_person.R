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

## Information:
# The HILDA survey consists of three different questionnaires
# 1. Household questionnarie
# 2. Enumerated person questionnaire
# 3. Respondent person questionnaire
####

# Enumerated person file: contains information on ALL household members

# All files are loaded, cleaned and combined via the person ID

## IMPORTANT!!!
# The household ID changes across waves

## Variables ##################################
# _hhbfxid = biological Father's cross wave id
# _hhbmxid = biological Mother's cross wave id
# xwaveid  = cross wave ID
# hgyob    = year of birth
###############################################

### EP: Load the enumerated person questionaire ------------------------

# Create a vector of variables
vars <- c("hbfxid", "hbmxid", "xwaveid", "hgyob")

# Create a wave vector
waves <- letters[1:21] # 21 Waves

# Create a container for the household questionnaires
ep <- vector("list", length = length(waves))

# Loop over waves
for (wave in seq_along(waves)){
  cat("Working on wave", wave, " \n")

# Load the first wave
path <- paste0("raw/STATA-files/Eperson_", waves[wave], "210c.dta")

# Load the data
dta  <- haven::read_dta(path) |> as_tibble()

# Remove the beginning of the variables
names(dta) <- str_remove(names(dta), paste0("^", waves[wave]))

# Children variables
dta <- dta |> select(tidyselect::matches(vars))

# Assign the wave
dta$wave <- wave

# Assign the data to the list
ep[[wave]] <- dta

}

# Bind the data.frames together
ep <- rbindlist(ep)

# Rename the variables
ep <- ep[ , .(fath_id = hhbfxid,
              moth_id = hhbmxid,
              pid     = xwaveid,
              yob     = hgyob,
              wave)]

# Save the data
save(ep, file = "data/ep_cleaned.Rda")

### Descriptives -----------------------------------

# Distributions 
dist <- ep[, .(counts = .N), by = xwaveid]
hist(dist$counts)


#


#


### END #############################################
