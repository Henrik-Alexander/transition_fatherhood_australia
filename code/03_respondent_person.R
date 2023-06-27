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
# All files are loaded, cleaned and combined via the person ID

## IMPORTANT!!!
# The household ID changes across waves


### RP: Load the respondent person questionaire ------------------------

# Variables
vars <- c("dxyr", "dcany", "psyobf", "psyobm", "bsad", "bsoy", "tcnr")

# Create a wave vector
waves <- letters[1]

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
dta <- dta |> select(str_match(vars))

# Assign the wave
dta$wave <- wave

# Assign to list
rp[[wave]] <- dta

}

## Look for the variables matches
str_hits(variables, "cany")

# Bind the data
rp <- rbindlist(rp)

# Filter the important variables
