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

### EP: Load the enumerated person questionaire ------------------------

# Create a vector of variables
vars <- c("hhbfxid", "hhbmxid", "hhpxid", "xwaveid", "hhrhid",
          "hgni", "wscef", "wscei", "hgyob", "hhwte", "hhwtes")

# Create a wave vector
waves <- letters[1:21]

# Create a container for the household questionnaires
ep <- vector("list", length = length(waves))

# Loop over waves
for (wave in seq_along(waves)) {

  cat("Working on wave", wave, " \n")

# Load the first wave
path <- paste0("raw/STATA-files/Eperson_", waves[wave], "210c.dta")

# Load the data
dta  <- haven::read_dta(path) |> as_tibble()

# Remove the beginning of the variables
names(dta) <- str_remove(names(dta), paste0("^", waves[wave]))

# Children variables
dta <- dta[, vars]

# Assign the wave
dta$wave <- wave

# Assign the data
ep[[wave]] <- dta

}

# Bind the data
ep <- rbindlist(ep)

# Rename variables
ep <- ep  |>
rename(id_moth = hhbfxid,
       id_fath = hhbmxid,
       id_part = hhpxid,
       id      = xwaveid,
       id_hh   = hhrhid,
       int     = hgni,
       inc     = wscef,
       inc_imp = wscei,
       yob     = hgyob)

       
# Save the data
save(ep, file = "data/enumerate_data_cleaned.Rda")

### END ############################################