#######################################
# Purpose: Loading the household data #
# Author: Henrik-Alexander Schubert   #
# Date: 14.06.2023                    #
# E-Mail: schubert@demogr.mpg.de      #
# Pre-Requisites: full repository     #
#######################################

## IMPORTANT!!!
# The household ID changes across waves
# -> All files are loaded, cleaned and combined via the person ID

rm(list = ls())

# Load the functions
source("functions/packages.R")
source("functions/functions.R")
source("functions/graphics.R")

# Load the packages
library(tidyverse)
library(data.table)

### HH: Load the household questionnaire ------------------------

# Create a wave vector
waves <- letters[1:21] # 21 Waves

# List the variables
vars_hh <- c("hhrhid", "hgxid", "hgsex", "hgage", "hhsize",
             "hhstate", "hhtype", "hifditp", "hifditn", "hhrih",
             "hhhqivw")

# Create a container for the household questionnaires
hh_long <- hh <- vector("list", length = length(waves))

# Loop over waves
for (wave in seq_along(waves)){

cat("Working on wave", wave, " \n")

# Load the first wave
path <- paste0("raw/STATA-files/Household_", waves[wave], "210c.dta")
dta  <- haven::read_dta(path) |> zap_labels()

# Remove the wave-letter at the beginning of the variable name
names(dta) <- str_remove(names(dta), paste0("^", waves[wave]))

# Select the important variables
dta <- dta |> select(matches(vars_hh))

# Transform the date
dta$hhhqivw[dta$hhhqivw %in% c("-4/-4/  -4", " ./  /" )] <- NA_character_
dta$hhhqivw <- dmy(dta$hhhqivw)

### Clean the multi factor variables ---------------------

# Insert a dash between numbers and letters
names(dta) <- sub("(\\d+)$", "_\\1", names(dta))

# Create long variables
dta1 <- long_hh_data(dta, "hgsex", new_variable = "sex")
dta2 <- long_hh_data(dta, "hgxid", new_variable = "pid")
dta3 <- long_hh_data(dta, "hgage", new_variable = "age")
dta4 <- long_hh_data(dta, "hhrih", new_variable = "rel")

# Combine the data
dta_long <- merge(dta1, dta2) |>
                merge(dta3)    |>
                merge(dta4)

# Filter persons
dta_long <- dta_long[pid != "-------", ]

# Add the wave
dta_long$wave <- wave

### Continue with the single item data -----------------------

# Assign the wave number to the variable "wave"
dta$wave <- wave

# Estimate the disposable annual household income
dta$inc_disp <- with(dta, hifditp - hifditn)

# Keep the single-item variables in the data
dta <- dta |> select(!contains("_"), !contains("hifdit"))

# Assign the data to the lists
hh[[wave]] <- dta
hh_long[[wave]] <- dta_long

}

# Bind the data
hh      <- rbindlist(hh)
hh_long <- rbindlist(hh_long)

# Rename variables
setnames(hh, c("hhrhid", "hhstate", "hhhqivw"),
             c("id_hh", "reg", "int_date"))

# Rename variables
setnames(hh_long, c("hhrhid"), c("id_hh"))

# Save the data
save(hh, file = "data/hh_cleaned.Rda")
save(hh_long, file = "data/hh_long_cleaned.Rda")

###### END ########################################