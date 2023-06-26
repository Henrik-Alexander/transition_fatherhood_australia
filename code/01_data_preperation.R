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

### Load the data --------------------------------

# Create a wave vector
waves <- letters[1:21]

# Create a container
data <- vector("list", length = length(waves))

# Loop over waves
for(wave in seq_along(waves)){
  
  cat("Working on wave", wave, " \n")

# Load the first wave
path <- paste0("raw/STATA-files/Combined_", waves[wave], "210c.dta")

# Load the data
dta  <- haven::read_dta(path) |> as.data.frame()

# Variables to be selected for the analysis
vars <- paste0(waves[wave], c("xwaveid", "hhrpid",  # Panel information
                              "hhrhid", "hhhqivw", "hhpers", # Household information
                              "hhpers", "hhsm",    # Sample and household information
                              "lsrelsp", "ordfmar", # Relationship and marriage
                             "tchad", "chave",      # Number of children
                             "hhiage",             # Respondents age
                             "cepn",                # Child's person number
                             "hgsex", "hgyob", "yodeath",    # Demographics
                              "hhwte", "hhwtrps", "hhwtrp")) # Weigths 

# Assign the wave
dta$wave <- wave

# Children variables
dta <- dta |> select( matches(vars))

# Remove the beginning of the variables
names(dta) <- str_remove(names(dta), paste0("^", waves[wave]))

# Rename variables
dta <- dta |> 
  rename(
    pid = hhrpid,
    hid = hhrhid,
    int_date = hhhqivw,
    hh_per = hhpers,
    rel = lsrelsp,
    mar = ordfmar,
    yob = hgyob,
    wht_des = hhwte,
    wht_rep = hhwtrps,
    wht_rep2 = hhwtrp
    
  )

# Assign the data to the list
data[[wave]] <- dta 

}

# Combine the data
data <- bind_rows(data)

# Filter the respondents sex varables
data <- data |> 
  pivot_longer(cols = matches("sex"), 
               names_to = "respondent", 
               values_to = "sex", 
               names_prefix = "hgsex") |> 
  mutate(respondent = as.double(respondent)) |> 
  filter(respondent == hh_per) |> 
  select(-respondent)

# Mutate the data
data <- data |> mutate(int_date = dmy(int_date))

# Save the data
save(data, file = "data/waves.Rda")

###### END ########################################
