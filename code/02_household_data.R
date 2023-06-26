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




### HH: Load the household questionnaire ------------------------

### Variables #######################################
# sex: hgsex1-15
# age: hgage1-15
# id : xwaveid
# hid: hhrhid, Random Household ID -> changes across waves
# mid: hhbmxid, Mother ID
# fid: hhbfxid, Father ID
# hh_size
# _hgage = Age last birthday at JUne 30 2021
# _hgyob = year of birth

#########################################

# Create a wave vector
waves <- letters[1] # 21 Waves

# List the variables
vars_hh <- c("hgsex", "hgage", "xwaveid", "hhrid", "hhbmxid", "hhbfxid", "hh_size", "hgage", "hgyob")

# Create a container for the household questionnaires
hh <- vector("list", length = length(waves))

# Loop over waves
for(wave in seq_along(waves)){
  
  cat("Working on wave", wave, " \n")

# Load the first wave
path <- paste0("raw/STATA-files/Household_", waves[wave], "210c.dta")

# Load the data
dta  <- haven::read_dta(path) |> as.data.frame()

# Remove the beginning of the variables
names(dta) <- str_remove(names(dta), paste0("^", waves[wave]))


# Children variables
#dta <- dta |> select( matches(vars))

# Assign the wave
dta$wave <- wave


# Create long data of respondents


}

# Bind the data
hh <- rbindlist(hh)




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




# Children variables
dta <- dta |> select( matches(vars))

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
###### END ########################################
