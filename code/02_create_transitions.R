#######################################
# Purpose: Creating event data        #
# Author: Henrik-Alexander Schubert   #
# Date: 14.06.2023                    #
# E-Mail: schubert@demogr.mpg.de      #
# Pre-Requisites: full repository     #
#######################################

rm(list = ls())

# Load the packages
library(tidyverse)

# Load the functions
#source("Functions/packages.R")
source("Functions/functions.R")
source("Functions/graphics.R")

# Load the data
load("data/waves.Rda")

### Load the data --------------------------------

# Arrange the data
data <- data |> 
  group_by(pid) |>
  arrange(int_date)

# Create a birth variable
data <- data |> 
  mutate(birth = ifelse(lag(tchad) != tchad & !is.na(tchad) & !is.na(tchad), 1, 0))

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