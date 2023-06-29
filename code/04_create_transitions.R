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
source("functions/functions.R")
source("functions/graphics.R")


### Load the data --------------------------------

#  Load the data
load("data/rp_data.Rda")
load("data/hh_cleaned.Rda")
load("data/hh_long_cleaned.Rda")
load("data/ep_cleaned.Rda")


# 1. Estimate births over consecutive waves --------

# Group and sort the data
rp <- rp[order(id, wave), ]

# Estimate the transitions
rp <- rp[ , .(birth = nchild - shift(nchild, n = 1, type = "lag")), by = id]

#

# 2. Estimate the person members in hh -------------

# Use hh data
head(hh_long)



# 3. Estimate the relatives outside hh -------------

# Load the ep data

# Load the 


# 4. Estimate deceased children ------------------

### Combine the different data sets --------------


### 

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