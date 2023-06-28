#######################################
# Purpose: Creating event data        #
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

# Load the data
load("data/waves.Rda")

### Load the data --------------------------------

# Load the final data
head(data)