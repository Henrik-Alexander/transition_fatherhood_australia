#######################################
# Purpose: Quality check              #
# Author: Henrik-Alexander Schubert   #
# Date: 14.06.2023                    #
# E-Mail: schubert@demogr.mpg.de      #
# Pre-Requisites: full repository     #
#######################################

# Load the functions
source("functions/packages.R")
source("functions/functions.R")
source("functions/graphics.R")

### Prepare the data from the human fertiity collection ---------------

# Load the data
hmc <- read.table("raw/male_fert_collec_AUS.txt", sep = ",", header = TRUE)

# Clean the data
hmc <- hmc  |>
        clean_names() |>
        select(country, year1, age, age_int, asfr, cpfr)

# Plot the data
ggplot(hmc, aes(x = age, y = asfr, colour = year1, group = year1)) + 
    geom_line()


### Load the survey data and estiamte the asfr -----------------------


### Compare the estimates --------------------------------------------



### END ##############################################################
