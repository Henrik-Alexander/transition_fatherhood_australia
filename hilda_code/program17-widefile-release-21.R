# Purpose of the program:
# =======================
# This program creates an unbalanced and a balanced longitudinal wide data file, using the combined files from each of the first 5 waves.
# The new data files are in R's wide format.
# 
# Created by: Mossamet Nesa
# Date: 3/11/2022


library(haven)
library(tidyverse)

wave <- 5 #  Number of wave data files to extract. Here uses wave=5 as an example.
maxwave <- 21 # Update to the latest wave.
rls <- 210 # Update to the latest release.
origdatdir <- paste0("H:/HILDA/Release ",rls,"/files/STATA ",rls,"c") # Location of original HILDA data files
newdatadir <- "H:/new-data" # Location of writing new data files

# SECTION 1: Creating an unbalanced dataset (wide-format)

setwd(origdatdir)
var <- c("hhrhid", "hhrpid", "hhpxid", "hhresp", "hhstate", "hhsos", "ancob", "losathl", "wsce",
         "wscei", "wscef", "wscme", "wscmei", "wscmef", "wscoe", "wscoei", "wscoef")

for( i in 1:wave) {
  file_list <- paste0("Combined_", letters[i], rls,"c.dta")
  temp <- read_dta(file_list)
  var_add <- paste0(letters[i], var) # Add wave letter onto the variable names
  temp <- temp %>% dplyr::select(xwaveid, any_of(var_add)) # any_of() lets the program avoid selecting the variable not included in a specific wave and set NA for that variable.
  names(temp)[-1] <- substring(names(temp)[-1], 2) # Remove wave letter from variable names except for xwaveid
  names(temp)[-1] <- paste0(names(temp)[-1], "_w", i) # Use _w1 format to rename the variables in wave 1
  if (i == 1 ){
    widefile <- temp
  } else {
    widefile <- merge(widefile, temp, by = "xwaveid", all.x = TRUE, all.y = TRUE) 
  }
}

# Use the master file
master_file <- paste0("Master_", letters[maxwave], rls,"c.dta")
master <- read_dta(master_file)
master <- master[c("xwaveid", "ivwptn")] # Can keep more variables 
final_data <- merge(master, widefile, by = "xwaveid",  suffixes = c("", ""), all.x = TRUE) # "xwaveid" is the only common variable in these two datasets, so we match on "xwaveid"

# Save new data set
setwd(newdatadir)
save(final_data, file = "wide-file-unbalanced.Rdata")
# write_dta(final_data, "wide-file-unbalanced.dta") # Could save as a stata data file

# SECTION 2: Creating a balanced dataset (wide-format)
# We can use the variable ivwptn which contains the interview pattern for each person.

# final_data = load("wide-file-unbalanced.Rdata") # import dataset if needed. Be aware of current directory.
intvw_pattern <- paste(rep("X", wave), collapse = "") # Create the pattern that people have been interviewed in each of the first 5 waves
balwide = final_data[substr(final_data$ivwptn, 1, wave) == intvw_pattern, ] # Keep people that have been interviewed in each of the first 5 waves

setwd(newdatadir) # Set the directory to save new data file
save(balwide, file = "wide-file-balanced.Rdata")
# write.table(balwide, file = "wide-file-balanced.txt", sep = ",", row.names = FALSE) # Can also save as a txt file
