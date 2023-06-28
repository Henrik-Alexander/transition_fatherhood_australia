# Purpose of the program:
# =======================
# This program creates an unbalanced and a balanced longitudinal long data file, using the combined files from each of the first 5 waves.
# The new data files are in R's long format.
#
# Updated by: Mossamet Nesa
# Date: 3/11/2022 


library(haven)
library(tidyverse)

wave <- 5      #  Number of wave data files to extract. Here uses wave=5 as an example.
maxwave <- 21  # Update to the latest wave.
rls <- 210     # Update to the latest release.
origdatdir <- paste0("H:/HILDA/Release ",rls,"/files/STATA ",rls,"c") # Location of original HILDA data files
newdatadir <- "H:/new-data" # Location of writing new data files

# SECTION 1: Creating an unbalanced dataset (long-format)

setwd(origdatdir)
# Could adjust for personal needs.
var <- c("hwhmhl", "hhrhid", "hhrpid", "hhpxid", "hhresp", "hhstate", "hhsos", "ancob", "losathl", "wsce",
         "wscei", "wscef", "wscme", "wscmei", "wscmef", "wscoe", "wscoei", "wscoef")

for( i in 1:wave) {
  file_list <- paste0("Combined_", letters[i], rls, "c.dta")
  temp <- read_dta(file_list)
  var_add <- paste0(letters[i], var) # Add wave letter onto the variable names
  temp <- temp %>% dplyr::select(xwaveid, any_of(var_add)) 
  # any_of() lets the program avoid selecting the variable not included in a specific wave and set NA to that variable. eg: "hwhmhl" not included in wave 1
  names(temp)[-1] <-substring(names(temp)[-1], 2) # Remove wave letter from variable names except for xwaveid
  temp$wave <- i
  if (i == 1 ){
    longfile <- temp
  } else {
    longfile <- bind_rows(longfile, temp) # Append the data file from each wave
  }
}

# Save new data set
setwd(newdatadir)
save(longfile, file = "long-file-unbalanced.Rdata")
# write_dta(longfile, "long-file-unbalanced.dta") # Could save as a stata data file

# SECTION 2: Creating a balanced dataset (long-format)
# We can use the variable ivwptn which contains the interview pattern for each person.

# Use the master file
setwd(origdatdir)
master_file <- paste0("Master_", letters[maxwave], rls,"c.dta")
master <- read_dta(master_file)
master <- master[c("xwaveid", "ivwptn")] # Can keep more variables 

intvw_pattern <- paste(rep("X", wave), collapse = "") # Create the pattern that people have been interviewed in each of the first 5 waves
master_long = master[substr(master$ivwptn, 1, wave) == intvw_pattern, ] # Keep people that have been interviewed in each of the first 5 waves
final_data <- merge(master_long, longfile, by = "xwaveid", all.x = TRUE)
final_data <- final_data[order(final_data$xwaveid, final_data$wave), ] # Sort the dataset by xwaveid and wave

# Save new data set
setwd(newdatadir)
save(final_data, file = "long-file-balanced.Rdata")
# write.table(final_data, file = "long-file-balanced.txt", sep = ",", row.names = FALSE) # Can also save as a txt file


