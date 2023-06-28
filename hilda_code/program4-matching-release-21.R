# Purpose of the program:
# =======================
# Merge wave 1 household and responding person files.
#
# Created by: Mossamet Nesa
# Date 3/11/2022

library(haven)
# library(tidyverse) # import tidyverse package if using left_join command

origdatdir <- "H:/orig-data" # Location of original HILDA data files
newdatadir <- "H:/new-data" # Location of writing new data files

# Use responding person file and household file
setwd(origdatdir)
rperson <- read_dta("Rperson_a210c.dta") # Here uses STATA data files as an example
hh <- read_dta("Household_a210c.dta") 

var <- c("ahhrhid", "ahifefn", "ahifefp", "ahifdip", "ahifdin")
hh_temp <- hh[, var] # Create a dataset with the subset of household variables

# Merge household file to the responding person file
final_data <- merge(rperson, hh_temp, by = "ahhrhid", suffixes = c("", ""), all.x = TRUE)  # Keeping all observations of rperson file
# final_data <- left_join(rperson, hh_temp, by = c("ahhrhid" = "ahhrhid"))

# Save new data set
setwd(newdatadir)
save(final_data, file = "person-2001.Rdata")
# write.table(final_data, file = "person-2001.txt", sep = ",", row.names = FALSE) # Can also save as a txt file









