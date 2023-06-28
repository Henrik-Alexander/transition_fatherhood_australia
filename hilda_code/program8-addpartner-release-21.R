# Purpose of the program:
# =======================
# This program indicates how to merge partner information to the responding person file. 
# This example uses wave 1 data.
#
# Updated by: Mossamet Nesa
# Date: 3/11/2022


library(haven)
# library(tidyverse) # import tidyverse package if using left_join command

origdatdir <- "H:/orig-data" # Location of original HILDA data files
newdatadir <- "H:/new-data" # Location of writing new data files

# Use responding person file 
setwd(origdatdir)
rperson <- read_dta("Rperson_a210c.dta")

var <- c("xwaveid", "ahgage", "ahgsex") # Specify variables needed
partner <- rperson[, var] # Create a dataset with the variables of partners
colnames(partner) <- c("ahhpxid", "apartage", "apartsex") # Rename the variables in partner dataset

# Use tidyverse to create the subset and rename the variables as below:
# partner <- rperson %>% dplyr::select(xwaveid, ahgage, ahgsex) %>%
#                        rename(ahhpxid=xwaveid, apartage=ahgage, apartsex=ahgsex)


# Merge partner file to the responding person file
final_data <- merge(rperson, partner, by = "ahhpxid", suffixes = c("", ""), all.x = TRUE)  # Keeping all observations of rperson file
# final_data <- left_join(rperson, partner, by = "ahhpxid")
final_data <- final_data[order(final_data$xwaveid), ] # Sort the dataset by xwaveid

# Save new data set
setwd(newdatadir)
save(final_data, file = "rperson-partner.Rdata")
# write.table(final_data, file = "rperson-partner.txt", sep = ",", row.names = FALSE) # Can also save as a txt file

