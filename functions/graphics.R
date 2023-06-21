### Male fertility in household surveys


## This file adjusts the graphic style for the reproduction of the graphs



### 1. Graphi Scheme -------------------------------


library(tidyverse)

# set theme
theme_set(theme_test(base_size = 14, base_family = "serif"))
theme_update(plot.margin = margin(0.1, 0.6, 0.1, 0.1, "cm"),
             panel.grid.major.y = element_line(colour = "grey80"),
             panel.grid.major.x = element_blank(),
             panel.grid.minor.x = element_blank(),
             panel.grid.minor.y = element_blank(),
             legend.background = element_rect(fill = "white"),
             legend.title = element_text(face = "bold"),
             legend.position = "bottom",
             legend.key.width = unit(2, "cm"),
             legend.key.height = unit(0.2, "cm"),
             axis.title.x = element_text(face = "bold", size = 14),
             axis.title.y = element_text(face = "bold", size = 14),
             plot.title = element_text(hjust = 0.5),
             title = element_text(face = "bold")
)



### Basic colours #############################################################
MPIDRgreen <- "#066E6E"
MPIDRpurple <- "#3E2C51"
MPIDRred <- "#8E2A3B"
MPIDRorange <- "#EF7D00"
MPIDRblue <- "#08445F"
MPIDRyellow <- "#FAAF3B"

# Create a pallete
MPIDRpallette <- c(MPIDRgreen, MPIDRorange, MPIDRred, MPIDRpurple, MPIDRblue)
