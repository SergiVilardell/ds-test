library(tidyverse)
library(data.table)

#Read data
lifetime_df <- fread("Courier_lifetime_data.csv", sep = ",", header= TRUE)
weekly_df <- fread("Courier_weekly_data.csv", sep = ",", header= TRUE)


