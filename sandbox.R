library(tidyverse)
library(data.table)
library(corrplot)
library(viridis)

#Read data
lifetime.df <- fread("Courier_lifetime_data.csv", sep = ",", header= TRUE)
weekly.df <- fread("Courier_weekly_data.csv", sep = ",", header= TRUE)


#Correlation matrix and plot
corr.matrix <- round(cor(weekly.df[,-(1:2)]), 2)

corrplot(corr.matrix, method = "color", col = viridis(200),
		 type = "upper", order = "hclust", number.cex = .7,
		 addCoef.col = "black", 
		 tl.col = "black",
		 diag = FALSE)


#Add lifetime feature_1 into weekly 

a <-  which(lifetime.df[["courier"]] == weekly.df[["courier"]])
b <- lifetime.df[ids,2]
weekly.df[["feature_18"]] <- b


