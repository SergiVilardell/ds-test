library(tidyverse)
library(data.table)
library(corrplot)
library(viridis)
library(GGally)

#Read data
lifetime.df <- fread("Courier_lifetime_data.csv", sep = ",", header= TRUE)
weekly.df <- fread("Courier_weekly_data.csv", sep = ",", header= TRUE)

#Add lifetime feature_1 into weekly 
ids <-  match(weekly.df[["courier"]], lifetime.df[["courier"]])
b <- as.vector(lifetime.df[ids,2])
weekly.df$feature_18 <- b


Correlation matrix and plot
corr.matrix <- round(cor(weekly.df[,-(1:2)]), 2)

ggplot(lifetime.df[lifetime.df$feature_2 %in% -10:100 ], aes(x = feature_2))+
  geom_histogram(binwidth = 1)+
  facet_wrap(~feature_1)

ggplot(lifetime.df, aes(x = feature_1))+
  geom_bar()

  
  
  
plot(lifetime.df$feature_2[lifetime.df$feature_2 %in% -10:100 ], col = as.factor(lifetime.df$feature_1))



#PRAISE HADLEY
ggpairs(weekly.df[, c(3:10)], aes(colour = weekly.df$feature_18, alpha = 0.5))






#PRAISE HADLEY
a <- ggpairs(weekly.df[, c(3:10)], aes(colour = weekly.df$feature_18, alpha = 0.5))
ggally_cor(weekly.df[, c(3:10)])


corrplot(corr.matrix, method = "color", col = viridis(200),
		 type = "upper", order = "hclust", number.cex = .7,
		 addCoef.col = "black", 
		 tl.col = "black",
		 diag = FALSE)




