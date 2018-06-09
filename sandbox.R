library(tidyverse)
library(data.table)
library(corrplot)
library(viridis)
library(GGally)
library(fitdistrplus)
library(broom)

#Read data
lifetime.df <- fread("Courier_lifetime_data.csv", sep = ",", header= TRUE)
weekly.df <- fread("Courier_weekly_data.csv", sep = ",", header= TRUE)

#Add lifetime feature_1 into weekly
ids <-  match(weekly.df[["courier"]], lifetime.df[["courier"]])
b <- as.vector(lifetime.df[ids,2])
weekly.df$feature_18 <- b


#Correlation matrix and plot
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



weekly.df %>%
  group_by(courier) %>%
  summarise(n = n()) %>%
  ggplot() +
  geom_bar(aes(x = as.factor(n), y  = ..count../sum(..count..)*100, fill = (..count..))) +
  theme_minimal() +
  scale_fill_viridis(option = "D") +
  theme(legend.position = "NONE") +
  xlab("Number of Weeks worked") +
  ylab("(%)")

weekly.df %>%
  group_by(courier) %>%
  mutate(churn = (n() == 1)) %>%
  ggplot() +
  geom_histogram(aes(x = feature_1, fill = churn), position = "identity") +
  theme_minimal() +
  facet_grid(churn~., scale = "free_y") +
  scale_fill_viridis(discrete = TRUE)

weekly.df %>%
  group_by(courier) %>%
  mutate(churn = (n() == 1)) %>%
  ggplot() +
  geom_histogram(aes(x = feature_2, fill = churn), position = "identity") +
  theme_minimal() +
  facet_grid(churn~., scale = "free_y") +
  scale_fill_viridis(discrete = TRUE)

weekly.df %>%
  group_by(courier) %>%
  mutate(churn = (n() == 1)) %>%
  group_by(churn) %>%
  bootstrap(1e3, by_group=TRUE) %>%
  ggplot() +
  geom_histogram(aes(x = feature_3, fill = churn), position = "identity") +
  theme_minimal() +
  facet_grid(churn~., scale = "free_y") +
  scale_fill_viridis(discrete = TRUE)

churn_df <- weekly.df %>%
  group_by(courier) %>%
  mutate(churn = (n() == 1)) %>%
  filter(churn == TRUE)

nochurn_df <-	weekly.df %>%
  group_by(courier) %>%
  mutate(churn = (n() == 1)) %>%
  filter(churn == FALSE)

boot_df <- churn_df %>%
  sample_n(size = floor(nrow(nochurn_df)/nrow(churn_df)), replace = TRUE) %>%
  rbind(nochurn_df)




# Test Feature 3 ----------------------------------------------------------

library(MASS)
emp <- weekly.df$feature_3
hist(emp)


# Poisson
fit_poisson <- fitdistr(emp, "poisson")
theo <- rpois(length(emp), lambda = fit_poisson$estimate[["lambda"]])
qqplot(emp,theo)

# Gamma
fit_gamma <- fitdistr(emp, "gamma")
theo <- rgamma(length(emp), shape = fit_gamma$estimate[["shape"]], rate = fit_gamma$estimate[["rate"]])
qqplot(emp,theo)

# Weibull
fit_weibull <- fitdistr(emp, "weibull")
theo <- rweibull(length(emp), shape = fit_weibull$estimate[["shape"]], scale = fit_weibull$estimate[["scale"]])
qqplot(emp,theo)

# It looks quite like straight line, but let us make it sure

e0 <- ks.test(emp,
              "pweibull", shape = fit_weibull$estimate[["shape"]],
              scale = fit_weibull$estimate[["scale"]]
)$statistic

estimates <- c()
for(i in 1:1e5){
  estimates[i] <- ks.test(sample(emp, size = 1e2, replace = TRUE),
                          "pweibull", shape = fit_weibull$estimate[["shape"]],
                          scale = fit_weibull$estimate[["scale"]]
  )$statistic
}
z <- ecdf(estimates)
1-z(e0)


# Negative Binomial
x<- rnbinom(length(emp), size = fitdist(emp, "nbinom")$estimate[["size"]], mu = fitdist(emp, "nbinom")$estimate[["mu"]])
hist(x)
hist(emp)

e0 <- ks.test(emp,
              "pnbinom", size = fit_nbinom$estimate[["size"]],
              mu = fit_nbinom$estimate[["mu"]]
)$statistic

fit_nbinom <- fitdist(emp, "nbinom")
theo <- rnbinom(length(emp), size = fitdist(emp, "nbinom")$estimate[["size"]], mu = fitdist(emp, "nbinom")$estimate[["mu"]])

estimates <- c()
for(i in 1:1e5){
  
  estimates[i] <- ks.test(sample(emp, size = 1e2, replace = TRUE),
                          "pnbinom", size = fit_nbinom$estimate[["size"]],
                          mu = fit_nbinom$estimate[["mu"]]
  )$statistic
}

z <- ecdf(estimates)
1-z(e0)


rweibull(length(emp), shape = fit_weibull$estimate[["shape"]], scale = fit_weibull$estimate[["scale"]]) %>%
  trunc() %>%
  table() %>%
  barplot()

barplot(table(emp))



# Train Model -------------------------------------------------------------

#Read data
lifetime.df <- fread("Courier_lifetime_data.csv", sep = ",", header= TRUE)
weekly.df <- fread("Courier_weekly_data.csv", sep = ",", header= TRUE)

colnames(lifetime.df) <- c("courier", "feature_18", "feature_19")
total_df <- merge(weekly.df, lifetime.df, all = FALSE, by = "courier")


ids <- unique(total_df$courier)
total_df$target <- NA
x <- total_df %>% data.frame()
weeks_of_interest <- c(9,10,11)
courier <- x[["courier"]]
week <- x[["week"]]
for(i in ids){
  if(sum(week[which(courier == i)] %in% weeks_of_interest) == 3){
    x[x$courier == i,"target"] <- 0
  }else{
    x[x$courier == i,"target"] <- 1
  }
}
total_df

