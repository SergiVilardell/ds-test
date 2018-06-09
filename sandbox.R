library(tidyverse)
library(data.table)
library(corrplot)
library(viridis)
library(GGally)
library(fitdistrplus)
library(broom)
library(magrittr)

#Read data
lifetime.df <- fread("Courier_lifetime_data.csv", sep = ",", header= TRUE)
weekly.df <- fread("Courier_weekly_data.csv", sep = ",", header= TRUE)

#Add lifetime feature_1 into weekly
ids <-  match(weekly.df[["courier"]], lifetime.df[["courier"]])
b <- as.vector(lifetime.df[ids,2])
weekly.df$feature_18 <- b


# NAs ---------------------------------------------------------------------
lifetime.df <- lifetime.df %>% 
  mutate( is_na = ifelse(is.na(feature_2), T, F))

table.NA <- table(lifetime.df$is_na, lifetime.df$feature_1)


list.NA <- list()
 
group = c("a", "b", "c", "d")

for( i in 1:4){
beh <- lifetime.df %>%
    filter(feature_1 == group[i], is_na == F) 
  
list.NA[[i]] <- sample(beh$feature_2, table.NA[,group[i]][2], replace = T)
}

a.df <-  lifetime.df%>% 
  filter(feature_1 == "a") 

a.df$feature_2[is.na(boh$feature_2)] <- list.NA[[1]][1:sum(boh$is_na)]

# Plots AL bleh -----------------------------------------------------------


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

colnames(lifetime.df) <- c("courier", "feature_18", "feature_19")
total.df <- merge(weekly.df, lifetime.df, all = FALSE, by = "courier")


ids <- unique(total.df$courier)
total.df$target <- NA
train.df <- total.df %>% data.frame()
weeks.of.interest <- c(9,10,11)
courier <- train.df[["courier"]]
week <- train.df[["week"]]
for(i in ids){
  if(sum(week[which(courier == i)] %in% weeks.of.interest) == 3){
    train.df[train.df$courier == i,"target"] <- 0
  }else{
    train.df[train.df$courier == i,"target"] <- 1
  }
}

train.df <- train.df[, -c(20,21)] 
train.df %<>% 
  group_by(courier) %>% 
  filter(!(week %in% c(8,9,10,11))) %>% 
  summarise_all("mean")


