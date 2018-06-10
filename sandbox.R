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

#GUARRO

a.df <-  lifetime.df%>% 
  filter(feature_1 == "a") 

a.df$feature_2[is.na(a.df$feature_2)] <- list.NA[[1]][1:sum(a.df$is_na)]

b.df <-  lifetime.df%>% 
  filter(feature_1 == "b") 

b.df$feature_2[is.na(b.df$feature_2)] <- list.NA[[2]][1:sum(b.df$is_na)]

c.df <-  lifetime.df%>% 
  filter(feature_1 == "c") 

c.df$feature_2[is.na(c.df$feature_2)] <- list.NA[[3]][1:sum(c.df$is_na)]

d.df <-  lifetime.df%>% 
  filter(feature_1 == "d") 

d.df$feature_2[is.na(d.df$feature_2)] <- list.NA[[4]][1:sum(d.df$is_na)]


lifetime.df <- rbind(a.df, b.df, c.df, d.df)





#Add lifetime feature_2 into weekly
ids <-  match(weekly.df[["courier"]], replaced.df[["courier"]])
b <- as.vector(replaced.df[ids,3])
weekly.df$feature_19 <- b


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



train.df %<>% 
  group_by(courier) %>% 
  filter(!(week %in% c(8,9,10,11))) %>% 
  summarise_all("mean")

#Add lifetime feature_1 into weekly
ids <-  match(train.df[["courier"]], lifetime.df[["courier"]])
b <- as.vector(lifetime.df[ids,2])
train.df$feature_18 <- b

train.df <- train.df[, -20]
write_csv(train.df, "train.csv")




# Random Forest -----------------------------------------------------------


library(randomForest)

train.df <- fread("train.csv", sep = ",", header= TRUE)
train.df$target <- as.factor(train.df$target)
train.df$feature_18 <- as.factor(train.df$feature_18)

set.seed(666)
one.df <- train.df %>% 
  filter(target == 1) %>% 
  sample_n(123)

zero.df <- train.df %>% 
  filter(target == 0) 

train.sampled <- rbind(zero.df, one.df)





rf_model <- randomForest(formula = target ~ ., 
                         data = train.sampled, 
                         mtry = 4, 
                         ntree = 8000, 
                         nodesize=15
                         )
print(rf_model)


# Show model error
plot(rf_model$err.rate[,1],ylab = "OOB error", xlab = "Trees")
plot(rf_model$err.rate[,2],ylab = "0 class. error", xlab = "Trees")
plot(rf_model$err.rate[,3],ylab = "1 class. error", xlab = "Trees")
legend('topright', colnames(rf_model$err.rate), col=1:3, fill=1:3)

## IMPORTANCE OF THE VARIABLES ##

# Get importance
table(rf_model$confusion)
varImpPlot(rf_model)

# MEASURING THE PREDICTIVE ABILITY OF THE MODEL
rf.pred <- predict(rf_model, rf.test[,-1])
rf.pred <- ifelse(rf.pred> 0.5,1,0)
confmat <- table(observed = rf.test[, "is_duplicate"], predicted = rf.pred)
result <- (confmat[1,1]+confmat[2,2]+3441)/(sum(confmat)+3441)

