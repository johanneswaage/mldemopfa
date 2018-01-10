library(tidyverse)
library(ggplot2movies)
library(scales)

imdb <- ggplot2movies::movies



# Explore variables -------------------------------------------------------
ggplot(data = imdb, aes(x=year)) +
  geom_histogram(fill = "salmon")

ggplot(data = imdb, aes(x=length)) +
  geom_density(fill = "salmon") +
  scale_x_log10()

sd_length <- sd(imdb$length)
mean_length <- mean(imdb$length)

ggplot(data = imdb, aes(x=length)) +
  geom_density(fill = "salmon") +
  scale_x_log10() +
  geom_vline(xintercept = c( mean_length-sd_length, mean_length+sd_length))

ggplot(data = imdb %>% filter(length > mean_length-sd_length), 
       aes(x=length)) + 
  geom_density(fill = "salmon") +
  scale_x_continuous(limits = c(0,200))

imdb %>% select(year, budget) %>% 
  na.omit %>% 
  group_by(year) %>% 
  ggplot(aes(x = factor(year), y = budget)) +
  geom_boxplot() +
  scale_y_log10() +
  theme(axis.text.x = element_text(angle = 90))


imdb %>% select(year, budget, Action) %>% 
  na.omit %>% 
  group_by(year) %>% 
  ggplot(aes(x = factor(year), y = budget)) +
  geom_boxplot() +
  scale_y_log10() +
  theme(axis.text.x = element_text(angle = 90))

imdb %>% select(year, budget, Action) %>% 
  na.omit %>%
  filter(year > 1990) %>% 
  group_by(year) %>% 
  ggplot(aes(x = factor(year), y = budget, fill = factor(Action))) +
  geom_boxplot() +
  scale_y_log10() +
  theme(axis.text.x = element_text(angle = 90))

# Model -------------------------------------------------------------------
# Linear regression
linear_model <- imdb %>% 
  select(budget, year, length, rating, r1,r2,r3,r4,r5, Action, Animation, Comedy, Drama, Documentary) %>% 
  na.omit() %>% 
#  sample_n(5000) %>% 
  lm(data = ., formula = rating ~ .)

linear_model %>% summary

# Can we predict if action movie
# install.packages("party")

library(caret)
initial_set <- imdb %>%
  select(-title) %>% 
  mutate(mpaa = as.factor(mpaa)) %>% 
  na.omit()

data_split <- caret::createDataPartition(y = initial_set$Action, p = 0.75)
train_set <- initial_set[data_split[[1]],]
test_set  <- initial_set[-data_split[[1]],]


# Logistic regression -----------------------------------------------------
log_reg <- glm(data = train_set, formula = factor(Action) ~ ., family = binomial(link='logit'))
new_predictions <- predict(log_reg, test_set, type = "response")
(round(new_predictions) == test_set$Action) %>% sum %>% magrittr::divide_by(nrow(test_set))

# Decision tree -----------------------------------------------------------
library(party)
tree_model <- ctree(data = train_set, formula = factor(Action) ~ .)
plot(tree_model)

new_predictions <- predict(tree_model, test_set)
(new_predictions == test_set$Action) %>% sum %>% magrittr::divide_by(nrow(test_set))

# Random forest -----------------------------------------------------------
library(randomForest)
random_forest <- randomForest(factor(Action) ~ ., data = train_set)
new_predictions <- predict(random_forest, test_set)
(new_predictions == test_set$Action) %>% sum %>% magrittr::divide_by(nrow(test_set))


# xgboost -----------------------------------------------------------------
library(xgboost)
train_set_dmatrix <- Matrix::sparse.model.matrix(~., data = train_set %>% select(-Action) %>% as.data.frame)
test_set_dmatrix  <- Matrix::sparse.model.matrix(~., data = test_set %>% select(-Action) %>% as.data.frame)

xgb <- xgboost(data = train_set_dmatrix, label = train_set$Action, nrounds = 50, params = list(objective = "binary:logistic"))

new_predictions <- predict(xgb, test_set_dmatrix)
(round(new_predictions) == test_set$Action) %>% sum %>% magrittr::divide_by(nrow(test_set))






