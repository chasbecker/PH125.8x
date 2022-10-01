# Eighth module in the professional certificate series
# rm(list=ls())
# install.packages("installr")
# run in RGui
# library(installr)
# updateR()
# library(tidyverse)

# Example code: downloading data from the FRED repository
# library(fredr)
# FRED API key set in .Renviron file !!!!!
# unemploymentRate <- tibble( fredr("UNRATE") )
# str(unemploymentRate)
# unemploymentRateTrimmed <- select( unemploymentRate, -c( series_id, realtime_start, realtime_end ))

# n <- 1000000
# vct1 <- integer(n)
# for ( i in 1:n ){
#   vct1[i] <- i+7
# }
# vct1[897000]

# Begin 125_8 course material
# install.packages("caret")
rm(list = ls())
library( tidyverse )
library(caret)
library( dslabs )
data( heights )

y <- heights$height
x <- heights$sex
set.seed(2, sample.kind="Rounding")
test_index <- createDataPartition(y, times=1, p=0.5, list=FALSE)
test_set <- heights[test_index,]
train_set <- heights[-test_index,]

# simply guessing, male or female
# y_hat <- sample( c("Male", "Female"),length(test_index), replace=TRUE   )
y_hat <- sample( c("Male","Female"), length(test_index), replace=TRUE) %>%
  factor(levels = levels(test_set$sex))
mean( y_hat == test_set$sex )

zum <- heights %>% group_by(sex) %>% summarize(mean(height), sd(height))
param_male <- filter( zum, sex=="Male")
mean_param_male <- param_male$`mean(height)`
sd_param_male <- param_male$'sd(height)'
pred <- mean_param_male - ( 2 * sd_param_male )

heights %>% group_by(sex) %>% summarize(mean(height), sd(height))
# sample code read "x > 62" but that must have been a typo
y_hat <- ifelse( y > 66, "Male", "Female") %>% factor(levels = levels(test_set$sex))
# most accurate when y > [64 or 65]
mean(y_hat == factor(heights$sex))

cutoff <- seq(61, 70)
accuracy <- map_dbl( cutoff, function(co){
  y_hat <- ifelse( train_set$height > co, "Male", "Female" ) %>%
    factor( levels = levels( test_set$sex ) )
  mean( y_hat == train_set$sex)
} )

data.frame( cutoff, accuracy ) %>%
  ggplot(aes(cutoff, accuracy)) +
  geom_point() +
  geom_line()
max(accuracy)

best_cutoff <- cutoff[which.max(accuracy)]
best_cutoff

y_hat <- ifelse(test_set$height > best_cutoff, "Male", "Female") %>% 
  factor(levels = levels(test_set$sex))
y_hat <- factor(y_hat)
mean(y_hat == test_set$sex)

table( predicted = y_hat, actual = test_set$sex )
test_set %>%
  mutate( y_hat = y_hat ) %>%
  group_by( sex ) %>%
  summarize( accuracy = mean( y_hat == sex ) )
prev <- mean( y_hat == "Male" )

confusionMatrix( data = y_hat, reference = test_set$sex )

# balanced accuracy and F1

cutoff <- seq(61,70)
F_1 <- map_dbl( cutoff, function(co){
  y_hat <- ifelse( train_set$height > co, "Male", "Female" ) %>%
    factor( levels = levels( test_set$sex ))
  F_meas( data = y_hat, reference = train_set$sex )
})

data.frame( cutoff, F_1 ) %>%
  ggplot( aes( cutoff, F_1 )) +
  geom_point() +
  geom_line()

max(F_1)

best_cutoff <- cutoff[which.max(F_1)]
best_cutoff

y_hat <- ifelse( test_set$height > best_cutoff, "Male", "Female" ) %>%
  factor( levels = levels( test_set$sex ))
sensitivity( data = y_hat, reference = test_set$sex )
specificity( data = y_hat, reference = test_set$sex )

# ROC and PR graphs
p <- 0.9
n <- length(test_index)
y_hat <- sample( c("Male", "Female"), n, replace = TRUE, prob = c(p, 1-p) ) %>%
  factor( levels = levels(test_set$sex) )
mean( y_hat == test_set$sex )

probs <- seq( 0, 1, length.out = 10 )
guessing <- map_df( probs, function(p){
  y_hat <- sample(c("Male", "Female"), n, replace = TRUE, prob = c(p, 1-p) ) %>%
    factor( levels = c("Female", "Male") )
  list( method =  "Guessing", FPR = 1- specificity( y_hat, test_set$sex),
        TPR = sensitivity( y_hat, test_set$sex ))
})
guessing %>% qplot( FPR, TPR, data = ., xlab = "1 - Specificity", ylab = "Sensitivity" )

cutoffs <- c( 50, seq(60,75), 80)
height_cutoff <- map_df( cutoffs, function(co){
  y_hat <- ifelse( test_set$height > co, "Male", "Female" ) %>%
    factor( levels = c("Female", "Male") )
  list( method = "Height cutoff",
        FPR = 1 - specificity( y_hat, test_set$sex),
        TPR = sensitivity( y_hat, test_set$sex))
})

bind_rows( guessing, height_cutoff ) %>%
  ggplot( aes( FPR, TPR, color = method ) ) +
  geom_line() +
  geom_point() +
  xlab( "1 - Specificity") +
  ylab( "Sensitivity" )

library(ggrepel)

map_df( cutoffs, function(co){
  y_hat <- ifelse( test_set$height > co, "Male", "Female") %>%
    factor( levels = c("Female", "Male") )
  list( method = "Neight cutoff",
        cutoff = co,
        FPR = 1 - specificity( y_hat, test_set$sex ),
        TPR = sensitivity( y_hat, test_set$sex))
}) %>%
  ggplot( aes( FPR, TPR, label = cutoff)) +
  geom_line() +
  geom_point() +
  geom_text_repel(nudge_x = 0.01, nudge_y = -0.01)

guessing <- map_df( probs, function(p){
  y_hat <- sample( c("Male","Female"), length(test_index),
                   replace = TRUE, prob = c( p, 1-p ) ) %>%
    factor( levels = c("Female", "Male"))
  list( method = "Guess",
        recall = sensitivity(y_hat, test_set$sex ),
        precision = precision( y_hat, test_set$sex ))
})

height_cutoff <- map_df( cutoffs, function(co){
  y_hat <- ifelse(test_set$height > co, "Male", "Female") %>%
    factor( levels = c("Female", "Male") )
  list( method = "Height cutoff",
        recall = sensitivity( y_hat, test_set$sex),
        precision = precision( y_hat, test_set$sex))
})

bind_rows( guessing, height_cutoff ) %>%
  ggplot( aes( recall, precision, color = method )) +
  geom_line() +
  geom_point()

bind_rows(guessing, height_cutoff) %>%
  ggplot(aes(recall, precision, color = method)) +
  geom_line() +
  geom_point()


guessing <- map_df(probs, function(p){
  y_hat <- sample(c("Male", "Female"), length(test_index), replace = TRUE, 
                  prob=c(p, 1-p)) %>% 
    factor(levels = c("Male", "Female"))
  list(method = "Guess",
       recall = sensitivity(y_hat, relevel(test_set$sex, "Male", "Female")),
       precision = precision(y_hat, relevel(test_set$sex, "Male", "Female")))
})

height_cutoff <- map_df(cutoffs, function(x){
  y_hat <- ifelse(test_set$height > x, "Male", "Female") %>% 
    factor(levels = c("Male", "Female"))
  list(method = "Height cutoff",
       recall = sensitivity(y_hat, relevel(test_set$sex, "Male", "Female")),
       precision = precision(y_hat, relevel(test_set$sex, "Male", "Female")))
})
bind_rows(guessing, height_cutoff) %>%
  ggplot(aes(recall, precision, color = method)) +
  geom_line() +
  geom_point()

rm(list = ls())
library( tidyverse ) # includes dplyr
library( lubridate )
library(caret) # this is ML, right?
library( dslabs )
data( reported_heights )


# Q1 - Q6
dat <- mutate( reported_heights, date_time = ymd_hms( time_stamp)) %>%
  filter( date_time >= make_date( 2016,01,25 ) & date_time < make_date( 2016, 02, 1 )) %>%
  mutate( type = ifelse( day(date_time) == 25 & hour(date_time) == 8 & between( minute(date_time), 15, 30), "inclass", "online")) %>%
  select( sex, type )

class(dat$sex)
# dat$sex is a character type.  convert to a factor type....

y <- factor(dat$sex, c("Female", "Male"))
x <- dat$type

tclass <- data.frame( y, x )

iclass <- tclass %>% filter( x == "inclass")
mean(iclass$y == "Female")

oclass <- tclass %>% filter( x == "online")
mean(oclass$y == "Female")

y_hat <- ifelse( tclass$x == "inclass", "Female", "Male") %>% 
  factor(levels = levels(tclass$y)) 
mean(y_hat==y)

t1 <- table(y_hat, y)
t1
sensitivity(t1)
specificity(t1)

mean( y == "Female")

rm(list = ls())
library( tidyverse ) # includes dplyr
library( lubridate )
library(caret) # this is ML, right?
library( dslabs )
data( iris )

# Q7 - Q11
iris <- iris[-which(iris$Species=='setosa'),]
y <- iris$Species
y

set.seed(2, sample.kind='Rounding')
test_index <- createDataPartition(y, times=1, p=0.5, list=FALSE)
test <- iris[test_index,]
train <- iris[-test_index,]

# Q8
f <- function(x){
  rv <- seq(min(x), max(x), by=0.1) #rv = ranged values
  sapply(rv,
         function(i){
           y_hat <- ifelse(x > i,'virginica','versicolor')
           mean(y_hat == train$Species)} #here we can find the accuracy 
  )}

predictions <- apply(train[,-5],MARGIN = 2, FUN = f)
sapply(predictions, max) 

predictions <- f(train[,3])
# max(predictions)
# min(predictions)
rv <- seq( min(train[,3]), max(train[,3]), by=0.1 ) 

cutoffs <- rv[which(predictions == max(predictions))]
y_hat <- ifelse( test[,3]>cutoffs[1], 'virginica', 'versicolor' )
mean( y_hat == test$Species )

# Q10
predictions_t <- apply(test[,-5],MARGIN = 2, FUN = f)
sapply(predictions_t,max)

# Q11
plot( iris, pch=21, bg=iris$Species )

# Q12
LPetal <- seq( min(train$Petal.Length), max(train$Petal.Length), by=0.1 )
WPetal <- seq( min(train$Petal.Width), max(train$Petal.Width), by=0.1 )

l_predict <- sapply( LPetal, function(i){
  y_hat <- ifelse( train$Petal.Length > i, 'virginica', 'versicolor')
  mean( y_hat == train$Species )
})
l_cutoff <- LPetal[which.max(l_predict)]

w_predict <- sapply( WPetal, function(i){
  y_hat <- ifelse( train$Petal.Width > i, 'virginica', 'versicolor')
  mean( y_hat == train$Species )
})
w_cutoff <- WPetal[which.max(w_predict)]

y_hat <- ifelse( test$Petal.Length>l_cutoff | test$Petal.Width>w_cutoff, 'virginica', 'versicolor')
mean( y_hat==test$Species)

rm(list=ls())
library(tidyverse)

# Q1
tp <- .85
pr <- .02
fp <- .90
# TruPos*Prev divided by TruPos*Prev plus (1-FlsPos)*(1-Prev)
prob <-  (tp*pr)/((tp*pr)+((1-fp)*(1-pr)))
prob

bayes_func <- function( tp, pr, fp ){
  return( (tp*pr)/((tp*pr)+((1-fp)*(1-pr))) )
}

bayes_func_prob <- bayes_func(tp, pr, fp)
rm(list=ls())

set.seed(1, sample.kind = "Rounding")
disease <- sample( c(0,1), size = 1e6, replace = TRUE, prob = c(0.98, 0.02 ) )
test <- rep( NA, 1e6 )
test[disease == 0] <- sample( c(0,1), size = sum(disease == 0), replace = TRUE, prob = c(0.90, 0.10))
test[disease == 1] <- sample( c(0,1), size = sum(disease == 1), replace = TRUE, prob = c(0.15, 0.85))
mean(test)
mean(disease[test==0])
tp <- mean(disease[test==1]==1)
pr <- mean(disease)
tp/pr

# Q6
rm(list=ls())
library(tidyverse)
library(dslabs)
data("heights")
ps <- seq(0, 1, 0.1)
heights %>% 
  # MISSING CODE
  mutate(g = cut(height, quantile(height, ps), include.lowest = TRUE )) %>%
  group_by(g) %>%
  summarize(p = mean(sex == "Male"), height = mean(height)) %>%
  qplot(height, p, data =.)

#install.packages("MASS")
library(MASS)

Sigma <- 9*matrix(c(1, 0.5, 0.5, 1), 2, 2)
dat <- MASS::mvrnorm( n=10000, c( 69,69 ), Sigma ) %>%
  data.frame() %>%
  setNames(c("x", "y"))
plot(dat)


pst <-seq(0,1,0.1)
dat %>%  mutate(g = cut(x, quantile(x, pst), include.lowest = TRUE)) %>%
  group_by(g) %>%
  summarize(y = mean(y), x = mean(x)) %>%
  qplot( x, y, data = . )

# what is this DOING?
xyg <- dat %>%  mutate(g = cut(x, quantile(x, pst), include.lowest = TRUE))
xyg


rm(list=ls())
library(tidyverse)
# install.packages("HistData")
library(HistData)
# make sure the MASS library is not loaded
# if it is, must use "dplyr::select()" syntax

galton_heights <- GaltonFamilies %>%
  filter( childNum == 1 & gender == "male" ) %>%
  select( father, childHeight ) %>%
  rename( son = childHeight )

library(caret)
y <- galton_heights$son
test_index <- createDataPartition(y, times = 1, p = 0.5, list = FALSE )

train_set <- galton_heights %>% slice( -test_index )
test_set <- galton_heights %>% slice( test_index )

avg <- mean( train_set$son )
avg

mean( ( avg - test_set$son )^2 )

fit <- lm( son ~ father, data = train_set )
fit$coef

y_hat <- fit$coef[1] + fit$coef[2]*test_set$father
mean( ( y_hat - test_set$son)^2 )

y_hat <- predict( fit, test_set )
mean( (y_hat - test_set$son)^2 )

# Comprehension Check: Linear Regression

rm(list=ls())
library( tidyverse )
library( caret )
library( MASS )

set.seed( 1, sample.kind = "Rounding" )
n <- 100
Sigma <-  9*matrix( c(1.0, 0.5, 0.5, 1.0), 2, 2 )
dat <- MASS::mvrnorm( n = 100, c(69,69), Sigma ) %>%
  data.frame() %>%
  setNames( c("x", "y") )

set.seed( 1, sample.kind = "Rounding" )
rmse <- replicate( n, { 
  
  test_index <- createDataPartition( dat$y, times = 1, p = 0.5, list = FALSE )
  train_set <- dat %>% slice( -test_index )
  test_set <- dat %>% slice( test_index )
  
  fit <- lm( y ~ x, data = train_set )
  
  y_hat <- predict( fit, newdata = test_set )
  
  sqrt(mean( (y_hat - test_set$y)^2 ))
  
} )

rmse
mean(rmse)
sd(rmse)

# Q2
rm(list=ls())
library( tidyverse )
library( caret )
library( MASS )

set.seed( 1, sample.kind = "Rounding" )
n <- c( 100, 500, 1000, 5000, 10000 )
results <- sapply( n, function(t){
  Sigma <- 9*matrix( c( 1.0, 0.5, 0.5, 1.0 ), 2, 2 )
  dat <-  MASS::mvrnorm( t, c(69,69), Sigma ) %>%
    data.frame() %>% setNames( c("x", "y") )
  
  
  rmse <- replicate( 100, {
    test_index <- createDataPartition( dat$y, times = 1, p = 0.5, list = FALSE )
    train_set <- dat %>% slice( -test_index )
    test_set <- dat %>% slice( test_index )
    fit <- lm( y ~ x, data = train_set )
    y_hat <- predict( fit, newdata = test_set )
    sqrt( mean( (y_hat - test_set$y )^2 ))
    })
  c( avg = mean(rmse), sd = sd(rmse) )
})

results

# Q4
rm(list=ls())
library( tidyverse )
library( caret )
library( MASS )

set.seed( 1, sample.kind = "Rounding" )
n <- 100
Sigma <- 9*matrix( c( 1.0, 0.95, 0.95, 1.0 ), 2, 2 )
dat <-  MASS::mvrnorm( n = 100, c(69,69), Sigma ) %>%
  data.frame() %>% setNames( c("x", "y") )

set.seed( 1, sample.kind = "Rounding" )
rmse <- replicate( 100, {
    test_index <- createDataPartition( dat$y, times = 1, p = 0.5, list = FALSE )
    train_set <- dat %>% slice( -test_index )
    test_set <- dat %>% slice( test_index )
    fit <- lm( y ~ x, data = train_set )
    y_hat <- predict( fit, newdata = test_set )
    sqrt( mean( (y_hat - test_set$y )^2 ))
})
mean(rmse)
sd(rmse)

# Q6

rm(list=ls())
set.seed(1, sample.kind = "Rounding")
Sigma <- matrix( c(1.0, 0.75, 0.75, 0.75, 1.0, 0.25, 0.75, 0.25, 1.0), 3, 3 )
dat <- MASS::mvrnorm( n = 100, c(0,0,0), Sigma ) %>%
  data.frame() %>% setNames( c( "y", "x_1", "x_2") )
cor(dat)

test_index <- createDataPartition( dat$y, times = 1, p = 0.5, list = FALSE )
train_set <- dat %>% slice( -test_index )
test_set <- dat %>% slice( test_index )

fit <- lm( y ~ x_1, data = train_set )
y_hat <- predict( fit, newdata = test_set )
sqrt( mean( (y_hat - test_set$y )^2 ))

fit <- lm( y ~ x_2, data = train_set )
y_hat <- predict( fit, newdata = test_set )
sqrt( mean( (y_hat - test_set$y )^2 ))

fit <- lm( y ~ x_1 + x_2, data = train_set )
y_hat <- predict( fit, newdata = test_set )
sqrt( mean( (y_hat - test_set$y )^2 ))

# ?!?!?!?!?

set.seed(1, sample.kind="Rounding") # if using R 3.6 or later
Sigma <- matrix(c(1.0, 0.75, 0.75, 0.75, 1.0, 0.25, 0.75, 0.25, 1.0), 3, 3)
dat <- MASS::mvrnorm(n = 100, c(0, 0, 0), Sigma) %>%
  data.frame() %>% setNames(c("y", "x_1", "x_2"))

set.seed(1, sample.kind="Rounding") # if using R 3.6 or later
test_index <- createDataPartition(dat$y, times = 1, p = 0.5, list = FALSE)
train_set <- dat %>% slice(-test_index)
test_set <- dat %>% slice(test_index)

fit <- lm(y ~ x_1, data = train_set)
y_hat <- predict(fit, newdata = test_set)
sqrt(mean((y_hat-test_set$y)^2))

fit <- lm(y ~ x_2, data = train_set)
y_hat <- predict(fit, newdata = test_set)
sqrt(mean((y_hat-test_set$y)^2))

fit <- lm(y ~ x_1 + x_2, data = train_set)
y_hat <- predict(fit, newdata = test_set)
sqrt(mean((y_hat-test_set$y)^2))


# regression for a categorical outcome

rm(list=ls())
library(tidyverse)
library(dslabs)
library(caret)
data("heights")
y <- heights$height

set.seed(2, sample.kind = "Rounding")
test_index <- createDataPartition(y, times = 1, p = 0.5, list=FALSE)
train_set <- heights %>% slice(-test_index)
test_set <- heights %>% slice(test_index)

train_set %>%
  filter( round(height) == 66 ) %>%
  summarize( y_hat = mean(sex == "Female") )

heights %>%
  mutate( x = round(height) ) %>%
  group_by(x) %>%
  filter( n() >= 10 ) %>%
  summarize( prop = mean( sex == "Female") ) %>%
  ggplot( aes(x, prop)) +
  geom_point()

lm_fit <- mutate( train_set, y = as.numeric(sex == "Female") ) %>%
  lm( y ~ height, data = .)
p_hat <- predict( lm_fit, test_set )
y_hat <-  ifelse( p_hat > 0.5, "Female", "Male" ) %>% factor()
confusionMatrix( y_hat, test_set$sex )$overall["Accuracy"]


heights %>%
  mutate( x= round(height) ) %>%
  group_by(x) %>%
  filter( n() >= 10 ) %>%
  summarize( prop = mean( sex == "Female")) %>%
  ggplot(aes(x, prop)) +
  geom_point() +
  geom_abline(intercept = lm_fit$coef[1], slope = lm_fit$coef[2])

range(p_hat)

glm_fit <- train_set %>%
  mutate( y = as.numeric(sex == "Female") ) %>%
  glm( y ~ height, data=., family = "binomial" )

p_hat_logit <- predict( glm_fit, newdata = test_set, type = "response" )

tmp <- heights %>%
  mutate( x = round(height) ) %>%
  group_by( x ) %>%
  filter( n() >= 10 ) %>%
  summarize( prop = mean( sex == "Female") )

logistic_curve <- data.frame( x = seq(min(tmp$x), max(tmp$x))) %>%
  mutate( p_hat = plogis( glm_fit$coef[1] + glm_fit$coef[2]*x ) )

tmp %>%
  ggplot(aes(x, prop)) +
  geom_point() +
  geom_line( data = logistic_curve, mapping = aes(x, p_hat), lty = 2)

y_hat_logit <- ifelse( p_hat_logit > 0.5, "Female", "Male" ) %>% factor
confusionMatrix(y_hat_logit, test_set$sex)$overall[["Accuracy"]]


# assessment ===================================
rm(list=ls())
library(tidyverse)
library(dslabs)
library(caret)


mnist <- read_mnist()
typeof(mnist)
mnist_27
mnist     # okay, so this is like "WTF?" (look at the console)
is <- mnist_27$index_train[c( which.min(mnist_27$train$x_1), which.max(mnist_27$train$x_1))]
titles <- c("smallest", "largest")
tmp <- lapply(1:2, function(i){
       expand.grid(Row=1:28, Column=1:28) %>%
       mutate(label=titles[i], value = mnist$train$images[is[i],])
})
tmp

tmp <- Reduce(rbind, tmp)
View(tmp)  # <--- WTF is this?!?!?!?
tmp %>% ggplot(aes(Row, Column, fill=value)) +
  geom_raster() +
  scale_y_reverse() +
  scale_fill_gradient(low="white", high="black") +
  facet_grid(.~label) +
  geom_vline(xintercept = 14.5) +
  geom_hline(yintercept = 14.5)

data("mnist_27")
mnist_27$train %>% ggplot(aes(x_1,x_2, color=y)) +
  geom_point()

is <- mnist_27$index_train[c(which.min(mnist_27$train$x_2), which.max(mnist_27$train$x_2))]
titles <- c("smallest", "largest")
tmp <- lapply(1:2, function(i){
  expand.grid(Row=1:28, Column=1:28) %>%
    mutate(label=titles[i],
           value = mnist$train$images[is[i],])
})

tmp <- Reduce(rbind, tmp)

# manually typed
tmp %>% ggplot(aes(Row, Column, fill=value)) +
  geom_raster() +
  scale_y_reverse() +
  scale_fill_gradient(low="white", high="black") +
  facet_grid(.~label) +
  geom_vline(xintercept = 14.5) +
  geom_hline(yintercept = 14.5)

fit_glm <- glm( y~x_1+x_2, data=mnist_27$train, family="binomial")
p_hat_glm <- predict( fit_glm, mnist_27$test)
y_hat_glm <- factor(ifelse(p_hat_glm>0.5, 7, 2))
confusionMatrix( data = y_hat_glm, reference = mnist_27$test$y)$overall["Accuracy"]

mnist_27$true_p %>% ggplot(aes(x_1, x_2, z=p, fill=p)) +
  geom_raster()

mnist_27$true_p %>% ggplot(aes(x_1, x_2, z=p, fill=p)) +
  geom_raster() +
  scale_fill_gradientn(colors=c("#F8766D", "white", "#00BFC4")) +
  stat_contour( breaks=c(0.5), color="black" )

p_hat <- predict(fit_glm, newdata = mnist_27$true_p)
mnist_27$true_p %>%
  mutate(p_hat = p_hat) %>%
  ggplot(aes(x_1, x_2, z=p_hat, fill=p_hat)) +
  geom_raster() +
  scale_fill_gradientn(colors=c("#F8766D", "white", "#00BFC4")) +
  stat_contour(breaks=c(0.5), color="black")

p_hat <- predict(fit_glm, newdata = mnist_27$true_p )
mnist_27$true_p %>%
  mutate(p_hat=p_hat) %>%
  ggplot() +
  stat_contour(aes(x_1, x_2, z=p_hat), breaks=c(0.5), color="black") +
  geom_point(mapping = aes(x_1, x_2, color=y), data = mnist_27$test )

# Comprehension Check: Logistic Regression

rm(list=ls())
library(tidyverse)
library(caret)

set.seed(2, sample.kind="Rounding")
make_data <- function( n=1000, p=0.5, mu_0=0, mu_1=2, sigma_0=1, sigma_1=1 ){
  y <-rbinom(n, 1, p)
  f_0 <- rnorm( n, mu_0, sigma_0 )
  f_1 <- rnorm( n, mu_1, sigma_1 )
  x <- ifelse( y==1, f_1, f_0 )
  test_index <- createDataPartition( y, times=1, list= FALSE )
  list(train=data.frame(x=x, y=as.factor(y))%>% slice(-test_index),
       test=data.frame(x=x, y=as.factor(y)) %>% slice(test_index))
}
  
dat = make_data()

dat$train %>% ggplot(aes(x, color = y)) + geom_density()

set.seed(1, sample.kind = "Rounding")

delta <- seq(0, 3, len = 25)
res <- sapply(delta, function(d){
  dat <- make_data(mu_1 = d)
  fit_glm <- dat$train %>% glm(y~x, family = "binomial", data = .)
  y_hat_glm <- ifelse( predict(fit_glm, dat$test) > 0.5, 1, 0 ) %>% factor(levels=c(0,1))
  mean( y_hat_glm == dat$test$y )
})
qplot(delta, res)

# Introduction to Smoothing
rm(list=ls())
library(tidyverse)
library(caret)
library(dslabs)

data("polls_2008")
qplot(day, margin, data = polls_2008)


# bin smoothers
span <- 7
fit <- with(polls_2008, ksmooth( day, margin, x.points = day, kernel = "box", bandwidth = span ))
polls_2008 %>% mutate( smooth = fit$y ) %>%
  ggplot( aes( day, margin )) +
  geom_point( size = 3, alpha = 0.5, color = "grey" ) +
  geom_line( aes(day, smooth), color = "red" )

# kernel
span <- 7
fit <- with(polls_2008, ksmooth( day, margin, x.points = day, kernel = "normal", bandwidth = span ))
polls_2008 %>% mutate( smooth = fit$y ) %>%
  ggplot( aes( day, margin )) +
  geom_point( size = 3, alpha = 0.5, color = "grey" ) +
  geom_line( aes( day, smooth ), color = "red" )

polls_2008 %>% ggplot(aes(day, margin)) +
  geom_point() +
  geom_smooth( color = "red", span = 0.15, method = "loess", method.args = list(degree=1))


total_days = diff(range(polls_2008$day))
span <- 21/total_days

fit <- loess( margin ~ day, degree = 1, span = span, data = polls_2008 )

polls_2008 %>% mutate( smooth = fit$fitted ) %>%
  ggplot( aes( day, margin)) +
  geom_point( size = 3, alpha = 0.5, color = "grey") +
  geom_line( aes( day, smooth), color = "red")

# Comprehension Check: Smoothing

# install.packages("pdftools")

rm(list = ls())
library(tidyverse)
library(lubridate)
library(purrr)
library(pdftools)

fn <- system.file( "extdata", "RD-Mortality-Report_2015-18-180531.pdf", package="dslabs")
dat <- map_df(str_split(pdf_text(fn), "\n"), function(s){
  s <- str_trim(s)
  header_index <- str_which(s, "2015")[1]
  tmp <- str_split(s[header_index], "\\s+", simplify = TRUE)
  month <- tmp[1]
  header <- tmp[-1]
  tail_index <- str_which(s, "Total")
  n <- str_count(s, "\\d+")
  out <- c(1:header_index, which(n==1), which(n>=28), tail_index:length(s))
  s[-out] %>%
    str_remove_all("[^\\d\\s]") %>%
    str_trim() %>%
    str_split_fixed("\\s+", n=6) %>%
    .[,1:5] %>%
    as_tibble() %>%
    setNames(c("day", header)) %>%
    mutate(month=month, day=as.numeric(day)) %>%
    gather(year, deaths, -c(day, month)) %>%
    mutate(deaths=as.numeric(deaths))
}) %>%
  # uses Spanish "Agosto" (AGO) instead of English "August" (AUG)
  mutate(month = recode(month, "JAN"=1, "FEB"=2, "MAR"=3, "APR"=4, "MAY"=5, "JUN"=6,
                      "JUL"=7, "AGO"=8, "SEP"=9, "OCT"=10, "NOV"=11, "DEC"=12)) %>%
  mutate(date = make_date(year, month, day)) %>%
  dplyr::filter(date <= "2018-05-01")

sp <-  60 / as.numeric(diff(range(dat$date)))
fit <- dat %>% mutate(x=as.numeric(date)) %>% loess(deaths ~ x, data = ., span = sp, degree=1 )
dat %>% mutate(smooth=predict(fit, as.numeric(date))) %>%
  ggplot() +
  geom_point(aes(date, deaths)) +
  geom_line(aes(date, smooth), lwd=2, col=2)

dat %>% mutate(smooth=predict(fit, as.numeric(date)), day=yday(date), year=as.character(year(date))) %>%
  ggplot(aes(day, smooth, col=year)) +
  geom_line(lwd=2)

library(broom)
library(dslabs)
data("mnist_27")
mnist_27$train %>% glm(y~x_2, family="binomial", data=.) %>% tidy()
qplot(x_2, y, data=mnist_27$train)

mnist_27$train %>% mutate(y=ifelse(y=="7", 1, 0)) %>%
  ggplot(aes(x_2, y)) +
  geom_smooth(method="loess")

#lecture
rm(list=ls())
library(tidyverse)
library(dslabs)
if( !exists("mnist")) mnist <- read_mnist()

class(mnist$train$images)
x <- mnist$train$images[1:1000,]
y <- mnist$train$labels[1:1000]
class(x)
class(y)

length(x[,1])
x_1 <- 1:5
x_2 <- 6:10
x_bound <- cbind(x_1, x_2)
x_bound
dim(x)
dim(x_1)
dim(as.matrix(x_1))
dim(x)

vcb <- 2
vcb
class(vcb)
typeof(vcb)
is.vector(vcb)
is.matrix(vcb)

rm(list=ls())
my_vector <- 1:15

mat <- matrix(my_vector, 5, 3, byrow=FALSE)
mat

mat_t <- matrix(my_vector, 3, 5, byrow=TRUE)
mat_t
identical(t(mat), mat_t)

matrix(my_vector, 5, 5)

grid <- matrix(x[3,], 28, 28)
image(1:28, 1:28, grid)
image(1:28, 1:28, grid[,28:1])
