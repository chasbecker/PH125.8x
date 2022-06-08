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
xyz <- dat %>%  mutate(g = cut(x, quantile(x, pst), include.lowest = TRUE))

rm(list=ls())
library(tidyverse)