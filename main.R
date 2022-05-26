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
library( tidyverse )
library(caret)
library( dslabs )
data( heights )
