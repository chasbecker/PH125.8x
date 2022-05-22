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

# this is a comment.

table( predicted = y_hat, actual = test_set$sex )
test_set %>%
  mutate( y_hat = y_hat ) %>%
  group_by( sex ) %>%
  summarize( accuracy = mean( y_hat == sex ) )
prev <- mean( y_hat == "Male" )

confusionMatrix( data = y_hat, reference = test_set$sex )


rm(list = ls())
library( tidyverse )
library(caret)
library( dslabs )
data( heights )
