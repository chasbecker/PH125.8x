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

rm(list = ls())
library( tidyverse )
