(list = ls())
getwd()
library("ggmosaic")
library("epitools")
library("tidyverse")
tidyverse_update()

####Binomial Test####
49+41 # n = total number of trials
model01 <- binom.test(x= 41, n=90, p=0.5, alternative = "greater", conf.level = 0.95 )
model01

####Chi-squared goodness of fit####
