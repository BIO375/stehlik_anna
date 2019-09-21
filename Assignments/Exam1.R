# ANNA, you need to start by clearing everything out.  I use the function rm, but you can 
# also restart R

# ANNA, because you did not load the entire tidyverse, your code below does not work because ggplot is 
# in the package ggplot2, which is not loaded
# If this were part of the same script as you used for the Artemesia problem, and if that script loaded
# the tidyverse library, then it probably worked for you, but the code breaks if ran all by itself.
# you need a line library(tidyverse)

library(readr)

chap13e5SagebrushCrickets <- read_csv("datasets/abd/chapter13/chap13e5SagebrushCrickets.csv")

View(chap13e5SagebrushCrickets)

ggplot(chap13e5SagebrushCrickets) +
  geom_histogram(aes(timeToMating), binwidth = 10)+
  facet_wrap(~feedingStatus)

chap13e5SagebrushCrickets<-mutate(chap13e5SagebrushCrickets, log_timeToMating = log(timeToMating))

ggplot(chap13e5SagebrushCrickets) +
  geom_histogram(aes(log_timeToMating), binwidth = 0.5)+
  facet_wrap(~feedingStatus)
