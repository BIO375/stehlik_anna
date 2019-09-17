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
