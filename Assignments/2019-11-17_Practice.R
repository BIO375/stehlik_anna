rm(list = ls())
getwd()
library("tidyverse")
tidyverse_update()
library("DescTools")
library(readr)
peake <- read_csv("datasets/demos/peake.csv")
View(peake)

ggplot(data = peake)+
  geom_point(aes(x = AREA, y = SPECIES))
#Linear Regression#
model01 <-lm(SPECIES ~ AREA, data = peake)
autoplot(model01)
ggplot(data = peake)+
  geom_point(aes(x = AREA, y = SPECIES))
#Mutate#
peake <- peake %>%
  mutate(log10area = log10(AREA))
ggplot(data = peake)+
  geom_point(aes(x = log10area, y = SPECIES))
#Linear Regression with transformation#
#examine data#
model02<-lm(SPECIES ~ log10area, data = peake)
autoplot(model02)
#including residuals#
ggplot(data = peake)+
  geom_point(aes(x = log10area, y = resid(model02)))
#normal so...#
summary(model02)
#create confidence band#
ggplot(data = peake, aes(x = log10area, y = SPECIES)) +
  geom_point() +
  geom_smooth(method = "lm", level=0.95) +
  theme_bw()+
  labs( x = "log10(Clump Area)", y = "Number of Species")


