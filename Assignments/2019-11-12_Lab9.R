rm(list = ls())
getwd()
library("ggfortify")
library("broom")
library("tidyverse")
tidyverse_update()
library(readr)
fowler <- read_csv("datasets/demos/fowler.csv")
View(fowler)
head(fowler)

ggplot(data = fowler) +
  geom_point(mapping = aes(x = FERTILIZER, y = YIELD ))

ggplot(data = fowler) +
  geom_point(mapping = aes(x = FERTILIZER, y = YIELD),
             colour = "firebrick", size = 2)+
  theme_bw()+
  labs( x = "FERTILIZER", y = "YIELD")

ggplot(data = fowler)+
  geom_histogram(aes(YIELD), binwidth = 8)

ggplot(data = fowler)+
  geom_boxplot(aes("", YIELD))
ggplot(data = fowler)+
  geom_qq(aes(sample = YIELD))

####Linear Regression####
fowler <- read_csv("datasets/demos/fowler.csv")
model01 <- lm(YIELD ~ FERTILIZER, data = fowler)
autoplot(model01, smooth.colour = NA)

#Option 1: Augment
fowler_plus <- augment(model01)
ggplot(data = fowler_plus)+
  geom_point(aes(x = FERTILIZER, y= .resid))
#Residuals look normally distributed. Good. Proceed with linear regression.

summary(model01)

ggplot(data = fowler, aes(x = FERTILIZER, y = YIELD)) +
  geom_point() +
  geom_smooth(method = "lm", level=0.95) +
  theme_bw()+
  labs( x = "FERTILIZER", y = "YIELD")

ggplot(data = fowler, aes(x = FERTILIZER, y = YIELD)) +
  geom_point() +
  geom_smooth(method = "lm", level=0.99) +
  theme_bw()+
  labs( x = "FERTILIZER", y = "YIELD")

#### 10/10 code runs without breaking ####