#Problem 15-22 a, b, c, d#
rm(list = ls())
getwd()
library("ggfortify")
library("multcomp")
library("nlme")
library("tidyverse")
tidyverse_update()
library(readr)
WalkingStickHeads<- read_csv("datasets/abd/chapter15/chap15q22WalkingStickHeads.csv", 
                             col_types = cols(specimen = col_factor() ) )
View(WalkingStickHeads)
#a# random effects ANOVA
head(WalkingStickHeads)
summary(WalkingStickHeads)

ggplot(WalkingStickHeads, aes(x = "", y = headwidth))+
  geom_boxplot()
ggplot(WalkingStickHeads) +
  geom_histogram(aes(headwidth), binwidth = 0.01)+
  facet_wrap(~"")

model02 <- lme(fixed = headwidth ~ 1,
               random = ~1|specimen, data = WalkingStickHeads)
model02_varcomp <- VarCorr(model02)
model02_varcomp

varAmong  <- as.numeric( model02_varcomp[1,1] )
varWithin <- as.numeric( model02_varcomp[2,1] )
repeatability <- varAmong / (varAmong + varWithin)
repeatability
#a# varation within groups is 0.01%

#b# variance among groups is 0.02%

#c# 59.7%

#d# Femur length has higher repeatability (74%, compared to 59.7% for head width). 
  #The measurement error for head width is 0.01% and it is 0.03% for femur length.
  #Femur length is more affected by measurement error. 
