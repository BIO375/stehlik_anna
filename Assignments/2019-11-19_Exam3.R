####Question 10####
rm(list = ls())
getwd()
library("ggfortify")
library("multcomp")
library("nlme")
library("tidyverse")
tidyverse_update()
library(readr)
aphids <- read_csv("datasets/exams/aphids.csv")
View(aphids)
aphids <-read_csv("datasets/exams/aphids.csv", col_types = cols(
  gall_number = col_factor() ))
View(aphids)
head(aphids)
summary(aphids)
model02 <- lme(fixed = thorax_length ~ 1,
               random = ~1|gall_number, data = aphids)
model02_varcomp <- VarCorr(model02)
model02_varcomp
varAmong  <- as.numeric( model02_varcomp[1,1] )
varWithin <- as.numeric( model02_varcomp[2,1] )
repeatability <- varAmong / (varAmong + varWithin)
repeatability

####Question 11####
rm(list = ls())
getwd()
library("ggfortify")
library("broom")
library("tidyverse")
tidyverse_update()
library(readr)
glucose <- read_csv("datasets/exams/glucose.csv")
View(glucose)
head(glucose)
ggplot(data = glucose) +
  geom_point(mapping = aes(x = blood_glucose, y = HbA1c ))
ggplot(data = glucose) +
  geom_point(mapping = aes(x = blood_glucose, y = HbA1c),
             colour = "firebrick", size = 2)+
  theme_bw()+
  labs( x = "blood_glucose", y = "HbA1c")
ggplot(data = glucose)+
  geom_histogram(aes(blood_glucose), binwidth = 1)
ggplot(data = glucose)+
  geom_histogram(aes(HbA1c), binwidth = 1)

glucoseCor <- cor.test(~ blood_glucose + HbA1c, data = glucose,
                     method = "pearson")
glucoseCor
r <- glucoseCor$estimate
r


####Question 12####
rm(list = ls())
getwd()
library("ggfortify")
library("multcomp")
library("nlme")
library("tidyverse")
tidyverse_update()
library(readr)
DriverVision <- read_csv("datasets/exams/DriverVision.csv")
View(DriverVision)
model01 <- lm(Distance ~ Age, data = DriverVision)
autoplot(model01, smooth.colour = NA)
ggplot(data = DriverVision)+
  geom_point(aes(x = Age, y = Distance))
summary(model01)
