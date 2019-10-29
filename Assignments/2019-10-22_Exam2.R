#Question 9#
rm(list = ls())
getwd()
library("tidyverse")
tidyverse_update()
library("DescTools")
library(readr)
feathers <- read_csv("datasets/exams/feathers.csv")
View(feathers)
feathers <- mutate(feathers, diff = odd - typical)
ggplot(feathers) +
  geom_histogram(aes(diff), binwidth = 0.01)

ggplot(feathers) +
  geom_boxplot(aes(x = "", y = diff))

ggplot(feathers)+
  geom_qq(aes(sample = diff))

t.test(feathers$odd, feathers$typical, 
       alternative = "less", paired =  TRUE, conf.level = 0.95)

#Question 10#
rm(list = ls())
getwd()
library("tidyverse")
tidyverse_update()
library("DescTools")
library(readr)
baker <- read_csv("datasets/exams/baker.csv")
View(baker)
baker <- mutate(baker, diff = After - Before)
ggplot(baker) +
  geom_histogram(aes(diff), binwidth = 1)

ggplot(baker) +
  geom_boxplot(aes(x = "", y = diff))

ggplot(baker)+
  geom_qq(aes(sample = diff))

SignTest(baker$diff, alternative = "greater", mu = 0, conf.level = 0.95)

#Question 11#
rm(list = ls())
getwd()
library("tidyverse")
tidyverse_update()
library("DescTools")
library(readr)
CO2levels <- read_csv("datasets/exams/CO2levels.csv")
View(CO2levels)
summ_growth_rate <- CO2levels %>%
  group_by(treatment) %>% 
  summarise(mean_growth_rate = mean(growth_rate),
            sd_growth_rate = sd(growth_rate),
            n_growth_rate = n(),
            var_growth_rate = var(growth_rate))
View(summ_growth_rate)
ratio <-(max(summ_growth_rate$sd_growth_rate))/(min(summ_growth_rate$sd_growth_rate))
ggplot(CO2levels) +
  geom_histogram(aes(growth_rate), binwidth = 0.2)+
  facet_wrap(~treatment)

ggplot(CO2levels) +
  geom_boxplot(aes(x = treatment, y = growth_rate))

ggplot(CO2levels)+
  geom_qq(aes(sample = growth_rate, color = treatment))

t.test(growth_rate ~ treatment, data = CO2levels, var.equal = TRUE, alternative = "two.sided", conf.level = 0.95)

