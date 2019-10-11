# Clean up the working environment
rm(list = ls())
# Verify working directory, should be ~/Documents/Analyses/lastname_first
getwd()

# Load tidyverse
library("tidyverse")
# Check for updates
tidyverse_update()

# To perform sign tests, install and load the package DescTools
# install.packages("DescTools")
library("DescTools")

#Chapter 13####
#Question 20####
#a.) Welch's t-test or Mann-Whitney U-test
library(readr)
SalmonColor <- read_csv("datasets/abd/chapter13/chap13q20SalmonColor.csv")
View(SalmonColor)
summ_SalmonColor <- SalmonColor %>%
  group_by(species) %>% 
  summarise(n_skinColor = n(),
            mean_skinColor = mean(skinColor),
            median_skinColor = median(skinColor),
            IQR_skinColor = IQR(skinColor),
            sd_skinColor = sd(skinColor),
            var_skinColor = var(skinColor))
View(summ_SalmonColor)
ratio <-(max(summ_SalmonColor$sd_skinColor))/(min(summ_SalmonColor$sd_skinColor))
ggplot(SalmonColor) +
  geom_histogram(aes(skinColor), binwidth = 0.05)+
  facet_wrap(~species)

ggplot(SalmonColor) +
  geom_boxplot(aes(x = species, y = skinColor))

ggplot(SalmonColor)+
  geom_qq(aes(sample = skinColor, color = species))

t.test(skinColor ~ species, data = SalmonColor, alternative = "two.sided", conf.level = 0.95)


#b.) Transformation

SalmonColor<-mutate(SalmonColor, log_skinColor = log(skinColor))
ggplot(SalmonColor) +
  geom_histogram(aes(log(skinColor)), binwidth = 0.25)+
  facet_wrap(~species)
ggplot(SalmonColor)+
  geom_boxplot(aes(x = species, y = log_skinColor), notch = FALSE, varwidth = TRUE)
#There is a difference in mean skin color between the two groups. Boxplots are very different. No overlap of boxes.

#Question 25####
rm(list = ls())
getwd()
library("tidyverse")
tidyverse_update()
library("DescTools")
library(readr)
Clearcuts <- read_csv("datasets/abd/chapter13/chap13q25Clearcuts.csv")
View(Clearcuts)
summ_Clearcuts <- Clearcuts %>%
  group_by() %>% 
  summarise(n_biomassChange = n(),
            mean_biomassChange = mean(biomassChange),
            median_biomassChange = median(biomassChange),
            IQR_biomassChange = IQR(biomassChange),
            sd_biomassChange = sd(biomassChange),
            var_biomassChange = var(biomassChange))

View(summ_Clearcuts)

#One-Sample t-test
null_mean <- 0  
sample_mean <- -0.5055556
sample_sd <- 3.556961
sample_n <- 36
df <- sample_n -1
t_sample <- (sample_mean - null_mean)/(sample_sd/sqrt(sample_n))
negative_tail <- pt(t_sample, df)

#Answer
#There was not a significant difference observed in change in biomass of rainforests following clear-cutting.
#(One-sample one-sided t-test: t35=-0.85, p>0.05).