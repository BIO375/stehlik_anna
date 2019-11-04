install.packages("rlang")
rm(list = ls())
getwd()
library("ggfortify")
library("multcomp")
library("nlme")
library("tidyverse")
tidyverse_update()

#Question 1#

library(readr)
Jaffe <- read_csv("datasets/demos/Jaffe.csv")
View(Jaffe)
Jaffe <-read_csv("datasets/demos/Jaffe.csv", col_types = cols(
  Depth = col_factor() ))
View(Jaffe)
head(Jaffe)
summary(Jaffe)

ggplot(Jaffe, aes(x = Depth, y = Aldrin))+
  geom_boxplot() +
  theme_bw() +
  coord_flip()
ggplot(Jaffe) +
  geom_histogram(aes(Aldrin), binwidth = 1)+
  facet_wrap(~Depth)
ggplot(Jaffe)+
  geom_qq(aes(sample = Aldrin, color = Depth))

model01 <- lm(Aldrin~Depth, data = Jaffe)

summ_Aldrin <- Jaffe %>%
  group_by(Depth) %>% 
  summarise(mean_Aldrin = mean(Aldrin),
            sd_Aldrin = sd(Aldrin),
            n_Aldrin = n())
ratio <-(max(summ_Aldrin$sd_Aldrin))/(min(summ_Aldrin$sd_Aldrin))
autoplot(model01)
anova(model01)
summary(model01)

Jaffe<-mutate(Jaffe, log10_Aldrin = log10(Aldrin))

model02 <- lm(log10_Aldrin~Depth, data = Jaffe)
autoplot(model02)
anova(model02)
summary(model02)
summ_log10_Aldrin <- Jaffe %>%
  group_by(Depth) %>% 
  summarise(mean_log10_Aldrin = mean(log10_Aldrin),
            sd_log10_Aldrin = sd(log10_Aldrin),
            n_log10_Aldrin = n())
ratio <-(max(summ_log10_Aldrin$sd_log10_Aldrin))/(min(summ_log10_Aldrin$sd_log10_Aldrin))
autoplot(model02)

#HCB Data#
ggplot(Jaffe, aes(x = Depth, y = HCB))+
  geom_boxplot() +
  theme_bw() +
  coord_flip()
ggplot(Jaffe) +
  geom_histogram(aes(HCB), binwidth = 0.2)+
  facet_wrap(~Depth)
ggplot(Jaffe)+
  geom_qq(aes(sample = HCB, color = Depth))

model03 <- lm(HCB~Depth, data = Jaffe)

summ_HCB <- Jaffe %>%
  group_by(Depth) %>% 
  summarise(mean_HCB = mean(HCB),
            sd_HCB = sd(HCB),
            n_HCB = n())
ratio <-(max(summ_HCB$sd_HCB))/(min(summ_HCB$sd_HCB))
autoplot(model03)
anova(model03)
summary(model03)


tukey <- glht(model02, linfct = mcp(Depth = "Tukey"))
summary(tukey)
