####Problem 15-22 a, b, c, d#
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
 # The measurement error for head width is 0.01% and it is 0.03% for femur length.
 # Femur length is more affected by measurement error. 


####Problem 15-23####
#a# Planned comparison one-way ANOVA
rm(list = ls())
getwd()
library("ggfortify")
library("multcomp")
library("nlme")
library("tidyverse")
tidyverse_update()
library(readr)
LodgepolePineCones <- read_csv("datasets/abd/chapter15/chap15q23LodgepolePineCones.csv",
                               col_types = cols(habitat = col_factor() ) )
View(LodgepolePineCones)
head(LodgepolePineCones)
summary(LodgepolePineCones)

ggplot(LodgepolePineCones, aes(x = habitat, y = conemass))+
  geom_boxplot()
ggplot(LodgepolePineCones) +
  geom_histogram(aes(conemass), binwidth = 0.8)+
  facet_wrap(~habitat)
ggplot(LodgepolePineCones)+
  geom_qq(aes(sample = conemass, color = habitat))

model01 <- lm(conemass~habitat, data = LodgepolePineCones)

summ_conemass <- LodgepolePineCones %>%
  group_by(habitat) %>% 
  summarise(mean_conemass = mean(conemass),
            sd_conemass = sd(conemass),
            n_conemass = n())
ratio <-(max(summ_conemass$sd_conemass))/(min(summ_conemass$sd_conemass))
autoplot(model01)
anova(model01)
summary(model01)

planned <- glht(model01, linfct = 
                  mcp(habitat = c("island.present - island.absent = 0")))
confint(planned)
summary(planned)


####Problem 15-26####
rm(list = ls())
getwd()
library("ggfortify")
library("multcomp")
library("nlme")
library("tidyverse")
tidyverse_update()
library(readr)
Malaria <- read_csv("datasets/abd/chapter15/chap15q26MalariaFungusVenom.csv",
                    col_types = cols(treatmentGroup = col_factor() ) )
View(Malaria)
head(Malaria)
summary(Malaria)

ggplot(Malaria, aes(x = treatmentGroup, y = logSporozoiteNumbers))+
  geom_boxplot()
ggplot(Malaria) +
  geom_histogram(aes(logSporozoiteNumbers), binwidth = 2)+
  facet_wrap(~treatmentGroup)
ggplot(Malaria)+
  geom_qq(aes(sample = logSporozoiteNumbers, color = treatmentGroup))

model01 <- lm(logSporozoiteNumbers~treatmentGroup, data = Malaria)

summ_logSporozoiteNumbers <- Malaria %>%
  group_by(treatmentGroup) %>% 
  summarise(mean_logSporozoiteNumbers = mean(logSporozoiteNumbers),
            sd_logSporozoiteNumbers = sd(logSporozoiteNumbers),
            n_logSporozoiteNumbers = n())
ratio <-(max(summ_logSporozoiteNumbers$sd_logSporozoiteNumbers))/(min(summ_logSporozoiteNumbers$sd_logSporozoiteNumbers))

autoplot(model01)
anova(model01)
summary(model01)
#tukey because 3 is more than p-1
tukey <- glht(model01, linfct = mcp(treatmentGroup = "Tukey"))
summary(tukey)



####Problem 15-30####
rm(list = ls())
getwd()
library("ggfortify")
library("multcomp")
library("nlme")
library("tidyverse")
tidyverse_update()
library(readr)
Crabs <- read_csv("datasets/abd/chapter15/chap15q30FiddlerCrabFans.csv",
                  col_types = cols(crabType = col_factor() ) )
Crabs <- slice(Crabs,-85)
View(Crabs)

head(Crabs)
summary(Crabs)
ggplot(Crabs, aes(x = crabType, y = bodyTemperature))+
  geom_boxplot()
ggplot(Crabs) +
  geom_histogram(aes(bodyTemperature), binwidth = 0.1)+
  facet_wrap(~crabType)
ggplot(Crabs)+
  geom_qq(aes(sample = bodyTemperature, color = crabType))
model01 <- lm(bodyTemperature~crabType, data = Crabs)

summ_bodyTemperature <- Crabs %>%
  group_by(crabType) %>% 
  summarise(mean_bodyTemperature = mean(bodyTemperature),
            sd_bodyTemperature = sd(bodyTemperature),
            n_bodyTemperature = n())
ratio <-(max(summ_bodyTemperature$sd_bodyTemperature))/(min(summ_bodyTemperature$sd_bodyTemperature))
#equal variance

autoplot(model01)
anova(model01)
summary(model01)
