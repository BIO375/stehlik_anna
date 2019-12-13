rm(list = ls())
library("DescTools")
library("ggfortify")
library("multcomp")
library("nlme")
library("broom")
library("ggmosaic")
library("epitools")
library("tidyverse")
tidyverse_update()

####Scenario 1####
library(readr)
insulation <- read_csv("datasets/final/insulation.csv")
View(insulation)
model01 <- lm(heat_loss ~ leanness, data = insulation)
autoplot(model01, smooth.colour = NA)
insulation_plus <- augment(model01)
ggplot(data = insulation_plus)+
  geom_point(aes(x = leanness, y= .resid))
summary(model01)

####Scenario 2####
rm(list = ls())
library("DescTools")
library("ggfortify")
library("multcomp")
library("nlme")
library("broom")
library("ggmosaic")
library("epitools")
library("tidyverse")
tidyverse_update()
library(readr)
caffeine <- read_csv("datasets/final/caffeine.csv")
caffeine <-read_csv("datasets/final/caffeine.csv", col_types = cols(
  group = col_factor() ))
View(caffeine)
head(caffeine)
summary(caffeine)
ggplot(caffeine, aes(x = group, y = half_life))+
  geom_boxplot() +
  theme_bw() +
  coord_flip()
ggplot(caffeine) +
  geom_histogram(aes(half_life), binwidth = 3)+
  facet_wrap(~group)
ggplot(caffeine)+
  geom_qq(aes(sample = half_life, color = group))
model01 <- lm(half_life~group, data = caffeine)

summ_half_life <- caffeine %>%
  group_by(group) %>% 
  summarise(mean_half_life = mean(half_life),
            sd_half_life = sd(half_life),
            n_half_life = n())
ratio <-(max(summ_half_life$sd_half_life))/(min(summ_half_life$sd_half_life))
autoplot(model01)
anova(model01)
summary(model01)

planned <- glht(model01, linfct = 
                  mcp(group = c("male - norm_prog = 0",
                                   "norm_prog - high_prog = 0")))
confint(planned)
summary(planned)

####Scenario 3####
rm(list = ls())
library("DescTools")
library("ggfortify")
library("multcomp")
library("nlme")
library("broom")
library("ggmosaic")
library("epitools")
library("tidyverse")
tidyverse_update()
library(readr)
davis <- read_csv("datasets/final/davis.csv",
col_types = cols(day = col_factor()))
View(davis)
davis_summ <- davis %>%
  group_by(race_ethn)%>%
  summarise(race_ethn_n = n())
davis_summ <- add_column(davis_summ, expected= c(0.030,0.018,0.156,0.319,0.423,0.054)) %>%
  mutate(expected_p = expected/1.00)

model03 <-chisq.test(x = davis_summ$race_ethn_n, p = davis_summ$expected_p)
model03
