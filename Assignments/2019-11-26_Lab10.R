(list = ls())
getwd()
library("ggmosaic")
library("epitools")
library("tidyverse")
tidyverse_update()

####Binomial Test####
(list = ls())
getwd()
library("ggmosaic")
library("epitools")
library("tidyverse")
tidyverse_update()
49+41 # n = total number of trials
model01 <- binom.test(x= 41, n=90, p=0.5, alternative = "greater", conf.level = 0.95 )
model01

####Chi-squared goodness of fit####
(list = ls())
getwd()
library("ggmosaic")
library("epitools")
library("tidyverse")
tidyverse_update()
library(readr)
Flowers <- read_csv("datasets/demos/Flower_Color.csv")
View(Flowers)
Flowers_summ <- Flowers %>%
  group_by(Color)%>%
  summarise(Color_n = n())
Flowers_summ <- add_column(Flowers_summ, expected= c(75,25)) %>%
  mutate(expected_p = expected/100)
model03 <-chisq.test(x = Flowers_summ$Color_n, p = Flowers_summ$expected_p)
model03

####Contingency table analysis####
(list = ls())
getwd()
library("ggmosaic")
library("epitools")
library("tidyverse")
tidyverse_update()
library(readr)
Sex_Ratio <- read_csv("datasets/demos/Sex_Ratio.csv")
View(Sex_Ratio)
Sex_RatioTable <- table(Sex_Ratio$Locality, Sex_Ratio$Sex)
Sex_RatioTable
oddsratio(Sex_RatioTable, method = "wald")
model04 <- chisq.test(Sex_Ratio$Locality, Sex_Ratio$Sex, correct = FALSE)
model04

