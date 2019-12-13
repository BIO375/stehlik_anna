#A: Chi Square test of independence#
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
ChestPain <- read_csv("datasets/FinalPractice/ChestPain.csv")
View(ChestPain)
#test assumptions#
tab01 <- matrix(c(822, 229, 18296, 1534), 2, 2, byrow=TRUE)
dimnames(tab01) <- list("Survival" = c("died", "survived"),
                        "Chest Pain" = c("chest pain", "no chest pain"))
as.matrix(tab01)
model_01 <- chisq.test(tab01, correct = FALSE)
model_01$expected
#do test and report results#
model_01
# (vi)
# We found that people admitted to emergency rooms with heart attacks were significantly
# more likely to die if they did not have chest pain (Chi-sq test of Independence: X-sq = 254.99,
# df = 1, p < 0.0001).  (Look at the expected frequencies compared to the observed frequencies to
#see which pairing has the largest difference.)


####Problem B####
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
Cichlid <- read_csv("datasets/FinalPractice/Cichlid.csv")
View(Cichlid)
