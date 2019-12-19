#Question 1
rm(list = ls())
getwd()
library("tidyverse")
tidyverse_update()
library("DescTools")
library(readr)
obliquity_data <- read_csv("datasets/demos/obliquity_data.csv")
View(obliquity_data)
summ_Obliquity <- obliquity_data %>%
  group_by() %>% 
  summarise(n_Obliquity = n(),
            mean_Obliquity = mean(Obliquity),
            median_Obliquity = median(Obliquity),
            IQR_Obliquity = IQR(Obliquity),
            sd_Obliquity = sd(Obliquity),
            var_Obliquity = var(Obliquity))

View(summ_Obliquity)
null_mean <- 23.4722
sample_mean <- 23.49878
sample_sd <- 	0.019613
sample_n <- 5
df <- sample_n -1
obliquity_data <- read_csv("datasets/demos/obliquity_data.csv")
y<-obliquity_data$Obliquity
sample_mean <-mean(y)
sample_sd <- sd(y)
sample_n <- as.numeric(length(y))
df <- sample_n -1
t_sample <- (sample_mean - null_mean)/(sample_sd/sqrt(sample_n))
two_tailed <- 2*(1-pt(abs(t_sample), df))
t.test(obliquity_data$Obliquity, 
       alternative = "two.sided", mu = 0, conf.level = 0.95)

### MU SHOULD BE SAME AS NULL MEAN

#Question 2
rm(list = ls())
getwd()
library("tidyverse")
tidyverse_update()
library("DescTools")
library(readr)
HeartAttack_short <- read_csv("datasets/demos/HeartAttack_short.csv",col_names = TRUE,col_types = cols(group = col_character()) )
View(HeartAttack_short)
summ_cholest <- HeartAttack_short %>%
  group_by(group) %>% 
  summarise(mean_cholest = mean(cholest),
            sd_cholest = sd(cholest),
            n_cholest = n())
ratio <-(max(summ_cholest$sd_cholest))/(min(summ_cholest$sd_cholest))
ggplot(HeartAttack_short) +
  geom_histogram(aes(cholest), binwidth = 20)+
  facet_wrap(~group)
ggplot(HeartAttack_short) +
  geom_boxplot(aes(x = group, y = cholest))
ggplot(HeartAttack_short)+
  geom_qq(aes(sample = cholest, color = group))
t.test(cholest ~ group, data = HeartAttack_short, alternative = "two.sided", conf.level = 0.95)

#Question 3
rm(list = ls())
getwd()
library("tidyverse")
tidyverse_update()
library("DescTools")
library(readr)
furness <- read_csv("datasets/quinn/chpt3/furness.csv")
View(furness)
ggplot(furness) +
  geom_histogram(aes(METRATE), binwidth = 100)+
  facet_wrap(~SEX)
ggplot(furness) +
  geom_boxplot(aes(x = SEX, y = METRATE))
ggplot(furness)+
  geom_qq(aes(sample = METRATE, color = SEX))
wilcox.test(METRATE ~ SEX, data = furness, alternative = "two.sided", conf.level = 0.95)

#Question 4
rm(list = ls())
getwd()
library("tidyverse")
tidyverse_update()
library("DescTools")
library(readr)
untidy_elgar <- read_csv("datasets/quinn/chpt3/elgar.csv")
View(untidy_elgar)
untidy_elgar <- mutate(untidy_elgar, diff = HORIZDIM - HORIZLIG)
ggplot(untidy_elgar) +
  geom_histogram(aes(diff), binwidth = 14)
ggplot(untidy_elgar) +
  geom_boxplot(aes(x = "", y = diff))
ggplot(untidy_elgar)+
  geom_qq(aes(sample = diff))
t.test(untidy_elgar$HORIZDIM, untidy_elgar$HORIZLIG, 
       alternative = "two.sided", paired = TRUE, conf.level = 0.95)

#### 10/10 code runs without breaking ####