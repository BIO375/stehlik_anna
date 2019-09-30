library("tidyverse")
tidyverse_update()
birth_rates<-read_csv("datasets/demos/birth_rates.csv")
summ_Diff <- birth_rates %>%
  group_by() %>% 
  summarise(n_Diff = n(),
            mean_Diff = mean(Diff),
            median_Diff = median(Diff),
            IQR_Diff = IQR(Diff),
            sd_Diff = sd(Diff),
            var_Diff = var(Diff))
View(summ_Diff)
ggplot(birth_rates) +
  geom_histogram(aes(Diff), binwidth = 20)+
  facet_wrap(~1)

library(readr)
data01 <- read_csv("datasets/abd/chapter12/chap12e3HornedLizards.csv")
data01 <- data01 %>% slice(-105)
View(data01)
summ_squamosalHornLength <- data01 %>%
  group_by(Survival) %>% 
  summarise(n_squamosalHornLength = n(),
            mean_squamosalHornLength = mean(squamosalHornLength),
            median_squamosalHornLength = median(squamosalHornLength),
            IQR_squamosalHornLength = IQR(squamosalHornLength),
            sd_squamosaHornLength = sd(squamosalHornLength),
            var_squamosalHornLength = var(squamosalHornLength))
View(summ_squamosalHornLength)
ggplot(data01) +
  geom_histogram(aes(squamosalHornLength), binwidth = 1)+
  facet_wrap(~Survival)
