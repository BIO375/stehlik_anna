library("tidyverse")
tidyverse_update()
birth_rates<-read_csv("datasets/demos/birth_rates.csv")
summ_Diff <- birth_rates %>%
  group_by(Country) %>% 
  summarise(n_Diff = n(),
            mean_Diff = mean(Diff),
            median_Diff = median(Diff),
            IQR_Diff = IQR(Diff),
            sd_Diff = sd(Diff),
            var_Diff = var(Diff))
View(summ_Diff)
