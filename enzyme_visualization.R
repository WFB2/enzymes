library(readxl)
library(tidyverse)
Standards <- read_excel("C:/Users/wanja/Documents/Kiel/Master Thesis/enzymes/Standards/Standards_full.xlsx")
Enzymes <- read_excel("C:/Users/wanja/Documents/Kiel/Master Thesis/enzymes/Enzymes_full.xlsx")
Data <- Enzymes %>% left_join(Standards, by = c("Station"="Station"))
Enzymes %>% filter(Station == 10)
Data <- Data %>% 
    mutate(concentration_gain1 = case_when(enzyme != "Lpase" & Standard == "MUF" ~ gain1/gain1slope,
                             enzyme == "Lpase" & Standard == "MCA" ~ gain1/gain1slope)) %>% 
  mutate(concentration_gain2 = case_when(enzyme != "Lpase" & Standard == "MUF" ~ gain2/gain2slope,
                                         enzyme == "Lpase" & Standard == "MCA" ~ gain2/gain2slope)) %>% 
  mutate(concentration_gain3 = case_when(enzyme != "Lpase" & Standard == "MUF" ~ gain3/gain3slope,
                                         enzyme == "Lpase" & Standard == "MCA" ~ gain3/gain3slope)) %>% 
  
  filter(!is.na(concentration_gain1))
     

Data %>%
  filter(enzyme == "NAG") %>%
  ggplot(aes(x = Time, y = concentration_gain2, color = as.factor(depth)))+
  geom_point()+
  geom_smooth()+
  facet_wrap(~Station, scale = "free_y")+
  labs(title = "NAG", y = "Fluorescence Gain 2", x = "Incubation Time (h)")

