library(readxl)
library(tidyverse)
library(cowplot)
library(viridis)
Standards <- read_excel("C:/Users/wanja/Documents/Kiel/Master Thesis/enzymes/Standards/Standards_full.xlsx")
Enzymes <- read_excel("C:/Users/wanja/Documents/Kiel/Master Thesis/enzymes/Enzymes_full.xlsx")
Data <- Enzymes %>% left_join(Standards, by = c("Station"="Station"))

Data <- Data %>% 
    mutate(concentration_gain1 = case_when(enzyme != "Lpase" & Standard == "MUF" ~ gain1/gain1slope,
                             enzyme == "Lpase" & Standard == "MCA" ~ gain1/gain1slope)) %>% 
  mutate(concentration_gain2 = case_when(enzyme != "Lpase" & Standard == "MUF" ~ gain2/gain2slope,
                                         enzyme == "Lpase" & Standard == "MCA" ~ gain2/gain2slope)) %>% 
  mutate(concentration_gain3 = case_when(enzyme != "Lpase" & Standard == "MUF" ~ gain3/gain3slope,
                                         enzyme == "Lpase" & Standard == "MCA" ~ gain3/gain3slope))  
  
# filter option for day stations
day_stations <- c(1,3,5,7,9,11,13,15,17,19,21,23,25,27,28,30,32)

# Theme for plots
theme_set(theme_cowplot())
# Bgase plot
Data %>%
  filter(enzyme == "Bgase") %>% filter(!(is.na(concentration_gain1))) %>% 
  filter(Station %in% day_stations) %>% 
  mutate(Incub_Time = as.numeric(Incub_Time)) %>% 
  ggplot(aes(x = Station, y = concentration_gain1/Incub_Time, color = as.factor(sizefraction)))+
  geom_point()+
  geom_smooth()+
  scale_x_reverse()+
  facet_wrap(~depth, scale = "free_y")+
  labs(title = "Bgase", y = "Bgase (µM)/h", x = "Station", shape = "Depth")

ggsave("C:/Users/wanja/Documents/Kiel/Master Thesis/enzymes/plots/Agase.jpeg", width = 9, height = 7)




