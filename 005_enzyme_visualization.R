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
  
  #filter(!is.na(concentration_gain1))
day_stations <- c(1,3,5,7,9,11,13,15,17,19,21,23,25,27,28,30,32)
#Theme
theme_set(theme_cowplot())
# Bgase plot
Data %>%
  filter(enzyme == "Agase") %>% filter(!(is.na(concentration_gain1))) %>% 
  mutate(Incub_Time = as.numeric(Incub_Time)) %>% 
  ggplot(aes(x = Station, y = concentration_gain1/Incub_Time, color = as.factor(sizefraction)))+
  geom_point()+
  geom_smooth()+
  scale_x_reverse()+
  #scale_color_viridis()+
  #scale_y_reverse()+
  facet_wrap(~depth,scale = "free_y")+
  labs(title = "Bgase", y = "Bgase (µM)/h", x = "Station", shape = "Depth")

ggsave("C:/Users/wanja/Documents/Kiel/Master Thesis/enzymes/plots/Agase.jpeg", width = 9, height = 7)

# Agase plot
Data %>%
  filter(enzyme == "Bgase") %>%
  ggplot(aes(x = Time, y = concentration_gain1, color = as.factor(depth),shape = as.factor(depth)))+
  geom_point()+
  facet_wrap(~Station, scale = "free_y")+
  labs(title = "Agase", y = "Fluorescence Gain 2", x = "Incubation Time (h)", color = "Depth", shape = "Depth")
ggsave("C:/Users/wanja/Documents/Kiel/Master Thesis/enzymes/plots/Agase.png", width = 9, height = 7)

# NAG plot
Data %>%
  filter(enzyme == "NAG") %>%
  ggplot(aes(x = Time, y = concentration_gain2, color = as.factor(depth),shape = as.factor(depth)))+
  geom_point()+
  geom_smooth()+
  facet_wrap(~Station, scale = "free_y")+
  labs(title = "NAG", y = "Fluorescence Gain 2", x = "Incubation Time (h)", color = "Depth", shape = "Depth")
ggsave("C:/Users/wanja/Documents/Kiel/Master Thesis/enzymes/plots/NAG.png", width = 16, height = 10)

# Apase plot
Data  %>% 
  filter(Station %in% day_stations) %>% 
  filter(enzyme == "Apase", depth == 1) %>%
  ggplot(aes(x = as.factor(Time), y = concentration_gain2, color = as.factor(sizefraction)))+#,color = as.factor(depth),shape = as.factor(depth)))+
  geom_point(position = position_jitter(height = 0, width = 0.1))+
  geom_smooth(method = "lm", formula = )+
  facet_wrap(~Station, scale = "free_y")+
  labs(title = "Apase", y = "Fluorescence Gain 1", x = "Incubation Time (h)", color = "Depth", shape = "Depth")
ggsave("C:/Users/wanja/Documents/Kiel/Master Thesis/enzymes/plots/Apase", width = 16, height = 10)

# Lpase plot
Data %>%
  filter(enzyme == "Lpase") %>%
  ggplot(aes(x = Time, y = concentration_gain1, color = as.factor(depth),shape = as.factor(depth)))+
  geom_point()+
  geom_smooth()+
  facet_wrap(~Station, scale = "free_y")+
  labs(title = "Lpase", y = "Fluorescence Gain 1", x = "Incubation Time (h)", color = "Depth", shape = "Depth")
ggsave("C:/Users/wanja/Documents/Kiel/Master Thesis/enzymes/plots/Lpase.png", width = 16, height = 10)



