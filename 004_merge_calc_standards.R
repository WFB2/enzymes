library(readxl)
library(tidyverse)
library(tidyxl)
library(xlsx)
setwd("C:/Users/wanja/Documents/Kiel/Master Thesis/enzymes/Standards/")

path= paste("./")

files <- dir(path = path, pattern = " .xlsx", full.names = TRUE )
files
## preallocate the container to store the individual 
standard <- list()

### Loop the  file and create a list of station x enzyme files
for (j in 1:length(files)){
  standard[[j]] <- read_excel(files[j], sheet = 1)
}

standard <- data.table::rbindlist(standard) %>% as_tibble()
standard <- standard %>% filter(Station != 4 & Station != 5) 
standard %>%
  group_by(Station, Standard) %>% 
  ggplot(aes(y = gain2, x = concentration, color = Standard))+
  geom_point()+
  geom_smooth(method = "lm")+
  facet_wrap(~Station)


library(broom)
Gain1 <- standard %>% group_by(Station,Standard) %>% do(tidy(lm(data= .,gain1~concentration))) %>% 
  select(Station,Standard,estimate,term) %>% 
  pivot_wider(names_from = term,values_from = estimate) %>% 
  mutate(gain1slope = concentration - `(Intercept)`) %>% 
  select(-`(Intercept)`,-concentration) 

Gain2 <- standard %>% group_by(Station,Standard) %>% do(tidy(lm(data= .,gain2~concentration))) %>%
  select(Station,Standard,estimate,term) %>% 
  pivot_wider(names_from = term,values_from = estimate) %>% 
  mutate(gain2slope = concentration - `(Intercept)`) %>% 
  select(-`(Intercept)`,-concentration)


Gain3 <- standard %>% 
  filter(concentration < 5) %>% 
  group_by(Station,Standard) %>% do(tidy(lm(data= .,gain3~concentration))) %>% 
  select(Station,Standard,estimate,term) %>% 
  pivot_wider(names_from = term,values_from = estimate) %>% 
  mutate(gain3slope = concentration - `(Intercept)`) %>% 
  select(-`(Intercept)`,-concentration) 

Standards <- full_join(Gain1,Gain2) %>% full_join(Gain3) %>% ungroup()

write.xlsx2(Standards, "Standards_full.xlsx", sheetName = "Sheet1",
            col.names = T, row.names = T, append = F)



standard %>% 
  group_by(Station, Standard) %>% 
  #summarise(average_standard_gain1 = mean(gain1)) %>% 
  ggplot(aes(x = Station, y = gain1, color = Standard))+
  geom_point()+
  geom_smooth()+
  facet_wrap(~concentration, scales = "free_y")+
  scale_x_reverse()
