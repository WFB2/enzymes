library(readxl)
library(tidyverse)
library(tidyxl)
library(xlsx)
setwd("C:/Users/wanja/Documents/Kiel/Master Thesis/enzymes/Stations_clean/")

path= paste("./")

files <- dir(path = path, pattern = " .xlsx", full.names = TRUE )
files
## preallocate the container to store the individual 
enz <- list()

### Loop the  file and create a list of station x enzyme files
for (j in 1:length(files)){
  enz[[j]] <- read_excel(files[j], sheet = 1)
}
#enz[[2]] <- remove()
enz <- data.table::rbindlist(enz) %>% as_tibble()
# add target depth from flowcytometry data
Flow_depth <- read_excel("../../FlowCyto/Phytoplankton SO287.xlsx",sheet = 2)

Flow_depth <- Flow_depth %>% 
  select(Station, Target_depth) %>%
  mutate(depth = case_when(Target_depth  == 6 ~ 1,
                           Target_depth == 50 ~ 2,
                           Target_depth > 50 & Target_depth < 200 ~ 3,
                           Target_depth == 200 ~ 4,
                           Target_depth > 200 & Target_depth < 1000 ~ 5,
                           Target_depth == 1000 | Target_depth == 2000 ~ 6,
                           Target_depth > 2000 ~ 7))

enz$depth <- Flow_depth$depth[match(enz$depth,Flow_depth$Target_depth)]
write.xlsx2(enz, "../Enzymes_full.xlsx", sheetName = "Sheet1",
            col.names = T, row.names = T, append = F)        

