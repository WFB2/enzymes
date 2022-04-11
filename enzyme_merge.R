library(readxl)
library(tidyverse)
library(tidyxl)
setwd("C:/Users/wanja/Documents/Kiel/Master Thesis/enzymes/")
files <- dir(path = "./Enzymes_SO287_20211230/Enzymes_SO287_20211230/Station 1/", pattern = ".xlsx", full.names = TRUE )
files
## preallocate the container to store the individual 
enz <- list()

### Loop the  file and create a list of station x enzyme files
for (i in 1:length(files)){
  enz[[i]] <- read_excel(files[i], sheet = 2, skip = 12)
}
ID <- list()
for (i in 1:length(files)){
  ID[[i]] <- xlsx_cells(files[i], sheet = 2)
  ID[[i]] <- ID [[i]][ID[[i]]$data_type == "character",c("address", "character")] %>%
    filter(address == "A8" | address == "A9") %>%
    pull(character) %>% substring(6)
}

ID[[7]][2]
VecID <- list()
VecTime <- list()

for (i in 1:length(files)){
  VecTime[i] <-ID[[i]][2]
  VecID[i] <- ID[[i]][1]
}


for (i in 1:length(files)){
  enz[[i]] <- enz[[i]] %>% mutate(ID = ID[[i]][1]) %>% mutate(Time = VecTime[[i]])
}

enz <- data.table::rbindlist(enz) %>% as_tibble()
enz %>% view()

depth_2_rows <- c("A","B","C","E", "F", "G")
depth_1_rows <- c("A","B","C")
Bpase <- c("A01","A02","A03","A05", "A06", "A07")
enz %>% mutate(enzyme = 
                 if_else(Well %in% Apase, "Apase","BBBB" ))
enz %>% 
  separate(Well,c("well_row","well_col"),sep = "[A-H]", extra = "merge", remove = F, convert = T) %>% 
  separate(Well,c("well_row","col2"),sep = "[0-9]", extra = "merge", remove = F, convert = T) %>% 
  extract(ID, into = c("plate", "depth"), regex = "(...)(_.*)") %>%
  dplyr::select(-"col2") %>% 
  mutate(enzyme = if_else(nchar(depth) > 3,case_when(
                                    (well_row %in% depth_2_rows & well_col == 1 ~ "Agase"),
                                    (well_row %in% depth_2_rows & well_col == 3 ~"Bgase"),
                                    (well_row %in% depth_2_rows & well_col == 5 ~"NAG"),
                                    (well_row %in% depth_2_rows & well_col == 7 ~"Apase"),
                                    (well_row %in% depth_2_rows & well_col == 9 ~"Lpase")),
                          case_when((well_row %in% depth_1_rows & well_col == 1 ~ "Agase"),
                                    (well_row %in% depth_1_rows & well_col == 3 ~"Bgase"),
                                    (well_row %in% depth_1_rows & well_col == 5 ~"NAG"),
                                    (well_row %in% depth_1_rows & well_col == 7 ~"Apase"),
                                    (well_row %in% depth_1_rows & well_col == 9 ~"Lpase")))) %>% 
  filter(!is.na(enzyme)) %>% 
  mutate(depth = case_when(well_row %in% depth_1_rows & depth == "_d1_d2" ~ "_d1",
                           !(well_row %in% depth_1_rows) & depth == "_d1_d2" ~ "_d2",
                           depth != "d1_d2" ~ depth)) %>% 
  rename(gain1 = `Raw Data (355/460 1)`, gain2 = `Raw Data (355/460 2)`, gain3 = `Raw Data (355/460 3)`)
      



