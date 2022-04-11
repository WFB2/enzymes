library(readxl)
library(tidyverse)
library(tidyxl)
library(xlsx)
setwd("C:/Users/wanja/Documents/Kiel/Master Thesis/enzymes/")

path= paste("./Enzymes_SO287_20211230/Enzymes_SO287_20211230/Station 29")

files <- dir(path = path, pattern = ".xlsx", full.names = TRUE )
files
## preallocate the container to store the individual 
enz <- list()

### Loop the  file and create a list of station x enzyme files
for (j in 1:length(files)){
  enz[[j]] <- read_excel(files[j], sheet = 2, skip = 12)
}
ID <- list()
for (k in 1:length(files)){
  ID[[k]] <- xlsx_cells(files[k], sheet = 1)
  ID[[k]] <- ID [[k]][ID[[k]]$data_type == "character",c("address", "character")] %>%
    filter(address == "A10" | address == "A11") %>%
    pull(character) %>% substring(6)
}
ID
ID[[7]][2] # for some reason this works but calling it inside the loop to create the time col doesnt.

lstTime <- list() # that's why I created another list

for (l in 1:length(files)){
  lstTime[l] <-ID[[l]][2]
  
}
# adding the ID and the incubation time to the main list of tables
for (m in 1:length(files)){
  enz[[m]] <- enz[[m]] %>% mutate(ID = ID[[m]][1]) %>% mutate(Time = lstTime[[m]])
}
# merging the list of tibbles
enz <- data.table::rbindlist(enz) %>% as_tibble()


# create columns for Well position and seperate plate ID from depth/size fraction
#renaming gain columns
enz <- enz %>% 
  separate(Well,c("well_row","well_col"),sep = "[A-H]", extra = "merge", remove = F, convert = T) %>% 
  separate(Well,c("well_row","col2"),sep = "[0-9]", extra = "merge", remove = F, convert = T) %>% 
  tidyr::extract(ID, into = c("plate", "depth"), regex = "(...)(_.*)") %>% #splitting ID col into plate and depth column
  dplyr::select(-"col2") %>% 
  rename(gain1 = `Raw Data (355/460 1)`, gain2 = `Raw Data (355/460 2)`, gain3 = `Raw Data (355/460 3)`)

depth_2_rows <- c("A","B","C", "E","F", "G") # need to check for each station!
depth_1_rows <- c("A","B","C")  
# add enzymes based on well position and no. of samples on plate
# remove empty wells
enz %>% 
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
  filter(!is.na(enzyme) & gain1 > 200) -> enz

enz <- enz %>% 
  mutate(sizefraction = if_else(str_detect(string = depth, "_d\\d_d\\d.?3.m") == TRUE & well_row %in% depth_1_rows, 1,0)) 

# restructuring depth column
enz <- enz %>% 
  mutate(depth = case_when(str_detect(string = depth, "_d1_d1.?3.m") == TRUE   ~ 1,
                           str_detect(string = depth, "_d2_d2.?3.m") == TRUE   ~ 2,
                           str_detect(string = depth, "_d3_d3.?3.m") == TRUE   ~ 3,
                           str_detect(string = depth, "_d4_d4.?3.m") == TRUE   ~ 4,
                           str_detect(string = depth, "_d5") == TRUE   ~ 5,
                           str_detect(string = depth, "_d6") == TRUE   ~ 6),
         depth = as.numeric(depth),
         Time = as.numeric(substring(Time,2)),
         Station = i)




enz %>% 
  select(depth) %>%
  mutate(depth= as.factor(depth)) %>% unique()
# Cleanup
enz <- enz %>% 
  select(Station, Well, depth, Time, enzyme, sizefraction, gain1, gain2, gain3) 
# diagnostics
enz %>% 
  group_by(Time) %>% 
  summarise(n())


# depending if 2 or 1 depth/size fraction are on 1 plate
Gain1SetupA <- c("B21","C21","D21","E21","F21","G21","H21","I21","J21","K21","L21","M21",
                "B22","C22","D22","E22","F22","G22","H22","I22","J22","K22","L22","M22")
Gain2SetupA <- c("B32","C32","D32","E32","F32","G32","H32","I32","J32","K32","L32","M32",
                "B33","C33","D33","E33","F33","G33","H33","I33","J33","K33","L33","M33")
Gain3SetupA <- c("B43","C43","D43","E43","F43","G43","H43","I43","J43","K43","L43","M43",
                 "B44","C44","D44","E44","F44","G44","H44","I44","J44","K44","L44","M44")
empty <- list()
for (o in 1:length(files)){
  empty[[o]] <- xlsx_cells(files[o], sheet = 1)
  empty[[o]] <- empty[[o]][empty[[o]]$data_type == "numeric", c("address","numeric")] %>% 
    filter(address %in% Gain1SetupA | address %in% Gain2SetupA | address %in% Gain3SetupA)
}
ID

for (p in 1:length(files)){
  empty[[p]] <- empty[[p]] %>% mutate(ID = ID[[p]][1])
}
# filter out possible errors by specific thresholds for each gain
empty <- data.table::rbindlist(empty) %>% as_tibble()
errors <- empty %>% mutate(gain = case_when(address %in% Gain1SetupA ~ "gain_1",
                                            address %in% Gain2SetupA ~ "gain_2",
                                            address %in% Gain3SetupA ~ "gain_3")) %>% 
  filter((numeric > 100 & gain == "gain_1") | (numeric > 500 & gain == "gain_2") | (numeric > 2000 & gain == "gain_3"))

check <- function(empty){
  if(empty %>% filter(numeric > 100) %>% nrow() >5){
    return(warning("evtl. falsch pipettiert"))
    if(empty %>% filter(numeric > 100) %>% nrow() > 50){
      return(warning("setup might be wrong"))
    }
  }
  
  else(return(warning("all good")))
}  
check(empty)


file <- paste("Station_ 3 .xlsx")
errorfile <- paste("Station_ 3 errors.xlsx")
if(is.data.frame(enz) == T){
  write.xlsx2(enz, file, sheetName = "Sheet1",
              col.names = TRUE, row.names = TRUE, append = FALSE)
  write.xlsx2(errors, errorfile, sheetName = "Sheet1",
              col.names = TRUE, row.names = TRUE, append = FALSE)

}




