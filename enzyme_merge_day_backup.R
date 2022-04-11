library(readxl)
library(tidyverse)
library(tidyxl)
library(xlsx)
setwd("C:/Users/wanja/Documents/Kiel/Master Thesis/enzymes/")
#for(i in 1:32){
  #print(i)
  path= paste("./Enzymes_SO287_20211230/Enzymes_SO287_20211230/Station 6")

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
  
  depth_2_rows <- c("A","B","C", "F", "G","H") # need to check for each station!
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
           Station = 7)
                             
                             
    
  # enz %>% 
  #   ggplot(aes(x = enzyme, y = gain1))+
  #   geom_point()
  # enz %>% 
  #   group_by(depth, Time, enzyme) %>% 
  #   summarise(n()) %>% 
  #   view()
  enz %>% 
    select(depth) %>%
    mutate(depth= as.factor(depth)) %>% unique()
  # Cleanup
  enz %>% 
    select(Station, Well, depth, Time, enzyme, sizefraction, gain1, gain2, gain3) %>% view()
  # file <- paste("Station_",i,".xlsx")
  # # if(is.data.frame(enz) == T){
  #  print(i)
  #   write.xlsx2(enz, file, sheetName = "Sheet1",
  #               col.names = TRUE, row.names = TRUE, append = FALSE)
  # # }
#}  
  
# diagnostics
enz %>% 
  group_by(Time) %>% 
  summarise(n())

empty <- list()
# depending if 2 or 1 depth/size fraction are on 1 plate
WellSetupA <- c("B21","C21","D21","E21","F21","G21","H21","I21","J21","K21","L21","M21","B22","C22","D22","E22","F22","G22","H22","I22","J22","K22","L22","M22")
 
for (o in 1:length(files)){
  empty[[o]] <- xlsx_cells(files[o], sheet = 1)
  empty[[o]] <- empty[[o]][empty[[o]]$data_type == "numeric", c("address","numeric")] %>% 
    filter(address %in% WellSetupA)
}
ID
for (p in 1:length(files)){
  empty[[p]] <- empty[[p]] %>% mutate(ID = ID[[p]][1])
}
empty <- data.table::rbindlist(empty) %>% as_tibble()
empty %>% filter(numeric > 100)

check <- function(empty){
  if(empty %>% filter(numeric > 100) %>% nrow() >10){
    return(warning("setup might be wrong"))
  }
  if(empty %>% filter(numeric > 100) %>% nrow() > 5){
    return(warning("evtl. falsch pipettiert"))
  }
  else(return(warning("all good")))
}  
check(empty)

