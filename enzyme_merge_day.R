library(readxl)
library(tidyverse)
library(tidyxl)
library(xlsx)
setwd("C:/Users/wanja/Documents/Kiel/Master Thesis/enzymes/")

day_stations <- c(3,5,7,9,11,13,15,17,19,21,23,25,27,28,30,32)
for(i in 1:32){
  print(i)
  path= paste("./Enzymes_SO287_20211230/Enzymes_SO287_20211230/Station 31")

  files <- dir(path = path, pattern = "\\d.xlsx", full.names = TRUE ) #not including the standard.xlsx file
  files
  ## preallocate the container to store the individual excel files
  enz <- list()
  
  ### Loop the  file and create a list of station x enzyme files
  for (j in 1:length(files)){
    enz[[j]] <- read_excel(files[j], sheet = 2, skip = 12)
  }
  #preallocate IDs
  ID <- list()
  #extract incubation time and depth
  for (k in 1:length(files)){
    ID[[k]] <- xlsx_cells(files[k], sheet = 1)
    ID[[k]] <- ID [[k]][ID[[k]]$data_type == "character",c("address", "character")] %>%
      filter(address == "A10" | address == "A11") %>%
      pull(character) %>% substring(6)
  }
  ID
  ID[[7]][2] # for some reason this works but calling it inside the loop to create the time col doesn't.
  
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
    tidyr::extract(ID, into = c("plate", "depth"), regex = "((?!_)(?!d)..?[_| ]?\\d)(_.*)") %>% #splitting ID col into plate and depth column
    dplyr::select(-"col2") %>% 
    rename(gain1 = `Raw Data (355/460 1)`, gain2 = `Raw Data (355/460 2)`, gain3 = `Raw Data (355/460 3)`)

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
  empty <- data.table::rbindlist(empty) %>% as_tibble()
  errors <- empty %>% mutate(gain = case_when(address %in% Gain1SetupA ~ "gain_1",
                                              address %in% Gain2SetupA ~ "gain_2",
                                              address %in% Gain3SetupA ~ "gain_3")) %>% 
    filter((numeric > 200 & gain == "gain_1") | (numeric > 1000 & gain == "gain_2") | (numeric > 4000 & gain == "gain_3"))
  num_of_errors <- errors %>% group_by(gain) %>% summarise(n())
  
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
  
  if(sum(num_of_errors$`n()`) > 30){
    depth_2_rows <- c("A","B","C", "E", "F","G") # need to double check for each station!
    depth_1_rows <- c("A","B","C")
    
  }else{
  depth_2_rows <- c("A","B","C", "F", "G","H") # need to double check for each station!
  depth_1_rows <- c("A","B","C")  
  }
  # add enzymes based on well position and no. of samples on plate
  # remove empty wells
 enz <- enz %>%
    filter(!is.na(depth)) %>% 
    mutate(enzyme = if_else(nchar(depth) > 3, case_when(
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
   filter(!is.na(enzyme))
  
  enz <- enz %>%
    mutate(sizefraction = if_else(str_detect(string = depth, "_d\\d_d\\d.?3.m") == TRUE & well_row %in% depth_1_rows, 1,0)) 
  # adding station number
  
  enz <- enz %>% mutate(Station = i)
  
  # restructuring depth column
    enz <- enz %>% mutate(depth = case_when(
      str_detect(string = depth, "_d1_d1.?3.m") == TRUE   ~ 1,
      str_detect(string = depth, "_d1_d2") == TRUE & well_row %in% depth_1_rows ~ 1,
      str_detect(string = depth, "_d2_d2.?3.m") == TRUE   ~ 2,
      str_detect(string = depth, "_d1_d2") == TRUE & !(well_row %in% depth_1_rows) ~ 2,
      str_detect(string = depth, "_d3_d3.?3.m") == TRUE   ~ 3,
      str_detect(string = depth, "^_d3$") == TRUE ~ 3,
      str_detect(string = depth, "_d4_d4.?3.m") == TRUE   ~ 4,
      str_detect(string = depth, "^_d4$") == TRUE ~ 4,
      str_detect(string = depth, "^_d5$") == TRUE   ~ 5,
      str_detect(string = depth, "_d5_d6") == TRUE & well_row %in% depth_1_rows ~ 5,
      str_detect(string = depth, "^_d6$") == TRUE   ~ 6,
      str_detect(string = depth, "_d5_d6") == TRUE & !(well_row %in% depth_1_rows) ~ 6,
      str_detect(string = depth, "_d6_d7") == TRUE & well_row %in% depth_1_rows ~ 6,
      str_detect(string = depth, "_d6_d7") == TRUE & !(well_row %in% depth_1_rows) ~ 7,
      str_detect(string = depth, "^_d7$") == TRUE ~ 7)) 
           
  # enz <- enz %>% mutate(depth = case_when(
  #   ## day stations
  #   str_detect(string = depth, "_d1_d1.?3.m") == TRUE   ~ 1,
  #   str_detect(string = depth, "_d2_d2.?3.m") == TRUE   ~ 2,
  #   str_detect(string = depth, "_d3_d3.?3.m") == TRUE   ~ 3,
  #   str_detect(string = depth, "_d4_d4.?3.m") == TRUE   ~ 4,
  #   str_detect(string = depth, "^_d5$") == TRUE   ~ 5,
  #   str_detect(string = depth, "^_d6$") == TRUE   ~ 6))
  # }
  # if(!(4 %in% day_stations)){
  #   ## night stations
  # enz <- enz %>% mutate(depth = case_when( 
  #   str_detect(string = depth, "_d1_d2") == TRUE & well_row %in% depth_1_rows ~ 1,
  #   str_detect(string = depth, "_d1_d2") == TRUE & !(well_row %in% depth_1_rows) ~ 2,
  #   str_detect(string = depth, "^_d3$") == TRUE ~ 3,
  #   str_detect(string = depth, "^_d4$") == TRUE ~ 4,
  #   str_detect(string = depth, "^_d5$") == TRUE   ~ 5,
  #   str_detect(string = depth, "^_d6$") == TRUE   ~ 6,
  #   str_detect(string = depth, "_d5_d6") == TRUE & well_row %in% depth_1_rows ~ 5,
  #   str_detect(string = depth, "_d5_d6") == TRUE & !(well_row %in% depth_1_rows) ~ 6,
  #   str_detect(string = depth, "_d6_d7") == TRUE & well_row %in% depth_1_rows ~ 6,
  #   str_detect(string = depth, "_d6_d7") == TRUE & !(well_row %in% depth_1_rows) ~ 7,
  #   str_detect(string = depth, "^_d7$") == TRUE ~ 7))
  # }                  
  enz <-  enz  %>% 
    mutate(depth = as.numeric(depth),
         Time = as.numeric(substring(Time,2)))
  
  
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
  
  

  
  
  file <- paste("Station_",i,".xlsx")
  errorfile <- paste("Station_",i,"errors.xlsx")
   if(is.data.frame(enz) == T){
   print(i)
    write.xlsx2(enz, file, sheetName = "Sheet1",
                col.names = TRUE, row.names = TRUE, append = FALSE)
    if(nrow(errors) != 0){
      write.xlsx2(errors, errorfile, sheetName = "Sheet1",
                  col.names = TRUE, row.names = TRUE, append = FALSE)
  }
 }
}
depth_na
