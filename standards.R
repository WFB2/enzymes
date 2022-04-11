library(readxl)
library(tidyverse)
library(tidyxl)
library(xlsx)
setwd("C:/Users/wanja/Documents/Kiel/Master Thesis/enzymes/")
for(i in 1:32){
  if(i != 4 & i != 5){
  print(i)
  path= paste("./Enzymes_SO287_20211230/Enzymes_SO287_20211230/Station",i)
  
  files <- dir(path = path, pattern = "Standard.xlsx", full.names = TRUE )
  files
  ## preallocate the container to store the individual 
  standard <- list()
  
  ### Loop the  file and create a list of station x enzyme files
  for (j in 1:length(files)){
    standard[[j]] <- read_excel(files[j], sheet = 2, skip = 12)
  }


  
  standard <- as_tibble(standard[[1]])
  # create columns for Well position and seperate plate ID from depth/size fraction
  #renaming gain columns
  standard <- standard %>% 
    separate(Well,c("well_row","well_col"),sep = "[A-H]", extra = "merge", remove = F, convert = T) %>% 
    separate(Well,c("well_row","col2"),sep = "[0-9]", extra = "merge", remove = F, convert = T) %>% 
    dplyr::select(-"col2") %>% 
    rename(gain1 = `Raw Data (355/460 1)`, gain2 = `Raw Data (355/460 2)`, gain3 = `Raw Data (355/460 3)`)
  
  standard <- standard %>% 
    mutate(concentration = case_when(
      (well_col == 1 ~ 0.019),
      (well_col == 2 ~ 0.038),
      (well_col == 3 ~ 0.075),
      (well_col == 4 ~ 0.3),
      (well_col == 5 ~ 1.5),
      (well_col == 6 ~ 3),
      (well_col == 7 ~ 15),
      (well_col == 8 ~ 30)
      ))
  
  MCA <- c("F", "G","H") 
  MUF <- c("A","B","C")  
  
  # add enzymes based on well position and no. of samples on plate
  # remove empty wells
  standard <- standard %>%
    mutate(Standard = case_when(
      (well_row %in% MCA & well_col <= 8 ~ "MCA"),
      (well_row %in% MUF & well_col <= 8 ~"MUF"))) %>% 
    filter(!is.na(Standard)) ## need to include threshold! not just exclude NAs

  
 
  # adding station number
  
  standard <- standard %>% mutate(Station = i)
  
  
  # Cleanup
  standard <- standard %>% 
    select(Station, Well, Standard, concentration, gain1, gain2, gain3) 

  
  
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
  empty <- as_tibble(empty[[1]])

  errors <- empty %>% mutate(gain = case_when(address %in% Gain1SetupA ~ "gain_1",
                                              address %in% Gain2SetupA ~ "gain_2",
                                              address %in% Gain3SetupA ~ "gain_3")) %>% 
    filter((numeric > 100 & gain == "gain_1") | (numeric > 500 & gain == "gain_2") | (numeric > 2000 & gain == "gain_3"))
  
  
  file <- paste("./Standards/Standard",i,".xlsx")
  errorfile <- paste("./Standards/errors/Standard",i,"errors.xlsx")
  if(is.data.frame(standard) == T){
    print(i)
    write.xlsx2(standard, file, sheetName = "Sheet1",
                col.names = TRUE, row.names = TRUE, append = FALSE)
    if(nrow(errors) != 0){
      write.xlsx2(errors, errorfile, sheetName = "Sheet1",
                  col.names = TRUE, row.names = TRUE, append = FALSE)
    }
  }
  }  
}  


