library(readxl)
library(tidyverse)
library(tidyxl)
library(xlsx)
setwd("C:/Users/wanja/Documents/Kiel/Master Thesis/enzymes/")

path= paste("./")

files <- dir(path = path, pattern = " .xlsx", full.names = TRUE )
files
## preallocate the container to store the individual 
enz <- list()

### Loop the  file and create a list of station x enzyme files
for (j in 1:length(files)){
  enz[[j]] <- read_excel(files[j], sheet = 1)
}
enz[[2]] <- remove()
enz <- data.table::rbindlist(enz) %>% as_tibble()

write.xlsx2(enz, "Enzymes_full.xlsx", sheetName = "Sheet1",
            col.names = T, row.names = T, append = F)        
