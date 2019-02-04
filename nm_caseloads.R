##### Read PDFs and Extract Tables #####
library(rJava)
library(tabulizer)
library(plyr)
library(tidyverse)
library(data.table)
library(splitstackshape)

## directory 

base <- "H:/Public/Justice Program/Reports and Pubs/Crim/Arnold Foundation Fees and Fines (Phase II)/Data/Cost Data/New Mexico/Caseloads2/caseloads"
setwd(base)

## obtain pdfs from directory 

pdfs <- list.files(base)

  
                      ## District Criminal ##
## 2017 

dc_2017 <- list(NA) 

for(i in 6:20){
  dc_2017[[i]] <- extract_tables(pdfs[[3]], i)
}

## 2016

dc_2016 <- list(NA)

for(i in 6:19){
  dc_2016[[i]] <- extract_tables(pdfs[[2]], i)
}

## 2015 
dc_2015 <- list(NA)

for(i in 6:19){
  dc_2015[[i]] <- extract_tables(pdfs[[1]],i)
}

setwd("H:/Public/Justice Program/Reports and Pubs/Crim/Arnold Foundation Fees and Fines (Phase II)/Data/Cost Data/New Mexico/Caseloads2")
source("cleaning_functions.R")

## year 2017 is a special case ## 
dc_2017[[19]][[1]][3,1] <- "Statewide District Court"
dc_2017[[20]][[1]][3,1] <- "Statewide District Court"


# cleaning function
dc_2017_c <- clean_cd(dc_2017)
dc_2016_c <- clean_cd(dc_2016)
dc_2015_c <- clean_cd(dc_2015)


listnames_2017 <- lapply(dc_2017_c, colnames)
dc_2017_c[[7]] <- dc_2017_c[[7]][,c(1:7, 9:20,8)]
dc_2017_c[[13]] <- dc_2017_c[[13]][,c(1:5,7:20,6)]


listnames_2016 <- lapply(dc_2016_c, colnames)
dc_2016_c[[7]] <- dc_2016_c[[7]][,c(1:7,9:20,8)]
dc_2016_c[[13]] <- dc_2016_c[[13]][,c(1:5,7:20,6)]
dc_2016_c[[14]] <- dc_2016_c[[14]][,c(1:7,9:20,8)]

listnames_2015 <- lapply(dc_2015_c, colnames)
dc_2015_c[[7]] <- dc_2015_c[[7]][,c(1:7,9:20,8)]
dc_2015_c[[13]] <- dc_2015_c[[13]][,c(1:5, 7:20,6)]


dc_2017_c <- lapply(dc_2017_c, custom_names)
dc_2016_c <- lapply(dc_2016_c, custom_names)
dc_2015_c <- lapply(dc_2015_c, custom_names)

rm(listnames_2017, listnames_2016, listnames_2015)

dc_2017_c <- do.call(rbind.data.frame, dc_2017_c)
dc_2016_c <- do.call(rbind.data.frame, dc_2016_c)
dc_2015_c <- do.call(rbind.data.frame, dc_2015_c)

## creating group variables ##
dc_2017_c <- create_group(dc_2017_c, 2017)
dc_2016_c <- create_group(dc_2016_c, 2016)
dc_2015_c <- create_group(dc_2015_c, 2015)

## one dataset ## 
rm(dc_2015, dc_2016, dc_2017)

criminal_district <- rbind(dc_2015_c, dc_2016_c, dc_2017_c)

criminal_district <- criminal_district %>%
  arrange(Year)

## write to csv 

setwd("H:/Public/Justice Program/Reports and Pubs/Crim/Arnold Foundation Fees and Fines (Phase II)/Data/Cost Data/New Mexico/Caseloads2/datasets")
write.csv(criminal_district, "CriminalDistrictCaseloads.csv")


                          ## Metro Court ## 


                        ## Magistrate Criminal ## 
## 2017 

mc_2017 <- list(NA) 

for(i in 52:71){
  mc_2017[[i]] <- extract_tables(pdfs[[3]], i)
}

## 2016

mc_2016 <- list(NA)

for(i in 50:69){
  mc_2016[[i]] <- extract_tables(pdfs[[2]], i)
}

## 2015 
mc_2015 <- list(NA)

for(i in 49:68){
  mc_2015[[i]] <- extract_tables(pdfs[[1]],i)
}











