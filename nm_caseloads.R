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

# does not read the criminal court cases, will do manually 

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

setwd("H:/Public/Justice Program/Reports and Pubs/Crim/Arnold Foundation Fees and Fines (Phase II)/Data/Cost Data/New Mexico/Caseloads2")
source("cleaning_functions.R")

mc_2017_c <- clean_mc(mc_2017)
mc_2016_c <- clean_mc(mc_2016)
mc_2015_c <- clean_mc(mc_2015)

## remove statewide states because they were messy - 
mc_2017_c[[20]] <- NULL
mc_2016_c[[20]] <- NULL
mc_2015_c[[20]] <- NULL

## change those other problematic ones ## 
sapply(mc_2017_c, function(x) x[ncol(x)==20])

  ##change 16 
  mc_2017_c[[16]] <- arrange.cols(mc_2017_c[[16]])

sapply(mc_2016_c, function(x) x[ncol(x)==20])

  ##change 1
  mc_2016_c[[1]] <- arrange.cols(mc_2016_c[[1]]) 
  
  ##change 16 
  mc_2016_c[[16]] <- arrange.cols(mc_2016_c[[16]])

sapply(mc_2015_c, function(x) x[ncol(x)==20])

  ## change 1
  mc_2015_c[[1]] <- arrange.cols(mc_2015_c[[1]])
  
  ## change 16
  mc_2015_c[[16]] <- arrange.cols(mc_2015_c[[16]])

## check 
lapply(mc_2017_c, colnames)
lapply(mc_2016_c, colnames)
lapply(mc_2015_c, colnames)

  mc_2015_c[[1]] <- mc_2015_c[[1]][,c(1,2,3,7,8,4,9,10,5,6,11,12,13,14,
                                      15,16,17,18,19,20,21)]
## statewide data
sw_17 <- cleansw(mc_2017[[71]])
sw_17 <- sw_17[[1]][-1]
sw_17[11,] <- sw_17[2,]
sw_17 <- sw_17[-2,]
sw_16 <- cleansw(mc_2016[[69]])
sw_16 <- sw_16[[1]][-1]
sw_15 <- cleansw(mc_2015[[68]])
sw_15 <- sw_15[[1]][-1]

## make final df 
mc_2017_c<-lapply(mc_2017_c, custom_names2)
colnames(sw_17) <- c("Court","Crime","PendStart", "New", "Reopen", "Closed",
           "PendatEnd", "Pending0.6", "Pending6plus", 
           "PendingInact.BW", "JuryTrials", "NonJuryTrials",
           "Convict", "Acquit", "PleaTrial", "DismTrial", 
           "DismBrief", "PleaBef", "DismbyPros", "PostJActiv", "Other")

mc_2016_c <- lapply(mc_2016_c, custom_names2)
colnames(sw_16) <- c("Court","Crime","PendStart", "New", "Reopen", "Closed",
                      "PendatEnd", "Pending0.6", "Pending6plus", 
                      "PendingInact.BW", "JuryTrials", "NonJuryTrials",
                      "Convict", "Acquit", "PleaTrial", "DismTrial", 
                      "DismBrief", "PleaBef", "DismbyPros", "PostJActiv", "Other")

mc_2015_c <- lapply(mc_2015_c, custom_names2)
colnames(sw_15) <- c("Court","Crime","PendStart", "New", "Reopen", "Closed",
                     "PendatEnd", "Pending0.6", "Pending6plus", 
                     "PendingInact.BW", "JuryTrials", "NonJuryTrials",
                     "Convict", "Acquit", "PleaTrial", "DismTrial", 
                     "DismBrief", "PleaBef", "DismbyPros", "PostJActiv", "Other")

rm(list=ls(pattern="\\d$"))