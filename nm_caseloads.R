##### Read PDFs and Extract Tables #####
library(rJava)
library(tabulizer)
library(plyr)
library(tidyverse)
library(data.table)

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



























#remove empty lists from pdf_tables
pdf_tables_2017 <- pdf_tables_2017[lapply(pdf_tables_2017,length)>0]

# list entries of length 4 or greater  
greater4 <- pdf_tables_2017[lapply(pdf_tables_2017,length)>=4]
pdf_tables_2017 <- pdf_tables_2017[lapply(pdf_tables_2017,length)==1]

greater4_ed1 <- NA 
for(i in 1:length(greater4)){
  greater4_ed1[[i]] <- list(greater4[[i]][1]) 
}
greater4_ed2 <- NA
for(i in 1:length(greater4)){
  greater4_ed2[[i]] <- list(greater4[[i]][2])  
}
greater4_ed3 <- NA
for(i in 1:length(greater4)){
  greater4_ed3[[i]] <- list(greater4[[i]][3])  
}
greater4_ed4 <- NA
for(i in 1:length(greater4)){
  greater4_ed4[[i]] <- list(greater4[[i]][4])  
}

greater4 <- c(greater4_ed1, greater4_ed2, greater4_ed3, greater4_ed4)
rm(greater4_ed1,greater4_ed2,greater4_ed3,greater4_ed4)

#add individual lists to pdf_tables_2017
pdf_tables_2017 <- c(pdf_tables_2017, greater4)

pdf_tables_2017_dfs <- lapply(pdf_tables_2017, data.frame)

