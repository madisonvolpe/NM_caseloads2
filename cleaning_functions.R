## clean columns function ## 

clean_cols <- function(df){
  col_match <- vector(mode = "character")
  for(i in names(df)){
       if(names(df[i]) != "X1" & sum(str_detect(string = df[[i]], pattern = "\\s")) ==
       (nrow(df)-2) |
       names(df[i]) != "X1" & sum(str_detect(string = df[[i]],pattern = "\\s")) ==
       (nrow(df)-1)){
    col_match[i] <- names(df[i]) 
      }
    }
j = cSplit(df, splitCols = col_match, sep = " ")
return(j)
}

clean_cols <- purrr::possibly(clean_cols, otherwise = NA)

## drop NA columns ##

dropNAcols <- function(df){
df <- data.frame(df)
df <- df[,colSums(is.na(df))<nrow(df)]
return(df)
}

## make 1st row blank 

first_blank <-  function(df){
df[1, 2:length(df)] <- str_replace_all(string = df[1,2:length(df)],
                                       pattern = "[\\w\\.]",
                                       replacement = "")
return(df)
}

## clean cols2 

clean_cols2 <- function(df){
  col_match <- vector(mode = "character")
  for(i in names(df)){
    if(!names(df[i]) %in% c("X1", "court") & 
       sum(str_detect(string = df[[i]], pattern = "\\s")) == (nrow(df))){
      col_match[i] <- names(df[i]) 
    }
  }
  j = cSplit(df, splitCols = col_match, sep = " ")
  return(j)
}

## clean cols3 

clean_cols3 <- function(df){
  col_match <- vector(mode = "character")
  for(i in names(df)){
    if(!names(df[i]) %in% c("X1", "court") & 
       sum(str_detect(string = df[[i]], pattern = "\\s")) >=1 ){
      col_match[i] <- names(df[i]) 
    }
  }
  j = cSplit(df, splitCols = col_match, sep = " ")
  return(j)
}


## set colnames 
custom_names <- function(df){
  colnames(df) <- c("Court","PendStart", "New", "Reopen", "Closed",
                    "PendatEnd", "Pending0.6", "Pending6plus", 
                    "PendingInact.BW", "JuryTrials", "NonJuryTrials",
                    "Convict", "Acquit", "PleaTrial", "DismTrial", 
                    "DismBrief", "PleaBef", "DismbyPros", "PostJActiv", "Other")
  return(df)
}

custom_names2 <- function(df){
  colnames(df) <- c("Court","Crime","PendStart", "New", "Reopen", "Closed",
                    "PendatEnd", "Pending0.6", "Pending6plus", 
                    "PendingInact.BW", "JuryTrials", "NonJuryTrials",
                    "Convict", "Acquit", "PleaTrial", "DismTrial", 
                    "DismBrief", "PleaBef", "DismbyPros", "PostJActiv", "Other")
  return(df)
}

## clean criminal district ## 

clean_cd <- function(lis){
  
  ## cleaning 
  lis <- lis[lapply(lis,length) >0 &
          lapply(lis,is.logical)==FALSE] # remove empty lists
  lis <- lapply(lis, data.frame) # make each entry in a list a df 
  lis <- lapply(lis, function(x){
    x[] <- x[lapply(x, function(y) sum(y == ""))!= nrow(x)]
  }) #remove empty columns in each df in the list 
  lis <- lapply(lis, function(df) df[-c(1:2),]) #remove first 2 rows of df
  lis <- lapply(lis, function(df) mutate_all(df, as.character))
  lis <- lapply(lis, first_blank)
  lis <- lapply(lis, clean_cols)
  lis <- lapply(lis, dropNAcols)
return(lis)
}

## Create Grouping Variables ## 

create_group <- function(df,num){
  
  courts <- c("First District Court", "Second District Court",
              "Third District Court", "Fourth District Court",
              "Fifth District Court", "Sixth District Court", 
              "Seventh District Court", "Eighth District Court",
              "Ninth District Court", "Tenth District Court", 
              "Eleventh District Court", "Twelfth District Court",
              "Thirteenth District Court", "Statewide District Court")
   
  df <- df %>%
    mutate(court.type = case_when(
      grepl(pattern = "District Court",x=lag(Court,1))~
        lag(Court,1))) %>%
    filter(!Court %in% courts)
  
  while(length(ind <- which(is.na(df[,"court.type"]))) > 0){
    df[,"court.type"][ind] <- df[,"court.type"][ind -1]
  }
  
  df <- df %>%
    mutate(Year = num)
  
  return(df)
}

## clean magistrate criminal

clean_mc <- function(lis){
  
  ## cleaning 
  lis <- lis[lapply(lis,length) >0 &
               lapply(lis,is.logical)==FALSE] # remove empty lists
  lis <- lapply(lis, data.frame) # make each entry in a list a df
  
  lis <- lapply(lis, function(df) df %>%
                  mutate(court = ifelse(grepl("Magistrate Court",x=df[,"X1"]),
                                        as.character(X1), NA)) %>%
                  mutate(court = ifelse(lead(X1,1)=="Magistrate Court" |
                                          lead(X1,1) == "Court", 
                                        paste(X1, lead(X1,1), sep = " "), court)) %>%
                  mutate(court = ifelse(court == "Magistrate Court",
                                        NA, court)))
  lis <- lapply(lis, function (df) df %>%
                  tidyr::fill(court))
  lis <- lapply(lis, function(df) df[-c(1:3),])
  lis <- lapply(lis, function(df) df[rowSums(df[,1:ncol(df)] == "")!=
                                       ncol(df)-2,])
  lis <- lapply(lis, function(df) mutate_all(df, as.character))
  lis <- lapply(lis, function(df) df %>%
                  select(court, starts_with("X")))
  lis <- lapply(lis, function(df) na.omit(df))
  lis <- lapply(lis, clean_cols2)
  lis <- lapply(lis, dropNAcols)
  lis <- lapply(lis, function(x){
     x[] <- x[lapply(x, function(y) sum(y == ""))!= nrow(x)]
   }) #remove empty columns in each df in the list
  return(lis)
}

## arranging problematic columns ##
arrange.cols <- function(df){
  re <- "\\s[0-9]{1,5}"
  cleaned <- cbind(df, with(df, data.frame(name=substr(X1, 1L, regexpr(re,X1)),
                                           value=substr(X1, regexpr(re, X1) + 1L, 1000L))))
  cleaned <- cleaned[,c(1,21,22,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20)]
  cleaned <- cleaned[,-c(4)]
  return(cleaned)
}

### cleaning statewide ## 
cleansw <- function(lis){
  lis <- lapply(lis, data.frame) # make each entry in a list a df
  lis <- lapply(lis, function(df) df %>%
                  mutate(court = "Statewide Magistrate Court"))
  lis <- lapply(lis, function(df) df[-c(1:3),])
  lis <- lapply(lis, function(df) df[rowSums(df[,1:ncol(df)] == "")!=
                                       ncol(df)-2,])
  lis <- lapply(lis, function(df) mutate_all(df, as.character))
  lis <- lapply(lis, function(df) df %>%
                  select(court, starts_with("X")))
  lis <- lapply(lis, function(df) na.omit(df))
  lis <- lapply(lis, function(x){
    x[] <- x[lapply(x, function(y) sum(y == ""))!= nrow(x)]
  }) #remove empty columns in each df in the list
  lis <- lapply(lis, clean_cols3)
  lis <- lapply(lis, dropNAcols)
  lis <- lapply(lis, function(df) df %>%
                  rownames_to_column() %>%
                  gather(variable, value, -rowname) %>%
                  filter(!is.na(value)) %>%
                  group_by(rowname) %>%
                  mutate(indx = row_number())%>%
                  select(-variable) %>%
                  spread(indx, value) %>%
                  select("1", "2","4", "5", "3", "6", "7", "8", "9", "10", "11",
                         "12", "13", "14", "15", "16", "17", "18", "19", "20", "21", -rowname)
  )
  return(lis)
}









