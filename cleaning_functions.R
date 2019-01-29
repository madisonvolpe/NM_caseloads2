## clean columns function ## 

clean_cols <- function(df){
  col_match <- vector(mode = "character")
  for(i in names(df)){
    if(sum(str_detect(string = df[[i]], pattern = "\\s")) ==
       (nrow(df)-2)){
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

## clean criminal district ## 

clean_cd <- function(lis){
  
  ## cleaning 
  lis <- lis[lapply(lis,length) >0 &
          lapply(lis,is.logical)==FALSE] # remove empty lists
  lis <- lapply(lis, data.frame) # make each entry in a list a df 
  lis <- lapply(lis, function(x){
    x[] <- x[lapply(x, function(y) sum(y == ""))!= nrow(x)]
  }) #remove empty columns in each df in the list 
  
  names(lis) <- 
  
  lis <- lapply(lis, function(df) df[-c(1:3),]) #remove first 3 rows of df
  lis <- lapply(lis, function(df) mutate_all(df, as.character))
  lis <- lapply(lis, clean_cols)
  lis <- lapply(lis, dropNAcols)
return(lis)
}

dc_2017_c <- clean_cd(dc_2017)
dc_2016_c <- clean_cd(dc_2016)
dc_2015_c <- clean_cd(dc_2015)











