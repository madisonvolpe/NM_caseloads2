clean_cols2 <- function(df){
  col_match <- vector(mode = "character")
  for(i in names(df)){
    if((names(df[i]) != "X1" & names(df[i]) != "court") & 
       (sum(str_detect(string = df[[i]], pattern = "\\s")) == (nrow(df)))){
      col_match[i] <- names(df[i]) 
    }
  }
  j = cSplit(df, splitCols = col_match, sep = " ")
  return(j)
}















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
  #lis <- lapply(lis, function(df) df[rowSums(df[,2:11] == "")!=10,])
  #lis <- lapply(lis, function(df) df[rowSums(df[,2:10] == "") !=9,])
  #lis <- lapply(lis, function(df) mutate_all(df, as.character))
  # #lis <- lapply(lis, function(df) df %>%
  #                 select(court, starts_with("X")))
  #lis <- lapply(lis, clean_cols2)
  #lis <- lapply(lis, dropNAcols)
  # lis <- lapply(lis, function(x){
  #   x[] <- x[lapply(x, function(y) sum(y == ""))!= nrow(x)]
  # }) #remove empty columns in each df in the list 
  return(lis)
    }






# for future reference
# within(df, X7<-data.frame(do.call('rbind', strsplit(as.character(X7),"\\s"))))

# ex3<-separate(ex, ex2[2], sep = "\\s", into =
#                 paste("X", 1:str_count(ex[,ex2[2]][1], 
#                                        pattern = "\\d{1,5}"), sep = "_"))
# 
# new <- NA
# for(i in 1:length(ex2)){
#   ex <- separate(ex, ex2[i], sep = "\\s", into =
#                    paste("X", 1:str_count(ex[,ex2[i]][1],
#                                           pattern = "\\d{1,5}"), sep = "_"))
  

