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
  lis <- lapply(lis, function(df) df[rowSums(df[,2:11] == "")!=10,])
  lis <- lapply(lis, function(df) df[rowSums(df[,2:10] == "") !=9,])
  lis <- lapply(lis, function(df) mutate_all(df, as.character))
  lis <- lapply(lis, clean_cols)
  lis <- lapply(lis, dropNAcols)
  lis <- lapply(lis, function(x){
    x[] <- x[lapply(x, function(y) sum(y == ""))!= nrow(x)]
  }) #remove empty columns in each df in the list 
  return(lis)
    }




# for future reference
# within(df, X7<-data.frame(do.call('rbind', strsplit(as.character(X7),"\\s"))))
