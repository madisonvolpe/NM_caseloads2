## clean criminal district ## 

# remove empty lists 
dc_2017 <- dc_2017[lapply(dc_2017,length)>0 & 
                     lapply(dc_2017, is.logical)==FALSE]

# convert each to df 
dc_2017 <- lapply(dc_2017, data.frame)

ddply(dat, "V1", summarize, newCol = paste(V3, collapse = ""))


clean_dc <- function(lis){
  lis <- lis[lapply(lis,length) >0 &
          lapply(lis,is.logical)==FALSE] # remove empty lists
  lis <- lapply(lis, data.frame) # make each entry in a list a df 
  lis <- lapply(lis, function(x){
    x[] <- x[lapply(x, function(y) sum(y == ""))!= nrow(x)]
  }) #remove empty columns in each df in the list 
  lis <- lapply(lis, function(df) df[-c(1:3),]) #remove first 3 rows of df
  return(lis)
}

dc_2017_c <- clean_dc(dc_2017)
dc_2016_c <- clean_dc(dc_2016)
dc_2015_c <- clean_dc(dc_2015)


all.spp.data <- lapply(all.spp.data, function(x) {
  x[] <- lapply(x, as.numeric)
  x
})

try <- lapply(dc_2017_c, function(x){
  x [] <- lapply(x[2:ncol(x)], function(y){
      if(sum(grepl(pattern="\\s", y))>10){
        splits.df <- data.frame(do.call(rbind, str_split(string = y, pattern = "\\s")))
      }
  })
})

try <- lapply(try, function(x){
  x <- x[lapply(x, is.null)==FALSE]
  x <- data.frame(x)
})

ex <- lapply(dc_2017_c, function(x){
  x[] <- x[lapply(x[2:ncol(x)],
                  function(y) sum(grepl(pattern="\\s", x = y)))<2]})

