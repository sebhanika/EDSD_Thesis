
# EDSD Thesis 2021/22
# Functions file
# Author: Sebstian Hanika
# Date: 07/03/2022

# This file contains self-defined functions used for my EDSD thesis



# Functions data exploration ----------------------------------------------


# create named list of dataframes 
listn <- function(...) {
  objs <- as.list(substitute(list(...)))[-1L]
  nm <- as.character(objs)
  v <- lapply(nm,get)
  names(v) <- nm
  return(v)
}

# function for counting NAs in each datframe
count_NA <- function(dataframe){
  counts <- dataframe %>%
    summarize(across(everything(), ~sum(is.na(.))/length(.)*100))
}



# summariy function
try1 <- function(dataframe){
  dataframe %>% 
  summarize(across(.cols = where(is.numeric),
                   .names = "{.col}__{.fn}",
                   .fns = list(nmiss = ~sum(is.na(.))/length(.)*100,
                               mean = ~mean(.x, na.rm = TRUE),
                               min = ~min(.x, na.rm = TRUE),
                               max = ~max(.x, na.rm = TRUE),
                               sd = ~sd(.x, na.rm = TRUE)
                   )
  )
  ) 

}


# apply function across all dataframes and change names
output_dfs <- function(listdf, func){
  results2 <- sapply(listdf, func, USE.NAMES = TRUE)
  d <- names(results2)
  names(results2) <- paste("NA", d, sep="_")
  return(results2)
}

