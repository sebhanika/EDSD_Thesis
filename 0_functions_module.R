
# EDSD Thesis 2021/22
# Functions file
# Author: Sebstian Hanika
# Date: 07/03/2022

# This file contains self-defined functions used for my EDSD thesis



# Functions data exploration ----------------------------------------------


# these three functions are constructed to take a list of dataframes:
# and create a named list of dataframes then (1) 
# calculate the NAs and summary statistics (2)
# and then output one single dataframe (3) where the colums are the 
# summary statistics variables and each row is one variable in the dataframe


# (1) create named list of dataframes 
listn <- function(...) {
  objs <- as.list(substitute(list(...)))[-1L]
  nm <- as.character(objs)
  v <- lapply(nm,get)
  names(v) <- nm
  return(v)
}


# (2) summary statistsics function function
summary_func <- function(dataframe){
  temp_list <- dataframe %>% 
    summarize(across(.cols = where(is.numeric),
                     .names = "{.col}__{.fn}", #double for pivoting later
                     .fns = list(nmiss = ~sum(is.na(.))/length(.)*100,
                                 mean = ~mean(.x, na.rm = TRUE),
                                 min = ~min(.x, na.rm = TRUE),
                                 max = ~max(.x, na.rm = TRUE),
                                 sd = ~sd(.x, na.rm = TRUE)
                     ))) %>%
    pivot_longer(everything(), 
                 names_to = c("variable", "measure"),
                 names_sep = "__") %>% # now it is too long
    pivot_wider(names_from = "measure", values_from = "value")
}


# 3() apply summary stat function across all dataframes and rowbind
output_dfs <- function(listdf, func){
  results2 <- sapply(listdf, func, USE.NAMES = TRUE, simplify = F)

  summary_df <- bind_rows(results2, .id = "colum_label")
  
  return(summary_df)
  
  
}

