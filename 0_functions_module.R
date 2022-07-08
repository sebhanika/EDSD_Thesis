
# EDSD Thesis 2021/22
# Functions file
# Author: Sebstian Hanika
# Date: 07/03/2022

# This file contains self-defined functions and settings used for my EDSD thesis
# This includes custom summary statistics and GGPLOT themes as well as my database loading instructions


# small functions that help or avoid masking errors
filter <- dplyr::filter    #EDIT
select <- dplyr::select
`%!in%` = Negate(`%in%`) # function needed for later 

# GGplot2 settings --------------------------------------------------------

basic_theme <- theme_set(theme_bw() +
                           theme(panel.grid.major = element_blank(),
                                 panel.grid.minor = element_blank(),
                                 panel.background = element_blank(), 
                                 axis.line = element_line(colour = "black"),
                                 axis.title = element_text(size=16, color = "grey30"),
                                 axis.text = element_text(size=16, color = "grey30"),
                                 legend.text = element_text(size=16, color = "grey30"),
                                 legend.title=element_blank()))



# Query settings  ---------------------------------------------------------

pw <- {"Thes1s_EDSD?"} 
drv <- dbDriver("PostgreSQL") # loads the PostgreSQL driver
# creates a connection to the postgres database
con <- dbConnect(drv, dbname = "MT",
                 host = "localhost", port = 5432,
                 user = "postgres", password = pw)

tableList <- c("ref_regiostar7", "gs2019_age_groups", "gs2019_amk_arbl",
               "gs2019_amk_econ_ao", "gs2019_amk_svp_ao", "gs2019_amk_svp_wo",
               "gs2019_oldsvp_ao", "gs2019_oldsvp_marg","gs2019_pop_muni")

names_df <- c("muni_ref", "pop_age", "arbl", "econ_ao", "svp_ao",
              "svp_wo", "oldsvp_ao", "oldsvp_marg", "pop_tot")


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


# (2) summary statistics function function
summary_func <- function(dataframe){
  
  # calculate summary stats
  temp_list <- dataframe %>% 
    summarize(across(.cols = where(is.numeric),
                     .names = "{.col}__{.fn}", #double for pivoting later
                     .fns = list(nmiss = ~sum(is.na(.))/length(.)*100, # %of NAs
                                 mean = ~mean(.x, na.rm = TRUE),
                                 min = ~min(.x, na.rm = TRUE),
                                 max = ~max(.x, na.rm = TRUE),
                                 sd = ~sd(.x, na.rm = TRUE)
                     ))) %>%
    
    # make dataframe in right format
    pivot_longer(everything(), 
                 names_to = c("variable", "measure"),
                 names_sep = "__") %>% # now it is too long
    pivot_wider(names_from = "measure", 
                values_from = "value")
}


# 3() apply summary stat function across all dataframes and rowbind
output_dfs <- function(listdf, func){
  
  #calc summary stat and pivot each dataframe
  results2 <- sapply(listdf,
                     func, 
                     USE.NAMES = TRUE, 
                     simplify = FALSE) # simplify needed for right format

  # rowbind all dataframes and clean df
  summary_df <- bind_rows(results2, .id = "colum_label") %>% 
    filter(variable %!in% c("year", "id"))  %>% # remove unwanted variables
    mutate(across(.cols =where(is.numeric),
                  .fns = ~round(., digits = 3))) # round all numerics
  
  return(summary_df)
  
}



