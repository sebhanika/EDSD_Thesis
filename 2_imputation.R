
# Title: EDSD Thesis 2021/22
# Topic: Code for Imputation
# Date: 21/06/2022


# Libraries ---------------------------------------------------------------

library(tidyverse)
library(dbplyr)
library(RPostgreSQL)
require(RPostgreSQL)
library(missForest) # used for imputation


# source functions from functions Module
source("0_functions_module.R")

options(scipen=999) # disable scientific notation 


# Database connection and query -------------------------------------------

### Query all tables from MT database, settings defined in 'functions_module'
tbl_list <- lapply(tableList, function(t) tbl(con, t))

# collect tables to tibble and trim white spaces
dfList <- lapply(tbl_list, function(t) collect(t) %>% 
                   mutate(across(where(is.character), str_trim)))

# set shorter names for tables
dfList <- setNames(dfList, names_df)

# add tables to Global Environment
list2env(dfList, envir=.GlobalEnv)

# Data prep ---------------------------------------------------------------

# create factors for muni_ref
muni_ref <- muni_ref %>%
  mutate(gen_rs7 = factor(gen_rs7,
                          levels = c('Large City, urban', # set factor levels
                                     'Medium-sized city, urban',
                                     'Small town, urban',
                                     'Central City, rural',
                                     'Medium-sized city, rural', 
                                     'Small town, rural')))

# filter population table
pop <- pop_tot %>% 
  rename(pop_total = total) %>%
  filter(year %in% 2009:2019)

# removing small data, only lose about 0.9% of total German population
pop_select <- pop %>% 
  filter(pop_total >= 1000) %>% #remove small towns
  group_by(muni_key) %>%  
  filter(n() == 11) %>%  # keep only complete observations
  ungroup()

# list of data frames to filter small municipalities
list_dfs <- listn(pop_age, arbl, econ_ao, svp_ao,
                  svp_wo, oldsvp_ao, pop, oldsvp_marg)

# apply filter to all data frames
list_dfs_filt <- sapply(list_dfs, filter, id %in% pop_select$id)

# add tables to Global Environment
list2env(list_dfs_filt, envir=.GlobalEnv)

# remove unwanted variables
rm(pop_tot, list_dfs_filt, tbl_list, dfList, list_dfs)







# First try randomForest imputation ---------------------------------------


try.imp.data <- svp_ao %>% 
  filter(year > 2015) %>% 
  select(where(is.numeric)) %>% 
  as.data.frame()



# performing imputation, this can take a while!!!
imp_rf <- missForest(xmis = try.imp.data, maxiter = 3, ntree = 11)

# extracting data
data_final <- imp_rf$ximp

# extracting the errors
errors_imp <- imp_rf$OOBerror



