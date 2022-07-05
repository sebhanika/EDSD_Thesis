
# Title: EDSD Thesis 2021/22
# Topic: Code for Imputation
# Date: 05/07/2022

# In this script I am creating an example of MICE Random Forest imputation on my dataset.
# I construct a dataset with a few relevant variabele for my analysis and use robust regression
# to see how the imputed dataset behave. The goal is to do the same with mean value imputation
# and imputation using fixed values.

# Libraries ---------------------------------------------------------------

library(tidyverse)
library(dbplyr)
library(RPostgreSQL)
require(RPostgreSQL)
library(robustbase)
library(missRanger)
library(mice)
library(broom)

# source functions from functions Module
source('0_functions_module.R')

options(scipen=999) # disable scientific notation 

filter <- dplyr::filter
select <- dplyr::select

# Database connection and query -------------------------------------------

### Query all employment tables from MT database, settings defined in 'functions_module'
### only found in the 'public schema' 

# specificng which tables to query from the database
table_list <- c("ref_regiostar7", "gs2019_oldsvp_ao",
                "gs2019_oldsvp_marg","gs2019_pop_muni")

tbl_list <- lapply(table_list, function(t) tbl(con, t))

# collect tables to tibble and trim white spaces
dfList <- lapply(tbl_list, function(t) collect(t) %>% 
                   mutate(across(where(is.character), str_trim)))

# set shorter names for tables
names_df <- c("muni_ref", "oldsvp_ao", "oldsvp_marg", "pop_tot")
dfList <- setNames(dfList, names_df)

# add tables to Global Environment
list2env(dfList, envir=.GlobalEnv)


# Query variables from gis schema
tbl_dist100pop <- tbl(con, in_schema('gis', 'pop100tsd_dist_db'))
# distance hub table, distance to large city
hub_dist100pop <- tbl_dist100pop %>%
  collect() %>%
  mutate(across(where(is.character), str_trim)) %>%
  select(c(5,6,19:20)) %>%
  rename(muni_key = ags, muni_name = gen, hubdist100 = hubdist) %>%
  mutate(hubdist100= (hubdist100/1000)*1.2)


# Data prep ---------------------------------------------------------------

east_germany <- paste(c('^12', '^13', '^14', '^15', '^16'), collapse = '|')

# create factors for muni_ref and add east german variable
# rs7 is regiostar7 classification 

geo_vars <- muni_ref %>%
  left_join(select(hub_dist100pop, "hubname", "hubdist100", "muni_key"), by = "muni_key") %>%
  mutate(rs7 = as.factor(regiostar7)) %>% 
  mutate(east_ger = if_else(condition = str_detect(muni_key, east_germany) ,
                            true = 'East',
                            false =  'West')) %>% 
  select(-c(gen_rs7, regiostar7)) #drop unneccesary columns


# filter population table
# removing small data, only lose about 0.9% of total German population
pop_select <- pop_tot %>% 
  rename(pop_total = total) %>%
  filter(year %in% 2009:2019,
         pop_total >= 1000) %>% 
  group_by(muni_key) %>%  
  filter(n() == 11) %>%  # keep only complete observations
  ungroup()

# list of data frames to filter small municipalities
list_dfs <- listn(oldsvp_ao, oldsvp_marg)

# apply filter to all data frames
list_dfs_filt <- lapply(list_dfs, filter, id %in% pop_select$id)

# add tables to Global Environment
list2env(list_dfs_filt, envir=.GlobalEnv)

# remove unwanted variables
rm(pop_tot, list_dfs_filt, tbl_list, dfList, list_dfs, tbl_dist100pop, hub_dist100pop)




# Imputation with fixed values --------------------------------------------


try.one <- oldsvp_ao %>% 
  mutate(across(where(is.numeric), ~replace_na(.x, 1)))

try.two <- oldsvp_ao %>% 
  mutate(across(where(is.numeric), ~replace_na(.x, 2)))


# Imputation with mean values ---------------------------------------------


mean_imp_oldsvp_marg <- oldsvp_marg %>%   # filters only groups that have a complete set of years
  group_by(muni_key) %>%
  mutate(across(.cols = -c('muni_name','id', 'year'),
                .fns = 
                  ~ case_when(
                    is.na(.) ~ mean(., na.rm = T), # replaces NAs with mean of group
                    !is.na(.) ~ .))) #conditions if  not NA

