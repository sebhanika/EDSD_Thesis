
# Title: EDSD Thesis 2021/22
# Topic: Imputing dataset with Random Forest Imputation
# Date: 08/07/2022

# In this script I am creating I am using MICE Random Forest imputation on my dataset.
# I construct 10 different datasets for my analysis. I will impute the datasets and then calculate the
# basic variables needed for my analysis so I can save them in my database. 

# Libraries ---------------------------------------------------------------

library(tidyverse)
library(dbplyr)
library(RPostgreSQL)
require(RPostgreSQL)
library(robustbase)
library(missRanger)
library(mice)
library(texreg)

# source functions/settings from functions Module
source('0_functions_module.R')

options(scipen=999) # disable scientific notation 

# Database connection and query -------------------------------------------

### Query all employment tables from MT database, settings defined in 'functions_module'
### only found in the 'public schema' 


tbl_list <- lapply(tableList, function(t) tbl(con, t))

# collect tables to tibble and trim white spaces
dfList <- lapply(tbl_list, function(t) collect(t) %>% 
                   mutate(across(where(is.character), str_trim)))

# set shorter names for tables
dfList <- setNames(dfList, names_df)

# add tables to Global Environment
list2env(dfList, envir=.GlobalEnv)


# Query variables from gis schema
tbl_dist100pop <- tbl(con, in_schema('gis', 'pop100tsd_dist_db'))
# distance hub table, distance to large city
hub_dist100pop <- tbl_dist100pop %>%
  collect() 


# Data prep ---------------------------------------------------------------

# used for filtering east german municipalities
east_germany <- paste(c('^12', '^13', '^14', '^15', '^16'), collapse = '|')

# prep hub_dist populaiton table,
hub_dist100pop <- hub_dist100pop %>%
  mutate(across(where(is.character), str_trim)) %>%
  select(c(5,19:20)) %>%
  rename(muni_key = ags, hubdist100 = hubdist) %>%
  mutate(hubdist100= (hubdist100/1000)*1.2)


work_age <- c("15to18","18to20", "20to25", "25to30", "30to35", "35to40", "40to45", "45to50", "50to55", "55to60", "60to65")


working_pop <- pop_age %>% 
  filter(year %in% 2009:2019) %>% 
  mutate(across(where(is.numeric), ~replace_na(.x, 0))) %>% 
  pivot_longer(cols = -c(1:3),
               names_sep = "_",
               names_to = c("sex", "age")) %>% 
  filter(age %in% work_age) %>% 
  group_by(muni_key, year, sex) %>% 
  summarize(across(.cols = value,
                          .fns = ~sum(.x)))
  





# calculate commuters -> join geovaraibles later
commuters <- oldsvp_ao %>%
  select(c(1:4)) %>%
  left_join(select(svp_ao, "inbound_commuter", "id"), by = "id") %>%
  left_join(select(svp_wo, "outbound_commuter", "id"), by = "id") %>%
  left_join(select(working_pop, "t_workpop", "id"), by = "id") %>%
  mutate(com_ratio = inbound_commuter-outbound_commuter,
         com_ratio_pop = (com_ratio/t_workpop)*100)



# calculating dominant economic sector per municipalites
econ_profile <- oldsvp_ao %>%
  select(c(2:3, 5)) %>%
  left_join(econ_ao, by = "id") %>%
  mutate(agri_p = (agri/total_total)*100, 
         prod_p = (production/total_total)*100,
         trade_p = (trade_transport/total_total)*100,
         service_p = (other_services/total_total)*100) %>%
  group_by(year) %>%
  mutate(mean_agri_p = mean(agri_p, na.rm = T),
         mean_prod_p = mean(prod_p, na.rm = T),
         mean_trade_p = mean(trade_p, na.rm = T),
         mean_service_p = mean(service_p, na.rm = T)) %>%
  ungroup() %>%
  mutate(dom_sec = with(., case_when( 
    (agri_p > prod_p & agri_p > trade_p & agri_p > service_p) ~ 'agri',
    (prod_p > agri_p & prod_p > trade_p & prod_p > service_p) ~ 'production',
    (trade_p > agri_p & trade_p > prod_p & trade_p > service_p) ~ 'trade_transport',
    (service_p > agri_p & service_p > prod_p & service_p > trade_p) ~ 'services'
  ))) %>%
  mutate(dom_sec = as.factor(dom_sec))





### Combine additonal geo and econ variables 

# create factors for muni_ref and add east german variable
# rs7 is regiostar7 classification 
geo_econ_vars <- muni_ref %>%
  left_join(hub_dist100pop, by = "muni_key") %>%
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


# Construct dataframe for imputation --------------------------------------

# select variables for model:
  # total_total for total job growth
  # men_65older_m, men_65older, women_65older_m, women_65older for ageing
  # muni_ref, east-germany and hubdist as geo-variables

df_impute <- oldsvp_ao %>% 
  select(c('id', 'year', 'muni_key', 'total_total', 
           'total_65older', 'total_65olderstand', 
           'men_65older', 'men_65olderstand',
           'women_65older', 'women_65olderstand')) %>% 
  # join marginal emplyoemtn variables
  left_join(select(oldsvp_marg, c('id', 'year', 'muni_key', 'total_total_m', 
                                  'total_65older_m', 'total_65olderstand_m',
                                  'men_65older_m', 'men_65olderstand_m',
                                  'women_65older_m', 'women_65olderstand_m')),
            by = c('id', 'year', 'muni_key')) %>%  
  # join geovariables
  left_join(select(geo_vars, c('muni_key', 'rs7',
                               'east_ger', 'hubdist100')),
            by = 'muni_key') %>% 
  drop_na(hubdist100)

