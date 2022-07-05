
# Title: EDSD Thesis 2021/22
# Topic: Code for Imputation
# Date: 21/06/2022

# In this script I am creaating an example of MICE Random Forest imputation on my dataset.
# I construct a dataset with a few relevant variabele for my analysis and use robust regression
# to see how the imputed dataset behave. The goal is to do the same with mean value imputation
# and imputation using fixed values.

### Old version with everything. might combine in the end.

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


# Imputation missRanger and mice ------------------------------------------

# select data for missing ranger imputation and model
# total_total for total job growth
# men_65older_m, men_65older, women_65older_m, women_65older for ageing
# muni_ref, east-germany and hubdist as geo-variables

df_ranger <- oldsvp_ao %>% 
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


# Generate 5 complete data sets with imputated values
imp_ranger <- replicate(5,missRanger(df_ranger, 
                              formula = .-muni_key-year-id-rs7-hubdist100-east_ger #exclude
                                          ~ 
                                        .-muni_key-year-id-rs7-hubdist100-east_ger,
                              splitrule = 'extratrees', 
                              maxiter = 5,
                              num.trees = 10,
                              pmm.k = 3), # needed to avoid decimal values
                 simplify = F)

# name create datasets for easier handling
names(imp_ranger) <-  c('test_a','test_b','test_c','test_d', 'test_e')

# add tables to Global Environment for saving or closer inspection
# list2env(imp_ranger, envir=.GlobalEnv)


# Saving and loading imputated datasets -----------------------------------

# Save created imputation to Rdata file, igonore in Git. 
# Done for quicker testing, delete later
# 
#  save(test_a, file = 'test_a.RData')
#  save(test_b, file = 'test_b.RData')
#  save(test_c, file = 'test_c.RData')
#  save(test_d, file = 'test_d.RData')
#  save(test_e, file = 'test_e.RData')

##### reload test files, everything else not needed

# 
# load('test_a.RData')
# load('test_b.RData')
# load('test_c.RData')
# load('test_d.RData')
# load('test_e.RData')

# create list of data frames, only if it was loaded or added to environment
list_test_df <- list(test_a, test_b, test_c, test_d, test_e)


# Applying calcualtion to list of imputed dataframes ----------------------

# based on the imputed values we can now calculate the growth rates and other variables
# we need for the analysis, this needs to be done for all 5 dataframes. This process takes a while!
# Calculation using a long format but output as wide dataframe for easier application in models

imp_ranger_calc <- lapply(imp_ranger, function(x) x %>% 
                            pivot_longer(cols = -c(id, year, muni_key, rs7, hubdist100, east_ger),
                                         names_to = c('sex', 'age', 'type'), names_sep = '_') %>% 
                            arrange(muni_key, year) %>%                                                             
                            mutate(type = replace_na(type, 'r')) %>% # for regular employment
                            group_by(muni_key, year) %>% 
                            mutate(share = value/value[sex == 'total' & age == 'total' & type == 'r']) %>%  # calculate share of workforce
                            #calculate growth rates
                            group_by(muni_key, sex, age, type) %>% 
                            mutate(grw = value - lag(value, n = 10),# absolute growth in employment numbers
                                   grw_p = (grw/lag(value, n = 10))*100, # relative growth
                                   grw_sh = (share - lag(share, n = 10))*100) %>% # change in share of employment
                            
                            # cleaning: replace NaN and Inf, faster than case_when
                            mutate(grw_p = if_else(is.nan(grw_p), 0, grw_p), 
                                   grw_p = if_else(is.infinite(grw_p), 0, grw_p)) %>% 
                            pivot_wider(names_sep = '_', 
                                        values_from = c('value', 'grw', 'grw_p', 'share', 'grw_sh'),
                                        names_from = c('sex', 'age', 'type'))
                          )


# Run a linear model for each of the completed data sets                          
rf_mice_models <- lapply(imp_ranger_calc, 
                         function(x) lmrob(grw_p_total_total_r ~ grw_sh_total_65older_r +
                                                                 rs7 + 
                                                                 hubdist100 +
                                                                 east_ger, 
                                                    x))
# Pool the results by mice
summary(pooled_fit <- pool(rf_mice_models))











# other code bits ---------------------------------------------------------


# trying out code for apply to lis

# calcualting grrowth rates of employment variables,
# using a pivot_long approach, however needs to be wide for model in the end?


# here only using pivot_long format
calc_long <- test_a %>% 
  left_join(select(geo_vars, c('muni_key', 'east_ger', 'hubdist100')), by = 'muni_key') %>% 
  mutate(regiostar7 = as.factor(regiostar7)) %>% 
  pivot_longer(cols = -c(id, year, muni_key, regiostar7, gen_rs7, east_ger, hubdist100),
               names_to = c('sex', 'age', 'type'), names_sep = '_') %>% 
  arrange(muni_key, year) %>%                                                             
  mutate(type = replace_na(type, 'r')) %>% # for regular employment
  group_by(muni_key, year) %>% 
  mutate(share = value/value[sex == 'total' & age == 'total' & type == 'r']) %>%  # calculate share of workforce
  #calculate growth rates
  group_by(muni_key, sex, age, type) %>% 
  mutate(grw = value - lag(value, n = 10),# absolute growth in employment numbers
         grw_p = (grw/lag(value, n = 10))*100, # relative growth
         grw_sh = (share - lag(share, n = 10))*100) %>% # change in share of employment
  
  # cleaning: replace NaN and Inf, faster than case_when
  mutate(grw_p = if_else(is.nan(grw_p), 0, grw_p), 
         grw_p = if_else(is.infinite(grw_p), 0, grw_p)) %>% 
  pivot_wider(names_sep = '_', 
              values_from = c('value', 'grw', 'grw_p', 'share', 'grw_sh'),
              names_from = c('sex', 'age', 'type'))


# save intermediate df 
#save(calc_long, file = 'cald_data.RData')
# load('cald_data.RData')


#simple model to test with only growth of the share of older workers (65+) and spatial classification
mod_test <- lmrob(grw_p_total_total_r ~ grw_sh_to   tal_65older_r + regiostar7 + hubdist100 + east_ger,
                   data = calc_long)

summary(mod_test)


