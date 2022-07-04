
# Title: EDSD Thesis 2021/22
# Topic: Code for Imputation
# Date: 21/06/2022


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
source("0_functions_module.R")

options(scipen=999) # disable scientific notation 

filter <- dplyr::filter

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



# Imputation with fixed values --------------------------------------------


try.one <- oldsvp_ao %>% 
  mutate(across(where(is.numeric), ~replace_na(.x, 1)))

try.two <- oldsvp_ao %>% 
  mutate(across(where(is.numeric), ~replace_na(.x, 2)))


# Imputation with mean values ---------------------------------------------


mean_imp_oldsvp_marg <- oldsvp_marg %>%   # filters only groups that have a complete set of years
  group_by(muni_key) %>%
  mutate(across(.cols = -c("muni_name","id", "year"),
                .fns = 
                ~ case_when(
                  is.na(.) ~ mean(., na.rm = T), # replaces NAs with mean of group
                  !is.na(.) ~ .))) #conditions if  not NA


# Imputation missRanger and mice ------------------------------------------


# select data for missing ranger imputation and model
# total_total for total job growth
# men_65older_m, men_65older, women_65older_m, women_65older
# muni_ref


df_ranger <- oldsvp_ao %>% 
  select(c('id', 'year', 'muni_key', 'total_total', 
           "total_65older", "total_65olderstand", 
           'men_65older', 'men_65olderstand',
           'women_65older', 'women_65olderstand')) %>% 
  # join dfs
  left_join(select(oldsvp_marg, c('id', 'year', 'muni_key', 'total_total_m', 
                                  "total_65older_m", "total_65olderstand_m",
                                  'men_65older_m', 'men_65olderstand_m',
                                  'women_65older_m', 'women_65olderstand_m')),
            by = c('id', 'year', 'muni_key')) %>%  
  left_join(select(muni_ref, c('muni_key', 'regiostar7', 'gen_rs7')), by = 'muni_key') %>% 
  mutate(regiostar7 = as.factor(regiostar7))




# Generate 5 complete data sets
imp_ranger <- replicate(5,missRanger(df_ranger, 
                              formula = .-muni_key - year - muni_name -id ~ .-muni_key - year - muni_name -id,
                              splitrule = "extratrees", 
                              maxiter = 5,
                              num.trees = 5,
                              pmm.k = 3),
                 simplify = F)

# names created datasets
names(imp_ranger) <-  c("test_a","test_b","test_c","test_d", "test_e")

# add tables to Global Environment
list2env(imp_ranger, envir=.GlobalEnv)

# Save created imputation to Rdata file, igonore in Git. 
# Done for quicker testing, delete later
# 
#  save(test_a, file = 'test_a.RData')
#  save(test_b, file = 'test_b.RData')
#  save(test_c, file = 'test_c.RData')
#  save(test_d, file = 'test_d.RData')
#  save(test_e, file = 'test_e.RData')




##### reload test files, everything else not needed

rm(arbl, pop, pop_age, svp_ao, svp_wo, try.one, try.two, econ_ao, mean_imp_oldsvp_marg)

# 
# load('test_a.RData')
# load('test_b.RData')
# load('test_c.RData')
# load('test_d.RData')
# load('test_e.RData')

# create list of data frames
list_test_df <- list(test_a, test_b, test_c, test_d, test_e)

# trying out code for apply to list



test_a_prep <- test_a %>% 
    mutate(regiostar7 = as.factor(regiostar7)) %>% 
    pivot_longer(cols = -c(id, year, muni_key, regiostar7, gen_rs7),
                 names_to = c("sex", "age", "type"), names_sep = "_") %>% 
    arrange(muni_key, year) %>%                                                             
    mutate(type = replace_na(type, "full")) %>% 
    group_by(muni_key, sex, age, type) %>% 
    mutate(grw = value - lag(value, n = 10), 
           grw_per = (grw/lag(value, n = 10))*100) %>% 
    mutate(grw_per = if_else(is.nan(grw_per), 0, grw_per), # replace NaN and Inf, faster than case_when
           grw_per = if_else(is.infinite(grw_per), 0, grw_per)) %>% 
    pivot_wider(names_sep = "_", 
                values_from = c("value", "grw", "grw_per"),
                names_from = c("sex", "age", "type"))



test_model <- lmrob(grw_per_total_total_full ~ grw_per_total_65older_full + regiostar7,
                    data = test_a_prep)

summary(test_model)









# calcualte the growth rates for all 5 dataframes
# in final version replace 'list_test_df' through 'imp_ranger'

imp_ranger_calc <- lapply(list_test_df, function(x) x %>% 
                                       pivot_longer(cols = -c(id, year, muni_key, regiostar7, gen_rs7),
                                                   names_to = c("sex", "age", "type"), names_sep = "_") %>% 
                                      arrange(muni_key, year) %>%                                                             
                                      mutate(type = replace_na(type, "full")) %>% 
                                      group_by(muni_key, sex, age, type) %>% 
                                      mutate(grw = value - lag(value, n = 10), 
                                             grw_per = (grw/lag(value, n = 10))*100
                                             ) %>% 
                                      pivot_wider(names_sep = "_", 
                                                  values_from = c("value", "grw", "grw_per"),
                                                  names_from = c("sex", "age", "type"))
                          
                          )




# Run a linear model for each of the completed data sets                          
models <- lapply(imp_ranger_calc, function(x) lmrob(grw_per_total_total_full ~ grw_per_total_total_full +
                                                      grw_
                                                                                regiostar7, x))

# Pool the results by mice
summary(pooled_fit <- pool(models))



