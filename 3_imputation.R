
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


## calculate working age population, requires reshaping of large dataframe

work_age <- c('15to18','18to20', '20to25', '25to30', '30to35', '35to40',
              '40to45', '45to50', '50to55', '55to60', '60to65')

working_pop <- pop_age %>% 
  filter(year %in% 2009:2019) %>% 
  mutate(across(where(is.numeric), ~replace_na(.x, 0))) %>% # NAs here were incorrectly loaded, should be zeros
  pivot_longer(cols = -c(1:3),
               names_sep = '_',
               names_to = c('sex', 'age')) %>% 
  filter(age %in% work_age, sex == 'total') %>% # if sex-specific is required, change here!
  group_by(id) %>% 
  summarize(across(.cols = value,
                          .fns = ~sum(.x))) %>% 
  left_join(select(pop_tot, c('id', 'total')), by = 'id') %>%  # nas that are created will be filter out by pop_select
  mutate(workpop_per = value/total) %>% 
  rename(workpop = value) %>% 
  select(-c(total))
  

# calculate commuters -> join geovaraibles later
# has some missing values that need to be imputed! 
commuters <- svp_ao %>%
  filter(year %in% 2009:2019) %>% 
  select(c(1, 10)) %>%
  left_join(select(svp_wo, 'outbound_commuter', 'id'), by = 'id') 


### Combine additonal geo and econ variables

# create factors for muni_ref and add east german variable
# rs7 is regiostar7 classification 
geo_econ_vars <-  oldsvp_ao %>% # oldsvp_ao as basis for joins to avoid NAs
  
  select(c(1,3,4)) %>% 
  # join commuting data
  left_join(commuters, by = 'id') %>% 
  # join working age population data
  left_join(working_pop, by = 'id') %>% 
  # join spatial typology
  left_join(select(muni_ref, c('muni_key', 'regiostar7')), by = 'muni_key') %>% 
  rename(rs7 = regiostar7) %>% 
  # join distance to large labor market
  left_join(hub_dist100pop, by = 'muni_key') %>%
  # assign east/west german variables
  mutate(east_ger = if_else(str_detect(muni_key, east_germany),'East', 'West'))



### filter population table
# removing small data, only lose about 0.9% of total German population
pop_select <- pop_tot %>% 
  rename(pop_total = total) %>%
  filter(year %in% 2009:2019,
         pop_total >= 1000) %>% 
  group_by(muni_key) %>%  
  filter(n() == 11) %>%  # keep only complete observations
  ungroup()

# list of data frames to filter small municipalities
list_dfs <- listn(oldsvp_ao, oldsvp_marg, geo_econ_vars)

# apply filter to all data frames
list_dfs_filt <- lapply(list_dfs, filter, id %in% pop_select$id)


### filter municipalities with a lot of missing employment data,
# limits effectivness of imputation (only drops 7 more municipalities)


oldsvp_onlyNA <- oldsvp_ao %>% 
  group_by(muni_key) %>% 
  summarise(across(.cols = starts_with(c("total", "men", "women")),
                   .fns = ~sum(is.na(.x)))) %>% 
  filter(total_total < 8)

# "01060068" "03151030" "03151035" "09372177" "09374146" "09574129" "15083580"


list_dfs_filt2 <- lapply(list_dfs_filt, filter, muni_key %in% oldsvp_onlyNA$muni_key)

# add tables to Global Environment
list2env(list_dfs_filt2, envir=.GlobalEnv)



### Some house-cleaning:
# remove unwanted variables
rm(pop_tot, list_dfs_filt, tbl_list, dfList, list_dfs, tbl_dist100pop, 
   commuters, list_dfs_filt2, pop_select,  hub_dist100pop, muni_ref, 
   econ_ao, svp_ao, svp_wo, pop_age, working_pop, arbl, oldsvp_onlyNA)
# free unused memory
gc() 

# Construct dataframe for imputation --------------------------------------


# Combine dataframe with employment variables, as well as controll variables
df_impute <- oldsvp_ao %>% 
  # join marginal employment variables
  left_join(select(oldsvp_marg, -c("muni_name")), by = c('id', 'year', 'muni_key')) %>%  
  # join geovariables
  left_join(geo_econ_vars,by = c('id', 'year', 'muni_key')) %>% 
  drop_na(hubdist100) # drop one city without distance, is a municipal reform problem


# Run imputation ----------------------------------------------------------

# 10 times,  with 10 trees
# Generate 10 complete data sets with imputed values

n = 10 # number of replications

imp_ranger <- replicate(n,missRanger(df_impute, 
                                     formula = .-muni_key-year-id-rs7-hubdist100-east_ger-muni_name-hubname #exclude
                                     ~ 
                                       .-muni_key-year-id-rs7-hubdist100-east_ger-muni_name-hubname,
                                     splitrule = 'extratrees', 
                                     maxiter = 5,
                                     num.trees = 10,
                                     pmm.k = 3), # needed to avoid decimal values
                        simplify = F)



# Export Imputation to database -------------------------------------------


names_imp_df <- paste0("rf_imp_", 1:n)
names(imp_ranger) <-  names_imp_df
 

## save data to rds as backup
#lapply(names(imp_ranger), function(df) 
 # saveRDS(imp_ranger[[df]], file = paste0(df, ".rds")))


# uploads data frame to Table
for (i in 1:length(names_imp_df)){
  dbWriteTable(con, name = paste0("rf_imp_", i), imp_ranger[[i]], row.names=FALSE, overwrite=TRUE)
}
  
## move to edsd schema, bug prevents it from working in the dbWriteTable function

sql_alter <- "" # empty string to store resutls in

for (i in 1:length(names_imp_df)){
  
  x <- paste0("ALTER TABLE rf_imp_",i," SET SCHEMA edsd;")
  sql_alter <- paste(sql_alter, x)
}
sql_alter

dbGetQuery(con, sql_alter)




# Calculate model variables -----------------------------------------------

### loading imputation variabel from database, all tables from edsd schema
tbl_list_imp <- lapply(names_imp_df, function(t) tbl(con, in_schema('edsd', t)))

# collect tables to tibble and trim white spaces
df_imps <- lapply(tbl_list_imp, function(t) collect(t) %>% 
                   mutate(across(where(is.character), str_trim))) #sometimes postgresql adds whitespace?

# set shorter names for tables
df_imps <- setNames(df_imps, names_imp_df)

# add tables to Global Environment
#list2env(df_imps, envir=.GlobalEnv)



### Calculation to reshape data frame

# reshaping data frames and calculating growth rates etc.
# takes a long time

imp_ranger_calc <- lapply(imp_ranger, function(x) x %>% 
                            pivot_longer(cols = -c(id, year, muni_key, muni_name, rs7, inbound_commuter,
                                                   hubdist100, outbound_commuter, workPop, workPop_per, hubname, east_ger),
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
                                   grw_p = if_else(is.infinite(grw_p), 0, grw_p))
                          )


# Export calc to database -------------------------------------------------



names_imp_calc <- paste0("imp_calc", 1:n)
names(imp_ranger_calc) <-  names_imp_calc


## save data to rds as backup
#lapply(names(imp_ranger_calc), function(df) 
# saveRDS(imp_ranger_calc[[df]], file = paste0(df, ".rds")))


# uploads data frame to Table
for (i in 1:length(names_imp_df)){
  dbWriteTable(con, name = paste0("imp_calc_", i), imp_ranger[[i]], row.names=FALSE, overwrite=TRUE)
}

## move to edsd schema, bug prevents it from working in the dbWriteTable function

sql_alter2 <- "" # empty string to store resutls in

for (i in 1:length(names_imp_df)){
  
  x <- paste0("ALTER TABLE imp_calc_",i," SET SCHEMA edsd;")
  sql_alter2 <- paste(sql_alter2, x)
}
sql_alter2

dbGetQuery(con, sql_alter2)


