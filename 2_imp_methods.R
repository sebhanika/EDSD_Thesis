
# Title: EDSD Thesis 2021/22
# Topic: Code for Imputation
# Date: 21/06/2022

# In this script I am creaating an example of MICE Random Forest imputation on my dataset.
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
library(texreg)

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


# Create dataframe to impute ----------------------------------------------

# select data for missing ranger imputation and model
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




# Imputation with fixed values --------------------------------------------

#imputate, replace NAs with 1
df_fix_one <- df_impute %>% 
  mutate(across(where(is.numeric), ~replace_na(.x, 1)))

# replce Nas with 2
df_fix_two <- df_impute %>% 
  mutate(across(where(is.numeric), ~replace_na(.x, 2)))


# calculate growth rates and shares, reshape dataframe IMP 1
df_fix_one_calc <- df_fix_one %>% 
  pivot_longer(cols = -c(id, year, muni_key, rs7, hubdist100, east_ger),
               names_to = c('sex', 'age', 'type'),
               names_sep = '_') %>% 
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


# calculate growth rates and shares, reshape dataframe IMP 2
df_fix_two_calc <- df_fix_two %>% 
  pivot_longer(cols = -c(id, year, muni_key, rs7, hubdist100, east_ger),
               names_to = c('sex', 'age', 'type'),
               names_sep = '_') %>% 
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

# Imputation with mean values ---------------------------------------------

mean_imp <- df_impute %>%
  group_by(muni_key) %>%
  mutate(across(.cols = -c('id', 'year', "rs7", "hubdist100", "east_ger"),
                .fns = 
                  ~ case_when(
                    is.na(.) ~ mean(., na.rm = T), # replaces NAs with mean of group
                    !is.na(.) ~ .))) #conditions if  not NA

mean_imp_calc <- mean_imp %>% 
  pivot_longer(cols = -c(id, year, muni_key, rs7, hubdist100, east_ger),
               names_to = c('sex', 'age', 'type'),
               names_sep = '_') %>% 
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



# Imputation missRanger and mice ------------------------------------------

# Generate 5 complete data sets with imputated values
imp_ranger <- replicate(5,missRanger(df_impute, 
                                     formula = .-muni_key-year-id-rs7-hubdist100-east_ger #exclude
                                     ~ 
                                       .-muni_key-year-id-rs7-hubdist100-east_ger,
                                     splitrule = 'extratrees', 
                                     maxiter = 5,
                                     num.trees = 10,
                                     pmm.k = 3), # needed to avoid decimal values
                        simplify = F)

# name create datasets for easier handling
names(imp_ranger) <-  c('rf_imp_a','rf_imp_b','rf_imp_c','rf_imp_d', 'rf_imp_e')

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


####

# Save and load final calc files ------------------------------------------


# temp saving these files and load again if needed, delete later
# save(imp_ranger_calc, file = "imp_ranger_calc.RData")
# save(df_fix_one_calc, file = "df_fix_one_calc.RData")
# save(df_fix_two_calc, file = "df_fix_two_calc.RData")
# save(mean_imp_calc, file = "mean_imp_calc.RData")



# Running models ----------------------------------------------------------

### With fix value imputation

# run simple robust regression IMP 1
fix_one_model <- lmrob(grw_p_total_total_r ~  
                         grw_sh_total_65older_m +
                         rs7 +
                         hubdist100 +
                         east_ger, 
                       data = df_fix_one_calc)

fix_one_model_sum <- summary(fix_one_model)


# run simple robust regression IMP 2
fix_two_model <- lmrob(grw_p_total_total_r ~  
                         grw_sh_total_65older_m +
                         rs7 +
                         hubdist100 +
                         east_ger, 
                       data = df_fix_two_calc)

fix_two_model_sum <- summary(fix_two_model)


### with mean-value imputation

# run simple robust regression
mean_imp_model <- lmrob(grw_p_total_total_r ~  
                          grw_sh_total_65older_m +
                          rs7 +
                          hubdist100 +
                          east_ger, 
                        data = mean_imp_calc)

mean_imp_model_sum <- summary(mean_imp_model)


### With Random forest MICE

# Run a linear model for each of the completed data sets                          
rf_mice_models <- lapply(imp_ranger_calc, 
                         function(x) lmrob(grw_p_total_total_r ~  
                                             grw_sh_total_65older_m +
                                             rs7 + 
                                             hubdist100 +
                                             east_ger, 
                                           x))

# Pool the results for MICE
rf_model_pool <- pool(rf_mice_models)
rf_model_sum <- summary(rf_model_pool)

# calculate GOFs (R squared)
rf_r2 <- pool.r.squared(rf_model_pool, adjusted = F)



# Create regression table LATEX -------------------------------------------

# turn pooled models into Texreg object
rf_model_texreg <- createTexreg(
        coef.names = as.character(rf_model_sum$term), # names of coefficients 
        coef = rf_model_sum$estimate,  # values of coef
        se = rf_model_sum$std.error, # stds of coef
        pvalues = rf_model_sum$p.value # pvalues
        )




# extract NObs and rsquares from the models
gofs <- list("Num. Obs." = c(nrow(drop_na(df_fix_one_calc)), nrow(drop_na(df_fix_two_calc)), 
                             nrow(drop_na(mean_imp_calc)),nrow(drop_na(imp_ranger_calc[[1]]))),
             "R^2" = c(fix_one_model_sum$r.squared, fix_two_model_sum$r.squared,
               mean_imp_model_sum$r.squared, rf_r2[1]))


model_labels <- c("Fix model 1", "Fix model 2", "Mean Model", "RF Model")
coef_labels <- c('Intercept' , 
                 "Delta share of workers above 65 marg",
                 'Medium-sized city, urban',
                 'Small town, urban', 'Central City, rural','Medium-sized city, rural',
                 'Small town, rural', "Distance to large city","West Germany")

texreg(l = list(fix_one_model, fix_two_model,
                   mean_imp_model, rf_model_texreg),
       custom.model.names = model_labels,
       custom.coef.names = coef_labels,
       custom.gof.rows = gofs,
       include.nobs = F,
       dcolumn = T,
       digits = 3
       )

?texreg
