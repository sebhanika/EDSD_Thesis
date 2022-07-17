
# Title: EDSD Thesis 2021/22
# Topic: Descriptive Statitscts and visualizations
# Date: 13/07/2022

# In this script I am applying my regression analysis to my dataset. Following the mice approach
# I replicate each analysis 10times and pool it in the end. 

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


# Loading test data -------------------------------------------------------



imp_data_tbl <- tbl(con, in_schema('edsd', 'imp_calc_5'))
imp_cal_5 <- try_data_tbl %>%
  collect() %>%
  mutate(across(where(is.character), str_trim))


# 2009 variables
imp_calc2009 <- try_imp_cal_5 %>%
  filter(year == 2009) %>%
  mutate(com_ratio09 = inbound_commuter-outbound_commuter,
         com_ratio_pop09 = (com_ratio09/workPop)*100,
         workPop_ln09 = log(workPop)) %>%
  group_by(muni_key) %>%
  mutate(yg_work_sh09 = share[type == "r" & age == "below25" & sex == "total"]) %>%
  ungroup() %>%
  select(muni_key, yg_work_sh09, com_ratio_pop09, workPop_ln09) %>%
  distinct()


## reshape data frame
wide_df_5 <- imp_cal_5 %>%
  mutate(rs7 = as.factor(rs7)) %>%
  filter(year == 2019) %>%
  select(-c(value, grw, inbound_commuter, outbound_commuter, workPop)) %>%
  pivot_wider(names_sep = '_',
              values_from = c('grw_p', 'share', 'grw_sh'),
              names_from = c('sex', 'age', 'type')) %>%
  left_join(try_imp_calc2009, by = 'muni_key')


