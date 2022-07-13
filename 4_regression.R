
# Title: EDSD Thesis 2021/22
# Topic: Imputing dataset with Random Forest Imputation
# Date: 13/07/2022

# In this script I am applying my regression analysis to my dataset. Following the mice approach
# I replicate each analysis 10times and pool it in the end. 

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




# Query data --------------------------------------------------------------


### loading imputation variabel from database, all tables from edsd schema
n <- 10 # copied from above
names_imp_calc <- paste0("imp_calc_", 1:n)

tbl_list_imp <- lapply(names_imp_calc, function(t) tbl(con, in_schema('edsd', t)))

# collect tables to tibble and trim white spaces
df_imps_calc <- lapply(tbl_list_imp, function(t) collect(t) %>% 
                    mutate(across(where(is.character), str_trim))) #sometimes postgresql adds whitespace?

# set shorter names for tables
df_imps_calc <- setNames(df_imps_calc, names_imp_calc)





