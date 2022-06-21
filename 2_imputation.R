
# Title: EDSD Thesis 2021/22
# Topic: Code for Imputation
# Date: 21/06/2022


# Libraries ---------------------------------------------------------------

library(tidyverse)
library(dbplyr)
library(RPostgreSQL)
require(RPostgreSQL)
library(RColorBrewer)
library(knitr)

# source functions from functions Module
source("0_functions_module.R")

options(scipen=999) # disable scientific notation 


# Database connection and query -------------------------------------------

pw <- {"Thes1s_EDSD?"} 
drv <- dbDriver("PostgreSQL") # loads the PostgreSQL driver
# creates a connection to the postgres database
con <- dbConnect(drv, dbname = "MT",
                 host = "localhost", port = 5432,
                 user = "postgres", password = pw)


tbl_muni_ref <- tbl(con, "ref_raumtyp")
tbl_popage <- tbl(con, "gs2019_age_groups")
tbl_arbl <- tbl(con, "gs2019_amk_arbl")
tbl_econ_ao <- tbl(con, "gs2019_amk_econ_ao")
tbl_marg_emp <- tbl(con, "gs2019_amk_marg_emp")
tbl_svp_ao <- tbl(con, "gs2019_amk_svp_ao")
tbl_svp_wo <- tbl(con, "gs2019_amk_svp_wo")
tbl_oldsvp_ao <- tbl(con, "gs2019_oldsvp_ao")
tbl_oldsvp_marg <- tbl(con, "gs2019_oldsvp_marg")
tbl_pop <- tbl(con, "gs2019_pop_muni")

#population age data
pop_age <- tbl_popage %>%
  collect() %>% #is necessary to create table
  mutate(across(where(is.character), str_trim)) # deletes whitespace

#unemployment data
arbl <- tbl_arbl %>%
  collect() %>%
  mutate(across(where(is.character), str_trim))

# economic sector data
econ_ao <- tbl_econ_ao %>% 
  collect() %>% 
  mutate(across(where(is.character), str_trim))

# Employees at place of work data
svp_ao <- tbl_svp_ao %>% 
  collect() %>% 
  mutate(across(where(is.character), str_trim))

# Employees at place of residence data
svp_wo <- tbl_svp_wo %>% 
  collect() %>% 
  mutate(across(where(is.character), str_trim))

# old age employees at the place of employment
oldsvp_ao <- tbl_oldsvp_ao %>% 
  collect() %>% 
  mutate(across(where(is.character), str_trim)) 

# population table
pop <- tbl_pop %>%
  collect() %>%
  mutate(across(where(is.character), str_trim)) %>%
  rename(pop_total = total) %>%
  filter(year %in% 2009:2019)

# old age marginal employees at the place of employment
oldsvp_marg <- tbl_oldsvp_marg %>% 
  collect() %>%
  mutate(across(where(is.character), str_trim))

# Municipal reference table/regiostar7 classification
muni_ref <- tbl_muni_ref %>%
  select(muni_key, muni_name, regiostar7) %>%
  collect() %>%
  mutate(across(where(is.character), str_trim)) %>% 
  mutate(regiostar7 = if_else(regiostar7 == 71, 72, regiostar7), # merge two urban categories
         gen_rs7 = case_when( # assign terms to rs7 values
  regiostar7 == 72 ~ 'Large City, urban',
  regiostar7 == 73 ~ 'Medium-sized city, urban',
  regiostar7 == 74 ~ 'Small town, urban',
  regiostar7 == 75 ~ 'Central City, rural',
  regiostar7 == 76 ~ 'Medium-sized city, rural',
  regiostar7 == 77 ~ 'Small town, rural'))




















# 
# 
#          , # merge two urban categories
#          gen_rs7 = factor(x = case_when( # assign terms to rs7 values
#            regiostar7 == 72 ~ 'Large City, urban',
#            regiostar7 == 73 ~ 'Medium-sized city, urban',
#            regiostar7 == 74 ~ 'Small town, urban',
#            regiostar7 == 75 ~ 'Central City, rural',
#            regiostar7 == 76 ~ 'Medium-sized city, rural',
#            regiostar7 == 77 ~ 'Small town, rural'),
#            levels = c('Large City, urban', # set factor levels
#                       'Medium-sized city, urban',
#                       'Small town, urban',
#                       'Central City, rural',
#                       'Medium-sized city, rural',
#                       'Small town, rural')))

# removing tbl objects from environment
rm(list = ls(pattern = "^tbl"))




# Data prep ---------------------------------------------------------------

# removing small data, only lose about 0.9% of total German population
pop_new <- pop %>% 
  filter(pop_total >= 1000) %>% #remove small towns
  group_by(muni_key) %>%  
  filter(n() == 11) %>%  # keep only complete observations
  ungroup()




#export to postgresql
sql_command <- "CREATE TABLE ref_regiostar7
  (
    muni_key character (20) PRIMARY KEY,
    muni_name character (50),
    regiostar7 numeric (10),
    gen_rs7 character (30)
  )
  WITH (
    OIDS=FALSE
  );"

# sends the command and creates the table
dbGetQuery(con, sql_command)

# uploads dataframe to Table
dbWriteTable(con, "ref_regiostar7", muni_ref, row.names=FALSE, append=TRUE)


























