
# EDSD Thesis 2021/22
# Code for Preliminary Stats and View of dataset
# Author: Sebstian Hanika
# Date: 25/02/2022


# libraries and set up ----------------------------------------------------


library(tidyverse)
library(dbplyr)
library(RPostgreSQL)
require(RPostgreSQL)
library(stringr)
library(summarytools)


options(scipen=999) # disable scientific notation 
filter <- dplyr::filter    #EDIT
select <- dplyr::select
group.colors <- c(w = "grey80", m = "red3") # sets groups colors for male female 


########## Database and loading data  ##########
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

# marg emp data
marg_emp <- tbl_marg_emp %>% 
  collect() %>% 
  mutate(across(where(is.character), str_trim))

# svp_ao data
svp_ao <- tbl_svp_ao %>% 
  collect() %>% 
  mutate(across(where(is.character), str_trim))

# svp_wo data
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

# marginal old age employees at the place of employment
oldsvp_marg <- tbl_oldsvp_marg %>% 
  collect() %>%
  mutate(across(where(is.character), str_trim))

# Municipal reference table/ regiostar/
muni_ref <- tbl_muni_ref %>%
  select(muni_key, muni_name, regiostar7) %>%
  collect() %>%
  mutate(across(where(is.character), str_trim)) %>%
  distinct(muni_key, muni_name, .keep_all = TRUE) #remove duplicate row

# removing tbl objects from environment
rm(tbl_arbl, tbl_econ_ao, tbl_marg_emp, tbl_muni_ref,
   tbl_oldsvp_ao, tbl_oldsvp_marg, tbl_pop, tbl_popage,
   tbl_svp_ao, tbl_svp_wo)


# overview statistics -----------------------------------------------------


# here to calculate basic statics
# focus on missing values

source("0_functions_module.R")


list_dfs <- listn(pop_age, arbl, econ_ao, marg_emp, svp_ao,
                  svp_wo, oldsvp_ao, pop, oldsvp_marg,  muni_ref)

result_list <- output_dfs(listdf = list_dfs , func = count_NA)

# output list of
list2env(result_list, .GlobalEnv)


