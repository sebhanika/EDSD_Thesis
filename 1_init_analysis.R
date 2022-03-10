
# EDSD Thesis 2021/22
# Code for Preliminary Stats and View of dataset
# Author: Sebstian Hanika
# Date: 25/02/2022



# Libraries ---------------------------------------------------------------

library(tidyverse)
library(dbplyr)
library(RPostgreSQL)
require(RPostgreSQL)
library(stringr)

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

# Marginal Employment data
marg_emp <- tbl_marg_emp %>% 
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
  distinct(muni_key, muni_name, .keep_all = TRUE) #remove duplicate row

# removing tbl objects from environment
rm(list = ls(pattern = "^tbl"))


# overview stats all observations -----------------------------------------

# named list of dataframes
list_dfs <- listn(pop_age, arbl, econ_ao, marg_emp, svp_ao,
                  svp_wo, oldsvp_ao, pop, oldsvp_marg)

# summary table
summary_dfs <- output_dfs(listdf = list_dfs , func = summary_func)


# Overview stats towns pop > 500 ------------------------------------------

# removing small data, only lose about 0.9% of total German population
pop_new <- pop %>% 
  filter(pop_total >= 1000) %>% #remove small towns
  group_by(muni_key) %>%  
  filter(n() == 11) %>%  # keep only complete observations
  ungroup()


# filter all dataframes
list_dfs_filt <- sapply(list_dfs, filter, id %in% pop_new$id)

# apply summary function
summary_dfs_filt <- output_dfs(listdf = list_dfs_filt , func = summary_func)


# calculate population that was excluded
pop_filter <- pop_new %>% 
  filter(year == 2019) %>% 
  summarize(pop_tot = sum(pop_total)) %>% 
  pull()

pop_complete <- pop %>% 
  filter(year == 2019) %>% 
  summarize(pop_tot = sum(pop_total)) %>% 
  pull()



# summary stats by regiostar7 ---------------------------------------------

summary_regio <- oldsvp_ao %>% 
  left_join(select(muni_ref, c("muni_key", "regiostar7")), by = "muni_key") %>% 
  filter(id %in% pop_new$id) %>% 
  group_by(regiostar7) %>% 
  summarize(across(.cols = where(is.numeric),
                   .names = "{.col}_{.fn}", #double for pivoting later
                   .fns = list(nmiss = ~sum(is.na(.))/length(.)*100, # %of NAs
                               mean = ~mean(.x, na.rm = TRUE),
                               min = ~min(.x, na.rm = TRUE),
                               max = ~max(.x, na.rm = TRUE),
                               sd = ~sd(.x, na.rm = TRUE)
                   ))) %>% 
  pivot_longer(cols = -c(regiostar7), 
               names_to = c( "gender","variable", "measure"),
               names_sep = "_") %>% 
  drop_na()



try_plot1 <-  summary_regio %>% 
  filter(measure == "nmiss") %>% 
  ggplot(aes(x=variable, y=value, fill = gender)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_wrap(.~regiostar7)

try_plot1



try_plot2 <-  summary_regio %>% 
  filter(measure == "nmiss") %>% 
  ggplot(aes(x=regiostar7, y=value, fill = gender)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_wrap(.~variable)

try_plot2

