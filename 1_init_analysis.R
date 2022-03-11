
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
         gen_rs7 = factor(x = case_when( # assign terms to rs7 values
           regiostar7 == 72 ~ 'Large City, urban',
           regiostar7 == 73 ~ 'Medium-sized city, urban',
           regiostar7 == 74 ~ 'Small town, urban',
           regiostar7 == 75 ~ 'Central City, rural',
           regiostar7 == 76 ~ 'Medium-sized city, rural',
           regiostar7 == 77 ~ 'Small town, rural'),
                          levels = c('Large City, urban', # set factor levels
                                     'Medium-sized city, urban',
                                     'Small town, urban',
                                     'Central City, rural',
                                     'Medium-sized city, rural', 
                                     'Small town, rural')))

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




kable(summary_dfs_filt, "latex")



# Summary stats regular employees by regiostar7 ---------------------------

summary_regio_reg <- oldsvp_ao %>% 
  left_join(select(muni_ref, c("muni_key", "gen_rs7")), by = "muni_key") %>% 
  filter(id %in% pop_new$id) %>% 
  group_by(gen_rs7) %>% 
  summarize(across(.cols = where(is.numeric),
                   .names = "{.col}_{.fn}", #double for pivoting later
                   .fns = list(nmiss = ~sum(is.na(.))/length(.)*100, # %of NAs
                               mean = ~mean(.x, na.rm = TRUE),
                               min = ~min(.x, na.rm = TRUE),
                               max = ~max(.x, na.rm = TRUE),
                               sd = ~sd(.x, na.rm = TRUE)
                   ))) %>% 
  pivot_longer(cols = -c(gen_rs7), 
               names_to = c( "gender","variable", "measure"),
               names_sep = "_") %>% 
  drop_na() %>% 
  mutate(gender = factor(str_to_title(gender), 
                         levels = c("Total", "Men", "Women")))


# plot percentage of missing values by region and 
p_NA_oldsvpao_regio <-  summary_regio_reg %>% 
  filter(measure == "nmiss") %>% 
  ggplot(aes(x=variable, y=value, fill = gender)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_wrap(.~gen_rs7) + 
  labs(x = "Variable",
       y = "Missing values (%)",
       title = "Missing values for employment data by spatial class",
       subtitle = "Regular employment data") +
  scale_fill_brewer(palette = "Set2", 
                    labels = c("Total", "Men", "Women")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

p_NA_oldsvpao_regio


p_NA_oldsvpao_age <-  summary_regio_reg %>% 
  filter(measure == "nmiss") %>% 
  ggplot(aes(x=gen_rs7, y=value, fill = gender)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_wrap(.~variable) + 
  labs(x = "Spatial class",
       y = "Missing values (%)",
       title = "Missing values for employment data by age group",
       subtitle = "Regular employment data") +
 scale_fill_brewer(palette = "Set2", 
                   labels = c("Total", "Men", "Women")) +
    theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))

p_NA_oldsvpao_age




# Summary stats marginal employees by regiostar7 --------------------------


summary_regio_marg <- oldsvp_marg %>% 
  left_join(select(muni_ref, c("muni_key", "gen_rs7")), by = "muni_key") %>% 
  filter(id %in% pop_new$id) %>% 
  group_by(gen_rs7) %>% 
  summarize(across(.cols = where(is.numeric),
                   .names = "{.col}_{.fn}", #double for pivoting later
                   .fns = list(nmiss = ~sum(is.na(.))/length(.)*100, # %of NAs
                               mean = ~mean(.x, na.rm = TRUE),
                               min = ~min(.x, na.rm = TRUE),
                               max = ~max(.x, na.rm = TRUE),
                               sd = ~sd(.x, na.rm = TRUE)
                   )))%>% 
  pivot_longer(cols = -c(gen_rs7), 
               names_to = c( "gender","variable", "del", "measure"),
               names_sep = "_") %>% 
  drop_na() %>% 
  mutate(gender = factor(str_to_title(gender), 
                         levels = c("Total", "Men", "Women")))


# plot percentage of missing values by region and 
p_NA_oldsvp_marg_regio <-  summary_regio_marg %>% 
  filter(measure == "nmiss") %>% 
  ggplot(aes(x=variable, y=value, fill = gender)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_wrap(.~gen_rs7) + 
  labs(x = "Variable",
       y = "Missing values (%)",
       title = "Missing values for employment data by spatial class",
       subtitle = "Marginal employment data") +
  scale_fill_brewer(palette = "Set2", 
                    labels = c("Total", "Men", "Women")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

p_NA_oldsvp_marg_regio




p_NA_oldsvp_marg_age <-  summary_regio_marg %>% 
  filter(measure == "nmiss") %>% 
  ggplot(aes(x=gen_rs7, y=value, fill = gender)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_wrap(.~variable) + 
  labs(x = "Spatial class",
       y = "Missing values (%)",
       title = "Missing values for employment data by age group",
       subtitle = "Marginal employment data") +
  scale_fill_brewer(palette = "Set2", 
                    labels = c("Total", "Men", "Women")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))

p_NA_oldsvp_marg_age




