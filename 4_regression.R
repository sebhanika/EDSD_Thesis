
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
# n <- 10 # copied from above
# names_imp_calc <- paste0("imp_calc_", 1:n)
# 
# tbl_list_imp <- lapply(names_imp_calc, function(t) tbl(con, in_schema('edsd', t)))
# 
# # collect tables to tibble and trim white spaces
# df_imps_calc <- lapply(tbl_list_imp, function(t) collect(t) %>% 
#                     mutate(across(where(is.character), str_trim))) #sometimes postgresql adds whitespace?
# 
# # set shorter names for tables
# df_imps_calc <- setNames(df_imps_calc, names_imp_calc)
# 


try_data_tbl <- tbl(con, in_schema('edsd', 'imp_calc_1'))
# distance hub table, distance to large city
try_imp_cal_1 <- try_data_tbl %>%
  collect() %>%
  mutate(across(where(is.character), str_trim))





# 2009 variables
try_imp_calc2009 <- try_imp_cal_1 %>% 
  filter(year == 2009) %>% 
  mutate(com_ratio09 = inbound_commuter-outbound_commuter,
         com_ratio_pop09 = (com_ratio09/workPop)*100,
         workPop_ln09 = log(workPop)) %>% 
  group_by(muni_key) %>% 
  mutate(yg_work_sh = share[type == "r" & age == "below25" & sex == "total"]) %>%
  ungroup() %>% 
  select(muni_key, yg_work_sh, com_ratio_pop09, workPop_ln09) %>% 
  distinct()



wide_df <- try_imp_cal_1 %>% 
  mutate(rs7 = as.factor(rs7)) %>% 
  filter(year == 2019) %>% 
  select(-c(value, grw, inbound_commuter, outbound_commuter, workPop, workPop_per)) %>% 
  pivot_wider(names_sep = '_', 
              values_from = c('grw_p', 'share', 'grw_sh'),
              names_from = c('sex', 'age', 'type')) %>% 
  left_join(try_imp_calc2009, by = 'muni_key')




# tryin gmodels




try_mod <- lmrob(grw_p_total_total_r ~ 
                   grw_sh_total_aboveRet_m +
                   hubdist100 + 
                   com_ratio_pop09 +
                   east_ger,
                 data = wide_df)


summary(try_mod)




# 
try_mod <- lmrob(grw_p_total_total_r ~ 
                   grw_sh_women_aboveRet_m +
                   hubdist100 + 
                   com_ratio_pop09 +
                   east_ger,
                 data = wide_df)


summary(try_mod)

car::vif(try_mod)





###### Models with ageing as dependet variable


try_mod_base <- lmrob(grw_sh_total_aboveRet_m ~ 
                        grw_p_total_total_m +
                        rs7 + 
                        hubdist100 + 
                        com_ratio_pop09 +
                        yg_work_sh +
                        east_ger,
                      data = wide_df)

summary(try_mod_base)


car::vif(try_mod_aboveret_r)



























# doing other stuff


rs7_len <- sort(unique(try_imp_cal_1$rs7))
try_mod_l <- list(NULL)

for (item in rs7_len){
  
  try_mod_l[[item]] <- lmrob(grw_p_total_total_r ~ 
                     grw_sh_total_aboveRet_r +
                     yg_work_sh +
                     hubdist100 + 
                     com_ratio_pop09 +
                     east_ger, data = subset(wide_df, rs7 == item)
                     )

}


mod_rs7 <- function (x) 
  lmrob(grw_p_total_total_r ~ 
          grw_sh_total_aboveRet_r +
          yg_work_sh +
          hubdist100 + 
          com_ratio_pop09 +
          east_ger, data = subset(wide_df, rs7 == x))



y_test <- map(rs7_len, mod_rs7)


sum_test <- lapply(y_test, summary)



# some plots for testing

wide_df %>% 
  filter(grw_p_total_total_r < 250,
         grw_sh_total_aboveRet_m < 20) %>% 
  ggplot(aes(x = grw_sh_men_aboveRet_m  , y = grw_p_total_total_r)) +
  geom_point() +
  geom_smooth(method = "lm")+
  facet_wrap(~rs7, nrow = 3)



wide_df %>% 
  filter(grw_p_total_total_r < 250,
         grw_sh_total_aboveRet_m < 20) %>% 
  ggplot(aes(x = grw_sh_women_aboveRet_m  , y = grw_p_total_total_r)) +
  geom_point() +
  geom_smooth(method = "lm")+
  facet_wrap(~rs7, nrow = 3)







try_imp_cal_1 %>% 
  ungroup() %>% 
  filter(age == "aboveRet", year == 2019, sex != "total") %>%
  ggplot(aes(x = as.factor(rs7), y = grw_p, fill = sex)) +
  scale_y_continuous(limits = c(-1000, 1000))+
  geom_boxplot()+
  facet_wrap(~type)



