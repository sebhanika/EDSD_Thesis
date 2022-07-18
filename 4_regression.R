
# Title: EDSD Thesis 2021/22
# Topic: Performing Robust regression
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

## loading imputation variabel from database, all tables from edsd schema
n <- 10 # copied from above
names_imp_calc <- paste0("imp_calc_", 1:n)

tbl_list_imp <- lapply(names_imp_calc, function(t) tbl(con, in_schema('edsd', t)))

# collect tables to tibble and trim white spaces
df_imps_calc <- lapply(tbl_list_imp, function(t) collect(t) %>%
                    mutate(across(where(is.character), str_trim))) #sometimes postgresql adds whitespace?

# set shorter names for tables
df_imps_calc <- setNames(df_imps_calc, names_imp_calc)



# Loading test data -------------------------------------------------------


# 
# try_data_tbl <- tbl(con, in_schema('edsd', 'imp_calc_1'))
# try_imp_cal_1 <- try_data_tbl %>%
#   collect() %>%
#   mutate(across(where(is.character), str_trim))

# 
# # 2009 variables
# try_imp_calc2009 <- try_imp_cal_1 %>% 
#   filter(year == 2009) %>% 
#   mutate(com_ratio09 = inbound_commuter-outbound_commuter,
#          com_ratio_pop09 = (com_ratio09/workPop)*100,
#          workPop_ln09 = log(workPop)) %>% 
#   group_by(muni_key) %>% 
#   mutate(yg_work_sh09 = share[type == "r" & age == "below25" & sex == "total"]) %>%
#   ungroup() %>% 
#   select(muni_key, yg_work_sh09, com_ratio_pop09, workPop_ln09) %>% 
#   distinct()
# 
# 
# ## reshape data frame
# wide_df <- try_imp_cal_1 %>% 
#   mutate(rs7 = as.factor(rs7)) %>% 
#   filter(year == 2019) %>% 
#   select(-c(value, grw, inbound_commuter, outbound_commuter, workPop)) %>% 
#   pivot_wider(names_sep = '_', 
#               values_from = c('grw_p', 'share', 'grw_sh'),
#               names_from = c('sex', 'age', 'type')) %>% 
#   left_join(try_imp_calc2009, by = 'muni_key')



# Data Wrangeling ---------------------------------------------------------

# select variables for 2009 values
data_09 <- lapply(df_imps_calc, function(x) x %>%
                      filter(year == 2009) %>% 
                      mutate(com_ratio09 = inbound_commuter-outbound_commuter,
                             com_ratio_pop09 = (com_ratio09/workPop)*100,
                             workPop_ln09 = log(workPop)) %>% 
                      group_by(muni_key) %>% 
                      mutate(yg_work_sh09 = share[type == "r" & age == "below25" & sex == "total"]) %>%
                      ungroup() %>% 
                      select(muni_key, yg_work_sh09, com_ratio_pop09, workPop_ln09) %>% 
                      distinct()
                    )
  
# reshape dataframes to wide format
data_wide <- lapply(df_imps_calc, function(x) x %>% 
                        mutate(rs7 = as.factor(rs7)) %>% 
                        filter(year == 2019) %>% 
                        select(-c(value, grw, inbound_commuter, outbound_commuter, workPop)) %>% 
                        pivot_wider(names_sep = '_', 
                                    values_from = c('grw_p', 'share', 'grw_sh'),
                                    names_from = c('sex', 'age', 'type')))

## join 2009 dataframes to each corresponding data data frame in list
df_models <- list() #empty list to store values in

for (i in 1:10){
  df_models[[i]] <- left_join(data_wide[[i]],
                              data_09[[i]], 
                              by = 'muni_key')
  }
                            
# set names for models
df_models_names  <-  paste0("df_model_", 1:10)
df_models <- setNames(df_models, df_models_names)

rm(df_imps_calc)

# Model Total Retirement Employment ---------------------------------------

# create base model with only rs7 and East-Germany as variables
ls_models_base <- lapply(df_models, 
                         function(x) lmrob(grw_sh_total_aboveRet_total ~ 
                                             rs7 +
                                             east_ger,
                                           data = x, k.max	= 300)
                      )


ls_models_A1 <- lapply(df_models, 
                      function(x) lmrob(grw_sh_total_aboveRet_total ~ 
                                          rs7 +
                                          east_ger +
                                          hubdist100 + 
                                          com_ratio_pop09 +
                                          yg_work_sh09,
                                        data = x, k.max	= 300)
                    )


ls_models_A2 <- lapply(df_models, 
                    function(x) lmrob(grw_sh_total_aboveRet_total ~ 
                                        rs7 +
                                        east_ger +
                                        hubdist100 + 
                                        com_ratio_pop09 +
                                        yg_work_sh09 +
                                        grw_p_total_total_r,
                                      data = x, k.max	= 300)
)


# Pool results and summarize for base model
models_base_pool <- pool(ls_models_base)
models_base_sum <- summary(models_base_pool)

# Pool results and summarize for Model A1
models_A1_pool <- pool(ls_models_A1)
models_A1_sum <- summary(models_A1_pool)

# Pool results and summarize for Model A2
models_A2_pool <- pool(ls_models_A2)
models_A2_sum <- summary(models_A2_pool)

# calculate GOFs (R squared)
models_base_r2 <- pool.r.squared(models_base_pool, adjusted = F)
models_A1_r2 <- pool.r.squared(models_A1_pool, adjusted = F)
models_A2_r2 <- pool.r.squared(models_A2_pool, adjusted = F)


# turn pooled BASE models into Texreg object
model_base_texreg <- createTexreg(
                    coef.names = as.character(models_base_sum$term), # names of coefficients 
                    coef = models_base_sum$estimate,  # values of coef
                    se = models_base_sum$std.error, # stds of coef
                    pvalues = models_base_sum$p.value) # pvalues

# turn pooled A1 models into Texreg object
model_A1_texreg <- createTexreg(
                    coef.names = as.character(models_A1_sum$term), # names of coefficients 
                    coef = models_A1_sum$estimate,  # values of coef
                    se = models_A1_sum$std.error, # stds of coef
                    pvalues = models_A1_sum$p.value) # pvalues

# turn pooled A2 models into Texreg object
model_A2_texreg <- createTexreg(
                    coef.names = as.character(models_A2_sum$term), # names of coefficients 
                    coef = models_A2_sum$estimate,  # values of coef
                    se = models_A2_sum$std.error, # stds of coef
                    pvalues = models_A2_sum$p.value) # pvalues

nobs.models <- nrow(drop_na(df_models[[1]]))

# extract NObs and rsquares from the models
gofs_A <- list("Num. Obs." = c(nobs.models, nobs.models, nobs.models),
             "R^2" = c(models_base_r2[1], models_A1_r2[1], models_A2_r2[1]))


model_A_labels <- c("Base", "A1", "A2")
coef_A_labels <- c('Intercept' , 
                 'Medium-sized city, urban',
                 'Small town, urban', 'Central City, rural','Medium-sized city, rural',
                 'Small town, rural', "West Germany", "Distance to large city",
                 'Comuter Ratio 2009', 'Share of young workers', 'Employment Growth')

texreg(l = list(model_base_texreg, model_A1_texreg, model_A2_texreg),
          custom.model.names = model_A_labels,
          #custom.coef.names = coef_A_labels,
          custom.gof.rows = gofs_A,
          include.nobs = F,
          dcolumn = T,
          digits = 3
          )


# Model with marginal employment as dp  -----------------------------------

ls_mod_marg <- lapply(df_models, 
                         function(x) lmrob(grw_sh_total_aboveRet_m ~ 
                                             rs7 +
                                             east_ger +
                                             hubdist100 + 
                                             com_ratio_pop09 +
                                             yg_work_sh09 +
                                             grw_p_total_total_r,
                                           data = x, k.max	= 300, fast.s.large.n = Inf)
                      )

# Pool results and summarize for base model
models_marg_pool <- pool(ls_mod_marg)
models_marg_sum <- summary(models_marg_pool)

models_marg_r2 <- pool.r.squared(models_marg_pool, adjusted = F)

# turn pooled marginal employment models into Texreg object

model_marg_texreg <- createTexreg(
  coef.names = as.character(models_marg_sum$term), # names of coefficients 
  coef = models_marg_sum$estimate,  # values of coef
  se = models_marg_sum$std.error, # stds of coef
  pvalues = models_marg_sum$p.value) # pvalues


# Model with regular employment as dp -------------------------------------


ls_mod_reg <- lapply(df_models, 
                      function(x) lmrob(grw_sh_total_aboveRet_r ~ 
                                          rs7 +
                                          east_ger +
                                          hubdist100 + 
                                          com_ratio_pop09 +
                                          yg_work_sh09 +
                                          grw_p_total_total_r,
                                        data = x, k.max	= 300)
)

# Pool results and summarize for base model
models_reg_pool <- pool(ls_mod_reg)
models_reg_sum <- summary(models_reg_pool)

models_reg_r2 <- pool.r.squared(models_reg_pool, adjusted = F)

# turn pooled regular employment models into Texreg object
model_reg_texreg <- createTexreg(
  coef.names = as.character(models_reg_sum$term), # names of coefficients 
  coef = models_reg_sum$estimate,  # values of coef
  se = models_reg_sum$std.error, # stds of coef
  pvalues = models_reg_sum$p.value) # pvalues



### Combine models in table

nobs.models <- nrow(drop_na(df_models[[1]]))

# extract NObs and rsquares from the models
gofs_B <- list("Num. Obs." = c(nobs.models, nobs.models),
               "R-squared" = c(models_marg_r2[1], models_reg_r2[1]))


model_B_labels <- c("Marginal", "Regular")
coef_B_labels <- c('Intercept' , 
                   'Medium-sized city, urban',
                   'Small town, urban', 'Central City, rural','Medium-sized city, rural',
                   'Small town, rural', "West Germany", "Distance to large city",
                   'Commuter Ratio in 2009', 'Share of young workers in 2009', 'Employment Growth')

texreg(l = list(model_marg_texreg, model_reg_texreg),
          custom.model.names = model_B_labels,
          custom.coef.names = coef_B_labels,
          custom.gof.rows = gofs_B,
          include.nobs = F,
          dcolumn = T,
          digits = 3
)










# Running model for each rs7 category -------------------------------------

# create empty lists to store values in
ls_texreg <- list()
df_models_filt <- list()
ls_mod_rs7 <- list()
models_rs7_pool <- list()
models_rs7_sum <- list()
models_rs7_r2 <- list()

# loop through rs7 cateogefores to calculate and pool all models
for (i in 72:77){
  
      
      df_models_filt[[i]] <- lapply(df_models, function(x) x %>%
                                    filter(rs7 == i) %>% 
                                    mutate(rs7 = as.integer(rs7))
                                    )
      
      
      ls_mod_rs7[[i]] <- lapply(df_models_filt[[i]], 
                          function(x) lmrob(grw_sh_total_aboveRet_m ~ 
                                              hubdist100 + 
                                              com_ratio_pop09 +
                                              yg_work_sh09 +
                                              rs7 +
                                              east_ger+
                                              grw_p_total_total_r,
                                            data = x, k.max	= 300)
      )
      

      # pool models
      models_rs7_pool[[i]] <- pool(ls_mod_rs7[[i]])
      models_rs7_sum[[i]] <- summary(models_rs7_pool[[i]])
      # calculate R squared
      models_rs7_r2[[i]] <- pool.r.squared(models_rs7_pool[[i]], adjusted = F)


      #create texreg object for each model
      ls_texreg[[i]] <- createTexreg(
        coef.names = as.character(models_rs7_sum[[i]]$term), # names of coefficients
        coef = models_rs7_sum[[i]]$estimate,  # values of coef
        se = models_rs7_sum[[i]]$std.error, # stds of coef
        pvalues = models_rs7_sum[[i]]$p.value) # pvalues

}


### Remove empty element from lista
ls_texreg <- ls_texreg[lengths(ls_texreg) > 0L]
models_rs7_r2 <- models_rs7_r2[lengths(models_rs7_r2) > 0L]
df_models_filt <- df_models_filt[lengths(df_models_filt) > 0L]

# extract number of observations
nob72 <- nrow(df_models_filt[[1]][[1]])
nob73 <- nrow(df_models_filt[[2]][[1]])
nob74 <- nrow(df_models_filt[[3]][[1]])
nob75 <- nrow(df_models_filt[[4]][[1]])
nob76 <- nrow(df_models_filt[[5]][[1]])
nob77 <- nrow(df_models_filt[[6]][[1]])

# creat gofs object
gofs_rs <- list("Num. Obs." = c(nob72, nob73, nob74, nob75, nob76, nob77),
               "R-squared" = c(models_rs7_r2[[1]][1], models_rs7_r2[[2]][1],
                               models_rs7_r2[[3]][1], models_rs7_r2[[4]][1],
                               models_rs7_r2[[4]][1], models_rs7_r2[[6]][1]))

# create Texreg output
screenreg(l = list(ls_texreg[[1]], ls_texreg[[2]], ls_texreg[[3]], 
                   ls_texreg[[4]], ls_texreg[[5]], ls_texreg[[6]]),
          include.nobs = F,
          custom.gof.rows = gofs_rs,
          dcolumn = T,
          digits = 3
)



