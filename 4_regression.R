
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

## loading imputation variabel from database, all tables from edsd schema
n <- 10 # copied from above
names_imp_calc <- paste0("imp_calc_", 1:n)

tbl_list_imp <- lapply(names_imp_calc, function(t) tbl(con, in_schema('edsd', t)))

# collect tables to tibble and trim white spaces
df_imps_calc <- lapply(tbl_list_imp, function(t) collect(t) %>%
                    mutate(across(where(is.character), str_trim))) #sometimes postgresql adds whitespace?

# set shorter names for tables
df_imps_calc <- setNames(df_imps_calc, names_imp_calc)





### loading test data
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
                                          hubdist100 + 
                                          com_ratio_pop09 +
                                          yg_work_sh09 +
                                          rs7 +
                                          east_ger,
                                        data = x, k.max	= 300)
                    )


ls_models_A2 <- lapply(df_models, 
                    function(x) lmrob(grw_sh_total_aboveRet_total ~ 
                                        hubdist100 + 
                                        com_ratio_pop09 +
                                        yg_work_sh09 +
                                        rs7 +
                                        east_ger+
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

screenreg(l = list(model_base_texreg, model_A1_texreg, model_A2_texreg),
          custom.model.names = model_A_labels,
          custom.coef.names = coef_A_labels,
          custom.gof.rows = gofs_A,
          include.nobs = F,
          dcolumn = T,
          digits = 3
)


###### Models with ageing as dependet variable







mod_base <- lmrob(grw_sh_total_aboveRet_total ~ 
                    rs7 +
                  east_ger,
                data = wide_df, k.max	= 300)

summary(mod_base)
car::vif(mod_base)



mod_contA1 <- lmrob(grw_sh_total_aboveRet_total ~ 
                    grw_p_total_total_r +
                    hubdist100 + 
                    com_ratio_pop09 +
                    yg_work_sh09 +
                    rs7 +
                    east_ger,
                  data = wide_df, k.max	= 300)

summary(mod_contA1)
car::vif(mod_contA1)


model_labels <- c("Base", "A1")

sum_mod_base <- summary(mod_base)
sum_mod_contA1 <- summary(mod_contA1)


gofs <- list("R^2" = c(sum_mod_base$r.squared, sum_mod_contA1$r.squared))

screenreg(l = list(mod_base, mod_contA1),
          custom.model.names = model_labels,
          include.nobs = T,
          dcolumn = T,
          digits = 3,
          custom.gof.rows = gofs
          
)





# Model with marginal employment as dp  -----------------------------------



mod_marg <- lmrob(grw_sh_total_aboveRet_m ~ 
                    grw_p_total_total_r +
                    hubdist100 + 
                    com_ratio_pop09 +
                    yg_work_sh09 +
                    rs7 +
                    east_ger,
                  data = wide_df, k.max	= 300)

summary(mod_marg)
car::vif(mod_marg)



# Model with regular employment as dp -------------------------------------

mod_reg <- lmrob(grw_sh_total_aboveRet_r ~ 
                    grw_p_total_total_r +
                    hubdist100 + 
                    com_ratio_pop09 +
                    yg_work_sh09 +
                    rs7 +
                    east_ger,
                  data = wide_df, k.max	= 300)

summary(mod_reg)
car::vif(mod_reg)












p <- try_imp_cal_1 %>% 
  ungroup() %>% 
  #filter(grw_sh < 60) %>% 
  filter(age == "aboveRet", year == 2019, sex == "total", type != "total") %>%
  ggplot(aes(x = as.factor(rs7), y = grw_sh, fill = type)) +
  scale_y_continuous(limits = c(-10, 10))+
  geom_boxplot()

p




wide_df %>% 
  filter(grw_p_total_total_r < 250) %>% 
  ggplot(aes(x =  grw_p_total_total_r , y =  share_total_aboveRet_r)) +
  geom_point() +
  geom_smooth(method = "lm")+
  facet_wrap(~rs7, nrow = 3) +
  labs(title ="Regular")



wide_df %>% 
  filter(grw_p_total_total_r < 250) %>% 
  ggplot(aes(x =  grw_p_total_total_r , y =  share_total_aboveRet_m)) +
  geom_point() +
  geom_smooth(method = "lm")+
  facet_wrap(~rs7, nrow = 3) +
  labs(title = "Marginal")
































# doing other stuff


rs7_len <- sort(unique(try_imp_cal_1$rs7))
try_mod_l <- list(NULL)

for (item in rs7_len){
  
  try_mod_l[[item]] <- lmrob(grw_sh_total_aboveRet_m ~ 
                               grw_p_total_total_r +
                               rs7 + 
                               hubdist100 + 
                               com_ratio_pop09 +
                               yg_work_sh09 +
                               east_ger,
                             data = subset(wide_df, rs7 == item), k.max = 300)
                     

}


mod_rs7 <- function (x) 
  lmrob(grw_sh_total_aboveRet_m ~ 
          grw_p_total_total_r +
          rs7 + 
          hubdist100 + 
          com_ratio_pop09 +
          yg_work_sh09 +
          east_ger, data = subset(wide_df, rs7 == x))



y_test <- map(rs7_len, mod_rs7)


sum_test <- lapply(y_test, summary)







cor(wide_df$grw_p_total_total_r, wide_df$grw_sh_total_aboveRet_total)


# some plots for testing

wide_df %>% 
  filter(
         grw_sh_total_aboveRet_total < 60) %>% 
  ggplot(aes(x =  grw_sh_total_aboveRet_m , y =  grw_sh_total_aboveRet_total)) +
  geom_point() +
  geom_smooth(method = "lm")+
  facet_wrap(~rs7, nrow = 3)


























# Models with employment change as dependent variable ---------------------



mod_base <- lmrob(grw_p_total_total_r ~ 
                    rs7 + 
                    hubdist100 + 
                    com_ratio_pop09 +
                    yg_work_sh09 +
                    east_ger,
                  data = wide_df, fast.s.large.n = Inf)

summary(mod_base)

mod_A1 <- lmrob(grw_p_total_total_r ~ 
                  grw_sh_total_aboveRet_r +
                  rs7 +
                  hubdist100 + 
                  com_ratio_pop09 +
                  yg_work_sh09 +
                  east_ger,
                data = wide_df, k.max	= 300)

summary(mod_A1)

mod_A2 <- lmrob(grw_p_total_total_r ~ 
                  grw_sh_total_aboveRet_r +
                  grw_sh_total_aboveRet_m +
                  rs7 + 
                  hubdist100 + 
                  com_ratio_pop09 +
                  yg_work_sh09 +
                  east_ger,
                data = wide_df)

summary(mod_A2)



model_labels <- c("Base", "A1", "A2")


screenreg(l = list(mod_base, mod_A1 ,mod_A2),
          custom.model.names = model_labels,
          include.nobs = T,
          dcolumn = T,
          digits = 3
)








