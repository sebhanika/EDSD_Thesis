
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
library(qwraps2)
library(RColorBrewer)


# source functions/settings from functions Module
source('0_functions_module.R')

options(scipen=999) # disable scientific notation 


# Loading test data -------------------------------------------------------

imp_data_tbl <- tbl(con, in_schema('edsd', 'imp_calc_5'))
imp_cal_5 <- imp_data_tbl %>%
  collect() %>%
  mutate(across(where(is.character), str_trim)) %>% 
  mutate(gen_rs7 = factor(x = case_when( # assign terms to rs7 values
           rs7 == 72 ~ 'Large City, urban',
           rs7 == 73 ~ 'Medium-sized city, urban',
           rs7 == 74 ~ 'Small town, urban',
           rs7 == 75 ~ 'Central City, rural',
           rs7 == 76 ~ 'Medium-sized city, rural',
           rs7 == 77 ~ 'Small town, rural'),
           levels = c('Large City, urban', # set factor levels
                      'Medium-sized city, urban',
                      'Small town, urban',
                      'Central City, rural',
                      'Medium-sized city, rural', 
                      'Small town, rural')))


# 2009 variables
imp_calc2009 <- imp_cal_5 %>%
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
  left_join(imp_calc2009, by = 'muni_key')


# Summary_stat ------------------------------------------------------------

sum_stat <- wide_df_5 %>%
  ungroup() %>%
  select(c(grw_sh_total_aboveRet_total, grw_sh_total_aboveRet_m, grw_sh_total_aboveRet_r,
           hubdist100, com_ratio_pop09,  yg_work_sh09, grw_p_total_total_r))
  
  
our_summary1 <-
  list("Change total aboveRet" =
         list("min"       = ~ round(min(grw_sh_total_aboveRet_total), digits = 3),
              "max"       = ~ round(max(grw_sh_total_aboveRet_total), digits = 3),
              "mean (sd)" = ~ qwraps2::mean_sd(grw_sh_total_aboveRet_total, digits = getOption("qwraps2_frmt_digits", 3))),
       "Change marginal aboveRet" =
         list("min"       = ~ round(min(grw_sh_total_aboveRet_m), digits = 3),
              "max"       = ~ round(max(grw_sh_total_aboveRet_m), digits = 3),
              "mean (sd)" = ~ qwraps2::mean_sd(grw_sh_total_aboveRet_m, digits = getOption("qwraps2_frmt_digits", 3))),
       "Change regular aboveRet" =
         list("min"       = ~ round(min(grw_sh_total_aboveRet_r), digits = 3),
              "max"       = ~ round(max(grw_sh_total_aboveRet_r), digits = 3),
              "mean (sd)" = ~ qwraps2::mean_sd(grw_sh_total_aboveRet_r, digits = getOption("qwraps2_frmt_digits", 3))),
       "Dist large city" =
         list("min"       = ~ round(min(hubdist100), digits = 3),
              "max"       = ~ round(max(hubdist100), digits = 3),
              "mean (sd)" = ~ qwraps2::mean_sd(hubdist100, digits = getOption("qwraps2_frmt_digits", 3))),
       "Change Commuter ratio" =
         list("min"       = ~ round(min(com_ratio_pop09), digits = 3),
              "max"       = ~ round(max(com_ratio_pop09), digits = 3),
              "mean (sd)" = ~ qwraps2::mean_sd(com_ratio_pop09, digits = getOption("qwraps2_frmt_digits", 3))),
       "Share of young people 2009" =
         list("min"       = ~ round(min(yg_work_sh09), digits = 3),
              "max"       = ~ round(max(yg_work_sh09), digits = 3),
              "mean (sd)" = ~ qwraps2::mean_sd(yg_work_sh09, digits = getOption("qwraps2_frmt_digits", 3))),
       "Change regular aboveRet" =
         list("min"       = ~ round(min(grw_p_total_total_r), digits = 3),
              "max"       = ~ round(max(grw_p_total_total_r), digits = 3),
              "mean (sd)" = ~ qwraps2::mean_sd(grw_p_total_total_r, digits = getOption("qwraps2_frmt_digits", 3)))
  )

group_whole <- summary_table(dplyr::group_by(wide_df_5, rs7), our_summary1)
group_whole


# Boxplots East-West ------------------------------------------------------

emp_type_label <-  as_labeller(c(`m` = "Marginal", `r` = "Regular"))

east_west_cols <- c('East' = "#DABF7F", 
                    'West' = "#01665E") 

# Change in share of post retimret workers
p2 <- imp_cal_5 %>% 
  ungroup() %>% 
  filter(age == "aboveRet", year == 2019, sex == "total", type != "total") %>%
  ggplot(aes(x = gen_rs7,
             y = grw_sh, 
             fill = east_ger)) +
  scale_y_continuous(limits = c(-10, 10))+
  geom_boxplot() +
  facet_wrap(~type, labeller = emp_type_label) +
  scale_fill_manual(values = east_west_cols, labels = c("East", "West"))+
  labs(x = "Spatial category",
       y = "Change in share of post retirement workers") +
  theme(legend.position = "right",
        strip.text.x = element_text(size = 16),
        axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))
p2

#ggsave("grw_sh_east_west_type.pdf", width = 32, height = 18, units = "cm")
ggsave("presentation/grw_sh_east_west_type.png", width = 32, height = 18, units = "cm")


# Share of post retimret workers
p3 <- imp_cal_5 %>% 
  ungroup() %>% 
  filter(age == "aboveRet", year == 2019, sex == "total", type == "total") %>%
  ggplot(aes(x = gen_rs7,
             y = share, 
             fill = east_ger)) +
  scale_y_continuous(limits = c(0, 0.2))+
  geom_boxplot() +
  #facet_wrap(~type, labeller = emp_type_label) +
  scale_fill_manual(values = east_west_cols, labels = c("East", "West"))+
  labs(x = "Spatial category",
       y = "Share of post retirement workers in %") +
  theme(legend.position = "right",
        strip.text.x = element_text(size = 16),
        axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))
p3

#ggsave("sh_east_west_type.pdf", width = 32, height = 18, units = "cm")

ggsave("presentation/sh_east_west_type.png", width = 32, height = 18, units = "cm")



# Boxplots males-females --------------------------------------------------

male_female_cols <- c('men' = "#DABF7F", 
                      'women' = "#01665E") 

p4 <- imp_cal_5 %>% 
  ungroup() %>% 
  filter(age == "aboveRet", year == 2019, sex != "total", type != "total") %>%
  ggplot(aes(x = gen_rs7,
             y = share, 
             fill = sex)) +
  scale_y_continuous(limits = c(-0.01, 0.15))+
  geom_boxplot(outlier.size = 1) +
  facet_wrap(~type, labeller = emp_type_label) +
  scale_fill_manual(values = male_female_cols, labels = c("Men", "Women"))+
  labs(x = "Spatial category",
       y = "Share of post retirement workers %") +
  theme(legend.position = "right",
        strip.text.x = element_text(size = 16),
        axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))
p4

ggsave("presentation/share_aboveRet_Sex.png", width = 32, height = 18, units = "cm")
#ggsave("share_aboveRet_Sex.pdf", width = 32, height = 18, units = "cm")


p5 <- imp_cal_5 %>% 
  ungroup() %>% 
  filter(age == "aboveRet", year == 2019, sex != "total", type != "total") %>%
  ggplot(aes(x = gen_rs7,
             y = grw_sh, 
             fill = sex)) +
  scale_y_continuous(limits = c(-7, 7))+
  geom_boxplot(outlier.size = 1) +
  facet_wrap(~type, labeller = emp_type_label) +
  scale_fill_manual(values = male_female_cols, labels = c("Men", "Women"))+
  labs(x = "Spatial category",
       y = "Change in share of post retirement workers %") +
  theme(legend.position = "bottom",
        strip.text.x = element_text(size = 16),
        axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))
p5



# Boxplot male-females EastGermany ----------------------------------------


male_female_cols <- c('men' = "#DABF7F", 
                      'women' = "#01665E") 



p5 <- imp_cal_5 %>% 
  ungroup() %>% 
  filter(age == "aboveRet", year == 2019, sex != "total", type == "m") %>%
  ggplot(aes(x = gen_rs7,
             y = share, 
             fill = sex)) +
  scale_y_continuous(limits = c(-0.01, 0.12))+
  geom_boxplot(outlier.size = 1) +
  facet_wrap(~east_ger) +
  scale_fill_manual(values = male_female_cols, labels = c("Men", "Women"))+
  labs(x = "Spatial category",
       y = "Share of post retirement workers %") +
  theme(legend.position = "bottom",
        strip.text.x = element_text(size = 16),
        axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))
p5


#ggsave("sh_aboveRet_Sex_East.pdf", width = 32, height = 18, units = "cm")








