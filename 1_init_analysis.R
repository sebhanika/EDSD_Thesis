
# EDSD Thesis 2021/22
# Code for Preliminary Stats and View of dataset
# Author: Sebstian Hanika
# Date: 25/02/2022


# libraries and set up ----------------------------------------------------


library(stargazer)
library(robustbase)
library(tidyverse)
library(dplyr)
library(dbplyr)
library(RPostgreSQL)
require(RPostgreSQL)
library(gridExtra)
library(grid)
library(stringr)


options(scipen=999) # disable scientific notation 
filter <- dplyr::filter    #EDIT
select <- dplyr::select
group.colors <- c(w = "grey80", m = "red3") # sets groups colors for male female 


########## Database and loading data  ##########
pw <- {"Calbhe1110?"} 
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
  collect() %>%
  mutate(across(where(is.character), str_trim))

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

# old age employment am arbeitsort, imputated
oldsvp_ao <- tbl_oldsvp_ao %>% 
  collect() %>% #is necessary to create table
  mutate(across(where(is.character), str_trim)) # deletes whitespace

# population table
pop <- tbl_pop %>%
  collect() %>%
  mutate(across(where(is.character), str_trim)) %>%
  rename(pop_total = total) %>%
  filter(year %in% 2009:2019)

# old age employment am arbeitsort, imputated
oldsvp_marg <- tbl_oldsvp_marg %>% 
  collect() %>% #is necessary to create table
  mutate(across(where(is.character), str_trim)) # deletes whitespace

# gemeinde referenz tabelle regiostar7, imputated
muni_ref <- tbl_muni_ref %>%
  select(muni_key, muni_name, regiostar7) %>%
  collect() %>%
  mutate(across(where(is.character), str_trim)) %>%
  distinct(muni_key, muni_name, .keep_all = TRUE) #remove duplicate row


######## data preparation #######




above_ret_age <- oldsvp_ao %>%
  mutate(men_abvret = men_65older- men_65olderstand,
         women_abvret = women_65older - women_65olderstand) %>%
  select(c(total_total, men_abvret , women_abvret, year )) %>% 
  group_by(year) %>%
  summarize(across(everything(), ~sum(.))) %>%
  mutate(Male_abvret_p = men_abvret/total_total*100,
         Female_abvret_p = women_abvret/total_total*100) %>%
  pivot_longer(cols = -c("year"), #not these Cols
               names_to = c("sex", "categ", "percent"), #new cols
               names_sep = "_") %>%
  mutate(sex = as.factor(sex))



plot_abvret <- above_ret_age %>%
  filter(percent == "p") %>%
  ggplot(aes(x = year, y = value)) +
  geom_point(aes(color = sex, shape = sex), size = 3) +
  scale_x_continuous(breaks = seq(2009, 2019, 2),
                     limits = c(2009, 2019)) + 
  scale_color_manual(values=c('darkblue','peru'))+
  theme_bw()+
  labs(y = "Post-retirment employees in % of workforce\n",
       x = "Year")+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"),
        axis.title = element_text(size=16, color = "grey30"),
        axis.text = element_text(size=18, color = "grey30"),
        legend.text = element_text(size=20, color = "grey30"),
        legend.title=element_blank(),
        legend.position = c(0.15, 0.90))
plot_abvret










init_calc <- oldsvp_marg %>%
  filter(year == 2019) %>%
  left_join(select(muni_ref, "regiostar7", "muni_key"), by = "muni_key") %>%
  left_join(select(pop, "pop_total", "id"), by = "id") %>%
  mutate(w_55_to_65_per = women_55to65_m/pop_total,
         m_55_to_65_per = men_55to65_m/pop_total,
         w_65_older_per = women_65olderstand_m/pop_total,
         m_65_older_per = men_65olderstand_m/pop_total) %>% 
  mutate(regiostar7 = if_else(regiostar7 == 71, 72, regiostar7)) %>% # merges metropolis and regiopolis
  mutate(gen_regiostar7 = with(., case_when( # classifies cities by registart7
    (regiostar7 == 72) ~ 'Large City, urban',
    (regiostar7 == 73) ~ 'Medium-sized city, urban',
    (regiostar7 == 74) ~ 'Small town, urban',
    (regiostar7 == 75) ~ 'Central City, rural',
    (regiostar7 == 76) ~ 'Medium-sized city, rural',
    (regiostar7 == 77) ~ 'Small town, rural'
  ))) %>%
  mutate(regiostar7 = as.factor(regiostar7)) %>%
  mutate(gen_regiostar7 = as.factor(gen_regiostar7)) %>%
  mutate(gen_regiostar7 = factor(gen_regiostar7, #putting factor into right order
                                 levels = c('Large City, urban',
                                            'Medium-sized city, urban',
                                            'Small town, urban',
                                            'Central City, rural',
                                            'Medium-sized city, rural', 
                                            'Small town, rural'))) %>%
  mutate(east_ger = with(., case_when( # classifies cities by registart7
    (str_detect(muni_key, "^12")) ~ 'East',
    (str_detect(muni_key, "^13")) ~ 'East',
    (str_detect(muni_key, "^14")) ~ 'East',
    (str_detect(muni_key, "^15")) ~ 'East',
    (str_detect(muni_key, "^16")) ~ 'East',
    TRUE                           ~ "West"
  )))











boxplot_data_55 <- init_calc %>%
  select(c("muni_key", "year", "gen_regiostar7", "east_ger", 
           "w_55_to_65_per", "m_55_to_65_per")) %>%
  pivot_longer(cols = -c("muni_key", "year", "gen_regiostar7", "east_ger"), #not these Cols
               names_to = c("sex", "categ"), #new cols
               names_sep = "_") %>%
  mutate(sex = as.factor(sex))




# boxplot 55 years to 65
boxplot_55 <- boxplot_data %>%
  drop_na() %>%
  filter(value < 0.05) %>%
  ggplot(aes(x=gen_regiostar7, y=value, fill = sex)) +
  scale_fill_manual(values=group.colors) + #manual coloring
  geom_boxplot(outlier.size = 1) + 
  xlab("Spatial type") +
  ylab("Share of marginal employed 55 to 65 in %") +
  theme_bw() +
  theme(axis.title = element_text(size=12),
        axis.text.x = element_text(angle = 45, hjust = 1),
        axis.text.y = element_text(),
        legend.text = element_text(),
        legend.title = element_blank(),
        axis.line = element_line(colour = "black", size = 0.5),
        panel.border = element_rect(colour = "black", fill=NA, size=0.5)) 


boxplot_55



#

boxplot_data_65 <- init_calc %>%
  select(c("muni_key", "year", "gen_regiostar7", "east_ger", 
           "w_65_older_per", "m_65_older_per")) %>%
  pivot_longer(cols = -c("muni_key", "year", "gen_regiostar7", "east_ger"), #not these Cols
               names_to = c("sex", "categ"), #new cols
               names_sep = "_") %>%
  mutate(sex = as.factor(sex))


boxplot_65 <- boxplot_data_65 %>%
  drop_na() %>%
  filter(value < 0.005) %>%
  ggplot(aes(x=gen_regiostar7, y=value, fill = sex)) +
  scale_fill_manual(values=group.colors) + #manual coloring
  geom_boxplot(outlier.size = 1) + 
  xlab("Spatial type") +
  ylab("Share of marginal employed 65 and older in %") +
  theme_bw() +
  theme(axis.title = element_text(size=12),
        axis.text.x = element_text(angle = 45, hjust = 1),
        axis.text.y = element_text(),
        legend.text = element_text(),
        legend.title = element_blank(),
        axis.line = element_line(colour = "black", size = 0.5),
        panel.border = element_rect(colour = "black", fill=NA, size=0.5)) 


boxplot_65

