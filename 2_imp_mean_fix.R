




# Imputation with fixed values --------------------------------------------


try.one <- oldsvp_ao %>% 
  mutate(across(where(is.numeric), ~replace_na(.x, 1)))

try.two <- oldsvp_ao %>% 
  mutate(across(where(is.numeric), ~replace_na(.x, 2)))


# Imputation with mean values ---------------------------------------------


mean_imp_oldsvp_marg <- oldsvp_marg %>%   # filters only groups that have a complete set of years
  group_by(muni_key) %>%
  mutate(across(.cols = -c('muni_name','id', 'year'),
                .fns = 
                  ~ case_when(
                    is.na(.) ~ mean(., na.rm = T), # replaces NAs with mean of group
                    !is.na(.) ~ .))) #conditions if  not NA

