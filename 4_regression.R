
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
