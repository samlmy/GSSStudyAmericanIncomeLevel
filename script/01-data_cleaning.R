#### Preamble ####
# Purpose: Clean the survey data downloaded from https://gss.norc.org/Get-The-Data
# Author: Mingyang Li, Zecheng Wu
# Data: 20 March 2020
# Contact: wowsmyl.li@mail.utoronto.ca
# License: MIT
# Pre-requisites: 
# - Need to have downloaded the 2021 GSS data and saved it to inputs/data



#### Workspace setup ####
# Use R Projects, not setwd().
#install.packages('haven')
#install.packages('tidyverse')
#load packages
library(haven)
library(tidyverse)
library(dplyr)
# Read in the raw data. 
raw_data <- haven::read_dta("inputs/data/gss2021.dta")
# create a subset of data by selecting some interested variables
names(raw_data)
reduced_data1 <-
  raw_data %>% 
  select(sexnow1, educ, eqwlth, respineq, taxrich) %>%
  rename(gender = sexnow1) %>%
  rename(education = educ) %>%
  rename(equal_wealth = eqwlth) %>%
  rename(responsibility = respineq)

# create another subset that need further cleaning
reduced_data2 <- 
  raw_data %>% 
  select(age, 
         raceacs1,
         raceacs2,
         raceacs3,
         raceacs4,
         raceacs5,
         raceacs6,
         raceacs7,
         raceacs8,
         raceacs9,
         raceacs10,
         raceacs11,
         raceacs12,
         raceacs13,
         raceacs14,
         raceacs15,
         raceacs16) %>%
  rename(White=raceacs1, Black=raceacs2, Native = raceacs3,
         Asian_Indian = raceacs4, Chinese = raceacs5, Filipino = raceacs6, 
         Japanese = raceacs7, Korean = raceacs8, Vietnamese = raceacs9, 
         Other_Asian = raceacs10, Native_Hawaiian = raceacs11, 
         Guamanian_or_Chamorro = raceacs12, Samoan = raceacs13, 
         Other_Pacific_Islander = raceacs14,
         Other = raceacs15, Hispanic = raceacs16)

#combine all race columns into one
reduced_data2 <- cbind(reduced_data2[1], race= names(reduced_data2)[max.col(reduced_data2[-1],"first")+1])
#combine two subset of data
reduced_data <- cbind(reduced_data1, reduced_data2)
#remove unneeded dataset
rm(raw_data)
rm(reduced_data1)
rm(reduced_data2)
         

#### What's next? ####
reduced_data <- 
  reduced_data %>% 
  mutate(gender = case_when(
    gender == 1 ~ "Male",
    gender == 2 ~ "Female",
    gender == 3 ~ "Transgender",
    gender == 4 ~ "None of these",
  )) %>% 
  mutate(equal_wealth = case_when(
    equal_wealth == 1 ~ "Strongly Agree", 
    equal_wealth == 2 ~ "Very Agree", 
    equal_wealth == 3 ~ "Agree", 
    equal_wealth == 4 ~ "Neutral",
    equal_wealth == 5 ~ "Disagree",
    equal_wealth == 6 ~ "Very Disagree",
    equal_wealth == 7 ~ "Strongly Disagree"
    
  )) %>%
  mutate(responsibility = case_when(
    responsibility == 1 ~ "Private Companies", 
    responsibility == 2 ~ "Government", 
    responsibility == 3 ~ "Trade Union", 
    responsibility == 4 ~ "High-Income Individuals", 
    responsibility == 5 ~ "Low-Income Individuals", 
    responsibility == 6 ~ "No Need To Be Reduced"
    
  )) %>%
  mutate(taxrich = case_when(
    taxrich == 1 ~ "Much Too High", 
    taxrich == 2 ~ "Too High", 
    taxrich == 3 ~ "About Right", 
    taxrich == 4 ~ "Too Low", 
    taxrich == 5 ~ "Much Too Low" 
  ))

#write the reduced_data into a new dta file
write_dta(reduced_data, "inputs/data/cleaned_data.dta")




         