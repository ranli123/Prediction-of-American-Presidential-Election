#### Preamble ####
# Purpose: Prepare and clean the survey data downloaded from [...UPDATE ME!!!!!]
# Author: Rohan Alexander and Sam Caetano [CHANGE THIS TO YOUR NAME!!!!]
# Data: 22 October 2020
# Contact: rohan.alexander@utoronto.ca [PROBABLY CHANGE THIS ALSO!!!!]
# License: MIT
# Pre-requisites: 
# - Need to have downloaded the ACS data and saved it to inputs/data
# - Don't forget to gitignore it!


#### Workspace setup ####
library(haven)
library(tidyverse)
library(datasets)
# Read in the raw data.
setwd("/Users/ranli/Desktop/STA304 PS3")
raw_data <- read_dta("post_strata.dta")
#glimpse(raw_data)

# Add the labels
raw_data <- labelled::to_factor(raw_data)

# Just keep some variables that may be of interest (change 
# this depending on your interests)
reduced_data <- 
  raw_data %>% 
  select(#region,
         stateicp,
         sex, 
         age,
         race,
         #hispan,
         #marst, 
         #bpl,
         #citizen,
         educd,
         empstat)

#### What's next? ####

## Here I am only splitting cells by age, but you 
## can use other variables to split by changing
## count(age) to count(age, sex, ....)

## Drop NA
reduced_data = reduced_data %>% drop_na()

## Add age group
# for (i in 1:nrow(reduced_data)) {
#   if (reduced_data$age[i] == "less than 1 year old"){
#     reduced_data$age[i] = 0
#   }
#   else if (reduced_data$age[i] == "90 (90+ in 1980 and 1990)"){
#     reduced_data$age[i] = 90
#   }
#   else if (reduced_data$age[i] == "100 (100+ in 1960-1970)"){
#     reduced_data$age[i] = 100
#   }
#   else if (reduced_data$age[i] == "112 (112+ in the 1980 internal data)"){
#     reduced_data$age[i] = 112
#   }
#   else if (reduced_data$age[i] == "115 (115+ in the 1990 internal data)"){
#     reduced_data$age[i] = 115
#   }
#   else{
#     reduced_data$age = as.numeric(reduced_data$age)
#   }
# }
# 


reduced_data = reduced_data[!reduced_data$age=="less than 1 year old"|
                              reduced_data$age=="90 (90+ in 1980 and 1990)"|
                              reduced_data$age=="100 (100+ in 1960-1970)"|
                              reduced_data$age=="112 (112+ in the 1980 internal data)"|
                              reduced_data$age=="115 (115+ in the 1990 internal data)",]
reduced_data$age = as.numeric(reduced_data$age)
reduced_data = reduced_data[!reduced_data$age < 18,]
age_group <- ifelse(
  reduced_data$age<=29 , "18-29",
  ifelse(
    reduced_data$age<=44, "30-44",
    ifelse(
      reduced_data$age<=64, "45-64",
      ifelse(
        reduced_data$age>=65, "65+", NA
      )
    )
  )
)
age_group
reduced_data = cbind(reduced_data, age_group)
reduced_data = reduced_data %>% drop_na()
reduced_data$age_group

reduced_data %>% count(age)
## Add state group
reduced_data = reduced_data[!(reduced_data$stateicp == "state groupings (1980 urban/rural sample)"|
reduced_data$stateicp == "military/mil. reservations"|
  reduced_data$stateicp == "state not identified"),]
levels(factor(reduced_data$stateicp))

library(stringr)
upper =str_to_title(reduced_data$stateicp)
reduced_data$stateicp = state.abb[match(upper,state.name)]
levels(as.factor(reduced_data$stateicp))
reduced_data$stateicp[is.na(reduced_data$stateicp)] <- "DC"
levels(as.factor(reduced_data$stateicp))




## Add education group
reduced_data = reduced_data[!reduced_data$educd == "n/a",]

levels(factor(reduced_data$educd))
education_group = ifelse(
  reduced_data$educd == "no schooling completed"|reduced_data$educd =="nursery school, preschool"|
    reduced_data$educd =="kindergarten"|reduced_data$educd =="grade 1"|reduced_data$educd =="grade 2"|reduced_data$educd =="grade 3", "3rd Grade or less",
  ifelse(reduced_data$educd =="grade 4"|reduced_data$educd =="grade 5"|reduced_data$educd =="grade 6"|reduced_data$educd =="grade 7"|reduced_data$educd =="grade 8",
         "Middle School - Grades 4 - 8",
         ifelse(reduced_data$educd =="grade 9"|reduced_data$educd =="grade 10"|reduced_data$educd =="grade 11"|reduced_data$educd =="12th grade, no diploma",
                "Completed some high school",
                ifelse(reduced_data$educd =="ged or alternative credential",
                       "Other post high school vocational training",
                    ifelse(reduced_data$educd =="regular high school diploma",
                       "High school graduate",
                          ifelse(reduced_data$educd =="some college, but less than 1 year"|reduced_data$educd ==
                                "1 or more years of college credit, no degree",
                              "Completed some college, but no degree",
                                ifelse(reduced_data$educd =="associate's degree, type not specified",
                                     "Associate Degree",
                                     ifelse(reduced_data$educd =="bachelor's degree",
                                            "College Degree (such as B.A., B.S.)",
                                            ifelse(reduced_data$educd =="master's degree"|reduced_data$educd =="professional degree beyond a bachelor's degree",
                                                   "Masters degree",
                                                   ifelse(reduced_data$educd =="doctoral degree",
                                                          "Doctorate degree", NA
                                                   ))))))))))
  
reduced_data = cbind(reduced_data, education_group)
reduced_data$education_group


## Add race group
levels(factor(reduced_data$race))
reduced_data$race
reduced_data = reduced_data[!reduced_data$race == "two major races",]
reduced_data = reduced_data[!reduced_data$race == "three or more major races",]

race_group = ifelse(reduced_data$race == "white", "White",
                    ifelse(reduced_data$race =="black/african american/negro",
                           "Black, or African American",
                           ifelse(reduced_data$race == "american indian or alaska native",
                                  "American Indian or Alaska Native",
                                  ifelse(reduced_data$race == "chinese",
                                         "Asian (Chinese)",
                                         ifelse(reduced_data$race == "japanese",
                                                "Asian (Japanese)",
                                                ifelse(reduced_data$race == "other asian or pacific islander",
                                                       "other asian or pacific islander",
                                                       ifelse(reduced_data$race == "other race, nec",
                                                              "Some other race",NA)))))))

race_group
reduced_data = cbind(reduced_data, race_group)
## Group Employment data
## levels(reduced_data$empstat)
##reduced_data = reduced_data[!reduced_data$empstat == "n/a",]

## Produce the final data cells

data <- 
  reduced_data %>%
  count(age_group, sex, stateicp, education_group, race_group, empstat) %>% group_by(age_group, sex, stateicp,education_group, race_group, empstat)
data
#reduced_data$age <- as.integer(reduced_data$age)

# Saving the census data as a csv file in my
# working directory
write_csv(data, "census_data.csv")

#read_csv("census_data.csv")


         