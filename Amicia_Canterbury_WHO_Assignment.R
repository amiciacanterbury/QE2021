# Author: Amicia Canterbury
# Date: 20 July 2021
# Quantitative Ecology
# Topic 8: WHO SGD Assignment 

# The collective datasets below deals with the way in which the World Health 
# Organisation (WHO) interprets the data on the progress towards achieving the 
# Sustainable Development Goals (SDGs). An agenda was adopted for the sustainable 
# development and in order to list 17 development goals that might be achieved by
# 2030. WHO collects the data and organizes it into a collection of indicators to 
# track the progress of the countries. The main goal is to attain 
# "A world free of poverty, hunger, disease and want" - WHO 

# Load packages:----------------------------------------------------------------
library(tidyverse)
library(vegan)
library(missMDA) # to impute missing values
library(ggcorrplot) # for the correlations

# Question 1: Code interpretations----------------------------------------------

# Load data:
# SDG 1.a Domestic general government health expenditure (GGHE-D) as percentage 
# of general government expenditure (GGE) (%):
SDG1.a <- read.csv("Quantitative_Ecology-main/exercises/WHO/WHO_SDG1.a_domestic_health_expenditure.csv") %>%
  filter(Period == 2016) %>%
  select(Indicator, ParentLocation, Location, FactValueNumeric) %>%
  mutate(SDG = "SDG1.a")

# Line 25: Loads dataset from folder on computer.
# Line 26: Filters data to keep all the data from the year 2016.
# Line 27: Select the variables named above.
# Line 28: Create a new column called SDG1.a with the variables that were 
# selected in line 27.
# The dataset is altered for our benefit and looks only at the data we want to 
# view. 

# SDG 3.1 Maternal mortality ratio (per 100 000 live births)):
SDG3.1_1 <- read.csv("Quantitative_Ecology-main/exercises/WHO/WHO_SDG3.1_maternal_mort.csv") %>%
  filter(Period == 2016,
         Indicator == "Maternal mortality ratio (per 100 000 live births)") %>%
  select(Indicator, ParentLocation, Location, FactValueNumeric) %>%
  mutate(SDG = "SDG3.1_1")

# Line 40 & 41: Filter out the rows found in the Indicator variable.
# Line 42: Select the variables named above.
# Line 43: Create a new column called SDG3.1_1 with the variables that were 
# selected in line 42.
# The new dataset looks at the variables for the Maternal mortality for the 
# year 2016.

# SDG 3.1 Births attended by skilled health personnel (%)):
SDG3.1_2 <- read.csv("Quantitative_Ecology-main/exercises/WHO/WHO_SDG3.1_skilled_births.csv") %>%
  filter(Period == 2016) %>%
  select(Indicator, ParentLocation, Location, FactValueNumeric) %>%
  mutate(SDG = "SDG3.1_2")

# Line 53-55: Same as previous explanations.
# Line 56: Create a new column named SDG3.2_1 with variables selected in line 55.
# Variables selected will only be in the newly named dataset.

# SDG 3.2 Number of neonatal deaths (Child mortality):
SDG3.2_1 <- read.csv("Quantitative_Ecology-main/exercises/WHO/WHO_SDG3.2_neonatal_deaths.csv") %>%
  filter(Period == 2016,
         Dim1 == "Both sexes") %>%
  select(Indicator, ParentLocation, Location, FactValueNumeric) %>%
  mutate(SDG = "SDG3.2_1")

# Line 64-65: Filter out the data and keep the rows that only contain "Both sexes",
# from the Dim1 variable.
# Line 67: Create a new column called SDG3.2_1 that collects the data 
# that was selected in line 66. The new dataset will only show the information
# of "Both sexes" in the year 2016.

# SDG 3.2 Number of under-five deaths (Child mortality):
SDG3.2_2 <- read.csv("Quantitative_Ecology-main/exercises/WHO/WHO_SDG3.2_under_5_deaths.csv") %>%
  filter(Period == 2016,
         Dim1 == "Both sexes") %>%
  select(Indicator, ParentLocation, Location, FactValueNumeric) %>%
  mutate(SDG = "SDG3.2_2")

# Line 77-78: Filter out the data and keep the rows that only contain "Both sexes",
# from the Dim1 variable.
# Line 80: Create a new column called SDG3.2_2 that collects the data that was 
# selected in line 79. The new dataset will only show the information 
# of "Both sexes" in the year 2016.

# SDG 3.2 Number of infant deaths (Child mortality):
SDG3.2_3 <- read.csv("Quantitative_Ecology-main/exercises/WHO/WHO_SDG3.2_infant_deaths.csv") %>%
  filter(Period == 2016,
         Dim1 == "Both sexes") %>%
  select(Indicator, ParentLocation, Location, FactValueNumeric) %>%
  mutate(SDG = "SDG3.2_3")

# Line 89-92: Same as the SDG3.2_2 column.
# Line 93: Creates a new column called SDG3.2_3 that collects the data that was 
# selected in line 92. The new dataset will only show the variables that was 
# selected in the new column. It will look at the infant deaths.

# SDG 3.3 New HIV infections (per 1000 uninfected population)):
SDG3.3_1 <- read.csv("Quantitative_Ecology-main/exercises/WHO/WHO_SDG3.3_new_HIV_infections.csv") %>%
  filter(Period == 2015,
         Dim1 == "Both sexes") %>%
  select(Indicator, ParentLocation, Location, FactValueNumeric) %>%
  mutate(SDG = "SDG3.3_1")

# Line 101 - 104: Same as before 
# Line 105: Creates a new column called the SDG3.3_1 that collects the data that 
# as selected in line 104. The new dataset will only show the variables that was
# selected in the new column. The new data will look at the # of new infections 
# of HIV found in both sexes in the year 2016.

# SDG 3.3 Incidence of tuberculosis (per 100 000 population per year)):
SDG3.3_2 <- read.csv("Quantitative_Ecology-main/exercises/WHO/WHO_SDG3.3_TB.csv") %>%
  filter(Period == 2016) %>%
  select(Indicator, ParentLocation, Location, FactValueNumeric) %>%
  mutate(SDG = "SDG3.3_2")

# Line 114 - 116: Same as before.
# Line 115: Creates a new column that collects the data that was selected in 
# line 116 with the specific variables. The new column is called SGD3.3_2. 
# The new dataset looks at the Incidence of TB (per 100 000 population/year), in
# the year 2016.

# SDG 3.3 Malaria incidence (per 1 000 population at risk)):
SDG3.3_3 <- read.csv("Quantitative_Ecology-main/exercises/WHO/WHO_SDG3.3_malaria.csv") %>%
  filter(Period == 2016) %>%
  select(Indicator, ParentLocation, Location, FactValueNumeric) %>%
  mutate(SDG = "SDG3.3_3")

# Line 126 - 128: Same as before.
# Line 129: Creates a new column that collects the data that was selected in line 
# 128 with the  specific variables. The new column is called SDG 3.3_3 and 
# looks at the malaria incidence (per 1000 population at risj), in 2016.

# SDG 3.3 Hepatitis B surface antigen (HBsAg) prevalence among children under
# 5 years-prevalence-among-children-under-5-years):
SDG3.3_4 <- read.csv("Quantitative_Ecology-main/exercises/WHO/WHO_SDG3.3_hepatitis_B.csv") %>%
  filter(Period == 2015) %>%
  select(Indicator, ParentLocation, Location, FactValueNumeric) %>%
  mutate(SDG = "SDG3.3_4")

# Line 138 - 140: Same as before.
# Line 141: Creates a new column that collects the data that was selected in line 
# 140 with the  specific variables. The new column is called SDG 3.3_4 and 
# looks at the Hepatitis B surface antigen prevalence 
# among children under 5 years old, in the year 2015.

# SDG 3.3 Reported number of people requiring interventions against NTDs:
SDG3.3_5 <- read.csv("Quantitative_Ecology-main/exercises/WHO/WHO_SDG3.3_NCD_interventions.csv") %>%
  filter(Period == 2016) %>%
  select(Indicator, ParentLocation, Location, FactValueNumeric) %>%
  mutate(SDG = "SDG3.3_5")

# Line 150 - 152: Same as before.
# Line 153: Creates a new column that collects the data that was selected in line 
# 152 with the  specific variables. The new column is called SDG 3.3_5 and 
# looks at the reported # people requiring interventions against NTDs in the 
# year 2016.

# SDG 3.4 Adult mortality rate (probability of dying between 15 and 60 years per 1000 population)):
SDG3.4_1 <- read.csv("Quantitative_Ecology-main/exercises/WHO/WHO_SDG3.4_adult_death_prob.csv") %>%
  filter(Period == 2016,
         Dim1 == "Both sexes") %>%
  select(Indicator, ParentLocation, Location, FactValueNumeric) %>%
  mutate(SDG = "SDG3.4_1")

# Line 162 - 165: Same as before.
# Line 153: Creates a new column that collects the data that was selected in line
# 165 with the specific variables. The new column is called SDG3.4_1 and looks at
# Adult mortality rate in both sexes in the year 2016.

# SDG 3.4 Number of deaths attributed to non-communicable diseases, by type of disease and sex:
SDG3.4_2 <- read.csv("Quantitative_Ecology-main/exercises/WHO/WHO_SDG3.4_NCD_by_cause.csv") %>%
  filter(Period == 2016,
         Dim1 == "Both sexes",
         Dim2 == "Diabetes mellitus") %>%
  mutate(Indicator = Dim2) %>%
  select(Indicator, ParentLocation, Location, FactValueNumeric) %>%
  mutate(SDG = "SDG3.4_2")

# Line 175 - 177: Filters out the rows in the variables Dim1 that contains both
# sexes. Filters out the rows in the variable Dim2 that contains the information
# about Diabetes mellitus. 
# Line 178: A new column is created called Indicator that contains all information
# of Dim2.
# Line 180: Creates a new column that collects the data that was selected in line
# 179 with the specific variables. The new column is called SDG3.4_2 and looks at
# # of deaths attributed to diabetes mellitus in both sexes, in the year 2016.

SDG3.4_3 <- read.csv("Quantitative_Ecology-main/exercises/WHO/WHO_SDG3.4_NCD_by_cause.csv") %>%
  filter(Period == 2016,
         Dim1 == "Both sexes",
         Dim2 == "Cardiovascular diseases") %>%
  mutate(Indicator = Dim2) %>%
  select(Indicator, ParentLocation, Location, FactValueNumeric) %>%
  mutate(SDG = "SDG3.4_3")

# Line 192 - 194: Filters out the rows in the variables Dim1 that contains both
# sexes. Filters out the rows in the variable Dim2 that contains the information
# about Cardiovascular diseases. 
# Line 195: A new column is created called Indicator that contains all information
# of Dim2.
# Line 197: Creates a new column that collects the data that was selected in line
# 196 with the specific variables. The new column is called SDG3.4_3 and looks at
# # of deaths attributed to Cardiovascular diseases in both sexes, in the year 2016.

SDG3.4_4 <- read.csv("Quantitative_Ecology-main/exercises/WHO/WHO_SDG3.4_NCD_by_cause.csv") %>%
  filter(Period == 2016,
         Dim1 == "Both sexes",
         Dim2 == "Respiratory diseases") %>%
  mutate(Indicator = Dim2) %>%
  select(Indicator, ParentLocation, Location, FactValueNumeric) %>%
  mutate(SDG = "SDG3.4_4")

# Line 209-211 : Filters out the rows in the variables Dim1 that contains both
# sexes. Filters out the rows in the variable Dim2 that contains the information
# about Respiratory diseases. 
# Line 212: A new column is created called Indicator that contains all information
# of Dim2.
# Line 214: Creates a new column that collects the data that was selected in line
# 213 with the specific variables. The new column is called SDG3.4_4 and looks at
# # of deaths attributed to Respiratory diseases in both sexes, in the year 2016.

# SDG 3.4 Crude suicide rates (per 100 000 population) (SDG 3.4.2)):
SDG3.4_5 <- read.csv("Quantitative_Ecology-main/exercises/WHO/WHO_SDG3.4_suicides.csv") %>%
  filter(Period == 2016,
         Dim1 == "Both sexes") %>%
  select(Indicator, ParentLocation, Location, FactValueNumeric) %>%
  mutate(SDG = "SDG3.4_5")

# Line 226 -229: Same as before.
# Line 230: Creates a new column that collects the data that was selected in line
# 229 with the specific variables. The new column is called SDG2.4_5 and looks at 
# the Crude suicide rates (per 100 000 population) in both sexes in the year 
# 2016. 

# SDG3.4 Total NCD Deaths (in thousands);
SDG3.4_6 <- read.csv("Quantitative_Ecology-main/exercises/WHO/WHO_SDG3.4_NCD_data_total.csv") %>%
  filter(Period == 2016,
         Dim1 == "Both sexes") %>%
  select(Indicator, ParentLocation, Location, FactValueNumeric) %>%
  mutate(SDG = "SDG3.4_6")

# Line 239 - 241: Same as before.
# Line 230: Creates a new column that collects the data that was selected in line
# 242  with the specific variables. The new column is called SDG2.4_6 and looks at
# the Total NCD deaths (in thousands) in both sexes in the year 2016.

# SDG 3.5 Alcohol, total per capita (15+) consumption (in litres of pure alcohol) 
# (SDG Indicator 3.5.2)-alcohol-per-capita-(15-)-consumption):
SDG3.5 <- read.csv("Quantitative_Ecology-main/exercises/WHO/WHO_SDG3.5_alcohol_consumption.csv") %>%
  filter(Period == 2015,
         Dim1 == "Both sexes") %>%
  select(Indicator, ParentLocation, Location, FactValueNumeric) %>%
  mutate(SDG = "SDG3.5")

# Line 252 - 254: Same as before.
# Line 256: Creates a new column that collects the data that was selected in line
# 255  with the specific variables. The new column is called SDG3.5 and looks at
# the Total amount of alcohol, per capita consumption in litres of pure alcohol
# in both sexes in the year 2015.

# SDG 3.6 Estimated road traffic death rate (per 100 000 population)):
SDG3.6 <- read.csv("Quantitative_Ecology-main/exercises/WHO/WHO_SDG3.6_traffic_deaths_prop.csv") %>%
  filter(Period == 2016,
         Dim1 == "Both sexes") %>%
  select(Indicator, ParentLocation, Location, FactValueNumeric) %>%
  mutate(SDG = "SDG3.6")

# Line 265 - 267: Same as before.
# Line 269: Creates a new column that collects the data that was selected in line
# 268  with the specific variables. The new column is called SDG3.6 and looks at
# the Estimated road traffic death rate ( per 100 000 pop.), in both sexes in the 
# year 2016.

# SDG 3.7 Adolescent birth rate (per 1000 women aged 15-19 years)):
SDG3.7 <- read.csv("Quantitative_Ecology-main/exercises/WHO/WHO_SDG3.7_adolescent_births.csv") %>%
  filter(Period == 2016) %>%
  select(Indicator, ParentLocation, Location, FactValueNumeric) %>%
  mutate(SDG = "SDG3.7")

# Line 278-280: Same as before.
# Line 281: Creates a new column that collects the data that was selected in line
# 280  with the specific variables. The new column is called SDG3.7 and looks at
# the Adolescent birth rate (per 1000 women aged 15 -19 years), in the year 2016.

# SDG 3.8 UHC Index of service coverage (SCI):
SDG3.8_1 <- read.csv("Quantitative_Ecology-main/exercises/WHO/WHO_SDG3.8_UHC_data_availability.csv") %>%
  filter(Period == "2013-2017") %>%
  select(Indicator, ParentLocation, Location, FactValueNumeric) %>%
  mutate(SDG = "SDG3.8_1")

# Line 289: Same as before.
# Line 290: Filtering out the data in years 2013 to 2017
# Line 292: Creates a new column that collects the data that was selected in line
# 291 with the specific variables. The new column is called SDG3.8_1 and looks at
# the UHC Index of service coverage, in the years 2013 -2017.

# SDG 3.8 Data availability for UHC index of essential service coverage (%)):
SDG3.8_2 <- read.csv("Quantitative_Ecology-main/exercises/WHO/WHO_SDG3.8_UHC_index_of_service_coverage.csv") %>%
  filter(Period == 2017) %>%
  select(Indicator, ParentLocation, Location, FactValueNumeric) %>%
  mutate(SDG = "SDG3.8_2")

# Line 301 - 303: Same as before.
# Line 304: Creates a new column that collects the data that was selected in line
# 303 with the specific variables. The new column is called SDG3.8_2 and looks at
# the Data availavility for UHC Index of essential service coverage 
# in percentage in the year 2017. 

# SDG 3.9 Poison control and unintentional poisoning:
SDG3.9_1 <- read.csv("Quantitative_Ecology-main/exercises/WHO/WHO_SDG3.9_unintentional_poisoning_prop.csv") %>%
  filter(Period == 2016,
         Dim1 == "Both sexes") %>%
  select(Indicator, ParentLocation, Location, FactValueNumeric) %>%
  mutate(SDG = "SDG3.9_1")

# Line 313 - 316: Same as before.
# Line 317: Creates a new column that collects the data that was selected in line
# 316 with the specific variables. The new column is called SDG3.9_1 and looks at
# the Poison control and unintentional poisoning, in both sexes in the year 2016.

# SDG 3.9 Mortality rate attributed to exposure to unsafe WASH services 
# (per 100 000 population) (SDG 3.9.2)-(sdg-3-9-2)):
SDG3.9_3 <- read.csv("Quantitative_Ecology-main/exercises/WHO/WHO_SDG3.9_WASH_mortalities.csv") %>%
  filter(Period == 2016,
         Dim1 == "Both sexes") %>%
  select(Indicator, ParentLocation, Location, FactValueNumeric) %>%
  mutate(SDG = "SDG3.9_3")

# Line 326 -329:same as before 
# Line 330: Creates a new column that collects the data that was selected in line
# 329 with the specific variables. The new column is called SDG3.9_3 and looks at
# the Mortality rate attributed to exposure to unsafe WASH services (per 100 000 
# population), in both sexes in the year 2016.

# SDG 16.1 Estimates of rate of homicides (per 100 000 population):
SDG16.1 <- read.csv("Quantitative_Ecology-main/exercises/WHO/WHO_SDG16.1_homicides.csv") %>%
  filter(Period == 2016,
         Dim1 == "Both sexes") %>%
  select(Indicator, ParentLocation, Location, FactValueNumeric) %>%
  mutate(SDG = "SDG16.1")

# Line 339 -342: same as before 
# Line 343: Creates a new column that collects the data that was selected in line
# 342 with the specific variables. The new column is called SDG16.1 and looks at
# the estimates of rate of homicides (per 100 000 population), in both sexes in the 
# year 2016.

# SDG 3.a Prevalence of current tobacco use among persons aged 15 years and 
# older (age-standardized rate):
SDG3.a <- read.csv("Quantitative_Ecology-main/exercises/WHO/WHO_SDG3.a_tobacco_control.csv") %>%
  filter(Period == 2016,
         Dim1 == "Both sexes") %>%
  select(Indicator, ParentLocation, Location, FactValueNumeric) %>%
  mutate(SDG = "SDG3.a")

# Line 353 - 356:same as before 
# Line 357: Creates a new column that collects the data that was selected in line
# 356 with the specific variables. The new column is called SDG3.a and looks at
# the Prevalence of current tobacco use among persons aged 15 years and 
# older (age-standardized rate), in both sexes in the year 2016.

# SDG 3.b Total net official development assistance to medical research and 
# basic health sectors per capita (US$), by recipient country:
SDG3.b_1 <- read.csv("Quantitative_Ecology-main/exercises/WHO/WHO_SDG3.b_dev_assistence_for_med_research.csv") %>%
  filter(Period == 2016) %>%
  select(Indicator, ParentLocation, Location, FactValueNumeric) %>%
  mutate(SDG = "SDG3.b_1")

# Line 367 -369: same as before 
# Line 370: Creates a new column that collects the data that was selected in line
# 369 with the specific variables. The new column is called SDG3.b_1 and looks at
# the Total net official development assistance to medical research and 
# basic health sectors per capita (US$), by recipient country in the year 2016.

# SDG 3.b Measles-containing-vaccine second-dose (MCV2) immunization coverage
# by the nationally recommended age (%)-immunization-coverage-by-the-nationally-recommended-age-(-)):
SDG3.b_2 <- read.csv("Quantitative_Ecology-main/exercises/WHO/WHO_SDG3.b_measles_vaccine.csv") %>%
  filter(Period == 2016) %>%
  select(Indicator, ParentLocation, Location, FactValueNumeric) %>%
  mutate(SDG = "SDG3.b_2")

# Line 380 - 382: same as before 
# Line 383: Creates a new column that collects the data that was selected in line
# 382 with the specific variables. The new column is called SDG3.b_2 and looks at
# the Measles-containing-vaccine second-dose (MCV2) immunization coverage
# by the nationally recommended age in the year 2016.

# SDG 3.b Diphtheria tetanus toxoid and pertussis (DTP3) immunization coverage 
# among 1-year-olds (%)-immunization-coverage-among-1-year-olds-(-)):
SDG3.b_3 <- read.csv("Quantitative_Ecology-main/exercises/WHO/WHO_SDG3.b_diphtheria_vaccine.csv") %>%
  filter(Period == 2016) %>%
  select(Indicator, ParentLocation, Location, FactValueNumeric) %>%
  mutate(SDG = "SDG3.b_3")

# Line 393 - 395: same as before 
# Line 396: Creates a new column that collects the data that was selected in line
# 395 with the specific variables. The new column is called SDG3.b_3 and looks at
# iphtheria tetanus toxoid and pertussis (DTP3) immunization coverage 
# among 1-year-olds in the year 2016.

# SDG 3.b Pneumococcal conjugate vaccines (PCV3) immunization coverage among 
# 1-year-olds (%)-immunization-coverage-among-1-year-olds-(-)):
SDG3.b_4 <- read.csv("Quantitative_Ecology-main/exercises/WHO/WHO_SDG3.b_pneumococcal_vaccine.csv") %>%
  filter(Period == 2016) %>%
  select(Indicator, ParentLocation, Location, FactValueNumeric) %>%
  mutate(SDG = "SDG3.b_4")

# Line 406 -408: same as before 
# Line 409: Creates a new column that collects the data that was selected in line
# 408 with the specific variables. The new column is called SDG3.b_4 and looks at
# Pneumococcal conjugate vaccines (PCV3) immunization coverage among 
# 1-year-olds 

# SDG 3.b Girls aged 15 years old that received the recommended doses of HPV vaccine:
SDG3.b_5 <- read.csv("Quantitative_Ecology-main/exercises/WHO/WHO_SDG3.b_HPV_vaccine.csv") %>%
  filter(Period == 2016) %>%
  select(Indicator, ParentLocation, Location, FactValueNumeric) %>%
  mutate(SDG = "SDG3.b_5")

# Line 418 - 420: same as before 
# Line 421: Creates a new column that collects the data that was selected in line
# 420 with the specific variables. The new column is called SDG3.b_5 and looks at
# Girls aged 15 years old that received the recommended doses of HPV vaccine, in 
# the year 2016.

# SDG 3.c SDG Target 3.c | Health workforce: Substantially increase health 
# financing and the recruitment, development, training and retention of the 
# health workforce in developing countries, especially in least developed
# countries and small island developing States:
SDG3.c_1 <- read.csv("Quantitative_Ecology-main/exercises/WHO/WHO_SDG3.c_health_workforce.csv")  %>%
  filter(Period == 2016,
         Indicator == "Medical doctors (per 10,000)") %>%
  select(Indicator, ParentLocation, Location, FactValueNumeric) %>%
  mutate(SDG = "SDG3.c_1")

# Line 433 -434: same as before 
# Line 435: Filters out the rows in the variable Indicator that includes "".
# Line 437: Creates a new column that collects the data that was selected in line
# 436 with the specific variables. The new column is called SDG3.c_1 and looks at
# Medical doctors in the health workforce in the year 2016.

SDG3.c_2 <- read.csv("Quantitative_Ecology-main/exercises/WHO/WHO_SDG3.c_health_workforce.csv")  %>%
  filter(Period == 2016,
         Indicator == "Nursing and midwifery personnel (per 10,000)") %>%
  select(Indicator, ParentLocation, Location, FactValueNumeric) %>%
  mutate(SDG = "SDG3.c_2")

# Line 445 - 447: same as before 
# Line 447: Filters out the rows in the variable Indicator that includes "".
# Line 448: Creates a new column that collects the data that was selected in line
# 447 with the specific variables. The new column is called SDG3.c_2 and looks at
# Nursing and midwifery personnel in the health workforce in the year 2016.

SDG3.c_3 <- read.csv("Quantitative_Ecology-main/exercises/WHO/WHO_SDG3.c_health_workforce.csv")  %>%
  filter(Period == 2016,
         Indicator == "Dentists (per 10,000)") %>%
  select(Indicator, ParentLocation, Location, FactValueNumeric) %>%
  mutate(SDG = "SDG3.c_3")

# Line 457-459: same as before 
# Line 459: Filters out the rows in the variable Indicator that includes "".
# Line 461: Creates a new column that collects the data that was selected in line
# 460 with the specific variables. The new column is called SDG3.c_3 and looks at
# Dentists in the health workforce in the year 2016.

SDG3.c_4 <- read.csv("Quantitative_Ecology-main/exercises/WHO/WHO_SDG3.c_health_workforce.csv")  %>%
  filter(Period == 2016,
         Indicator == "Pharmacists  (per 10,000)") %>%
  select(Indicator, ParentLocation, Location, FactValueNumeric) %>%
  mutate(SDG = "SDG3.c_4")

# Line 469 - 472: same as before 
# Line 471: Filters out the rows in the variable Indicator that includes "".
# Line 473: Creates a new column that collects the data that was selected in line
# 472 with the specific variables. The new column is called SDG3.c_4 and looks at
# Pharmacists in the health workforce in the year 2016.

# SDG 3.d Average of 13 International Health Regulations core capacity scores, 
# SPAR version:
SDG3.d_1 <- read.csv("Quantitative_Ecology-main/exercises/WHO/WHO_SDG3.d_health_risks.csv")  %>%
  filter(Period == 2016) %>%
  select(Indicator, ParentLocation, Location, FactValueNumeric) %>%
  mutate(SDG = "SDG3.d_1")

# Line 483 - 485: same as before 
# Line 486: Creates a new column that collects the data that was selected in line
# 485 with the specific variables. The new column is called SDG3.d_1 and looks at
# Average of 13 International Health Regulations core capacity scores, 
# SPAR version, in the year 2016.

# Other Life expectancy at birth (years)):
other_1 <- read.csv("Quantitative_Ecology-main/exercises/WHO/WHO_Other_life_expectancy.csv") %>%
  filter(Period == 2015,
         Dim1 == "Both sexes",
         Indicator == "Life expectancy at birth (years)") %>%
  select(Indicator, ParentLocation, Location, FactValueNumeric) %>%
  mutate(SDG = "other_1")

# Line 495 - 499: same as before 
# Line 500: Creates a new column that collects the data that was selected in line
# 499 with the specific variables. The new column is called other_1 and looks at
# Other life expectancy at birth(years), in both sexes in the year 2015.

# Other Life expectancy at age 60 (years)):
other_2 <- read.csv("Quantitative_Ecology-main/exercises/WHO/WHO_Other_life_expectancy.csv") %>%
  filter(Period == 2015,
         Dim1 == "Both sexes",
         Indicator == "Life expectancy at age 60 (years)") %>%
  select(Indicator, ParentLocation, Location, FactValueNumeric) %>%
  mutate(SDG = "other_2")

# Line 508 -512: same as before 
# Line 513: Creates a new column that collects the data that was selected in line
# 512 with the specific variables. The new column is called other_2 and looks at
# Other life expectancy at age 60(years), in both sexes in the year 2015.

# rbind the data:---------------------------------------------------------------

health <- do.call("rbind", lapply(ls(),get)) # Do.call carries out a function call 
# from a function or list of arguments given to it.
# rbind: a method that combines the rows into similar columns.
# All the data will be combined and sorted based on the shares in the columns.
head(health) # Looks at the first 6 rows and columns of the dataset. 

# Create list of SDGs used:-----------------------------------------------------
unique(health[, c(5, 1)]) # Unique() removes the duplicates in the dataset.
# Only information in column 5 and 1 must be kept, in order to create a list of 
# SDGs. 

# Pivot wider:------------------------------------------------------------------
health_wide <- health %>%
  arrange(Location) %>% # Arrange the data based on the location, alphabetically.
  select(-Indicator) %>% # Removes the column called 'Indicator'
  pivot_wider(names_from = SDG, values_from = FactValueNumeric) %>% # pivot_wider:
  # increases the number of columns and simultaneously reduces the number of rows, 
  # making the data 'wider'. Information is separated from one column to a new column,
  # to show all the hidden numerical values. The names from SDG are extracted and made 
  # into columns, and their corresponding values for FactValueNumeric are placed accordingly.
  as_tibble() # dataset is made into a table.


# Add world population data:----------------------------------------------------
popl <- read_csv("Quantitative_Ecology-main/exercises/WHO/WHO_population.csv") %>%
  filter(Year == 2016) %>%
  rename(popl_size = `Population (in thousands) total`, # Renames column `Population (in thousand) total` to popl_size.
         Location = Country) %>% # Renames column Country to Location
  select(Location, popl_size) %>% # Selects only the columns Location and popl_siza
  mutate(popl_size = as.numeric(gsub("[[:space:]]", "", popl_size)) * 1000)
 # Creates a new column that takes the popl_size column and makes it a numerical value.

health_wide <- health_wide %>% # Joins the data from each population per country 
  # to the health_wide data
  left_join(popl) # Data sets are joined based on the 'Location' column 

# Express some variables to unit of population size:----------------------------
health_wide <- health_wide %>%
  mutate(SDG3.4_4 = SDG3.4_4 / popl_size * 100000,
         SDG3.4_3 = SDG3.4_3 / popl_size * 100000,
         SDG3.4_2 = SDG3.4_2 / popl_size * 100000,
         SDG3.4_6 = SDG3.4_6 / 100,
         SDG3.2_2 = SDG3.2_2 / popl_size * 100000,
         SDG3.2_3 = SDG3.2_3 / popl_size * 100000,
         SDG3.2_1 = SDG3.2_1 / popl_size * 100000)

# Some of the columns are specifically standardized so that all the variables are 
# expressed as a unit of the population size.

# Histograms of missing values, and correlations:-------------------------------

# calculate histograms
health_wide$na_count <- apply(health_wide[, 3:(ncol(health_wide) - 1)], 1, function(x) sum(is.na(x)))
hist(health_wide$na_count, breaks = 14, plot = TRUE)

# A histogram is created that compares the frequency of the NA values, at equal frequencies,
# separated into 14 bins.
# When using the 

# remove rows where there are more than 10 NAs
health_wide <- health_wide %>%
  filter(na_count <= 10) %>%
  select(-na_count)

# calculate pairwise correlations
corr <- round(cor(health_wide[, 3:(ncol(health_wide) - 1)]), 1) 

# visualization of the correlation matrix
ggcorrplot(corr, type = 'upper', outline.col = "grey60", # Only the upper triangle will 
           # be shown above the 0-diagonal line. The value outlines are in grey60.
           colors = c("#1679a1", "white", "#f8766d"), # Colours used for the gradient.
           lab = TRUE) # lab = TRUE means that the labels must be shown.

# Impute remaining NAs:---------------------------------------------------------
health_wide_complete <- imputePCA(health_wide[, 3:(ncol(health_wide) - 1)])$completeObs
# The missing values are shown in the dataset, in order to do the PCA.

# Scale and center the data and do the PCA:-------------------------------------
health_wide_complete_std <- decostand(health_wide_complete, method = "standardize") # data is standardized
health_pca <- rda(health_wide_complete_std) # the PCA is worked out using the rda() function
health_pca # Eigenvalues are seen using this method. 

summary(health_pca) # A more robust summary is seen using the summary() function.
# One can see the eigenvalues, species scores and site scores. The importance component
# can be viewed using the above function. The variation explained can also be seen
# in the summary. 

# Graphical displays:-----------------------------------------------------------
biplot(health_pca, scaling = 1, main = "PCA scaling 1", choices = c(1, 2))
biplot(health_pca, scaling = 2, main = "PCA scaling 2", choices = c(1, 2))

# Biplot with scaling 1: Shows the relationship between sites.
# Biplot with scaling 2: Shows the relationship between the species.
# Usually PCA1 and PCA2 is used as these two eigenvalues have the bulk of the variation
# explained compared to the rest of the components. 

# Assemble the ordination plot using the vegan component functions:
pl1 <- ordiplot(health_pca, type = "none", scaling = 1, main = "PCA WHO/SDG") 
points(pl1, "sites", pch = 21, cex = 1.0, col = "grey20", bg = "grey80")
points(pl1, "species", pch = 21, col = "turquoise", arrows = TRUE)
text(pl1, "species", col = "blue4", cex = 0.9)

# Line 619: Renaming the ordiplot to p11 and using scaling method 1, where main = TITLE.
# Line 620: Sites are given aesthetics that are represented as points on the plot.
# Line 621: Points are now given the arrow aesthetic. 
# Line 622: The text is given specific aesthetics.

pl2 <- ordiplot(health_pca, type = "none", scaling = 2, main = "PCA WHO/SDG")
points(pl2, "sites", pch = 21, cex = 1.75, col = "grey80", bg = "grey80")
points(pl2, "species", pch = 21, col = "turquoise", arrows = TRUE)
text(pl2, "species", col = "blue4", cex = 0.9)
text(pl2, "sites", col = "red4", cex = 0.9)

# Same as above just using scaling method 2.

# Another way to make an ordination plot:
site_scores <- tibble(ParentLocation = health_wide$ParentLocation,
                      Location = health_wide$Location) 
site_scores <- tibble(cbind(site_scores, scores(health_pca, display = "sites", choices = c(1:7))))
species_scores <- data.frame(scores(health_pca, display = "species", choices = c(1:7)))
species_scores$species <- rownames(species_scores)
species_scores <- tibble(species_scores)

# Line 638 - 639: Site scores are made into tables with the specific variables. The columns
# called ParentLocation and Location is added to the table from the health_wide dataset.
# Line 640: The site scores are added to the first 7 PCAs in the health_pca dataset.
# Line 642: The column is renamed to species_scores.
# Line 643: The table that was created is formatted into a table. 

ggplot(data = site_scores, aes(x = PC1, y = PC2)) + # Data from the sites_scores is used.
  # Axis is created based on the data set. 
  geom_point(aes(col = ParentLocation)) + # Points are added according to the ParentLocation & colour.
  geom_segment(data = species_scores,  # Geom_segment creates a straight line according to x and y.
               aes(x = 0, y = 0, xend = PC1, yend = PC2), # Line is drawn from PC1 to PC2
               arrow = arrow(length = unit(0.4, "cm"), type = "closed"),# arrow aesthetics.
               color = "lightseagreen", alpha = 1, size = 0.3) + # Colour, size and transparency of arrow.
  geom_text(data = species_scores, # edit and add text in the plot.
            aes(x = PC1, y = PC2, label = species), # X = PC1 and y = PC2 according to the species
            color = "black") + # the colour of the text is black.
  xlab("PC1") + ylab("PC2") + # The x and y axis is given titles found in "".
  ggtitle("WHO SDGs, Scaling 2") # The title of the complete plot is given. 

# QUESTIONS 2 - 3: -------------------------------------------------------------

# Question 2: 
# Biplot scaling 1 shows the most variation amongst the plots. It shows the most 
# explained variation between PC1 and PC2. This plot focuses on the sites of the 
# dataset. The points are all clustered together towards the origin of the plot. 
# The reason why the sites are so close to one another is because they all use the 
# same method in order to obtain the data for the SDGs. The sites that are not as 
# close to the origin as the others are slightly different due to information that
# was used to form the SDGs. The way in which a country/continent obtains the information
# to create SDGs is dependent on the availability of the data. Major variables between the 
# data may be due to the GDP and the standard of living found across the world. 
# When looking at the SDGs, the financial status of the country and the socio-economic
# influences plays a role in the variations found in the variation caused between the 
# sites. There is trend of poorer to more stable countries from the left to the 
# right side of the plot. 

# Biplot scaling 2 shows the correlation present between the SDGs and measurements.
# A trend is present where the negative correlation is present on the left side and 
# continues to be less negative towards the right side of the plot. This observation 
# correlates with the above mentioned fact that the left hand side is for poorly financial
# countries to more wealthy countries. Dependent on how wealthy a country is, the more the
# measurement has an affect on the variation of the SDG it has. A poorer country such as 
# Africa, the SDG is influenced more by the malaria incident, children with Hepatitis B, 
# deaths due to road traffic, poison control and unintentional poison, HIV infections and 
# adult mortalities. These factors are due to the inadequate reliability of the health
# care system and the poor public safety of the areas. This makes it a negative correlation
# with wealthier continents due to their SDGs being influenced by more births attended by
# healthcare workers, higher life expectancy and many others. The poorer countries do 
# not have access to these factors which causes the negative correlation. 

view(site_scores)
site_scores <- site_scores %>% 
  filter(Location %in% c("South Africa", "Canada"))
# Filters the data so that the only site scores present are that of the stipulated locations. 

ggplot(data = site_scores, aes(x = PC1, y = PC2)) + 
  geom_point(aes(col = Location)) + 
  geom_segment(data = species_scores, 
               aes(x = 0, y = 0, xend = PC1, yend = PC2), 
               arrow = arrow(length = unit(0.4, "cm"), type = "closed"),  
               color = "lightseagreen", alpha = 1, size = 0.3) + 
  geom_text(data = species_scores, 
            aes(x = PC1, y = PC2, label = species), 
            color = "black") +  
  xlab("PC1") + ylab("PC2") + 
  ggtitle("WHO SDGs (Country based), Scaling 2") 

# South Africa is being compared to a highly-developed country, Canada. South 
# Africa's SDGs has a positive correlation with the factors that affect the 
# SDGs in Africa. South Africa's SDGs more strongly influenced by factors such 
# as high values in deaths by non-communicable diseases (Diabetes mellitus) and 
# Crude suicide rates. Due to South Africa being a more delveloped country than
# most countries found in Africa, it will not have similar factors that strongly
# affect the SDGs such as girls aged 15 years old that received the recommended 
# doses of HPV vaccine. 
# In Canada, there is a positive correlation between the factors associated by 
# a wealthier country as stated previously. South Africa's SDG is more correlated
# with those factors that one would associate with poorer countries. In contrast 
# to this, South Africa's SDGs is also positively correlated with factors that are 
# associated with wealthier countries such as deaths by respiratory diseases, 
# alcohol consumption rates, cardiovascular diseases, and prevalent usage of tobacco.
# The fact that South Africa has a positive correlation with these factors may be 
# due to the financial status of the country. Our country is seen as being a 
# developing country, however due to the financial inequality present, many of 
# the people are able to access these things more easily than others. Our health
# care system is similar to that of under-developed countries. Due to this unequal
# balance, South Africa is positively correlated with both factors that are 
# associated with wealthy and poor countries. There is a clear gap in the 
# standard of living in South Africa. 

# Question 3: 

# The data that was used may be out of date as there were incidents where one 
# could not use the data of a specified year (2016) because there was not data 
# for that year. Political factors were not taken into consideration throughout
# the observations as this may play a role in why countries are classified as 
# being under developed and developed. Wealthier countries have more access to 
# measure the data rather than that of under developed countries. 
# Due to the lack of information and biased relationship with wealthy countries,
# the ultimate goal will not be attainable. 