#Set working directory
setwd("~/Desktop/Masters/Dissertation/Data")

#Load libraries
library(tidyverse)
library(sf)
library(tmap)
library(naniar)
library(ggpubr)
library(ggplot2)

#Import datasets
GCSE_grades <- read_csv('Education/2019_GCSEs.csv')
tertiary_quals <- read_csv('Education/qualificationsbynuts3.csv')
labour_force <- read_csv('Work/labour_force.csv')
gender_pay_gap <- read_csv('Work/gender_pay_gap.csv')
sexual_offences <- read_csv('Safety/sexual-offences-2019:20.csv')
mps <- read_csv('Representation/MPs - Theyworkforyou.com.csv')
healthy_life_expec_f <- read_csv('Health/female_healthy_life_expectancy_2019.csv')
healthy_life_expec_m <- read_csv('Health/male_healthy_life_expectancy_2019.csv')
maternity_care <- read_csv('Health/maternal_health_care_PHE.csv')

#Import shapefiles
uk_UAs
uk_constituencies <- st_read('Boundaries/Westminster_Parliamentary_Constituencies__December_2017__Boundaries_UK-shp/Westminster_Parliamentary_Constituencies__December_2017__Boundaries_UK.shp')
uk_police_forces

#Tidy data - Remove unneeded columns
GCSE_tidy <- GCSE_grades %>%
  subset(select = -c(1:8, 11:13, 15:23, 25:115))

tquals_tidy <- tertiary_quals %>%
  subset(select = -c(3:8, 10:13))

labour_force_tidy <- labour_force %>%
  subset(select = -c(2, 3, 5:7, 9))

pay_gap_tidy <- gender_pay_gap %>%
  subset(select = -c(5:11))

sexual_offs_tidy <- sexual_offences %>%
  subset(select = -c(4:6))

mps_tidy <- mps %>%
  subset(select = -c(1:3, 5))

healthy_life_f_tidy <- healthy_life_expec_f %>%
  subset(select = -c(1:4, 7, 9:12, 14:26))
healthy_life_m_tidy <- healthy_life_expec_m %>%
  subset(select = -c(1:4, 7, 9:12, 14:26))

maternity_care_tidy <- maternity_care %>%
  subset(select = -c(1:4, 7:12, 14:26))

#Tidy data - Remove unneeded rows
GCSE_tidy <- GCSE_tidy[!is.na(GCSE_tidy$new_la_code), ]

tquals_tidy <- tquals_tidy[!is.na(tquals_tidy$X2), ]
tquals_tidy <- tquals_tidy[-c(1),]
  
labour_force_tidy <- labour_force_tidy[!is.na(labour_force_tidy$X8), ]
labour_force_tidy <- labour_force_tidy[-c(1),]

pay_gap_tidy <- pay_gap_tidy[!is.na(pay_gap_tidy$X2), ]
pay_gap_tidy <- pay_gap_tidy[-c(1),]

sexual_offs_tidy <- sexual_offs_tidy[!is.na(sexual_offs_tidy$X3), ]
sexual_offs_tidy <- sexual_offs_tidy[-c(1),]

#Tidy data - Rename columns 
GCSE_tidy <- GCSE_tidy %>%
  rename(
    percent_grade_5_plus_maths_english = `pt_l2basics_95`
  )

tquals_tidy <- tquals_tidy %>%
  rename(
    Code = `Highest Level of Qualifications Held by NUTS 3 Subregion (workplace basis),  Jan-Dec 2017`,
    UA = `X2`,
    Percent_level_4_and_above = `X9`
  )

labour_force_tidy <- labour_force_tidy %>%
  rename(
    Area = `annual population survey`,
    Percent_male_employment = `X4`,
    Percent_female_employment = `X8`
  )

pay_gap_tidy <- pay_gap_tidy %>%
  rename(
    Area = `Table 8.12 Gender pay gap (%)^  - For all employee jobs^^: United Kingdom, 2020`,
    UA_Code = `X2`,
    Median = `X3`,
    Mean = `X4`
  )

sexual_offs_tidy <- sexual_offs_tidy %>%
  rename(
    Area_code = `Table 15: Rate of sexual offences recorded by the police, by police force area, English regions and Wales, year ending March 20201,2,3`,
    Police_area = `X2`,
    Rate_per_1000_population = `X3`
  )

healthy_life_f_tidy <- healthy_life_f_tidy %>%
  rename(
    Healthy_life_expectancy = `Value`
  )
healthy_life_m_tidy <- healthy_life_m_tidy %>%
  rename(
    Healthy_life_expectancy = `Value`
  )

maternity_care_tidy <- maternity_care_tidy %>%
  rename(
    Percentage_with_early_access = `Value`
  )

#Tidy data - Format missing data
labour_force_tidy <- labour_force_tidy %>%
  replace_with_na(replace = list(Percent_male_employment = '-'))
labour_force_tidy <- labour_force_tidy %>%
  replace_with_na(replace = list(Percent_female_employment = '-'))
labour_force_tidy <- labour_force_tidy %>%
  replace_with_na(replace = list(Percent_female_employment = '!'))

pay_gap_tidy <- pay_gap_tidy %>%
  replace_with_na(replace = list(Median = 'x'))
pay_gap_tidy <- pay_gap_tidy %>%
  replace_with_na(replace = list(Mean = 'x'))

#Tidy data - Format data types




#Tidy data - 


#Explore datasets



#Export tidied data to QGIS for initial visualisation
write_csv(GCSE_tidy, file = 'GCSE_tidy.csv')
write_csv(tquals_tidy, file = 'tertiary_qualification_tidy.csv')
write_csv(labour_force_tidy, file = 'labour_force_tidy.csv')
write_csv(pay_gap_tidy, file = 'gender_pay_gap_tidy.csv')
write_csv(sexual_offs_tidy, file = 'sexual_offences_tidy.csv')
write_csv(mps_tidy, file = 'mps_tidy.csv')
write_csv(healthy_life_f_tidy, file = 'female_healthy_life_expectancy_tidy.csv')
write_csv(healthy_life_m_tidy, file = 'male_healthy_life_expectancy_tidy.csv')
write_csv(maternity_care_tidy, file = 'early_access_to_maternal_care_tidy.csv')

#Create new data frame containing all indicators

#Calculations/ ratios 

parliment_seats  
#Create index function (?) 




#Calculate index scores

#Spatial analysis

#Report results 


#Join data to shape files

#Transform data to sf ?? 


#Visualise data 


#Export to QGIS (?) 