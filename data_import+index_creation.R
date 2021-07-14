#Set working directory
setwd("~/Desktop/Masters/Dissertation/Data")

#Load libraries
library(tidyverse)
library(sf)
library(tmap)
library(tmaptools)
library(naniar)
library(ggpubr)
library(ggplot2)
library(stringr)
library(rgdal)
library(dplyr)
library(spdep)
library(RColorBrewer)
library(leaflet)

#Import datasets
GCSE_grades <- read_csv('Education/2019_GCSEs.csv')
education <- read_csv('Education/level_4_and_no_quals.csv')
labour_force <- read_csv('Work/labour_force.csv')
gender_pay_gap <- read_csv('Work/gender_pay_gap.csv')
sexual_assult_mf <- read_csv('Health_and_Safety/sexualoffencesprevalenceandvictimcharacteristicsappendixtables3 (1).csv')
mps <- read_csv('Representation/MPs - Theyworkforyou.com.csv')
healthy_life_expec_f <- read_csv('Health_and_Safety/female_healthy_life_expectancy_2019.csv')
healthy_life_expec_m <- read_csv('Health_and_Safety/male_healthy_life_expectancy_2019.csv')

#Import shapefiles
eng_regions_UAs <- st_read('Boundaries/regions_and_counties.shp')
eng_UAs <- st_read('Boundaries/Counties_and_Unitary_Authorities_(December_2020)_UK_BFC/Counties_and_Unitary_Authorities_(December_2020)_UK_BFC.shp')
constituencies <- st_read('Boundaries/Westminster_Parliamentary_Constituencies__December_2017__Boundaries_UK-shp/Westminster_Parliamentary_Constituencies__December_2017__Boundaries_UK.shp')
police_forces <- st_read('Boundaries/Police_Force_Areas_December_2016_EW_BFC_v2-shp/Police_Force_Areas_December_2016_EW_BFC_v2.shp')
eng_constituencies_UAs <- st_read('Boundaries/constituencies_and_UAs.shp')
eng_police_forces_UAs <- st_read('Boundaries/counties_and_police_areas.shp')

#Tidy data - Remove unneeded columns
GCSE_tidy <- GCSE_grades %>%
  subset(select = -c(1:8, 11:13, 15:23, 25:115))

education_tidy <- education %>%
  subset(select = -c(2, 3, 5:7, 9:11, 13:15, 17))

labour_force_tidy <- labour_force %>%
  subset(select = -c(2, 3, 5:7, 9))

pay_gap_tidy <- gender_pay_gap %>%
  subset(select = -c(5:11))

sexual_offs_tidy <- sexual_offences %>%
  subset(select = -c(4:6))

mps_tidy <- mps %>%
  subset(select = -c(1:3, 6))

healthy_life_f_tidy <- healthy_life_expec_f %>%
  subset(select = -c(1:4, 7, 9:12, 14:26))
healthy_life_m_tidy <- healthy_life_expec_m %>%
  subset(select = -c(1:4, 7, 9:12, 14:26))

sexual_assult_tidy <- sexual_assult_mf %>%
  subset(select = c(1:3))

#Tidy data - Remove unneeded rows
GCSE_tidy <- GCSE_tidy[!is.na(GCSE_tidy$new_la_code), ]

education_tidy <- education_tidy[!is.na(education_tidy$X4), ]
education_tidy <- education_tidy[-c(1),]
  
labour_force_tidy <- labour_force_tidy[!is.na(labour_force_tidy$X8), ]
labour_force_tidy <- labour_force_tidy[-c(1),]

pay_gap_tidy <- pay_gap_tidy[!is.na(pay_gap_tidy$X2), ]
pay_gap_tidy <- pay_gap_tidy[-c(1),]

sexual_assult_tidy <- sexual_assult_tidy[!is.na(sexual_assult_tidy$X2), ]
sexual_assult_tidy <- sexual_assult_tidy[-c(1:34),]

#Tidy data - Rename columns 
GCSE_tidy <- GCSE_tidy %>%
  rename(
    percent_grade_5_plus_maths_english = `pt_l2basics_95`
  )

education_tidy <- education_tidy %>%
  rename(
    UA = `annual population survey`,
    Percent_level_4_male = `X4`,
    Percent_level_4_female = `X8`,
    No_quals_male = `X12`, 
    No_quals_female = `X16`
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

sexual_assult_tidy <- sexual_assult_tidy %>%
  rename(
    Region = `Table 6: Percentage of adults aged 16 to 74 who were victims of sexual assault in the last year, by household and area characteristics and sex, year ending March 2018 to year ending March 2020 CSEW combined1,2,3`,
    Men = `X2`,
    Women = `X3`
  )

healthy_life_f_tidy <- healthy_life_f_tidy %>%
  rename(
    Healthy_life_expectancy = `Value`
  )
healthy_life_m_tidy <- healthy_life_m_tidy %>%
  rename(
    Healthy_life_expectancy = `Value`
  )

#Tidy data - Format missing data
education_tidy <- education_tidy %>%
  replace_with_na(replace = list(Percent_level_4_male = c('-', '!')))
education_tidy <- education_tidy %>%
  replace_with_na(replace = list(Percent_level_4_female = c('-', '!')))
education_tidy <- education_tidy %>%
  replace_with_na(replace = list(No_quals_male = c('-', '!')))
education_tidy <- education_tidy %>%
  replace_with_na(replace = list(No_quals_female = c('-', '!')))

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

#Tidy data - Format data types and remove unneeded characters
education_tidy$Percent_level_4_male <- as.numeric(education_tidy$Percent_level_4_male)
education_tidy$Percent_level_4_female <- as.numeric(education_tidy$Percent_level_4_female)
education_tidy$No_quals_male <- as.numeric(education_tidy$No_quals_male)
education_tidy$No_quals_female <- as.numeric(education_tidy$No_quals_female)

labour_force_tidy$Area <- stringr::str_replace(labour_force_tidy$Area, "lacu:", "")
labour_force_tidy$Percent_male_employment <- as.numeric(labour_force_tidy$Percent_male_employment)
labour_force_tidy$Percent_female_employment <- as.numeric(labour_force_tidy$Percent_female_employment)

pay_gap_tidy$Median <- as.numeric(pay_gap_tidy$Median)
pay_gap_tidy$Mean <- as.numeric(pay_gap_tidy$Mean)

sexual_assult_tidy$Men <- as.numeric(sexual_assult_tidy$Men)
sexual_assult_tidy$Women <- as.numeric(sexual_assult_tidy$Women)

#Creating variables 
GCSE_girls <- GCSE_tidy %>%
  subset(characteristic_gender == 'Girls')

GCSE_boys <- GCSE_tidy %>%
  subset(characteristic_gender == 'Boys')




#SCALES???? UA? constituency/ police area etc. 
#spatial join?? 
#HELP!!!!!!!!!!!!!!!!!!!


#Standardise administrative area scale 

#join mps and sexual offences to new shapefiles with UAs


#Create new data frame containing all indicators (the index)
index <- data.frame()


labour_force_tidy$labour_force_difference <- labour_force_tidy$Percent_male_employment - labour_force_tidy$Percent_female_employment


#create rank
#sort in numerical order



#Calculations/ ratios 

parlimentary_seats

#eg sum/ group by ua and calculate ratio of male to female mps (PSA workbook for code?) 

#Create index function (?) 


#Calculate index scores

