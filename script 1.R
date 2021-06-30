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
sexual_offences <- read_csv('Safety/sexual-offences-2019:20.csv')
mps <- read_csv('Representation/MPs - Theyworkforyou.com.csv')
healthy_life_expec_f <- read_csv('Health/female_healthy_life_expectancy_2019.csv')
healthy_life_expec_m <- read_csv('Health/male_healthy_life_expectancy_2019.csv')
maternity_care <- read_csv('Health/maternal_health_care_PHE.csv')

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

maternity_care_tidy <- maternity_care %>%
  subset(select = -c(1:4, 7:12, 14:26))

#Tidy data - Remove unneeded rows
GCSE_tidy <- GCSE_tidy[!is.na(GCSE_tidy$new_la_code), ]

education_tidy <- education_tidy[!is.na(education_tidy$X4), ]
education_tidy <- education_tidy[-c(1),]
  
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

sexual_offs_tidy$Rate_per_1000_population <- as.numeric(sexual_offs_tidy$Rate_per_1000_population)

#Split datasets into individual variables 
GCSE_girls <- GCSE_tidy %>%
  subset(characteristic_gender == 'Girls')

GCSE_boys <- GCSE_tidy %>%
  subset(characteristic_gender == 'Boys')

level_4 <- data.frame(education_tidy$UA, education_tidy$Percent_level_4_male, education_tidy$Percent_level_4_female)
for (col in 1:ncol(level_4)) {
  colnames(level_4)[col] <- sub("education_tidy.", "", colnames(level_4)[col])
}

level_4_female <- data.frame(level_4$UA,level_4$Percent_level_4_female)
for (col in 1:ncol(level_4_female)) {
  colnames(level_4_female)[col] <- sub("level_4.", "", colnames(level_4_female)[col])
}

level_4_male <- data.frame(level_4$UA,level_4$Percent_level_4_male)
for (col in 1:ncol(level_4_male)) {
  colnames(level_4_male)[col] <- sub("level_4.", "", colnames(level_4_male)[col])
}

no_quals <- data.frame(education_tidy$UA, education_tidy$No_quals_male, education_tidy$No_quals_female)
for (col in 1:ncol(no_quals)) {
  colnames(no_quals)[col] <- sub("education_tidy.", "", colnames(no_quals)[col])
}

no_quals_female <- data.frame(no_quals$UA, no_quals$No_quals_female)
for (col in 1:ncol(no_quals_female)) {
  colnames(no_quals_female)[col] <- sub("no_quals.", "", colnames(no_quals_female)[col])
}

no_quals_male <- data.frame(no_quals$UA, no_quals$No_quals_male)
for (col in 1:ncol(no_quals_male)) {
  colnames(no_quals_male)[col] <- sub("no_quals.", "", colnames(no_quals_male)[col])
}

#Join datasets to shapefiles and fit to study area (England)
GCSE_girls_joined <- merge(eng_UAs, GCSE_girls, by.x = "CTYUA20CD", by.y = "new_la_code")
GCSE_boys_joined <- merge(eng_UAs, GCSE_boys, by.x = "CTYUA20CD", by.y = "new_la_code")

level_4_female_joined <- merge(eng_UAs, level_4_female, by.x = "CTYUA20NM", by.y = "UA")
level_4_male_joined <- merge(eng_UAs, level_4_male, by.x = "CTYUA20NM", by.y = "UA")
no_quals_female_joined <- merge(eng_UAs, no_quals_female, by.x = "CTYUA20NM", by.y = "UA")
no_quals_male_joined <- merge(eng_UAs, no_quals_male, by.x = "CTYUA20NM", by.y = "UA")

labour_force_tidy <- labour_force_tidy[-c(153:225), ]
labour_force_joined <- merge(eng_UAs, labour_force_tidy, by.x = "CTYUA20NM", by.y = "Area")

pay_gap_tidy <- pay_gap_tidy[grepl("E", pay_gap_tidy$UA_Code), ]
pay_gap_joined <- merge(eng_UAs, pay_gap_tidy, by.x = "CTYUA20CD", by.y = "UA_Code")

sexual_offs_joined <- merge(police_forces, sexual_offs_tidy, by.x = "PFA16NM", by.y = "Area_code")

mps_joined <- merge(constituencies, mps_tidy, by.x = 'pcon17nm', by.y = 'Constituency')
mps_joined <- mps_joined[grepl("E", mps_joined$pcon17cd), ]

f_life_expec_joined <- merge(eng_UAs, healthy_life_f_tidy, by.x = "CTYUA20CD", by.y = "Area Code")

m_life_expec_joined <- merge(eng_UAs, healthy_life_m_tidy, by.x = "CTYUA20CD", by.y = "Area Code")

maternity_care_joined <- merge(eng_UAs, maternity_care_tidy, by.x = "CTYUA20CD", by.y = "Area Code")

#Explore datasets
summary(GCSE_girls_joined$percent_grade_5_plus_maths_english)
summary(GCSE_boys_joined$percent_grade_5_plus_maths_english)

summary(level_4_female)
summary(level_4_male)
summary(no_quals_female)
summary(no_quals_male)

summary(labour_force_joined$Percent_male_employment)
summary(labour_force_joined$Percent_female_employment)

summary(pay_gap_joined$Mean)
summary(pay_gap_joined$Median)

summary(sexual_offs_joined$Rate_per_1000_population)

mps_tidy %>% count(Male)
mps_tidy %>% count(Female)

summary(f_life_expec_joined$Healthy_life_expectancy)
summary(m_life_expec_joined$Healthy_life_expectancy)

summary(maternity_care_joined$Percentage_with_early_access)

#Transform to sf objects
GCSE_girls_sf <- st_as_sf(GCSE_girls_joined)
GCSE_boys_sf <- st_as_sf(GCSE_boys_joined)

level_4_female_sf <- st_as_sf(level_4_female_joined)
level_4_male_sf <- st_as_sf(level_4_male_joined)
no_quals_female_sf <- st_as_sf(no_quals_female_joined)
no_quals_male_sf <- st_as_sf(no_quals_male_joined)

labour_force_sf <- st_as_sf(labour_force_joined)

pay_gap_sf <- st_as_sf(pay_gap_joined)

sexual_offs_sf <- st_as_sf(sexual_offs_joined)

mps_sf <- st_as_sf(mps_joined)

f_life_expec_sf <- st_as_sf(f_life_expec_joined)
m_life_expec_sf <- st_as_sf(m_life_expec_joined)

maternity_sf <- st_as_sf(maternity_care_joined)

#Explore the data using interactive maps
tmap_mode('view')

tm_shape(GCSE_girls_sf) +
  tm_borders('black', alpha = 0.5) +
  tm_fill(col = 'percent_grade_5_plus_maths_english', id = 'la_name', palette = brewer.pal(6, 'RdYlBu')) +
  tm_layout(title = 'Percentage of girls achieving a Grade 5 or higher in Maths and English GCSEs')
  
tm_shape(GCSE_boys_sf) +
  tm_borders('black', alpha = 0.5) +
  tm_fill(col = 'percent_grade_5_plus_maths_english', id = 'la_name', palette = brewer.pal(6, 'RdYlBu')) +
  tm_layout(title = 'Percentage of boys achieving a Grade 5 or higher in Maths and English GCSEs')

tm_shape(level_4_female_sf) +
  tm_borders('black', alpha = 0.5) +
  tm_fill(col = 'Percent_level_4_female', id = 'CTYUA20NM', palette = brewer.pal(6, 'RdYlBu')) +
  tm_layout(title = 'Percentage of females holding qualifications at Level 4 or above')

tm_shape(level_4_male_sf) +
  tm_borders('black', alpha = 0.5) +
  tm_fill(col = 'Percent_level_4_male', id = 'CTYUA20NM', palette = brewer.pal(6, 'RdYlBu')) +
  tm_layout(title = 'Percentage of males holding qualifications at Level 4 or above')
#^^^^ NEEDS COLOUR BREAKS EDITED (so it can be compared with female map)




tm_shape(pay_gap_sf) +
  tm_borders('black', alpha = 0.5) +
  tm_fill(col = 'Mean', id = 'CTYUA20NM', palette = brewer.pal(6, 'RdYlBu')) +
  tm_layout(title = 'Mean gender pay gap')





#Export tidied data to QGIS to create visualisations for write-up 
st_write(GCSE_girls_sf, 'GCSE_girls_joined.shp', append = TRUE)
st_write(GCSE_boys_sf, 'GCSE_boys_joined.shp', append = TRUE)

st_write(level_4_sf, 'level_4.shp', append = TRUE)
st_write(no_quals_sf, 'no_quals.shp', append = TRUE)

st_write(labour_force_sf, 'labour_force_joined.shp', append = TRUE)

st_write(pay_gap_sf, 'gender_pay_gap.shp', append = TRUE)

st_write(sexual_offs_sf, 'sexual_offences.shp', append = TRUE)

st_write(mps_sf, 'mps_joined.shp', append = TRUE)

st_write(f_life_expec_sf, 'female_healthy_life_expectancy.shp', append = TRUE)
st_write(m_life_expec_sf, 'male_healthy_life_expectancy.shp', append = TRUE)

st_write(maternity_sf, 'access_to_maternity_care.shp', append = TRUE)



#SCALES???? UA? constituency/ police area etc. 
#spatial join?? 
#HELP!!!!!!!!!!!!!!!!!!!


#Standardise administrative area scale 

#join mps and sexual offences to new shapefiles with UAs


#Create new data frame containing all indicators (the index)
inequality_data 
area_profile

index 



#Calculations/ ratios 

parlimentary_seats

#eg sum/ group by ua and calculate ratio of male to female mps (PSA workbook for code?) 

#Create index function (?) 


#Calculate index scores

#Spatial analysis

  
  
#Define neighbour structure and spatial weights
eng_neighbours <- poly2nb(eng_UAs)
plot(eng_UAs$geometry)
plot(eng_neighbours, coords = eng_UAs$geometry, add = TRUE, col = 'red')
spatial_weights <- nb2listw(eng_neighbours, zero.policy = TRUE)

#Global Moran's I


#Local Moran's I



#Getis Ord Gi*


#Report results 


#Join data to shape files

#Transform data to sf ?? 


#Visualise data 


#Export to QGIS (?) 