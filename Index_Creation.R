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
library(magrittr)
library(hrbrthemes)
library(ggridges)
library(ggthemes)

#Import datasets
GCSE_grades <- read_csv('Education/2019_GCSEs.csv')
education <- read_csv('Education/level_4_and_no_quals.csv')
labour_force <- read_csv('Work/labour_force.csv')
gender_pay_gap <- read_csv('Work/gender_pay_gap.csv')
sexual_assault_mf <- read_csv('Health_and_Safety/sexualoffencesprevalenceandvictimcharacteristicsappendixtables3 (1).csv')
mps <- read_csv('Representation/MPs - Theyworkforyou.com.csv')
healthy_life_expec_f <- read_csv('Health_and_Safety/female_healthy_life_expectancy_2019.csv')
healthy_life_expec_m <- read_csv('Health_and_Safety/male_healthy_life_expectancy_2019.csv')

#Import shapefiles
eng_regions_UAs <- st_read('Boundaries/regions_and_counties.shp')
eng_UAs <- st_read('Boundaries/Counties_and_Unitary_Authorities_(December_2020)_UK_BFC/Counties_and_Unitary_Authorities_(December_2020)_UK_BFC.shp')
constituencies <- st_read('Boundaries/Westminster_Parliamentary_Constituencies__December_2017__Boundaries_UK-shp/Westminster_Parliamentary_Constituencies__December_2017__Boundaries_UK.shp')
eng_constituencies_UAs <- st_read('Boundaries/UAs_and_constituencies.shp')

#Tidy data - Remove unneeded columns
GCSE_tidy <- GCSE_grades %>%
  subset(select = -c(1:8, 11:13, 15:23, 25:115))

education_tidy <- education %>%
  subset(select = -c(2, 3, 5:7, 9:11, 13:15, 17))

labour_force_tidy <- labour_force %>%
  subset(select = -c(2, 3, 5:7, 9))

pay_gap_tidy <- gender_pay_gap %>%
  subset(select = -c(5:11))

mps_tidy <- mps %>%
  subset(select = -c(1:3, 6))

healthy_life_f_tidy <- healthy_life_expec_f %>%
  subset(select = -c(1:4, 7, 9:12, 14:26))
healthy_life_m_tidy <- healthy_life_expec_m %>%
  subset(select = -c(1:4, 7, 9:12, 14:26))

sexual_assault_tidy <- sexual_assault_mf %>%
  subset(select = c(1:3))

#Tidy data - Remove unneeded rows
GCSE_tidy <- GCSE_tidy[!is.na(GCSE_tidy$new_la_code), ]

education_tidy <- education_tidy[!is.na(education_tidy$X4), ]
education_tidy <- education_tidy[-c(1),]
  
labour_force_tidy <- labour_force_tidy[!is.na(labour_force_tidy$X8), ]
labour_force_tidy <- labour_force_tidy[-c(1),]

pay_gap_tidy <- pay_gap_tidy[!is.na(pay_gap_tidy$X2), ]
pay_gap_tidy <- pay_gap_tidy[-c(1),]

sexual_assault_tidy <- sexual_assault_tidy[!is.na(sexual_assault_tidy$X2), ]
sexual_assault_tidy <- sexual_assault_tidy[-c(1:34),]

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

sexual_assault_tidy <- sexual_assault_tidy %>%
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

sexual_assault_tidy$Men <- as.numeric(sexual_assault_tidy$Men)
sexual_assault_tidy$Women <- as.numeric(sexual_assault_tidy$Women)

#Creating variables 
GCSE_girls <- GCSE_tidy %>%
  subset(characteristic_gender == 'Girls')
GCSE_girls <- GCSE_girls %>%
  rename(
    girls_percent_grade_5_plus_maths_english = `percent_grade_5_plus_maths_english`
  )

GCSE_boys <- GCSE_tidy %>%
  subset(characteristic_gender == 'Boys')
GCSE_boys <- GCSE_boys %>%
  rename(
    boys_percent_grade_5_plus_maths_english = `percent_grade_5_plus_maths_english`
  )

GCSE <- merge(GCSE_boys, GCSE_girls, by = 'la_name')
GCSE <- GCSE %>%
  subset(select = c(1,2,4,7))

GCSE$new_la_code.x <- stringr::str_replace(GCSE$new_la_code.x, 'E10000002', 'E06000060')

level_4 <- data.frame(education_tidy$UA, education_tidy$Percent_level_4_male, education_tidy$Percent_level_4_female)
for (col in 1:ncol(level_4)) {
  colnames(level_4)[col] <- sub("education_tidy.", "", colnames(level_4)[col])
}

no_quals <- data.frame(education_tidy$UA, education_tidy$No_quals_male, education_tidy$No_quals_female)
for (col in 1:ncol(no_quals)) {
  colnames(no_quals)[col] <- sub("education_tidy.", "", colnames(no_quals)[col])
}

pay_gap_tidy <- pay_gap_tidy %>%
  subset(select = -3)
pay_gap_tidy <- pay_gap_tidy %>%
  rename(
    mean_pay_gap = `Mean`
  )

mps_tidy <- replace(mps_tidy, is.na(mps_tidy), 0)

healthy_life_f_tidy <- healthy_life_f_tidy %>%
  rename(
    female_healthy_years = `Healthy_life_expectancy`
  )

healthy_life_m_tidy <- healthy_life_m_tidy %>%
  rename(
    male_healthy_years = `Healthy_life_expectancy`
  )

healthy_life <- merge(healthy_life_f_tidy, healthy_life_m_tidy, by = 'Area Code')
healthy_life <- healthy_life %>%
  subset(select = c(1,2,4,7))
healthy_life$AreaName.x <- stringr::str_replace(healthy_life$AreaName.x, 'Herefordshire', 'Herefordshire,County of')
healthy_life$AreaName.x <- stringr::str_replace(healthy_life$AreaName.x, 'Kingston Upon Hull', 'Kingston Upon Hull,City of')

#Standardise administrative area scale 
mps_UAs <- merge(mps_tidy, eng_constituencies_UAs, by.x = 'Constituency', by.y = 'Constituen')
mps_sf <- st_as_sf(mps_UAs)
mps_nogeom <- st_drop_geometry(mps_sf)

sexual_assault_tidy$Region <- stringr::str_replace(sexual_assault_tidy$Region, 'Yorkshire and the Humber', 'Yorkshire and The Humber')
sexual_assault_regions <- merge(sexual_assault_tidy, eng_regions_UAs, by.x = 'Region', by.y = 'RGN20NM')
sa_sf <- st_as_sf(sexual_assault_regions)
sexual_assault_nogeom <- st_drop_geometry(sa_sf)

#Calculate inequality
GCSE$GCSE_difference <- GCSE$boys_percent_grade_5_plus_maths_english - GCSE$girls_percent_grade_5_plus_maths_english
level_4$level_4_difference <- level_4$Percent_level_4_male - level_4$Percent_level_4_female
no_quals$no_quals_difference <- no_quals$No_quals_male - no_quals$No_quals_female

# Pay gap data already shows difference #
labour_force_tidy$labour_force_difference <- labour_force_tidy$Percent_male_employment - labour_force_tidy$Percent_female_employment

x <- mps_nogeom %>%
  group_by(County_cod) %>%
  summarise(sum(Male, Female))
y <- mps_nogeom %>%
  group_by(County_cod) %>%
  summarise(sum(Male))
z <- mps_nogeom %>%
  group_by(County_cod) %>%
  summarise(sum(Female))

mps_total <- data.frame(x, y, z)
mps_total <- mps_total %>%
  subset(select = c(1, 2, 4, 6))
mps_total <- mps_total %>%
  rename(
    Total_MPs = `sum.Male..Female.`,
    Male_MPs = `sum.Male.`,
    Female_MPs = `sum.Female.`
  )

mps_total$mps_difference <- (mps_total$Male_MPs / mps_total$Total_MPs) - (mps_total$Female_MPs / mps_total$Total_MPs)

sexual_assault_nogeom$sexual_assault_difference <- sexual_assault_nogeom$Men - sexual_assault_nogeom$Women

healthy_life$life_expc_difference <- healthy_life$male_healthy_years - healthy_life$female_healthy_years

#Create new data frame containing all indicators (the index)
index <- data.frame(GCSE$la_name, GCSE$new_la_code.x, GCSE$GCSE_difference)

index <- merge(index, pay_gap_tidy, by.x = 'GCSE.new_la_code.x', by.y = 'UA_Code')
index <- merge(index, labour_force_tidy, by.x = 'GCSE.la_name', by.y = 'Area')
index <- index %>%
  subset(select = -c(4, 6, 7))
index <- merge(index, healthy_life, by.x = 'GCSE.new_la_code.x', by.y = 'Area Code')
index <- index %>%
  subset(select = -c(6:8))
index <- merge(index, level_4, by.x = 'GCSE.la_name', by.y = 'UA')
index <- index %>%
  subset(select = -c(7,8))
index <- merge(index, no_quals, by.x = 'GCSE.la_name', by.y = 'UA')
index <- index %>%
  subset(select = -c(8,9))
index <- merge(index, sexual_assault_nogeom, by.x = 'GCSE.new_la_code.x', by.y = 'CTYUA20CD')
index <- index %>%
  subset(select = c(1:9, 20))
index <- merge(index, mps_total, by.x = 'GCSE.new_la_code.x', by.y = 'County_cod')
index <- index %>%
  subset(select = -c(11:13))

#Rename and reorder columns 
index <- index[, c(1, 2, 9, 3, 7, 8, 4, 5, 11, 6, 10)]

index <- index %>%
  rename(
    Area_code = 'GCSE.new_la_code.x',
    Area_name = 'GCSE.la_name',
    GCSE_difference = 'GCSE.GCSE_difference'
  )

#Remove any UAs with incomplete datasets
index <- index %>%
  drop_na()

#Calculate absolute values 
index$GCSE_difference_abs <- abs(index$GCSE_difference)
index$level_4_difference_abs <- abs(index$level_4_difference)
index$no_quals_difference_abs <- abs(index$no_quals_difference)
index$mean_pay_gap_abs <- abs(index$mean_pay_gap)
index$labour_force_difference_abs <- abs(index$labour_force_difference)
index$mps_difference_abs <- abs(index$mps_difference)
index$life_expc_difference_abs <- abs(index$life_expc_difference)
index$sexual_assault_difference_abs <- abs(index$sexual_assault_difference)

#Sort data and assign ranks
index$GCSE_rank <- rank(index$GCSE_difference_abs)
index$level_4_rank <- rank(index$level_4_difference_abs)
index$no_quals_rank <- rank(index$no_quals_difference_abs)
index$pay_gap_rank <- rank(index$mean_pay_gap_abs)
index$labour_force_rank <- rank(index$labour_force_difference_abs)
index$mp_rank <- rank(index$mps_difference_abs)
index$life_rank <- rank(index$life_expc_difference_abs)
index$sexual_assult_rank <- rank(index$sexual_assault_difference_abs)

#Calculate mean rank (index score)
index$score <- rowMeans(index[, c(12:18)])
index$score_rank <- rank(index$score)
index$score <- as.numeric(index$score)

#Export index to QGIS for visualisation
#write_csv(index, 'final-index.csv')

#Calculate mean index score
mean_score <- mean(index$score)
print(mean_score)
