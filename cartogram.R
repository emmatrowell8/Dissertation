library(maptools)
library(cartogram)

uk_pop <- read_csv('ukpopestimatesmid2020on2021geography.csv')
uk_pop <- uk_pop %>%
  subset(select = c(1:4))
uk_pop <- uk_pop[-c(1:7),]


pop_geom <- merge(uk_pop, eng_UAs, by.x = 'Code', by.y = 'OBJECTID')

index_pop <- merge(uk_pop, test_join, by.x = 'Name', by.y = 'Area_name')

index_pop$`All ages` <- as.numeric(as.character(index_pop$`All ages`))

index_pop_sf <- st_as_sf(index_pop)


eng_cartogram <- cartogram(index_pop_sf, 'All ages', itermax = 5)
plot(eng_cartogram)
