#Graphs
#Histogram of index scores 
ggplot(index, aes(x = score)) +
  geom_histogram(fill = '#77c379') +
  xlab('Gender Equality Index Score') +
  ylab('UAs count')
  theme_ipsum() +
  theme(text = element_text(family = 'Open Sans', size = 15))

#Ridge plot showing regional variation in index score
ggplot(index, aes(x = score, y = Region, fill = Region)) +
  geom_density_ridges() +
  theme_ridges() +
  theme(legend.position = 'none') +
  scale_fill_brewer(palette = "Greens") +
  xlab('Gender Equality Index Score') +
  theme(text = element_text(family = 'Open Sans', size = 15))

#Correlation between index score and IMD 
imd <- read_csv('societal-wellbeing_imd2019_indicesbyla (1).csv')
index_imd <- merge(index, imd, by.x = 'Area_name', by.y = 'UA')

corr <- cor.test(index_imd$score, index_imd$IMD_2019, method = 'pearson')
print(corr)
ggscatter(index_imd, x = 'score', y = 'IMD_2019',
          add = 'reg.line', conf.int = TRUE,
          cor.coef = TRUE, cor.method = 'pearson',
          xlab = 'Gender Equality Index score', ylab = 'Index of Multiple Deprivation Score (2019)')
