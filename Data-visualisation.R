#Ridge plot showing regional variation in index score

library(ggridges)
library(extrafont)

ggplot(index, aes(x = score, y = Region, fill = Region)) +
  geom_density_ridges() +
  theme_ridges() +
  theme(legend.position = 'none') +
  scale_fill_brewer(palette = "YlOrBr") +
  xlab('Gender Equality Index Score') +
  theme(text = element_text(family = 'Open Sans', size = 15))