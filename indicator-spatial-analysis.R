

#Other possible analusis to suplement 


#Spatial analysis of individual indicators 





#^^ graph with colours showing regions 



ggplot(index_imd, aes(x = score, y = IMD_2019, colour = Region)) +
  geom_point(size = 6) +
  theme_economist(base_family = 'ITC Officina Sans')
  


# ^^^^ missing values?????? 





  

#CHANGE COLOURS, REORDER REGION NAMES ??? 


#rural vs urban? 


#population cartogram? 