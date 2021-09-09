#Spatial analysis
#Drop unneeded index columns
index_score <- index %>%
  subset(select = c(1:3, 28))

#Join index to UA shapefile
index_joined <- merge(index_score, eng_UAs, by.x = 'Area_name', by.y = 'CTYUA20NM')
index_sf <- st_as_sf(index_joined)

#Map index scores
tm_shape(index_sf) +
  tm_fill('score', palette = '-Blues', style = 'quantile', title = 'Gender Equality Index Score') +
  tm_borders(alpha = 0.4)

#Define neighbour structures
eng_neighbours <- poly2nb(index_sf)
eng_neighbours

plot(index_sf$geometry)
plot(eng_neighbours, coords = index_sf$geometry, add = TRUE, col = 'red')

rook_neighbours <- poly2nb(index_sf, queen = FALSE)
rook_neighbours

plot(index_sf$geometry)
plot(rook_neighbours, coords = index_sf$geometry, add = TRUE, col = 'blue')

#Spatial weights
spatial_weights <- nb2listw(eng_neighbours, style = 'W', zero.policy = TRUE)
print(spatial_weights, zero.policy = TRUE)

#Global Moran's I
index_global_moran <- moran.test(index_sf$score, spatial_weights, na.action = na.pass, zero.policy = TRUE)
index_global_moran

#Local Moran's I
#Moran plot 
local_moran <- moran.plot(index_sf$score, listw = spatial_weights, zero.policy = TRUE)

#Local Moran
index_local_moran <- localmoran(x = index_sf$score,
                                listw = spatial_weights,
                                na.action = na.pass,
                                zero.policy = TRUE)
local_moran_map <- cbind(index_sf, index_local_moran)

#Map results
tm_shape(local_moran_map) +
  tm_fill(col = 'Ii', style = 'quantile', title = 'Local Moran Statistic') +
  tm_borders(alpha = 0.2) +
  tm_layout(frame = FALSE)

#LISA Cluster map 
quadrant <- vector(mode = 'numeric',length = nrow(index_local_moran))
m.index <- index_sf$score - mean(index_sf$score)
m.local <- index_local_moran[,1] - mean(index_local_moran[,1])
signif <- 0.1

quadrant[m.index >0 & m.local>0] <- 4
quadrant[m.index <0 & m.local<0] <- 1
quadrant[m.index <0 & m.local>0] <- 2
quadrant[m.index >0 & m.local<0] <- 3
quadrant[index_local_moran[,5]>signif] <- 0

breaks <- c(0,1,2,3,4)
colours <- c('white','blue',rgb(0, 0, 1, alpha = 0.4), rgb(1, 0, 0, alpha = 0.4),'red')

np <- findInterval(quadrant, breaks, all.inside = FALSE)
plot(index_sf$geometry, col = colours[np])
mtext('LISA', cex = 1.5, side = 3, line = 1)
legend('topleft', legend = c('Insignificant','Low-Low','Low-High','High-Low','High-High'), fill = colours, bty = 'n')

#Getis Ord Gi* 
getis_weights <- nb2listw(eng_neighbours, style = 'B', zero.policy = TRUE)
print(getis_weights, zero.policy = TRUE)

plot(index_sf$geometry)
plot(getis_weights, coords = index_sf$geometry, add = TRUE, col = 'red')

Gi_index <- localG(index_sf$score, getis_weights, zero.policy = TRUE)
Gi_index_UAs <- cbind(index_sf, as.matrix(Gi_index))
names(Gi_index_UAs)[14] <- 'Getis Ord Gi*'

#Report results 
tm_shape(Gi_index_UAs) +
  tm_fill(col = 'Getis Ord Gi*', style = 'pretty', title = 'Getis Ord Gi*', palette = 'RdBu') +
  tm_borders(alpha = 0.2) +
  tm_layout(frame = FALSE)


  
