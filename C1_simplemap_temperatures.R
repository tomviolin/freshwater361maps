# Load libraries
loadlibrary(osmdata)
loadlibrary(maptiles)
loadlibrary(RColorBrewer)
loadlibrary(plotrix)
pdf("shipmap.pdf",width = 11,height=8.5,family = "Helvetica")  # also jpeg() png()  pdf() 
cpal = rev(RColorBrewer::brewer.pal(11,"Spectral"))[c(1,2,3,4,5,6,7,8,9,10,11)]
cpal = colorRampPalette(cpal)(1000)

NC = length(cpal)
hsvpal = rgb2hsv(col2rgb(cpal))
hsvpal[3,] = 1.0
hsvpal[2,] = .9
# hsvpal[1,7]= 0.15
# hsvpal[1,6] = 0.17

#hsvpal[1,] = seq(0,0.7,length.out=NC)
cpal = hsv(hsvpal[1,],hsvpal[2,],hsvpal[3,])
data = read.csv("shipdata2025-09-06.csv") ## Read in the ship data
# Establish the bounding box (the coordinates of a rectangle that defines the map)  +/- 0.001 for margin
bbox = sf::st_bbox(c(xmax=max(data$GPS_Long)+0.001,xmin=min(data$GPS_Long)-0.001,ymax=max(data$GPS_Lat)+0.001,ymin=min(data$GPS_Lat))-0.0001,crs=4623)
#get tiles
tiles_map <- get_tiles(x = bbox, provider = "OpenStreetMap.DE",zoom=13, crop=TRUE)


temps = data$Temp_C         # temps is in range min to max
temps = temps - min(temps)  # now temps is in range 0 to max-min
temps = temps / max(temps)  # now temps is in range 0 to 1

trackcolors = cpal[floor(temps*NC*0.999+1)]
#plot(data$GPS_Long,data$GPS_Lat,cex=.2,col=trackcolors, main="Ship Data 9/6/2025",asp=1) # ship track
plot_tiles(tiles_map, add = FALSE) # Plot the tiles
points(data$GPS_Long,data$GPS_Lat,cex=.3,col=rgb(0,0,1,0.5), main="Ship Data 9/6/2025",asp=1) # ship track
points(data$GPS_Long,data$GPS_Lat,cex=.2,col=trackcolors, main="Ship Data 9/6/2025",asp=1) # ship track
legendplaces = rep(NULL,NC)
leglabels = seq(
  round(min(data$Temp_C),1),
  round(max(data$Temp_C),1),
  by=0.1)

  

legpos = floor(1+NC*(leglabels - min(data$Temp_C)) / diff(range(data$Temp_C)))
legendplaces[legpos] = leglabels

color.legend(yt=quantile(range(data$GPS_Lat),0.99),
             yb=quantile(range(data$GPS_Lat),0.55),
             xl=quantile(range(data$GPS_Long),0.975),
             xr=quantile(range(data$GPS_Long),0.99),
             cex=0.8,

             legend=legendplaces,
             col="black", #cpal,
             rect.col=cpal, gradient = "y")
dev.off() # only if saving to file
system2("open","shipmap.pdf")
# get_credit("Esri.NatGeoWorldMap")
# [1] "Tiles © Esri - National Geographic, Esri, DeLorme, NAVTEQ, UNEP-WCMC, USGS, NASA, ESA, METI, NRCAN, GEBCO, NOAA, iPC"
##sf_polygon <- getbb("Milwaukee, WI", format_out = "sf_polygon") # this is useful!
