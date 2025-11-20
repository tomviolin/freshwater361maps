# Load libraries
loadlibrary(osmdata)
loadlibrary(maptiles)
loadlibrary(RColorBrewer)
loadlibrary(plotrix)
loadlibrary(sf)
loadlibrary(data.table)




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
alldata = fread("fixedhistory.csv.gz")# Read in the ship data
gap_kml = sf::st_read("gap_area.kml")

bb = st_bbox(gap_kml$geometry)

bbox_data = bb

data = alldata[
  (alldata$GPS_Lat >= bbox_data[2]) &
  (alldata$GPS_Lat <= bbox_data[4]) &
  (alldata$GPS_Long >= bbox_data[1]) &
  (alldata$GPS_Long <= bbox_data[3])
,]

data$recdate = as.POSIXct(data$recdate)

# Establish the bounding box (the coordinates of a rectangle that defines the map)  +/- 0.001 for margin
map_bbox = sf::st_bbox(c(
  xmin=min(data$GPS_Long)  -0.001,
  ymin=min(data$GPS_Lat)   -0.001,
  xmax=max(data$GPS_Long)  +0.001,
  ymax=max(data$GPS_Lat)   +0.001
  ),crs=4623)

#map_bbox = bb

#get tiles
tiles_map <- get_tiles(x = map_bbox, provider = "OpenStreetMap.DE",zoom=15, crop=TRUE)


temps = data$Temp_C         # temps is in range min to max
temps = temps - min(temps)  # now temps is in range 0 to max-min
temps = temps / max(temps)  # now temps is in range 0 to 1

trackcolors = cpal[floor(temps*NC*0.999+1)]
#plot(data$GPS_Long,data$GPS_Lat,cex=.2,col=trackcolors, main="Ship Data 9/6/2025",asp=1) # ship track

pdf("shipgap.pdf",width = 11,height=8.5,family = "Helvetica")  # also jpeg() png()  pdf() 


plot_tiles(tiles_map, add = FALSE) # Plot the tiles
#points(data$GPS_Long,data$GPS_Lat,cex=.3,col=rgb(0,0,1,0.5), main="Ship Data 9/6/2025",asp=1) # ship track
points(data$GPS_Long,data$GPS_Lat,cex=.2,col=trackcolors, main="Ship Data 9/6/2025",asp=1) # ship track


points(range(data$GPS_Long),range(data$GPS_Lat),cex=2,col='red')

# lines(
#   map_bbox[c(1,3)],
#   map_bbox[c(2,4)],
#   type='l',
#   col='red')



legendplaces = rep(NULL,NC)
leglabels = seq(
  round(min(data$Temp_C),1),
  round(max(data$Temp_C),1),
  by=0.1)

  

legpos = floor(1+NC*(leglabels - min(data$Temp_C)) / diff(range(data$Temp_C)))
legendplaces[legpos] = leglabels

legendbbox = c(
  xmin=map_bbox[1],
  ymin=map_bbox[2],
  xmax=map_bbox[3],
  ymax=map_bbox[4]
)




color.legend(yt=quantile(legendbbox[c(2,4)],0.99),
             yb=quantile(legendbbox[c(2,4)],0.55),
             xl=quantile(legendbbox[c(1,3)],0.975),
             xr=quantile(legendbbox[c(1,3)],0.99),
             cex=0.8,
             legend=legendplaces,
             col="black", #cpal,
             rect.col=cpal, gradient = "y")
#
print(map_bbox[c(1,3)])
print(map_bbox[c(2,4)])



dev.off() # only if saving to file
system2("open","shipgap.pdf")
# get_credit("Esri.NatGeoWorldMap")
# [1] "Tiles © Esri - National Geographic, Esri, DeLorme, NAVTEQ, UNEP-WCMC, USGS, NASA, ESA, METI, NRCAN, GEBCO, NOAA, iPC"
##sf_polygon <- getbb("Milwaukee, WI", format_out = "sf_polygon") # this is useful!

noaa_begin_to_POSIXct = function(noaa_begin) {
  as.POSIXct(paste(substr(noaa_begin,1,10),substr(noaa_begin,12,16),paste0("UTC",substr(noaa_begin,17,19))))
}



noaawdir = fread("open.NOAA-ISD.726400-14839.WIND_DIRECTION.csv")
noaawspd = fread("open.NOAA-ISD.726400-14839.WIND_SPEED_RATE.csv")

noaawdir$recdate = noaa_begin_to_POSIXct(noaawdir$begin)
noaawspd$recdate = noaa_begin_to_POSIXct(noaawspd$begin)

noaawdir$rechour = floor(as.numeric(noaawdir$recdate) / (60*60))*(60*60)
noaawspd$rechour = floor(as.numeric(noaawspd$recdate) / (60*60))*(60*60)

noaawind= sqldf("
  select
    noaawspd.rechour as wspdhour, 
    noaawspd.v as wspd,
    noaawdir.rechour as wdirhour,
    noaawdir.v as wdir
  from 
    noaawspd, noaawdir
  where noaawspd.rechour = noaawdir.rechour")


data$recdate = as.POSIXct(data$recdate)

data$rechour = floor(as.numeric(data$recdate) / (60*60)) * (60*60)

datawind = sqldf("
                SELECT 
                  data.rechour as dhour, 
                  data.GPS_Long, 
                  data.GPS_Lat,
                  min(data.Temp_C) as min_temp_c,
                  max(data.Temp_C) as max_temp_c, 
                  max(data.Temp_C) - min(data.Temp_C) as diff_temp_c,
                  data.Depth_m,
                  wdir,wspd
                FROM data 
                LEFT JOIN 
                 noaawind ON noaawind.wspdhour = data.rechour
                GROUP BY data.rechour
                ORDER BY diff_temp_c")

