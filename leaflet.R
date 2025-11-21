library(leaflet)
library(data.table)
loadlibrary(R.utils)
#data = read.csv("shipdata2025-09-20.csv")
dataraw = fread("fixedhistory.csv") #[1:800000,]
data = dataraw[
  (dataraw$GPS_Lat > 43.019) &
    (dataraw$GPS_Lat < 43.030) &
    (dataraw$GPS_Long < -87.892) &
    (dataraw$Depth_m < 13),]
filter = seq(1,nrow(data),by=1)
data=data[filter,]
data$GPS_Lat = as.numeric(data$GPS_Lat)
data$GPS_Long = as.numeric(data$GPS_Long)
data$Temp_c = as.numeric(data$Temp_c)
data$Depth_m = as.numeric(data$Depth_m)

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



temps = -data$Depth_m         # temps is in range min to max
temps = temps - min(temps)  # now temps is in range 0 to max-min
temps = temps / max(temps)  # now temps is in range 0 to 1

trackcolors = cpal[floor(temps*NC*0.999+1)]





m = leaflet() %>%
    addProviderTiles("CartoDB.DarkMatter") %>%
    #addPolylines(lng=data$GPS_Long, lat=data$GPS_Lat, weight=2)
    addCircleMarkers(lng=data$GPS_Long, lat=data$GPS_Lat,radius = 0.003, fillColor=trackcolors, fill=T, stroke=F)
print(m)

loadlibrary(rgl)
plot3d(x=data$GPS_Long,y=data$GPS_Lat,z=-data$Depth_m,col = trackcolors,aspect = c(1,1,.1))

loadlibrary(akima)
