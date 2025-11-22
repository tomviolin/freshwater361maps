# Load libraries
loadlibrary(osmdata)
loadlibrary(maptiles)
loadlibrary(RColorBrewer)
loadlibrary(plotrix)
loadlibrary(sf)
loadlibrary(data.table)
loadlibrary(sqldf)
loadlibrary(devtools)
loadlibrary(tcltk)

devtools::install_github("GuangchuangYu/emojifont")

loadlibrary(showtext)
loadlibrary(emojifont)
#loadlibrary(remoji)


"+" = function(x,y) {
  if(is.character(x) || is.character(y)) {
    return(paste(x , y, sep=""))
  } else {
    .Primitive("+")(x,y)
  }
}


# Automatically enable showtext for plotting
showtext.auto()

# Register the emoji font
#font_add_google("Open Sans Emoji", "emoji") 


# # Example plot
# set.seed(123)
# plot(1, type = "n", xlim = c(0, 10), ylim = c(0, 10), 
#      xlab = "X-axis", ylab = "Y-axis")
# 




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
bbox_data[3] = bbox_data[3]+0.02
bbox_data[1] = bbox_data[1]-0.002

data = data.table(alldata[
  (alldata$GPS_Lat >= bbox_data[2]) &
  (alldata$GPS_Lat <= bbox_data[4]) &
  (alldata$GPS_Long >= bbox_data[1]) &
  (alldata$GPS_Long <= bbox_data[3])
,])

data$recdate = as.POSIXct(data$recdate)


data=data[
  (data$recdate > as.POSIXct("2007-01-01")) &
  (data$recdate < as.POSIXct("2026-01-01")) &
  (data$Temp_C > 2.5)  &
  (data$Temp_C < 50)
  
  ,]

# Establish the bounding box (the coordinates of a rectangle that defines the map)  +/- 0.001 for margin
map_bbox = sf::st_bbox(c(
  xmin=min(data$GPS_Long)  -0.004,
  ymin=min(data$GPS_Lat)   -0.004,
  xmax=max(data$GPS_Long)  +0.004,
  ymax=max(data$GPS_Lat)   +0.004
  ),crs=4623)

#map_bbox = bb

# https://api.maptiler.com/maps/satellite/?key=cXP04RikwFXmuwI7jTM7#1.0/0.00000/0.00000

sat_provider <- create_provider(name="maptiler satellite",url="https://api.maptiler.com/maps/satellite/?key=cXP04RikwFXmuwI7jTM7#{z}/{x}/y}", citation="copyright maptiler.com")



opentopomap <- create_provider(
  name = "otm",
  url = "https://api.maptiler.com/maps/satellite/{z}/{x}/{y}@2x.jpg?key=cXP04RikwFXmuwI7jTM7",
  sub = c("a", "b", "c"),
  citation = "map data: © OpenStreetMap contributors, SRTM | map style: © OpenTopoMap (CC-BY-SA)"
)

#get tiles
tiles_map <- get_tiles(x = map_bbox, provider = opentopomap, zoom=15, crop=TRUE)


temps = data$Temp_C         # temps is in range min to max
temps = temps - min(temps)  # now temps is in range 0 to max-min
temps = temps / max(temps)  # now temps is in range 0 to 1

trackcolors = cpal[floor(temps*NC*0.999+1)]
#plot(data$GPS_Long,data$GPS_Lat,cex=.2,col=trackcolors, main="Ship Data 9/6/2025",asp=1) # ship track

pdf("shipgap.pdf",width = 20,height=10,family = "Courier")  # also jpeg() png()  pdf() 


plot_tiles(tiles_map, add = FALSE) # Plot the tiles
#points(data$GPS_Long,data$GPS_Lat,cex=.3,col=rgb(0,0,1,0.5), main="Ship Data 9/6/2025",asp=1) # ship track
points(data$GPS_Long,data$GPS_Lat,cex=.2,col=trackcolors, main="Ship Data 9/6/2025",asp=1) # ship track
#points(range(data$GPS_Long),range(data$GPS_Lat),cex=2,col='red')

# lines(
#   map_bbox[c(1,3)],
#   map_bbox[c(2,4)],
#   type='l',
#   col='red')



legendplaces = rep(NULL,NC)
leglabels = seq(
  round(min(data$Temp_C),1),
  round(max(data$Temp_C),1),
  by=2)

  

legpos = floor(1+NC*(leglabels - min(data$Temp_C)) / diff(range(data$Temp_C)))
legendplaces[legpos] = leglabels

legendbbox = c(
  xmin=map_bbox[1],
  ymin=map_bbox[2],
  xmax=map_bbox[3],
  ymax=map_bbox[4]
)




color.legend(yt=quantile(legendbbox[c(2,4)],0.99),
             yb=quantile(legendbbox[c(2,4)],0.05),
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
  as.POSIXct(
    paste0(
      substr(noaa_begin,1,10),
      " ",
      substr(noaa_begin,12,16),":00"
    )
    ,format="%Y-%m-%d %H:%M:%S",
    tz="America/Chicago"
  )
}

mindate = min(data$recdate)
maxdate = max(data$recdate)


noaawdir = fread("open.NOAA-ISD.726400-14839.WIND_DIRECTION.csv")
noaawspd = fread("open.NOAA-ISD.726400-14839.WIND_SPEED_RATE.csv")

noaawdir$recdate = noaa_begin_to_POSIXct(noaawdir$begin)
noaawspd$recdate = noaa_begin_to_POSIXct(noaawspd$begin)

noaawdirfilter =  (noaawdir$recdate >= mindate) &
                  (noaawdir$recdate <= maxdate)
noaawdir = noaawdir[noaawdirfilter,]

noaawspdfilter =  (noaawspd$recdate >= mindate) &
  (noaawspd$recdate <= maxdate)
noaawspd = noaawspd[noaawspdfilter,]

noaawdir$rechour = floor(as.numeric(noaawdir$recdate) / (60*60))*(60*60)
noaawspd$rechour = floor(as.numeric(noaawspd$recdate) / (60*60))*(60*60)

noaawind= sqldf("
  select
    noaawspd.rechour as wspdhour, 
    datetime(min(noaawspd.recdate),'unixepoch') as datemin,
    datetime(max(noaawspd.recdate),'unixepoch') as datemax,
    noaawspd.v as wspd,
    noaawdir.v as wdir
  from 
    noaawspd
  LEFT JOIN noaawdir
  ON noaawspd.rechour = noaawdir.rechour 
  group by wspdhour")


data$recdate = as.POSIXct(data$recdate)

data$rechour = floor(as.numeric(data$recdate) / (60*60)) * (60*60)

datawind = sqldf("SELECT * from (
                SELECT 
                  data.rechour as dhour, 
                  count(*) as N,
                  datetime(min(data.recdate),'unixepoch') as recdate_min,
                  datetime(max(data.recdate),'unixepoch') as recdate_max,
                  avg(data.GPS_Long) as avg_long, 
                  avg(data.GPS_Lat) as avg_lat,
                  min(data.GPS_Long) as min_long,
                  max(data.GPS_Long) as max_long,
                  min(data.Temp_C) as min_temp_c,
                  max(data.Temp_C) as max_temp_c, 
                  max(data.Temp_C) - min(data.Temp_C) as diff_temp_c,
                  data.Depth_m,
                  wdir,wspd
                FROM data 
                LEFT JOIN 
                 noaawind ON noaawind.wspdhour = data.rechour
                GROUP BY data.rechour
                ORDER BY diff_temp_c DESC)
                WHERE N > 15 and
                      (max_long-min_long > ( " + as.character(bbox_data[3]-bbox_data[1]) + ")*0.7 )
                ")

# FILTER FOR CANDIDATES

# select good candidates to demonstrate upwelling
# filter selecting only continuously increasing or continuously decreasing sets
upwelling_indexlist = c()
for (i in 1:nrow(datawind)) {
  datasub = data[data$rechour == datawind[i,]$dhour,]
  # is datasub longitude continuously increasing?
  longs = datasub$GPS_Long
  if ((sum(diff(longs)>0) == length(longs)-1) |
      (sum(diff(longs)<0) == length(longs)-1)) {
    upwelling_indexlist = append(upwelling_indexlist, c(i))
  }
}

datawind = datawind[c(upwelling_indexlist),]



# filter based on observed relative maxima in size of candidate sets
# between 47 and 56.
#datawind = datawind[(datawind$N >47) & (datawind$N < 56),]


h=hist(datawind$N)
text(x = h$mids, y = h$counts, labels = h$counts, pos = 3, offset = -1.0)


system("sleep 1",wait=TRUE)


result = as.character(tk_messageBox("okcancel","Proceed?"))
if (result != "ok") break
for (i in 1:nrow(datawind)) {
  datasub = data[data$rechour == datawind[i,]$dhour,]
  
  print(nrow(datasub))
  temps = datasub$Temp_C         # temps is in range min to max
  temps = temps - min(temps)  # now temps is in range 0 to max-min
  temps = temps / max(temps)  # now temps is in range 0 to 1
  
  trackcolors = cpal[floor(temps*NC*0.999+1)]
  
  pdf("shipgap.pdf",width = 20,height=10,family = "Courier")  # also jpeg() png()  pdf() 
  
  {
  plot_tiles(tiles_map, add = FALSE) # Plot the tiles
  tchans = col2rgb(trackcolors,alpha = T)/255
  tca = rgb(tchans[1,],tchans[2,],tchans[3,],0.4)
  #points.default(datasub$GPS_Long,datasub$GPS_Lat,cex=3,pch=21,col=tca,lwd=0,bg=tca ) # background for temps
  }
  #text(datasub$GPS_Long,datasub$GPS_Lat,cex=0.6,labels=as.character(round(datasub$Temp_C,1)),family="emoji",col="#ffffff88",) #trackcolors) # ship track
  text(datasub$GPS_Long,datasub$GPS_Lat,cex=1.8,labels=emoji("ship"),family="EmojiOne",col=trackcolors,adj=c(0.5,0)) # ship track
  text(datasub$GPS_Long,datasub$GPS_Lat,cex=0.7,labels=as.character(round(datasub$Temp_C,1)),family="Courier",col=trackcolors,adj=c(0.5,1.6)) # ship track
  # Plot emojis as text
  # text(rnorm(10, 5, 1), rnorm(10, 5, 1), emoji("grinning_face"), 
  #     col = "blue", cex = 3, family = "emoji")
  
  # plot temperature
  
  lines (datasub$GPS_Long,bbox_data[4]+(datasub$Temp_C/28.0)*(map_bbox[4]-bbox_data[4]),col="white")
  points(datasub$GPS_Long,bbox_data[4]+(datasub$Temp_C/28.0)*(map_bbox[4]-bbox_data[4]),col=trackcolors)
  segments(datasub$GPS_Long, bbox_data[4], datasub$GPS_Long, bbox_data[4]+(datasub$Temp_C/28.0)*(map_bbox[4]-bbox_data[4]),col=trackcolors)
  lines(bbox_data[c(1,3)],
        bbox_data[c(4,4)], col="#DDDDFF")
  
  pdepth = datasub$Depth_m
  pdmax =  max(pdepth)
  pdnorm = pdepth / max(pdepth)  # range of 0..1
  
  plotd = bbox_data[2] - (bbox_data[2] - map_bbox[2]) * (pdnorm*0.5)
  polygon(c(datasub$GPS_Long, rev(datasub$GPS_Long)), c(plotd,rep(map_bbox[2],length(plotd))), lwd=3, col="#654321")
  lines(datasub$GPS_Long, rep(bbox_data[2],length(datasub$GPS_Long)),col="blue")
  
  #points(range(data$GPS_Long),range(data$GPS_Lat),cex=2,col='red')
  
  # lines(
  #   map_bbox[c(1,3)],
  #   map_bbox[c(2,4)],
  #   type='l',
  #   col='red')
  
  
  
  legendplaces = rep(NULL,NC)
  leglabels = seq(
    round(min(datasub$Temp_C),1),
    round(max(datasub$Temp_C),1),
    by=2)
  leglabels[leglabels<min(datasub$Temp_C)]=datasub$Temp_C
  
  
  legpos = floor(1+NC*(leglabels - min(datasub$Temp_C)) / diff(range(datasub$Temp_C)))
  legendplaces[legpos] = leglabels
  
  legendbbox = c(
    xmin=map_bbox[1],
    ymin=map_bbox[2],
    xmax=map_bbox[3],
    ymax=map_bbox[4]
  )
  
  color.legend(yt=quantile(legendbbox[c(2,4)],0.99),
               yb=quantile(legendbbox[c(2,4)],0.05),
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
  #system("evince shipgap.pdf;sleep 3", wait=F)

  result = tkmessageBox(title="Continue?",
                        message="Do you want the next one?",
                        icon='question',
                        type='yesno')
    if (as.character(result) != 'yes') {
    break
  }
  


}
