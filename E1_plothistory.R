# Load libraries ----
loadlibrary(osmdata)
loadlibrary(maptiles)
loadlibrary(RColorBrewer)
loadlibrary(data.table)
# Read data ----
dataraw = fread("fixedhistory.csv") #[1:800000,]
data = dataraw[
  (dataraw$GPS_Lat > 43.015) &
  (dataraw$GPS_Lat < 43.030) &
  (dataraw$GPS_Long < -87.88),]
# Establish bounding box for map view ----
bbox = sf::st_bbox(c(xmax=max(data$GPS_Long)+0.001,xmin=min(data$GPS_Long)-0.001,ymax=max(data$GPS_Lat)+0.001,ymin=min(data$GPS_Lat))-0.0001,crs=4623)
# Fetch tiles ----
tiles_map <- get_tiles(x = bbox, provider = "OpenStreetMap",zoom=16, crop=TRUE)
# Output to PDF (off at first) ----
pdf("shipmap.pdf")
# Plot the tiles ----
plot_tiles(tiles_map)
# Plot the ship track ----
points(data$GPS_Long,data$GPS_Lat,pch='o',cex=0.09, col='blue', bg="#00000000")
# Save PDF (off at first) ----
dev.off() # only if saving to file
system2("open","shipmap.pdf")

# *** CTRL  -  SHIFT  -  S ***  to run