# Load libraries ----
loadlibrary(osmdata)
loadlibrary(maptiles)
loadlibrary(RColorBrewer)
# Read data ----
data = read.csv("shipdata2025-09-20.csv")
# Establish bounding box for map view ----
bbox = sf::st_bbox(c(xmax=max(data$GPS_Long)+0.001,xmin=min(data$GPS_Long)-0.001,ymax=max(data$GPS_Lat)+0.001,ymin=min(data$GPS_Lat))-0.0001,crs=4623)
# Fetch tiles ----
tiles_map <- get_tiles(x = bbox, provider = "OpenStreetMap",zoom=15, crop=TRUE)
# Output to PDF (off at first) ----
#pdf("shipmap.pdf")
# Plot the tiles ----
plot_tiles(tiles_map)
# Plot the ship track ----
points(data$GPS_Long,data$GPS_Lat,cex=0.2, col='blue')
# Save PDF (off at first) ----
#dev.off() # only if saving to file
#system2("open","shipmap.pdf")

# *** CTRL  -  SHIFT  -  S ***  to run