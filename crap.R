library(sf)
library(maptiles)
nc <- st_read(system.file("shape/nc.shp", package = "sf"), quiet = TRUE)
nc_osm <- get_tiles(nc, crop = TRUE, zoom = 6)
plot_tiles(nc_osm)

# Create a provider from a custom url
osm_tiles <- create_provider(
  name = "osm_tiles",
  #url = "https://tile.openstreetmap.org/{z}/{x}/{y}.png",
  url =  "https://b.tile.opentopomap.org/{z}/{x}/{y}.png",     #17/33235/48247
  citation = "© OpenStreetMap contributors."
)
# Download tiles and compose raster (SpatRaster)
nc_osm2 <- get_tiles(
  x = nc, provider = osm_tiles, crop = FALSE,
  zoom = 5, project = FALSE, verbose = TRUE
)
# Plot the tiles
plot_tiles(nc_osm2)
# Add attribution
mtext(get_credit(osm_tiles), side = 1, line = -1)
