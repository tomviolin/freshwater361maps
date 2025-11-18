library(leaflet)
data = read.csv("shipdata2025-09-20.csv")
m = leaflet() %>%
    addProviderTiles("CartoDB.Voyager") %>%
    addPolylines(lng=data$GPS_Long, lat=data$GPS_Lat, weight=2)
print(m)
