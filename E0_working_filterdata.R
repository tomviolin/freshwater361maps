
loadlibrary(R.utils)
loadlibrary(tictoc)
# raw data read into sdata ----
#  3.7 MILLION RECORDS!
{ tic()
sdata = data.table::fread("shiphistory_10sec.csv.gz")
toc() }
# convert types to sdatatf (types fixed)  ----
sdatatf = data.frame(list(
  recdate = sdata$recdate,
  GPS_Lat = as.double(sdata$GPS_Lat),
  GPS_Long = as.double(sdata$GPS_Long),
  Temp_C = as.double(sdata$Temp_C),
  Depth_m = as.double((sdata$Depth_m))
))

# filter NA-free data ----
filter = (!is.na(sdatatf$GPS_Lat)) & 
         (!is.na(sdatatf$GPS_Long)) & 
         (!is.na(sdatatf$Temp_C)) & 
         (!is.na(sdatatf$Depth_m))

# print filter info ----

print(length(filter))
print(sum(filter))

# apply filter to sdatanaf (NAs fixed)
sdatanaf = sdatatf[filter,]

print(nrow(sdatanaf))

# check for valid coordinates
sum(sdatanaf$GPS_Lat < 30)

sum(sdatanaf$GPS_Lat > 60)

# example data destructive error!!
#sum(sdatanaf$GPS_Long <-89)

sum(sdatanaf$GPS_Long < -90)
sum(sdatanaf$GPS_Long > -80)

locfilter = ((sdatanaf$GPS_Lat > 30) &
             (sdatanaf$GPS_Lat < 60) &
             (sdatanaf$GPS_Long > -90) &
             (sdatanaf$GPS_Long < -80))

sum(locfilter) 

sdatalocf = sdatanaf[locfilter,]
nrow(sdatalocf)

png("shiphistory.png")
plot(sdatalocf$GPS_Long,sdatalocf$GPS_Lat,
     pch='.',type='p')
dev.off()
system2("open","shiphistory.png")

data.table::fwrite(x=sdatalocf, file="fixedhistory.csv")
#saveRDS(sdatalocf, "fixedhistory.Rds")
