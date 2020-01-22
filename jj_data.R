########################################################################################################
#### NC COUNTIES DATA

# nc_counties <- read.table(file = "clipboard", sep = "\t", header=FALSE, stringsAsFactors = FALSE)
# nc_counties = as.vector(nc_counties$V1)

# write.csv(nc_counties, "C:/Users/jclar/Documents/R/win-library/3.5/jj/data/nc_counties.csv")
# write.csv(nc_counties, "E:/OneDrive/R Codes/Package/data/nc_counties.csv")
# 
# 
# system("mkdir C:/Users/jclar/Documents/R/win-library/3.5/jj/data/")
# file.copy("E:/OneDrive/R Codes/Package/data/nc_counties.csv", "C:/Users/jclar/Documents/R/win-library/3.5/jj/data/")
# nccounties <-read.csv("C:/Users/jclar/Documents/county_pop_centroids.csv")

rm(list=ls(all=TRUE))

nccounties = read.csv("E:/OneDrive/R Codes/Package/data/nc_counties.csv")
setDT(nccounties)
########################################################################################################

########################################################################################################
#### NC STATE CLIMATE OFFICE WEATHER STATION EXCEL SHEET

# ncstns = fread("E:/OneDrive/nc_sco_stations.csv")
# ncstns = ncstns[State=="NC"]
# ncstns$first_ob = ncstns$`First Ob`
# ncstns$last_ob = ncstns$`Most Recent Ob`
# ncstns$`First Ob` = NULL
# ncstns$`Most Recent Ob` = NULL
# 
# ncstns$first_ob = as.Date(ncstns$first_ob, format="%m/%d/%Y")
# ncstns$last_ob = as.Date(ncstns$last_ob, format="%m/%d/%Y")
# 
# nc_sco_stations = ncstns
# saveRDS(nc_sco_stations, "E:/OneDrive/R Codes/Package/data/nc_sco_stations.Rds")
# rm(list=ls(all=TRUE))
nc_sco_stations = readRDS("E:/OneDrive/R Codes/Package/data/nc_sco_stations.Rds")
########################################################################################################

# library(jj)
# data("jj_data")

########################################################################
### PLACE COORDINATES AND ELEVATIONS


adt = data.table(place=c("ch","dh","sh", "coates", "glhs","wfhs","crhs"))
adt$lat=c(35.9495, 35.905572, 35.2550, 35.915158, 35.772919, 35.986214, 36.056056)
adt$lon=c(-79.0473, -78.929542, -81.5190, -79.051444, -78.898802, -78.516339, -79.129879)
adt$elev=c(480,348,863,462,320,310,600)

# green level HS
# 35.772919, -78.898802, 320
# WFHS
# 35.986214, -78.516339, 310
# CRHS
# 36.056056, -79.129879, 600
# Coates
#  35.915158, -79.051444, 462

adt = rbindlist(list(adt, data.table(place=nc_sco_stations$Station, lat=nc_sco_stations$Latitude,
           lon=nc_sco_stations$Longitude, elev=nc_sco_stations$Elevation)))

saveRDS(adt, "E:/OneDrive/R Codes/Package/data/coords.Rds")
coords = readRDS("E:/OneDrive/R Codes/Package/data/coords.Rds")

########################################################################################################
########################################################################################################

save.image(file="E:/OneDrive/R Codes/Package/data/jj_data.RData") 


system("mkdir -p C:/Users/jclar/Documents/R/win-library/3.5/jj/data/")
file.copy("E:/OneDrive/R Codes/Package/data/jj_data.RData", "C:/Users/jclar/Documents/R/win-library/3.5/jj/data/")







