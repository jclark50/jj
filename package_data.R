











c <- read.table("E:/OneDrive/Shapefiles/us_counties/nc_counties_groups_20181201.txt", 
                        sep=",", header=TRUE)
colnames(c)

setDT(c)


c <- c[order(c$COUNTYFP),] 
c$COOCC = as.factor(c$COOCC)
levels(c$COOCC) = seq(1, 100, by=1)
c$COOCC=as.integer(as.character.factor(c$COOCC))

c[, COUNTYFP_1 := NULL]
c[, LATITUDE := NULL]
c[, LONGITUDE := NULL]
c[, INTPTLAT  := NULL]
c[, INTPTLON := NULL]
c[, FID := NULL]
c[, GEOID := NULL]

c$cnty_group = c$group
c$cnty_num = c$COUNTYFP
c$cnty_name = c$NAME

c[, group := NULL]
c[, COUNTYFP := NULL]
c[, NAME := NULL]

c = setcolorder(c, c("cnty_num", "COOCC", "cnty_name", "Shape_Area", "pop_2010", "lat_pop",
                     "lon_pop", "lat_county", "lon_county", "cnty_group"))

write.table(c, "C:/Users/jclar/Documents/R/win-library/3.5/jj/data/nccounties.txt")

###############################