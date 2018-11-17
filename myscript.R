# Test Script -------------------------------------------------------------


#My new script for learning R
x = 1:10

#Add in the packages
x <- c("ggmap", "rgdal", "rgeos", "maptools", "dplyr", "tidyr", "tmap", "geosphere")
#install.packages(x) # warning: uncommenting this may take a number of minutes
lapply(x, library, character.only = TRUE) # load the required packages

#load the first shapes
lnd <- readOGR(dsn = "data/london_sport.shp")


#View the first shape
head(lnd@data, n = 2)

#mean of the first shape
mean(lnd$Partic_Per) # short for mean(lnd@data$Partic_Per) 



# Actual Script -----------------------------------------------------------


#Agency :  Lat = 26.207010 Long = -80.143590
#Random Residence : 26.432510 Long = -80.123050

# creating a sample data.frame with your lat/lon points
lon <- c(-80.143590, -80.123050)
lat <- c(26.207010, 26.432510)
df <- as.data.frame(cbind(lon,lat))


# Miles to meters conversion
mile2meter <- function(x) {
  x * 1609.344
}

# plotting the map with some points on it
ggplot() + geom_polygon(data = usa, aes(x=long, y = lat, group = group)) + 
  coord_fixed(1.3) + geom_point(data = df, aes(x = lon, y = lat, fill = "red", alpha = 0.8), size = 5, shape = 21) +
  guides(fill=FALSE, alpha=FALSE, size=FALSE)

#Begin Geosphere Code for distances

distm(c(-80.143590, 26.207010), c(-80.123050, 26.432510), fun = distHaversine)/ 1609.34




# Load the City List ------------------------------------------------------
US_Data_CSV <- file.choose()
US_Data_CSV <- read.csv(US_Data_CSV)

#Working on detecting differences



US_Data_CSVLatLong <- cbind(US_Data_CSV$lat, US_Data_CSV$lng) 
p <- data.frame(lat = runif(6, -90, 90), 
                lon = runif(6, -180, 180))

# get row indices of pairs
row_pairs <- combn(nrow(p), 2)

# make data.frame of pairs
df_dist <- cbind(x = p[row_pairs[1,],], 
                 y = p[row_pairs[2,],])
# add distance column by calling distHaversine (vectorized) on each pair
df_dist$dist <- geosphere::distHaversine(df_dist[2:1], df_dist[4:3])
df_dist$distMiles <- df_dist$dist / 1609.34

df_dist


