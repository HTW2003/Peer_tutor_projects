#continuing research with the code Sloth Squad has written, but with foxes
#red fox, swift fox, and arctic fox
#Hanora Woodruff: 7/22/19 - 

#loading in pre-installed libraries (cited from variegatus_code.R)
library(spocc)
library(rgeos)
library(sp)
library(raster)
library(dismo)
library(ggmap)
library(spThin)
library(ENMeval)
library(rgdal)
#from spocc tutorial
knitr::opts_chunk$set(echo = TRUE)

#using occ() to retrieve fox occurence data from gbif
#limits approximated according to gbif (red fox had to be significantely cut -> from 400000)
red_occ <- occ(query = "Vulpes vulpes", from = 'gbif', limit = 20000)
swift_occ <- occ(query = "Vulpes velox",  from = 'gbif', limit = 250)
arctic_occ <- occ(query = "Vulpes lagopus", from = 'gbif', limit = 11000)

#converting the occurence data into data frames
red_df <-occ2df(red_occ)
swift_df <- occ2df(swift_occ)
arctic_df <- occ2df(arctic_occ)
View(red_df)
View(swift_df)
View(arctic_df)

#Set the API key again
api_key = "AIzaSyBK7lLbqoqnYFdzf-idYYposb-1gwyRAlQ"
register_google(key = api_key)

#Make dataframe:
world_map <- map_data("world")

#Assign the world map plot to the variable "world"
world <- ggplot() + 
  geom_polygon(data = world_map, aes(x=long, y = lat, group = group), fill = "grey", color = "darkgrey")
#plot the world map
world

#plotting species occurence points (one plot for each species)
#red fox
world +
  geom_point(data = red_df, aes(x = longitude, y = latitude),
             color = "red",
             size = 1)
#swift fox
world +
  geom_point(data = swift_df, aes(x = longitude, y = latitude),
             color = "green",
             size = 1)

#s.fox pop is so limited that a bounding box can be made
#swift_box <- make_bbox(lon = swift_df$longitude, lat = swift_df$latitude, f = 2)
#Get a satellite map at the location of the bounding box you made:
#swift_bbox_map <- get_map(location = swift_box, maptype = "satellite", source = "google")

#arctic fox
world +
  geom_point(data = arctic_df, aes(x = longitude, y = latitude),
             color = "blue",
             size = 1)

#data cleaning
#from sloth cleaning rmds
#skipping subsetting for now due to wide ranges of 2 species
#red fox
latlong_red <- c(3,2) #selecting lat and lon columns
red_df_ll <- red_df[latlong_red]
View(red_df_ll)
red_cleaned <- na.omit(red_df_ll)#getting rid of na values in lat and lon
View(red_cleaned)

#swift fox
latlong_swift <- c(3,2) #selecting lat and lon columns
swift_df_ll <- swift_df[latlong_swift]
View(swift_df_ll)
swift_cleaned <- na.omit(swift_df_ll)#getting rid of na values in lat and lon
View(swift_cleaned)

#arctic fox
latlong_arctic <- c(3,2) #selecting lat and lon columns
arctic_df_ll <- arctic_df[latlong_arctic]
View(arctic_df_ll)
arctic_cleaned <- na.omit(arctic_df_ll)#getting rid of na values in lat and lon
View(arctic_cleaned)

#Making bounding boxes and applying them to maps of occurrence points
#(have not yet thinned)
#zoom is a necessary arg.

#red fox -> will have to deal with Australia
#current issue: points are to big compared to the zoom of the map (which has to stay at 1 to include all the points)
red_bbox <- make_bbox(lon = red_cleaned$longitude, lat = red_cleaned$latitude, f = 2)
red_map <- get_map(location = red_bbox, source = "google", maptype = "satellite", zoom = 1)

ggmap(red_map) +
  geom_point(data = red_cleaned, aes(x = longitude, y = latitude),
             color = "red",
             size = 1)
#after arctic fox polygon did not work, I am trying red fox as a control
red_fox_polygon <- read.csv("red fox polygon coordinates.csv")
View(red_fox_polygon)

red_poly <- SpatialPolygons(list(Polygons(list(Polygon(red_fox_polygon)), ID=2)))

ggmap(red_map) +
  geom_point(data = red_cleaned, aes(x=longitude, y = latitude), color = "red", size = 1) +
  geom_polygon(data = red_poly,
               aes(x = long, y = lat), fill = NA, color = "blue", size = 1)

red_points <- SpatialPoints(red_cleaned)
View(red_points)

red_intersect <- over(red_points, red_poly)#where the dang error occcurs
View(red_intersect)

red_inter_rowNums <- as.numeric(which(!(is.na(red_intersect))))
View(red_inter_rowNums)

red_cleaned <- na.omit(red_cleaned, cols=c("longitude", "latitude"))
View(red_cleaned)
red_poly_occs <- red_cleaned[red_inter_rowNums,]
View(red_poly_occs)

ggmap(red_map) + 
  geom_point(data = red_poly_occs, aes(x=longitude, y = latitude), color = "red", size = 1) +
  geom_polygon(data = red_poly,
               aes(x = long, y = lat), fill = NA, color = "blue", size = 1)

#swift fox
swift_bbox <- make_bbox(lon = swift_cleaned$longitude, lat = swift_cleaned$latitude, f = 2)
swift_map <- get_map(location = swift_bbox, source = "google", maptype = "satellite", zoom = 5)

ggmap(swift_map) +
  geom_point(data = swift_cleaned, aes(x = longitude, y = latitude),
             color = "green",
             size = 1)

#arctic fox
#need to get rid of the suspicious points (in notes)
arctic_bbox <- make_bbox(lon = arctic_cleaned$longitude, lat = arctic_cleaned$latitude, f = 2)
arctic_map <- get_map(location = arctic_bbox, source = "google", maptype = "satellite", zoom = 1)

ggmap(arctic_map) +
  geom_point(data = arctic_cleaned, aes(x = longitude, y = latitude),
             color = "blue",
             size = 1)


#trying spatial polygon to get rid of sus points 
arctic_fox_polygon <- read_csv("arctic fox polygon coordinates - Sheet1.csv")
arctic_fox_polygon <- na.omit(arctic_fox_polygon)
View(arctic_fox_polygon)

arctic_poly <- SpatialPolygons(list(Polygons(list(Polygon(arctic_fox_polygon)), ID=1)))

ggmap(arctic_map) +
  geom_point(data = arctic_cleaned, aes(x=longitude, y = latitude), color = "blue", size = 1) +
  geom_polygon(data = arctic_poly,
               aes(x = long, y = lat), fill = NA, color = "red", size = 1)

#converting occurrences to spatial points to intersect with polygon
arctic_points <- SpatialPoints(arctic_cleaned)
View(arctic_points)
arctic_intersect <- over(arctic_points, arctic_poly)
View(arctic_intersect)

#find the row numbers of the occurrence points that fall within the polygon:
arc_inter_rowNums <- as.numeric(which(!(is.na(arctic_intersect))))
View(arc_inter_rowNums)

#Create a new dataframe of sloth occurrences containing only occurrences within your polygon
arctic_cleaned <- na.omit(arctic_cleaned, cols=c("longitude", "latitude"))
View(arctic_cleaned)
arctic_poly_occs <- arctic_cleaned[arc_inter_rowNums,]
View(arctic_poly_occs)

ggmap(arctic_map) +
  geom_point(data = arctic_poly_occs, aes(x=longitude, y = latitude), color = "blue", size = 1) +
  geom_polygon(data = arctic_poly,
               aes(x = long, y = lat), fill = NA, color = "red", size = 1)



