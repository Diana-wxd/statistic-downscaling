# Plot grid points for the reanalysis data

# Load the package
library("ggplot2", lib.loc="~/Library/R/3.2/library")
library("ggmap", lib.loc="~/Library/R/3.2/library")

# plot using ggmap
gridp <- data.frame(expand.grid(lon,lat))

mapImageData <- get_map(
  location =c(-97,35),
  zoom = 7,
  source="stamen",
  maptype = "terrain",
  color="bw"
)

ggmap(mapImageData,extent = "normal")+
  geom_point(aes(x= gridp$lon,y=gridp$lat),data=gridp,colour="red",size=2)

# map from maps
library(maps)
okmap <- map_data("state",region="oklahoma")
p <- ggplot()
 p<- p+ geom_polygon( data=okmap, aes(x=long, y=lat, group = group),colour="white", fill="#000099" )
 p<- p+ geom_point(data=gridp,aes(x=gridp$lon,y=gridp$lat),size=2,colour="red")

 # ERA grid points  European Centre for Medium-Range Weather Forecasts (ECMWF) 
 lon <- c(-98.25,-97.5,-96.75,-96)
 lat <- c(33.75,34.5,35.25,36)
 gridpoints1 <- data.frame(expand.grid(lon,lat))
 names(gridpoints1) <- c("lon","lat")
 gridpoints1$source <- rep("ERA")
 
 # NARR grid points
 x <- c(201,202,203,204,205)
 y <- c(92,93,94,95,96)
 gd <- expand.grid(x,y)
 lon<- mapply(function(x,y) long[x,y],gd$Var1,gd$Var2)
 lat <-mapply(function(x,y) lati[x,y],gd$Var1,gd$Var2)
 gridpoints2 <- data.frame(cbind(lon,lat))
 names(gridpoints2) <- c("lon","lat")
 gridpoints2$source<-rep("NARR")
 # MERRA renalysis from NASA
 lon <- c(-98,-97.33,-96.67,-96)
 lat <- c(34.5,35,35.5,36)
 gridpoints3 <- data.frame(expand.grid(lon,lat))
 names(gridpoints3) <- c("lon","lat")
 gridpoints3$source <- rep("MERRA")

 # combine the reanalysis grid data
 reanalysgrid <- rbind(gridpoints1,gridpoints2,gridpoints3)
 # plot
 library(maps)
 okmap <- map_data("state",region="oklahoma")
 p <- ggplot()
 p<- p+ geom_polygon( data=okmap, aes(x=long, y=lat, group = group),colour="black", fill="cornflowerblue" )
 p<- p+ geom_point(data=gridpoints1,aes(x=gridpoints1$lon,y=gridpoints1$lat),size=2,colour="black")

 p <- p+ annotate("point",x=-97.43,y=35.18,shape=3,colour="red",size=2)
 