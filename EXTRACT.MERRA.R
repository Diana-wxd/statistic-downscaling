getwd()
wd <- "/Users/Diana/Desktop/STATISTIC DOWNSCALING /MERRA raw"
setwd(wd)
T <- open.ncdf("MERRA100.prod.assim.inst3_3d_asm_Cp.19790101.SUB.nc")

library(maps)
m <- map("usa",plot=FALSE)
map("usa",project="albers",par=c(39,45))
map.grid(m)

# try with set 1 variable- First change directory
wd <- "/Users/Diana/Desktop/STATISTIC DOWNSCALING /MERRA raw/variable set 2"
setwd(wd)

# load the package
library(ncdf)

# open one file to examine the data structure
temp <- open.ncdf("MERRA100.prod.assim.tavg1_2d_slv_Nx.19800101.SUB.nc")
print(temp)

# access the names of the variables
names(temp$var)

# get the longitude and latitude
lon <- get.var.ncdf(temp,"longitude")
lat <- get.var.ncdf(temp,"latitude")
slp <- get.var.ncdf(temp,"slp")[124:127,250:253]
# Start building the function
##### FUNCTION STARTS HERE####
ext.MERRA <- function(filenames,dates){
  library(ncdf)
  # get general information using the first piece of data 
  d1 <- dates[1] # first date
  filename1 <- paste(filenames,".",d1,".SUB.nc",sep="") # create the full file name
  data <- open.ncdf(filename1) # open the ncdf file
  lon <- get.var.ncdf(data,"longitude") # get the longitude values
  lonmin <- which(lon=="-98")
  lonmax <- which(lon=="-96")
  lat <- get.var.ncdf(data,"latitude") # get the latitude values
  latmin <- which(lat=="34.5")
  latmax <- which(lat=="36")
  lonlatgrid <- expand.grid(lon[lonmin:lonmax],lat[latmin:latmax])
  varnames <- names(data$var) # get variable names in the data
  vectors <- c() # create an empty vector to store names for different variables
 # extract every variable then store every value at each point as a vector
  # note that a series of file have the same variables
 for (vname in varnames){
    var.value <- get.var.ncdf(data,vname)[lonmin:lonmax,latmin:latmax]
    vct <- as.vector(var.value)
    df <- cbind(lonlatgrid,vct)
   for ( i in c(1:nrow(df))){
     assign(paste(vname,round(df[i,1],1),round(df[i,2],1),sep=""),df[i,3])
     vectors <- c(vectors, as.character(paste(vname,round(df[i,1],1),round(df[i,2],1),sep="")))
   }
 }
  

# based on the structure for each file store the data using the same way  
  for(d in dates[-1]){
    filename <- paste(filenames,".",d,".SUB.nc",sep="")
    data <- open.ncdf(filename)
    for (vname in varnames){
      id <- which(varnames==vname)
      var.value <- get.var.ncdf(data,vname)[lonmin:lonmax,latmin:latmax]
      vct <- as.vector(var.value)
      for (i in c(1:length(vct))){
        assign(vectors[(id-1)*16+i],c(get(vectors[(id-1)*16+i]),vct[i]))
      }    
    }    
  }
 curEnv=environment() # it is vital tp specify the environment for the get function
  predictors <- data.frame(sapply(vectors,get,envir=curEnv))
return(predictors)
}
##### FUNCTION END HERE######

# start to use this function
# generate dates

dates<-seq(as.Date("1979/01/01"),as.Date("2010/12/31"),by="day")
# format the date
dates <- lapply(dates,format,format="%Y%m%d")
vardata <- ext.MERRA("MERRA100.prod.assim.tavg1_2d_flx_Nx",dates)

write.csv(vardata,"set2.1979.csv")
chan <- function(x) {
  for (i in x)
  {assign(i,sample(1:10,2))} 
  curEnv=environment()
  out <- sapply(x,get,envir=curEnv)
  out
}