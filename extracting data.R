#extract ncdf data

##### the following code is to check some of the information of the data
pcp1979 <- open.ncdf("acpcp.1979.nc")

print(pcp1979)
x <- get.var.ncdf(pcp1979,"lon")
nx <- dim(x)
head(x)
y <- get.var.ncdf(pcp1979,"lat")
ny <- dim(y)
t <- get.var.ncdf(pcp1979,"time")
att <- att.get.ncdf(pcp1979,"time","units")
pcparr <- get.var.ncdf(pcp1979,"acpcp")
slice <- pcparr[,,1]
image(x,y,slice)
slice2 <- pcparr[202,94,]
p1 <- as.vector(slice2)
p1
######################################### Here starts the general
# extrat process
# change to the working directory
getwd()
wd <- "/Users/Diana/Desktop/document"
setwd(wd)
# the variable name
varname <- "acpcp"

# years of data
year <- seq(from=1979,to=2014,by=1)

# a vector to store the data in one vector
vector = c()
for( year in year){
  filename <- paste(varname,".",year,".nc",sep="")
  data <- open.ncdf(filename)
  array <- get.var.ncdf(data,varname)
  slice <- as.vector(array[202,94,])
  vector <- c(vector,slice)
}

## to build in a function###
## this function can extract the data from NARR reanalysis
## which the file name is like" varname.year.nc"

EtractNARR <- function(varname,extent,year,x,y){
  vector = c()
  for( year in year){
    filename <- paste(varname,".",extent,".",year,".nc",sep="")
    data <- open.ncdf(filename)
    array <- get.var.ncdf(data,varname)
    slice <- as.vector(array[x,y,])
    vector <- c(vector,slice)
  }
  return(vector)  
}

