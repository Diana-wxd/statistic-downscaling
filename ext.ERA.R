getwd()
wd <- "/Users/Diana/Desktop/STATISTIC DOWNSCALING/ERA"
setwd(wd) 
era <- open.ncdf("netcdf-atls10-a562cefde8a29a7288fa0b8b7f9413f7-7KWOc5.nc")

lon <- get.var.ncdf(era,"longitude")
lon <- lapply(lon,"-",360)
lat <- get.var.ncdf(era,"latitude")
tim <- get.var.ncdf(era,"time")
head(tim)
tunits <- att.get.ncdf(era,"time","units")
lunits<- att.get.ncdf(era,"longitude","units")
tustr <- strsplit(tunits$value, " ")
tdstr <- strsplit(unlist(tustr)[3], "-")
tmonth = as.integer(unlist(tdstr)[2])
tday = as.integer(unlist(tdstr)[3])
tyear = as.integer(unlist(tdstr)[1])
chron(tim, origin = c(tmonth, tday, tyear))

cp <- get.var.ncdf(era,"cp")
cp1 <- cp[1,1,]
c <- c()
for (i in c(1:4018)){
  c[i] <- (cp1[2*i-1]+cp1[2*i])/2
}
