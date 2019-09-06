
require(lubridate)
require(maps)
require(mapdata)
require(maptools)
require(mapproj) 
require(ncdf4)
require(sp)
require(dplyr)
require(devtools)
library(rerddap)
library(plotdap)
library(rerddapXtracto)
# rerddap::cache_delete_all(force = TRUE) # if encouter an error reading the nc file clear the rerrdap cache: 

#west coast OR/WA
xcoord <- c(-123.99, -127.236)
ycoord <- c(48.34, 42.07)
zcoord <- c(0,0)
#just salish?
xcoord2 <- c(-123.73, -122.46)
ycoord2 <- c(47.186, 42.07)

#chlorophyll
dataInfo <- rerddap::info('erdMWchlamday')

parameter <- dataInfo$variable$variable_name #extracts the name from the metadata

global <- dataInfo$alldata$NC_GLOBAL #start and end times
tt <- global[ global$attribute_name %in% c('time_coverage_end','time_coverage_start'), "value", ]
tcoord <- c(tt[2],"last")

chl <- rxtracto_3D(dataInfo, parameter = parameter,
                      tcoord = tcoord, zcoord = zcoord,
                      xcoord = xcoord,ycoord=ycoord)


# spatially average all the data within the box for each dataset  
chl$avg <- apply(chl$chlorophyll, c(4),function(x) mean(x,na.rm=TRUE))

plot(as.Date(chl$time), scale(chl$avg), 
     type='b', bg="blue", pch=21, xlab="", cex=.7,
     xlim=as.Date(c("2002-01-01","2019-01-01")),
  #   ylim=c(10,25),
     ylab="Chlorophyll", main=ttext)

chl_data <- data.frame(date = chl$time, chl = chl$avg)
write.csv(chl_data, file = 'chl_data.csv', row.names = F)

### SST
dataInfo <- rerddap::info('erdMWsstdmday') #about the same? add zcoord if using

parameter <- dataInfo$variable$variable_name

global <- dataInfo$alldata$NC_GLOBAL
tt <- global[ global$attribute_name %in% c('time_coverage_end','time_coverage_start'), "value", ]
tcoord <- c(tt[2],"last")

sst <- rxtracto_3D(dataInfo,parameter=parameter,
                             tcoord=tcoord, zcoord = zcoord,
                             xcoord=xcoord,ycoord=ycoord)

sst$avg <- apply(sst$sst, c(4),function(x) mean(x,na.rm=TRUE))

plot(as.Date(sst$time), scale(sst$avg),
     type='b', bg="blue", pch=21, xlab="", cex=.7,
     xlim=as.Date(c("2002-01-01","2019-01-01")),
     #   ylim=c(10,25),
     ylab="sst")

sst_data <- data.frame(date = sst$time, sst = sst$avg)
write.csv(sst_data, file = 'sst_data.csv', row.names = F)

#### SST salish?

dataInfo <- rerddap::info('erdMWsstdmday') #about the same? add zcoord if using

parameter <- dataInfo$variable$variable_name

global <- dataInfo$alldata$NC_GLOBAL
tt <- global[ global$attribute_name %in% c('time_coverage_end','time_coverage_start'), "value", ]
tcoord <- c(tt[2],"last")

sst2 <- rxtracto_3D(dataInfo,parameter=parameter,
                   tcoord=tcoord, zcoord = zcoord,
                   xcoord=xcoord2,ycoord=ycoord2)

sst2$avg <- apply(sst2$sst, c(4),function(x) mean(x,na.rm=TRUE))

plot(as.Date(sst2$time), scale(sst2$avg),
     type='b', bg="blue", pch=21, xlab="", cex=.7,
     xlim=as.Date(c("2002-01-01","2019-01-01")),
     #   ylim=c(10,25),
     ylab="sst")

sst2_data <- data.frame(date = sst2$time, sst = sst2$avg)
write.csv(sst2_data, file = 'sst2_data.csv', row.names = F)

#prime prod -- all NAs from this source
# dataInfo <- rerddap::info('erdMWpp3day')
# parameter <- dataInfo$variable$variable_name
# global <- dataInfo$alldata$NC_GLOBAL
# tt <- global[ global$attribute_name %in% c('time_coverage_end','time_coverage_start'), "value", ]
# tcoord <- c(tt[2],"last")
# #zcoord = c(0,0)
# 
# pp <- rxtractogon(dataInfo,parameter=parameter,
#                         tcoord=tcoord, zcoord = zcoord,
#                         xcoord=xcoord,ycoord=ycoord)
# 
# pp$avg <- apply(pp$productivity, c(3),function(x) mean(x,na.rm=TRUE))
# 
# plot(as.Date(pp$time), scale(pp$avg),
#      type='b', bg="blue", pch=21, xlab="", cex=.7,
#      #xlim=as.Date(c("2003-01-01","2019-01-01")),
#      #   ylim=c(10,25),
#      ylab="Productivity")

##### sea surface winds ncdcOw6hr


#Mixed layer depth

#copernicus file covers 1/2000 to 11/2018 (12*18yrs + 11 mos == )
path <- paste(getwd(), '/', sep = '')
flist <- list.files(path = path, pattern = '^.*\\.(nc|NC|Nc|Nc)$')
cop <- flist[1]
nc <- nc_open(paste0(path, cop))

# attributes(nc)$names
# attributes(nc$var)$names
# nc$ndims
# nc$natts #netcdf attributes

#data goes from 1/1999 to 12/2017 2017-1999 = 19 years, 12 months each = 228
#mlotst (mixed layer depth, meters)
mld <- ncvar_get(nc, attributes(nc$var)$names[1]) #from -1 to 11 'height' in db

#each matrix slice is a month-year combination, so mean over c(1,2)?
#no clue what dims 1 and 2 are
mld_mean <- apply(mld, c(3), function(x) mean(x, na.rm = TRUE))
#length(mld_mean) == 227

mld_data <- data.frame(mld = c(scale(mld_mean), NA), year = rep(2000:2018, each = 12), 
                       month = rep(1:12, times = 19))

# ggplot(vwnd_data, aes(id, vwnd)) + geom_line()

#zo (absolute/geopotential height, meters)
ssh <- ncvar_get(nc, attributes(nc$var)$names[2])
ssh_mean <- apply(ssh, c(3), function(x) mean(x, na.rm = TRUE))
ssh_data <- data.frame(ssh = c(scale(ssh_mean), NA), year = rep(2000:2018, each = 12), 
                       month = rep(1:12, times = 19))

#to (temp, deg C)
temp <- ncvar_get(nc, attributes(nc$var)$names[3])
temp_mean <- apply(temp, c(3), function(x) mean(x, na.rm = TRUE))
temp_data <- data.frame(temp = c(scale(temp_mean), NA), year = rep(2000:2018, each = 12), 
                        month = rep(1:12, times = 19))


#ugo (geostrophic wind -- zonal/east, m/s)
uwnd <- ncvar_get(nc, attributes(nc$var)$names[4])
uwnd_mean <- apply(uwnd, c(3), function(x) mean(x, na.rm = TRUE))
uwnd_data <- data.frame(uwnd = c(scale(uwnd_mean), NA), year = rep(2000:2018, each = 12), 
                        month = rep(1:12, times = 19))

#vgo (geostrophic wind -- meridional/north, m/s)
vwnd <- ncvar_get(nc, attributes(nc$var)$names[5])
vwnd_mean <- apply(vwnd, c(3), function(x) mean(x, na.rm = TRUE))
vwnd_data <- data.frame(vwnd = c(scale(vwnd_mean), NA), year = rep(2000:2018, each = 12), 
                        month = rep(1:12, times = 19))


copernicus_dat <- vwnd_data %>%
  merge(uwnd_data, by = c('year', 'month')) %>%
  merge(temp_data, by = c('year', 'month')) %>%
  merge(mld_data, by = c('year', 'month')) %>%
  merge(ssh_data, by = c('year', 'month')) 

write.csv(copernicus_dat, file = 'copernicus_dat.csv', row.names = F)

