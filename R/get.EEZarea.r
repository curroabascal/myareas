#' This is a function to assign the EEZ area (country or territory) given longitude and the latitude
#' It uses the shapefile from MarineRegions.org (Flanders Marine Institute) 

#' @param londec Longitude in decimal degrees
#' @param latdec Latitude in decimal degrees
#' @return Returns a vector of EEZs.

#' @export
#' @examples
#' get.EEZarea(londec=c(-18,12.588,13.218,16,-15,5,0,-42,-21,-18.3,-24.6,-18),
#' latdec=c(22,-11.2237,-22.99078,-85,-22,-40,-13,15,-15,45,-30,28),type='country')
# 
#' get.EEZarea(londec=c(-18,12.588,13.218,16,-15,5,0,-42,-21,-18.3,-24.6,-18),
#' latdec=c(22,-11.2237,-22.99078,-85,-22,-40,-13,15,-15,45,-30,28),type='territory')

get.EEZarea<-function(londec,latdec,type=c('country')){
  require(sf)
  
  # #To remove
  # load('P:/Pacifico/Rwork/Rprojects/eezareas/eezareas/data/EEZSpatialPolygons.Rdata')
  checkfiles()
  eezs=readRDS(system.file("extdata", 'eezs.RDS', package = "myareas"))
  # data(eezs)
  ###IMPORTANTE
  #ReadOGR puede que no funcione ya. En la función get.ICCATarea_sp utilizo otra librería, para cuando haya que actualizar!!!!
  
  
  
  # countriesSP <- rgdal::readOGR(dsn = "P:/Pacifico/Rwork/ZEE/World_EEZ_v10_20180221", layer = "eez_v10")
  # save(countriesSP,file='P:/Pacifico/Rwork/ZEE/World_EEZ_v10_20180221/EEZSpatialPolygons.Rdata')
  # names(countriesSP)
  # countriesSPlowres <- rgdal::readOGR(dsn = "P:/Pacifico/Rwork/ZEE/World_EEZ_v10_20180221", layer = "eez_boundaries_v10")
  # countriesSPlowres<-countriesSPlowres[countriesSPlowres$Line_type=='200 NM',]
  # save(countriesSPlowres,file='P:/Pacifico/Rwork/ZEE/World_EEZ_v10_20180221/EEZSpatialLinesLowRes.Rdata')
  
  londec[is.na(londec) | is.na(latdec)]<- -999
  latdec[is.na(latdec) | is.na(latdec)]<- -999
  londec[londec>180 & londec!=-999]=londec[londec>180 & londec!=-999]-360
  pnts <- data.frame(x=londec,y=latdec)
  pnts_sf <- st_as_sf(pnts, coords = c('x', 'y'),crs=st_crs(eezs))
  sf_use_s2(FALSE)
  
  areas=st_intersects(pnts_sf,eezs)
  areaslengths=unlist(lapply(areas,length))
  noarea=which(areaslengths==0)
  if (length(noarea) > 0) {
    dummy=sapply(noarea,function(x){areas[[x]] <<- NA})
  }
  if (type=='country'){
  res<-eezs$SOVEREIGN1[unlist(areas)]
  } else if (type=='territory'){
    res<-eezs$TERRITORY1[unlist(areas)]
  }
  res[is.na(res) & londec!=-999 & latdec!=-999]<-'HS'
  
  return(res)
}

