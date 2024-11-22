#' Esta funcion asigna una zona FAO dadas las coordenadas.
#'
#' @param lon vector of longitudes
#' @param lat vector of latitudes
#' @param f_level c('MAJOR','SUBAREA','DIVISION','SUBDIVISION','SUBUNIT') Type of FAO area, being "MAJOR" the default.
#' @export
#' @examples
#' get.FAOarea(lon=c(-50,-30,-20,0),lat=c(20,-20,10,-20),f_level='MAJOR')

get.FAOarea<-function(lon,lat,f_level='MAJOR'){
  #f_level:c('MAJOR','SUBAREA','DIVISION','SUBDIVISION','SUBUNIT')
  require(sf)
  FAOareas<-readRDS(system.file("data", 'faoareas.RDS', package = "myareas"))
  
  FAOareas=FAOareas[FAOareas$F_LEVEL==f_level,]
  pnts <- data.frame(x=lon,y=lat)
  pnts_sf <- st_as_sf(pnts, coords = c('x', 'y'),crs=st_crs(FAOareas))
  
  
  ind=st_intersects(pnts_sf,FAOareas)
  
  fixbugs<-function(x){
    if(length(x)>0){x=x[1]}
    if(length(x)==0){x=NA}
    x
    }
  ind=unlist(lapply(ind,fixbugs))
  res=FAOareas$F_CODE[ind]
 res
}
