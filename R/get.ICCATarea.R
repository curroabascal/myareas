#' This is a function to assign the ICCAT statistical area and the stock.
#' given the 3-letter FAO code, the longitude and the latitude
#' @param faocode Species FAO code
#' @param londec Longitude in decimal degrees
#' @param latdec Latitude in decimal degrees
#' @param plot T or F
#' @return Returns a matrix with two columns (statistical areas and stocks).

#' @export
#' @examples
#' get.ICCATarea(faocode=c('SKJ','SKJ','SKJ','SKJ','YFT','YFT','YFT','TOT','TOT','SWO','SWO'),
#' londec=c(-20,-22.5,-22.5,160,-15,5,0,-42,-21,-18.3,-24.6),
#' latdec=c(-15,25,25,-85,-22,-40,-13,15,-15,45,-30))


get.ICCATarea<-function(faocode,londec,latdec){
require(sf)
res<-data.frame(nrec=1:length(faocode),area=rep(NA,length(faocode)),stock=rep(NA,length(faocode)))

data(speciesandlayers)
# layers<-system.file("extdata", "ICCAT_gis.gpkg", package = "iccatareas")
notinlist=unique(faocode[!faocode %in% unique(speciesandlayers$Species)])  
if (length(notinlist)>0){
  cat(paste('Species',paste(notinlist,collapse=";"),"not in the list of ICCAT areas. Layer for \"Others\" will be used"))
  faocode[faocode %in% notinlist]='OTH'
}

pnts <- data.frame(x=londec,y=latdec)


  for (sp in unique(faocode)){
    print(sp)
  arealayer<-readRDS(system.file("data", paste0(sp,'.RDS'), package = "myareas"))
  pnts_sf <- st_as_sf(pnts, coords = c('x', 'y'),crs=st_crs(arealayer))
  sf_use_s2(FALSE)
  
  areas=st_intersects(pnts_sf[faocode==sp,],arealayer)
  
  #Check only one area per point
  areaslengths=unlist(lapply(areas,length))

  noarea=which(areaslengths==0)
  if (length(noarea)>0){
    print(paste('Points',paste(res$nrec[faocode==sp][noarea],collapse=";"),'do not fall in any of the areas'))
    dummy=sapply(noarea,function(x){areas[[x]] <<- NA})
  }
  
  twoareas=which(areaslengths>1)
  if (length(twoareas)>0){
    print(paste('Points',paste(res$nrec[faocode==sp][twoareas],collapse=";"),'fall in more than one area- likely just in the border between two. Only the first one is assigned'))
    dummy=sapply(twoareas,function(x){areas[[x]]<<- areas[[x]][1]})
  }
  areas=unlist(areas)
  res$area[faocode==sp]=arealayer$CODE[areas]
  if (sp %in% c('BUM','WHM','SAI','SPF','SWO')){
    res$stock[faocode==sp]=data.frame(arealayer[areas,paste0('stock_',sp)])[,1]
  } else if(sp=='OTH'){
    res$stock[faocode==sp]=arealayer$stock_oth[areas]
  } else {
    res$stock[faocode==sp]=arealayer$stock[areas]
  }
  }
return(cbind(res$area,res$stock))
}
