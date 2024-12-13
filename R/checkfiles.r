#' This is a function to check the rds file with the shapes has been downloaded.
#' Git LFS is tricky

#' @export
checkfiles<-function(){
  eezfilename=system.file("extdata", "eezs.RDS", package = "myareas")
  if (file.info(eezfilename)$size<10000000 | is.na(file.info(eezfilename)$size)){
    options(timeout = max(600, getOption("timeout")))
    file_url="https://saco.csic.es/s/9ajEG4Ga36mxpNy/download?path=%2F&files=eezs.RDS"
    download.file(file_url, destfile = paste0(system.file("extdata",package = "myareas"), "/eezs.RDS"), mode = "wb")
  }
}
