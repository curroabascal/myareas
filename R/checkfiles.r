#' This is a function to check the rds file with the shapes has been downloaded.
#' Git LFS is tricky

#' @export
checkfiles<-function(){
  eezfilename=system.file("extdata", "eezs.RDS", package = "myareas")
  if (file.info(eezfilename)$size<10000000){
    file_url="https://github.com/curroabascal/myareas/raw/refs/heads/master/inst/extdata/eezs.RDS?download="
    download.file(file_url, destfile = eezfilename,mode='wb')
  }
}