## ------------------------------------------------------------------------
#' @title plot gages to a weather grid cell
#' @description plot to a weather grid polygon, based on all the files in the mauer daily east data set
#' @export
gage.plot.weather<-function(gages.spatial, weather.grid.poly=NULL, weather.grid.dir="C:/ALR/Data/ClimateData/Mauer/met_poly_GCS", 
                                           weather.dir="C:/ALR/Data/ClimateData/Mauer/daily/east", 
                            proj4="+proj=longlat +ellps=GRS80 +datum=NAD83 +no_defs", plot=F) {
     
     #load polygon representing grid cell for each weather record
     if (is.null(weather.grid.poly)) {
          weather.grid.poly<-readShapePoly(weather.grid.dir,proj4string=CRS(proj4))
          print(".... loaded weather grid polygons")
     }
     
     
     #map each gage location to a weather grid cell
     #  and save the file name of the matching weather timeseries
     gages.spatial$weather.grid.filename <- over( gages.spatial, 
                                                          weather.grid.poly)[,"file_nm"]
     print(".... mapped gages to weather grid cells")
     
     gages.spatial$weather.grid.filename<-as.character(gages.spatial$weather.grid.filename) 
                    #I can't remember why this was necessary, but leaving it in for now.. a factor issue?
     
     if (sum(is.na(gages.spatial$weather.grid.filename))>0)
         warning(paste(is.na(gages.spatial$weather.grid.filename), "gages were unable to map to weather grid cell\n", 
                       gages.spatial[is.na(gages.spatial$weather.grid.filename), "weather.grid.filename"],collapse=""))
     
     
     print(paste(length(unique(gages.spatial$weather.grid.filename)),"unique weather files to be used for",nrow(gages.spatial),"flow gages"))    
     if ( plot ) {
          print(".... plotting")
          plot(gages.spatial,col="red")
          plot(weather.grid.poly,border="blue",add=T)
          
     }
     

     return(gages.spatial)
     
     
}



