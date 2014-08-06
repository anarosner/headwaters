## ------------------------------------------------------------------------
#' @title plot gages to weather grid
#' @description plot to a weather grid polygon, based on all the files in the mauer daily east data set
#' @export
gage.plot.weather<-function(gages.spatial, 
                            plot=F) {
     
     
     #map each gage location to a weather grid cell
     #  and save the file name of the matching weather timeseries
     print("Mapping gages to weather grid cells...")
     temp<-over( gages.spatial,weather.grid.poly )
#      gages.spatial[ , c("weather.filename","region") ] <- 
#           over( gages.spatial,weather.grid.poly )[ , c("weather.filename","region") ]
     gages.spatial$weather.filename <- temp$weather.filename
     gages.spatial$region <- temp$region
     print("Completed mapping gages to weather grid cells...")
          
     gages.spatial$weather.filename<-as.character(gages.spatial$weather.filename) 
                    #I can't remember why this was necessary, but leaving it in for now.. a factor issue?
     
     if (sum(is.na(gages.spatial$weather.filename))>0)
         warning(paste(is.na(gages.spatial$weather.filename), "gages were unable to map to weather grid cell\n", 
                       gages.spatial[is.na(gages.spatial$weather.filename), "weather.filename"],collapse=""))
     
     
     print(paste(length(unique(gages.spatial$weather.filename)),"unique weather files to be used for",nrow(gages.spatial),"flow gages"))  
     
     if ( plot ) {
          print("Drawing plot...")
          plot(gages.spatial,col="red")
          plot(weather.grid.poly,border="blue",add=T)
     }
     
     return(gages.spatial)
     
}



