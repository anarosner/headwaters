
## ----voronoi function----------------------------------------------------
#' @title voronoi polygons
#' @export
#voronoi function by Carson Farmer
#http://www.carsonfarmer.com/2009/09/voronoi-polygons-with-r/
# To create a nice bounded Voronoi polygons tessellation of a point layer in R, we need two libraries: sp and deldir. 
# The following function takes a SpatialPointsDataFrame as input, and returns a SpatialPolygonsDataFrame 
# that represents the Voronoi tessellation of the input point layer.

voronoipolygons = function(layer) {
     crds = layer@coords
     z = deldir(crds[,1], crds[,2])
     w = tile.list(z)
     polys = vector(mode='list', length=length(w))
     for (i in seq(along=polys)) {
        pcrds = cbind(w[[i]]$x, w[[i]]$y)
        pcrds = rbind(pcrds, pcrds[1,])
        polys[[i]] = Polygons(list(Polygon(pcrds)), ID=as.character(i))
     }
     SP = SpatialPolygons(polys,proj4string=layer@proj4string)
     
     voronoi = SpatialPolygonsDataFrame(SP, 
                                        data=data.frame( x=z$summary$x, y=z$summary$y, 
                                             row.names=sapply(slot(SP, 'polygons'), function(x) slot(x, 'ID'))))
     voronoi@data$weather.filename <-apply( voronoi@data, MARGIN=1, FUN=function(df) paste0("data_",df["y"],"_",df["x"]) )
     voronoi <- merge.sp( voronoi, layer@data[ , c("weather.filename","region")], by = "weather.filename", all.x=T )
     
#      length(voronoi$weather.filename)
#      length(unique(voronoi$weather.filename))
#      length(layer$weather.filename)
#      length(unique(layer$weather.filename))
#      head(layer@data)
#      head(voronoi@data)
     
     return( voronoi )
    
}



## ------------------------------------------------------------------------
#' @title create weather grid
#' @export

weather.grid.create<-function(weather.dir="C:/ALR/Data/ClimateData/Mauer/daily",
                              regions=c("east"), 
                              proj4="+proj=longlat +ellps=GRS80 +datum=NAD83 +no_defs",
                              shapefile.dir=NULL) {
     
     weather.filenames<-as.data.frame( matrix( ncol=2, nrow=0 ) )
     names(weather.filenames)<-c("weather.filename", "region")
     for (i in regions) {
          cat( paste( "Retrieving grid centroids for region", i, "from file names...\n" ) )
          setwd( file.path( weather.dir, i ) )
          temp.files<-list.files()
          weather.filenames[ (nrow(weather.filenames)+1):(nrow(weather.filenames)+length(temp.files)),]<-
               cbind( temp.files, rep( i, times=length(temp.files)) )
     }
     weather.filenames <- weather.filenames[ !(duplicated(weather.filenames$weather.filename)), ]
     
     weather.filenames$y <- as.vector( sapply( weather.filenames$weather.filename,  
                   FUN=function(x) as.numeric( unlist(strsplit( x, "[_]" ))[2] )  ) )
     weather.filenames$x <- as.vector( sapply( weather.filenames$weather.filename,  
                   FUN=function(x) as.numeric( unlist(strsplit( x, "[_]" ))[3] )  ) )

#      grid.y<-as.numeric( sapply( X=weather.filenames$weather.filename, FUN=substr,6,12 ) )
#      grid.x<-as.numeric( sapply( X=weather.filenames$weather.filename, FUN=substr,14,21 ) )

     cat("Creating spatial object of grid centroids...\n")
     grid.points<-SpatialPointsDataFrame(coords=weather.filenames[ , c("x","y") ],
                                             data=weather.filenames,
                                             proj4string=CRS( proj4 ) )
     #      grid.points<-SpatialPointsDataFrame(coords=cbind( grid.x, grid.y ),
     #                                              data=weather.filenames,
     #                                              proj4string=CRS( proj4 ) )
          
     cat("Creating Voronoi polygons around grid centroids...\n")
     cat("    (this part could take a while)    \n")
     weather.grid.poly<-voronoipolygons(grid.points)
#      weather.grid.poly@data<-weather.grid.poly@data[,c(3,1,2)]
     
     
     if ( !is.null( shapefile.dir) ) {
          print("Saving grid shapefile...")
          grid.temp<-weather.grid.poly
          names(grid.temp)[1]<-"filename"
          setwd(shapefile.dir)
          writeOGR(grid.temp,  ".", layer="weather_grid", driver="ESRI Shapefile")
     }

     cat("Completed creating weather grid")
     return( weather.grid.poly )

}



## ------------------------------------------------------------------------
# #' @title load weather grid
# #' @export
# weather.grid.load<-function( weather.grid.file="c:/ALR/Data/ClimateData/Mauer/weather_grid", 
#                              proj4="+proj=longlat +ellps=GRS80 +datum=NAD83 +no_defs" ) {
#  
#      weather.grid.poly<-readShapePoly(weather.grid.file,proj4string=CRS(proj4))
#      return(weather.grid.poly)
# }


## ------------------------------------------------------------------------
# #' @title load weather grids coords only
# #' @export
# weather.grid.coords.load<-function( weather.grid.poly=NULL,
#                                     weather.grid.file="c:/ALR/Data/ClimateData/Mauer/weather_grid",
#                                     proj4="+proj=longlat +ellps=GRS80 +datum=NAD83 +no_defs" ) {
#      if ( is.null(weather.grid.poly) )
#           weather.grid.poly <- weather.grid.load( weather.grid.file, proj4 )
#      weather.grid.coords<-weather.grid.poly@data
#      
#      return( weather.grid.coords )
# }

