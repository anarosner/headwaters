## ------------------------------------------------------------------------
#' @title agg weather 
#' @description x 
#' @export

agg.function.weather<-function(df) {
     j<-names(df)[1] #a kinda sneaky way to determine the period, w/o requiring it to be passed as parameter, so only one paramter is needed
     cutoff<-create.template.periods()[j,"min.records"] #determine number of records for this periods to be considered "complete"
     return( c(
          precip.total=sum(df$precip.total),
          precip.e=sum(df$precip.e),
          tmin=mean(df$tmin),
          tmax=mean(df$tmax),
          tavg=mean(df$tavg),
          pet=sum(df$pet),
          gdd=sum(df$gdd),
          frozen=sum(df$frozen),
#           rain=sum(df$rain),
#           melt=sum(df$melt),
          melt.doy=max(df$melt.doy),
          complete=sum(!is.na(df$precip.total))>=cutoff) )
}



## ------------------------------------------------------------------------
#' @title col names for weather metrics
#' @description col names for our processed/saved weather metrics
#' @export

create.cols.weather<-function() {
     cols.weather<-c("precip.total","precip.e","precip.e.lag1","precip.e.lag2","precip.e.lag3",
                     "tmin","tmax","tavg",
                     "pet","pet.lag1","pet.lag2","gdd","gdd.lag1","gdd.lag2",
                     "frozen","melt.doy","melt.doy.lag1","melt.doy.lag2")
#      cols.weather<-c("precip.mm","rain","melt","precip.e","precip.e.lag1","precip.e.lag2","precip.e.lag3",
#                      "tmin","tmax","tavg","pet","gdd","frozen","melt.doy")
     return(cols.weather)
}



## ------------------------------------------------------------------------
#' @title import weather 
#' @description import weather
#' @export
weather.retrieve<-function(gages.spatial, 
                              periods=c("seasonal","annual"),
                              agg.function.weather = (conteStreamflow::agg.function.weather), 
                              template.date = NULL, template.period = NULL, cols.weather = NULL ) { 

       
          cache.check()

          #warn user if hasn't assigned weather grid.  later, change to just call function to assign weather grid if it hasn't been done already
     if ( !("weather.filename" %in% names(gages.spatial)) )
          stop("Please first plot gages to weather grids using \"gage.place.weather.grid\" ")
     
     #generate a data.frame of *unique* weather grids used by all gages
     selected.weather.files<-gages.spatial[,c("weather.filename","region")]
     selected.weather.files<-selected.weather.files[ !duplicated(selected.weather.files$weather.filename), ]

     
     #create templates and columns 
     if (is.null(template.date))
          template.date<-create.template.date()
     if (is.null(template.period))
          template.period<-create.template.periods()
     if (is.null(cols.weather))
          cols.weather<-create.cols.weather()
     #maybe later add a test here.  if user specifies custom cols.weather and agg.function, 
     #     need to make sure the columns match the agg function outputs

     cols.mauer <- create.cols.mauer()     
     template.date.mauer <- create.template.date.mauer()
     
     cache.load.data( "weather.grid.coords", file="weather_grid_coords.rdata", dir="weather_grid" )
#      weather.grid.coords <- weather.grid.coords.load()
     
     
     #create matrix for storing flow data
     w.matrices<-create.w.matrices(selected.weather.files$weather.filename, periods=periods,
                                   template.date=template.date, template.period=template.period, cols.weather=cols.weather)

     for (j in unique(selected.weather.files$region)) {
#           print(paste0("Region \"",j,"\""))
          setwd( file.path(cache.dir.global, "data", "weather_data") )
          dir.conditional.create( new.dir=j, quiet=T )

          for (k in selected.weather.files$weather.filename[selected.weather.files$region==j]) {
               cache.load.data( file=k, dir=paste0("weather_data","/",j), cache.only=T, quiet=T )
          }
     }


#      weather.dir<-file.path(cache.dir, "data","weather_data")
#      to.load <- c()
#      for (j in unique(selected.weather.files$region)) {
#           setwd(weather.dir)
#           dir.conditional.create( new.dir=j, quiet=T )
# #           if ( !(j %in% list.dirs()) )
# #                dir.create( file.path(weather.dir,j) )
#           setwd(file.path(weather.dir,j))
#           t <- unique( selected.weather.files$weather.filename[selected.weather.files$region==j] )
#           to.load <- c(  to.load, 
#                          paste0(j,"/",t[!(t %in% list.files())])  )
#      }
#      ### download catchment files as needed
#      setwd(weather.dir)
#      
#      if ( length(to.load)>0 ) {
#           cat(paste( "Downloading", length(to.load), "weather data files. (This will be cached locally for future use.)"  ))
#           for ( i in 1:length(to.load) ) {
#                cat(paste( "... now downloading file",i,"of",length(to.load)  ))
#                download.file( paste0(server.url,"/data/weather_data/",to.load[i]),
#                               paste0(cache.dir, "/data/weather_data/",to.load[i]), 
#                               method="wget", quiet=T)
#           }
#           cat("Download complete")
#      }

     
     #loop through weather grid cells and pull and aggregate observation records
     cat(paste( "Begin reading", nrow(selected.weather.files), "unique weather files used for",nrow(gages.spatial),"gages","\n" ))
     for (i in 1:nrow(selected.weather.files) ) {
     
          cat( paste( "  --  Loading file", i, "of", nrow(selected.weather.files), "  --  \n" ))
                    
          weather.file<-file.path(cache.dir.global,"data","weather_data", 
                         selected.weather.files$region[i],
                         selected.weather.files$weather.filename[i])        
                    
          if( file.exists(weather.file) ) { 
               #check that file exists.  shouldn't be a problem, 
               #   since polygon cells are generated from file names, but just want to make sure
               
               x<-read.table(file=weather.file,
                             col.names=cols.mauer)
               x[,c( "date", template.period$name )]<- 
                    template.date.mauer[,c( "date", template.period$name )]
                                   #date is date format  
                                   #others are characters, so they can be used as col names
               centroid <- weather.grid.coords[i, c("x","y")]
               
               
#                print("  --  calculating estimates of weather processes --  ")
               #run snowmelt model
               suppressWarnings(
                    x.snow<-SnowMelt( Date=x$date, lat_deg=centroid$y,
                                     precip_mm=x$precip.mm, Tmax_C=x$tmax, Tmin_C=x$tmin,
                                     windSp=x$wind,windHt=10 )  )
               x.snow$doy <- as.numeric(format(x.snow$Date,"%j"))
               x.snow$snow.pack <- x.snow$SnowDepth_m>0
               x.snow$snow.pack.doy <- 0
               x.snow$snow.pack.doy[x.snow$snow.pack] <- x.snow$doy[x.snow$snow.pack]
               x.snow$snow.pack.doy[x.snow$doy>200]<-0  #last day in the *spring* for which there is some snow pack
               #i've gone back and forth on whether to save rain & melt, or just effective precip
#                x[,c("rain","melt","melt.doy")]<-x.snow[,c("Rain_mm","SnowMelt_mm", "snow.pack.doy")]
               x[,"melt.doy"]<-x.snow[,"snow.pack.doy"]
               x[,"precip.e"] <- x.snow[,"Rain_mm"] + x.snow[,"SnowMelt_mm"]

               #temperature metrics, including estimated potential evapotranspiration and gdd
               x[,"pet"]<-PET_fromTemp(Jday=yday(x$date),Tmax_C=x$tmax ,Tmin_C=x$tmin, lat_radians=centroid$y*pi/180)
               x[,"tavg"]<-(x$tmin+x$tmax)/2
               x[,"gdd"]<-sapply(x$tavg, FUN=function(y) max( y-10, 0) )
               x[,"frozen"]<-sapply(x$tmin, FUN=function(y) y<=0)

               #rename total precip
               x[,"precip.total"] <-  x[,"precip.mm"]

               #aggregate for periods specified in function argument
#                print("  --  starting aggregation  --  ")
               for (j in periods ){ 
                    #aggregate
                              #replace this.  it's the columns, but can't use cols template, because doesn't include lags...
#                     names(x)[names(x) %in% cols.weather]
                    x.agg<-ddply(x[,c(j,names(x)[names(x) %in% cols.weather])],
                                 j, 
                                 agg.function.weather)
     #                sum(x.agg[,1]!=sort(x.agg[,1]))
                    x.agg[x.agg$complete==0,-which(names(x.agg) %in% c(j,"complete"))]<-NA 
     
                    #calculate lag precip values
                    x.agg<-slide(x.agg,Var="precip.e",NewVar="precip.e.lag1",slideBy=-1,reminder=F)
                    x.agg<-slide(x.agg,Var="precip.e",NewVar="precip.e.lag2",slideBy=-2,reminder=F)
                    x.agg<-slide(x.agg,Var="precip.e",NewVar="precip.e.lag3",slideBy=-3,reminder=F) #not really used, but might as well calculate it just in case
                    
                    #calculate lag temp values
                    x.agg<-slide(x.agg,Var="pet",NewVar="pet.lag1",slideBy=-1,reminder=F)
                    x.agg<-slide(x.agg,Var="pet",NewVar="pet.lag2",slideBy=-2,reminder=F)
                    x.agg<-slide(x.agg,Var="gdd",NewVar="gdd.lag1",slideBy=-1,reminder=F)
                    x.agg<-slide(x.agg,Var="gdd",NewVar="gdd.lag2",slideBy=-2,reminder=F)
                    x.agg<-slide(x.agg,Var="melt.doy",NewVar="melt.doy.lag1",slideBy=-1,reminder=F)
                    x.agg<-slide(x.agg,Var="melt.doy",NewVar="melt.doy.lag2",slideBy=-2,reminder=F)
     
#                     w.matrices[[j]][,i,] <- as.matrix(x.agg[,cols.weather])
                    x.final<-as.matrix(x.agg[,cols.weather])
                    w.matrices[[j]][,selected.weather.files$weather.filename[i],]<-x.final

               }#end loop periods
               
          }#end check that file exists
          else
               warning(paste("MISSING FILE: can't find",i))
          rm(x,x.snow,x.final,x.agg)
     }#end loop weather grids

     return(w.matrices)

}




