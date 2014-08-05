## ------------------------------------------------------------------------
agg.function.weather<-function(df) return( c(precip.mm=sum(df$precip.mm),
                                             tmin=mean(df$tmin),
                                             tmax=mean(df$tmax),
                                             tavg=mean(df$tavg),
                                             pet=sum(df$pet),
                                             gdd=sum(df$gdd),
                                             frozen=sum(df$frozen),
                                             rain=sum(df$rain),
                                             melt=sum(df$melt),
                                             complete=sum(!is.na(df$precip.mm))>=cutoff))


## ------------------------------------------------------------------------

weather.retrieve<-function() {
#      if(!exists("met.poly")) 
#           met.poly<-readShapePoly(file.path(data_dir,"ClimateData/Mauer/met_poly_GCS"),proj4string=CRS(proj4.GCS))
#      met.coord<-met.poly@data
#      rm(met.poly)
#      gc()
#      
#      setwd(file.path(data_dir,"ClimateData/Mauer/daily/east"))
#      
#      # agg.function2<-function(df,return.wind=F) {
#      #      x<-c(precip_mm=mean(df$precip_mm),
#      #           tavg=mean(df$tmax+df$tmin),
#      #           complete=sum(!is.na(df$precip_mm))>=cutoff)
#      #      if(return.wind)
#      #           x<-c(x,wind=mean(df$wind))
#      #      return(x) }
#      
#      
#      
#      
#      print(paste("### begin reading",length(met.filename.sorted),"met files"))
#      for (i in 1:length(met.filename.sorted) ) {
#      
#           print(i)
#           if(file.exists(as.character(met.filename.sorted[i]))) {
#                print("    getting file")
#                x<-read.table(file=as.character(met.filename.sorted[i]),col.names=mauer.cols)
#                x[,c( "date1","date","month.date","season.date","annual.date")]<- met.template[,c( "date1","date","month.date","season.date","annual.date")]
#                                                                                 #date1 is date format.  
#                                                                                 #date, month.date, season.date, annual.date are characters, so they can be used as col names
#      #           centroid<-met.poly@data[met.poly$file_nm==met.filename.sorted[i],]
#                centroid<-met.coord[met.coord$file_nm==met.filename.sorted[i],]
#                print("    calculating values")
#                suppressWarnings(
#                     x.snow<-SnowMelt(Date=x$date1,precip_mm=x$precip.mm,Tmax_C=x$tmax,Tmin_C=x$tmin,lat_deg=centroid$y,windSp=x$wind,windHt=10))
#                x[,c("rain","melt")]<-x.snow[,c("Rain_mm","SnowMelt_mm")]
#                #PET_fromTemp(Jday, Tmax_C, Tmin_C, lat_radians)           
#                x[,"pet"]<-PET_fromTemp(Jday=yday(x$date),Tmax_C=x$tmax ,Tmin_C=x$tmin, lat_radians=centroid$y*pi/180)
#                x[,"tavg"]<-(x$tmin+x$tmax)/2
#                x[,"gdd"]<-sapply(x$tavg, FUN=function(y) max( y-10, 0) )
#                x[,"frozen"]<-sapply(x$tmin, FUN=function(y) y<=0)
#      #           x[1:20,]
#      #           x[90:110,]
#      #           x[200:220,]
#      #           
#                print("     start agg")
#      #           for (j in 1:length(cmatrices) ){   
#                for (j in 3){     
#                     print(paste("    ",names(date.template)[j]))
#                     #skip daily aggregation (no aggregation), at least for now?
#                     cutoff<-complete.cutoff[j]     
#                     x.agg<-ddply(x, names(date.template)[j], agg.function2)
#      #                sum(x.agg[,1]!=sort(x.agg[,1]))
#                     x.agg[x.agg$complete==0,-which(names(x.agg) %in% c("month.date","complete"))]<-NA
#                     x.agg$precip.e<-x.agg$rain+x.agg$melt
#                     x.agg<-slide(x.agg,Var="precip.e",NewVar="precip.e.lag1",slideBy=-1,reminder=F)
#                     x.agg<-slide(x.agg,Var="precip.e",NewVar="precip.e.lag2",slideBy=-2,reminder=F)
#                     x.agg<-slide(x.agg,Var="precip.e",NewVar="precip.e.lag3",slideBy=-3,reminder=F)
#                     x.final<-as.matrix(x.agg[,met.cols])
#                     cmatrices[[j]][,i,]<-x.final
#                }
#      #           head(x.agg)
#      #           head(cmatrices[[j]][,i,])
#      #           tail(x.agg)
#      #           tail(cmatrices[[j]][,i,])
#                
#           }
#           else
#                print(paste("MISSING FILE: can't find",i))
#           rm(x,x.agg,x.final,x.snow)
#      }
}


