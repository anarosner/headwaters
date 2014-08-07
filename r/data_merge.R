## ------------------------------------------------------------------------

data.merge<-function( gages.spatial, q.matrix, w.matrix, periods=c("seasonal") ) {
     
     cols.flow<-create.cols.flow()
     cols.weather<-create.cols.weather()
     template.period<-create.template.periods()
     template.date<-create.template.date()

     #create list the length of all periods
     #   but only create matrices for the periods specified
     #   this is so the index in the list for period x is consistent no matter how many periods are specified
     d.matrices<-list()
     
     for (j in periods) {
#           print(j)
          i<-which(template.period$name==j)
          d.matrices[[j]] < -array(dim=c(nrow(template.date), #dates
                                         nrow(gages.spatial), #gages
                                         length(cols.flow) + length(cols.weather)  )) #climate and flow stats
#
#           .matrices[[i]]<-array(dim=c(   nrow(template.date[[j]]), 
#                                           length(gages.spatial$site_no), 
#                                           length(cols.flow))  ) 
          dimnames(   .matrices[[i]]   )[[1]]<-template.date[[j]][,1]
          dimnames(   .matrices[[i]]    )[[2]]<-gages.spatial$site_no
          dimnames(   .matrices[[i]]   )[[3]]<-cols.flow
     }
     
     names(d.matrices)<-template.period$name
     return(d.matrices)
     
     
}
# dseasonal1<-array(dim=c(dim(qseasonal)[1], #dates
#                         nrow(gages.met.spatial), #gages
#                         dim(cseasonal)[3]+dim(qseasonal)[3])) #climate and flow stats
# dimnames(dseasonal1)[[1]]<-dimnames(qseasonal)[[1]]
# dimnames(dseasonal1)[[2]]<-gages.met.spatial$site_no
# dimnames(dseasonal1)[[3]]<-c(dimnames(cseasonal)[[3]],
#                              paste0("flow.",dimnames(qseasonal)[[3]]))
#      # [1] "precip.mm"     "rain"          "melt"          "precip.e"      "precip.e.lag1" "precip.e.lag2"
#      # [7] "precip.e.lag3" "tmin"          "tmax"          "tavg"          "pet"           "gdd"          
#      # [13] "frozen"        "flow.mean"     "flow.max"      "flow.min"      "flow.low"      "flow.records" 
# 
# 
# for (i in 1:dim(dseasonal1)[[2]]) {  #loop gages
#      dseasonal1[,gages.met.spatial$site_no[i],
#                 (dim(cseasonal)[3]+1):(dim(cseasonal)[3]+dim(qseasonal)[3])]<-
#           qseasonal[,gages.met.spatial$site_no[i],]
#      m<-as.character(gages.met.spatial@data[gages.met.spatial$site_no== dimnames(dseasonal1)[[2]][i],"met_filename"]) 
#                #get met filename
#      print(m)
#      if (length(m)>0) {
#           dseasonal1[,gages.met.spatial$site_no[i],1:dim(cseasonal)[[3]]]<-cseasonal[,m,]
#      }
# }
# dseasonal2<-melt.3d(dseasonal1)
# dseasonal2<-subset(dseasonal2,!is.na(flow.mean))
# dseasonal<-merge(dseasonal2,gages.char.spatial@data)
# dseasonal$season<-to.season(d=dseasonal$date,"season")
# dseasonal$year<-year(as.Date(as.character(dseasonal$date)))
# dseasonal$precip.e<-dseasonal$rain+dseasonal$melt

