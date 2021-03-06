## ------------------------------------------------------------------------
#' @title melt 3d
# no export

melt.3d<-function(a, id.cols="site_no") {
#      out<-melt(a[,,1])
#      names(out)<-c("date","site_no",dimnames(a)[[3]][1])
#      
     x<-data.frame(id=1:dim(a)[[2]],site_no=dimnames(a)[[2]],stringsAsFactors=F)
     dimnames(a)[[2]]<-x$id
     out<-melt(a[,,1])
     names(out)<-c("date","id",dimnames(a)[[3]][1])
     out<-merge(out,x,by="id",sort=F)
     out<-out[,c("date","site_no",dimnames(a)[[3]][1])]
     for (i in 2:dim(a)[3]) {
          m<-melt(a[,,i])
          out[,dimnames(a)[[3]][i]]<-m[,3]
     }
     return(out)     
     
     if (id.cols!="site_no") {
          names(out)[names(out)=="site_no"] <- id.cols
     }
     #add option to change site_no to other id name
}


## ------------------------------------------------------------------------
#' @title data merge
#' @export
data.merge<-function( gages.spatial, q.matrices, w.matrices, periods=c("seasonal"), select.gages=NULL, template.period=NULL, id.cols="site_no" ) {
     
#      cols.flow<-create.cols.flow()
#      cols.weather<-create.cols.weather()
#      template.date<-create.template.date()
     
     if ( is.null(template.period) )
         template.period<-create.template.periods()

     #create list the length of all periods
     #   but only create matrices for the periods specified
     #   this is so the index in the list for period x is consistent no matter how many periods are specified
     d.matrices<-list()
     
     cat("Merging flow, weather, and basin char data\n")
     for ( j in template.period$name ) {
          if ( j %in% periods ) {
               cat(paste("  --  Period",j," --  \n        Gage "))          
               
               d1 <- array(dim=c(dim(q.matrices[[j]])[1], #dates
                                   dim(gages.spatial)[1], #gages
                                   dim(w.matrices[[j]])[3]+dim(q.matrices[[j]])[3])) #climate and flow stats
               dimnames(d1)[[1]]<-dimnames(q.matrices[[j]])[[1]]
               dimnames(d1)[[2]]<-gages.spatial@data[,id.cols]
               # dimnames(d.matrices[[j]]1)[[2]]<-dimnames(q.matrices[[j]])[[2]]
               dimnames(d1)[[3]]<-c(dimnames(w.matrices[[j]])[[3]],
                             paste0("flow.",dimnames(q.matrices[[j]])[[3]]))
               
               if ( !is.null(select.gages) )
                    g <- select.gages
               else 
                    g <- dimnames(d1)[[2]]
               
               for (i in g) {  #loop gages
#                     cat( paste0(" --  Gage ",which(g==i), " of ", length(g),"  --  \n") )
                    if (which(g==i)>1)
                         cat(", ") 
                    cat( which(g==i) )
#                for (i in dimnames(dseasonal1)[[2]]) {  #loop gages
                    d1[,i,
                       (dim(w.matrices[[j]])[3]+1):(dim(w.matrices[[j]])[3]+dim(q.matrices[[j]])[3])]<-
                         q.matrices[[j]][,i,]
                    m <- gages.spatial[gages.spatial$site_no==i, "weather.filename"][[1]]
                    if (length(m)>0) {
                         d1[,i,1:dim(w.matrices[[j]])[3]]<-
                              w.matrices[[j]][,m,]
                    }
               }     
               cat("\n")
                    
               d2<-melt.3d(d1)
               d2<-subset(d2,!is.na(flow.mean))
               d.matrices[[j]]<-merge(d2,gages.spatial@data)
               
               d.matrices[[j]]$season<-to.season(d=d.matrices[[j]]$date,"season")
               d.matrices[[j]]$year<-year(as.Date(as.character(d.matrices[[j]]$date)))
               
               
               
          }
          else
               d.matrices[[j]] <- NA
     }

     return(d.matrices)
     
     
}



## ----junk----------------------------------------------------------------



#      for (j in periods) {
# #           print(j)
#           i<-which(template.period$name==j)
#           d.matrices[[j]] < -array(dim=c(nrow(template.date), #dates
#                                          nrow(gages.spatial), #gages
#                                          length(cols.flow) + length(cols.weather)  )) #climate and flow stats
# #
# #           .matrices[[i]]<-array(dim=c(   nrow(template.date[[j]]), 
# #                                           length(gages.spatial$site_no), 
# #                                           length(cols.flow))  ) 
#           dimnames(   .matrices[[i]]   )[[1]]<-template.date[[j]][,1]
#           dimnames(   .matrices[[i]]    )[[2]]<-gages.spatial$site_no
#           dimnames(   .matrices[[i]]   )[[3]]<-cols.flow
#      }
#      
#      names(d.matrices)<-template.period$name




## ----scratch-------------------------------------------------------------
# 
# qseasonal<-q.matrices[["monthly"]]
# cseasonal<-w.matrices[["monthly"]]
# gages.met.spatial<-g.spatial
# 
# dseasonal1<-array(dim=c(dim(qseasonal)[1], #dates
#                         dim(gages.met.spatial)[1], #gages
#                         dim(cseasonal)[3]+dim(qseasonal)[3])) #climate and flow stats
# dim(dseasonal1)
# dimnames(dseasonal1)[[1]]<-dimnames(qseasonal)[[1]]
# dimnames(dseasonal1)[[2]]<-gages.met.spatial$site_no
# # dimnames(dseasonal1)[[2]]<-dimnames(qseasonal)[[2]]
# dimnames(dseasonal1)[[3]]<-c(dimnames(cseasonal)[[3]],
#                              paste0("flow.",dimnames(qseasonal)[[3]]))
#      # [1] "precip.mm"     "rain"          "melt"          "precip.e"      "precip.e.lag1" "precip.e.lag2"
#      # [7] "precip.e.lag3" "tmin"          "tmax"          "tavg"          "pet"           "gdd"          
#      # [13] "frozen"        "flow.mean"     "flow.max"      "flow.min"      "flow.low"      "flow.records" 
# 
# 
# 
# # dseasonal1<-array(dim=c(dim(qseasonal)[1], #dates
# #                         nrow(gages.met.spatial), #gages
# #                         dim(cseasonal)[3]+dim(qseasonal)[3])) #climate and flow stats
# # dimnames(dseasonal1)[[1]]<-dimnames(qseasonal)[[1]]
# # dimnames(dseasonal1)[[2]]<-gages.met.spatial$site_no
# # dimnames(dseasonal1)[[3]]<-c(dimnames(cseasonal)[[3]],
# #                              paste0("flow.",dimnames(qseasonal)[[3]]))
# #      # [1] "precip.mm"     "rain"          "melt"          "precip.e"      "precip.e.lag1" "precip.e.lag2"
# #      # [7] "precip.e.lag3" "tmin"          "tmax"          "tavg"          "pet"           "gdd"          
# #      # [13] "frozen"        "flow.mean"     "flow.max"      "flow.min"      "flow.low"      "flow.records" 
# # 
# # 
# for (i in dimnames(dseasonal1)[[2]]) {  #loop gages
#      dseasonal1[,i,(dim(cseasonal)[3]+1):(dim(cseasonal)[3]+dim(qseasonal)[3])]<-
#           qseasonal[,i,]
#      m <- gages.met.spatial[gages.met.spatial$site_no==i, "weather.filename"][[1]]
#      if (length(m)>0) {
#      dseasonal1[,i,1:dim(cseasonal)[3]]<-
#           cseasonal[,m,]
#      }
# }     
#      
# dseasonal2<-melt.3d(dseasonal1)
# dseasonal2<-subset(dseasonal2,!is.na(flow.mean))
# dseasonal<-merge(dseasonal2,gages.char.spatial@data)
# 
# dseasonal$season<-to.season(d=dseasonal$date,"season")
# dseasonal$year<-year(as.Date(as.character(dseasonal$date)))


