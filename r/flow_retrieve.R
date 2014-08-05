
## ----aggregate function to use w/ ddply to aggregate---------------------

agg.function.flow<-function(df,cutoff) {
     j<-names(df)[1]
     cutoff<-create.template.periods()[j,"min.records"]
     return(c(mean=mean(df$val,na.rm=T),
           max=max(df$val,na.rm=T),
           min=min(df$val,na.rm=T),
           low=min(df$rolling7,na.rm=T),
#                                     complete=sum(!is.na(df$val))>=get(x = "cutoff",envir = parent.env()),
           complete=sum(!is.na(df$val))>=cutoff,
           records.period=sum(!is.na(df$val)), 
           records.period.rolling=sum(!is.na(df$rolling7)) ))
}



## ----import and aggregate into flow metrics------------------------------
#' @title import flow 
#' @description import flow from nwis web service, and aggregate to various metrics and by various periods
#' @export
flow.retrieve<-function(gages.spatial, 
                        periods=c("seasonal","annual"),
                        agg.function = agg.function.flow,
                        log.dir=NULL) {
     
     gages<-gages.spatial@data
     gages.temp<-gages[,c("site_no","station_nm")]
#      if (is.null(template.date))
     template.date<-create.template.date()
     template.period<-create.template.periods()
     cols.flow<-create.cols.flow()
     
     q.matrices<-create.q.matrices(gages.spatial=gages.spatial, periods=periods, template.date=template.date)
#      print(str(q.matrices))
     
     missing<-c() #save site_no id's of gages missing all data, or if unable to retrieve records
     for (k in 1:length(gages$site_no))     {
          print(paste("Loading/aggregating gage", k, "of", length(gages$site_no)))
          flag<-F
          
          #read raw, daily flow data
          tryCatch(
               x.all<-importDVs(gages$site_no[k], code = "00060", stat = "00003"),
               error = function(e) {
                    missing<-c(missing,k)
                    print(paste("ERROR CAUGHT------ station_id:",gages$site_no[k],"missing"))
                    print(e)
                    flag<-T
               })
     
          #calculate 7-day rolling mean and aggregate to periods 
          if (!flag)
          {                              
               # uses waterData package functions to pull NWIS data from USGS web services 
                                             #  flow/discharge cfs, code 00060
                                             #  daily mean, code 00003
               x.all<-cleanUp(x.all,task="fix",replace=NA)
               print(paste0("     site id: ",gages$site_no[k],";   ",nrow(x.all)," rows"))
               if ( sum(duplicated(x.all$dates)) ) {
                    warning(paste("      Duplicated dates in gage",gages$site_no[k],":",
                                  x.all[(which(duplicated(x.all$dates))-1):which(duplicated(x.all$dates)),], "removed"))
                    x.all<-x.all[!duplicated(x.all$dates),]
               }
               
               #7 day rolling mean
                    #convert to zoo timeseries, all columns
               x.zoo<-zoo(x.all$val,x.all$dates) 
                    #rollapply, feed it only values, b/c rollapply works better that way
               suppressWarnings( 
                    rolling7<-rollapply(data = x.zoo,
                                        FUN = function(x) mean(x), 
                                        width = 7,
                                        align="center", 
                                        partial = F) 
               )
                    #recreate zoo timeseries, using zoo with all columns, plus rolling mean calucated above
               x.roll<-as.data.frame(merge(x.zoo, rolling7))
                    #                x.zoo<-merge(x.zoo, rolling7) #CHANGED
                    #                x.roll<-as.data.frame(x.zoo)
               names(x.roll)<-c("val","rolling7")
               x.roll$date<-as.Date(row.names(x.roll))
                    #         x.roll$val<-as.numeric(x.roll$val)
                    #         x.roll$rolling7<-as.numeric(x.roll$rolling7)     
                    #           row.names(x.roll)<-as.character(x.roll$dates)     

               x.roll[,"daily"]<-as.character( x.roll$date )
               x.roll[,"monthly"]<-as.character( to.month(x.roll$date) )
               x.roll[,"seasonal"]<-as.character( to.season(x.roll$date) )
               x.roll[,"annual"]<-as.character( to.water.year(x.roll$date) )
          
#                print("start periods loop")
               for (j in periods){     
#                     j<-periods[i]
#                     print(paste("======",j))
                    #determine number of records for this periods to be considered "complete"
#                     cutoff<-template.period[j,"min.records"]  
#                     assign("cutoff",value = template.period[j,"min.records"], envir = )
                    
                    #aggregate, i.e. mean, max, low, etc
                    suppressWarnings(
                         x.agg<-ddply(x.roll[,c(j,"val","rolling7")], j, agg.function.flow)
                    )
                    
                    #set records that don't have min # records required for period to NA
                    x.agg[x.agg$complete==0,!(names(x.agg) %in% c(j,"complete", "records.period", "records.period.rolling"))]<-NA
#                     x.agg[x.agg$complete==0,c("mean", "max", "low")]<-NA

                    #save a count of # complete records in gages data frame
#                     gages[k,paste0("records.",j)]<-sum(x.agg$complete)
                    gages.temp[k,paste0("records.",j)]<-sum(x.agg$complete)
                    
                    x.merge<-merge(template.date[[j]],x.agg,
                                   by.x="date",by.y=j,all.x=T,all.y=F)
#                     print("str qmatrices[[j]]")
#                     print(str(q.matrices[[j]]))
#                     print("str x.merge")
#                     print(str(x.merge))
#                     print("======== site no")
#                     print(gages$site_no[i])
#                     print("======== matrix gage dim")
#                     print(head(dimnames(q.matrices[[j]])[[3]]))
#                     print("dim q.matrices[[j]][,gages$site_no[i],l]")
#                     print(length(q.matrices[[j]][,gages$site_no[i],l]))
#                     print("dim x.merge[,l]")
#                     print(length(x.merge[,l]))
                    for (l in cols.flow) {
                         q.matrices[[j]][,gages$site_no[k],l]<-x.merge[,l]
                    }
#                          q.matrices[[j]][,gages$site_no[k],cols.flow]<-x.merge[,cols.flow]  
#                     q.matrices[[j]][,gages$site_no[i],"mean"]<-x.merge$mean
#                     q.matrices[[j]][,gages$site_no[i],"max"]<-x.merge$max
#                     q.matrices[[j]][,gages$site_no[i],"min"]<-x.merge$min
#                     q.matrices[[j]][,gages$site_no[i],"low"]<-x.merge$low
#                     q.matrices[[j]][,gages$site_no[i],"records"]<-x.merge$records

#                     cutoff<-Inf
#                     print(paste("====== finish",j))
               }#end loop periods
          }#end check flag
     
     print(paste0("     end gage ",k))
     }#end loop gages
     
     if(!is.null(log.dir)) {
          
          setwd(log.dir)
          
          log<-c("flow data retrieval log", format.Date(now()),"\r")
          {
          if (length(missing)>0) {
               log<-c(log,             
                    paste(length(gages$site_no[-missing]),"sites"),
                    paste(length(missing),"gages missing data, ignored"),
                    "\r","\r",
                    "gages missing all data",
                    gages$site_no[missing],
                    "\r","\r",
                    "gages used",
                    gages$site_no[-missing])
               write.table(gages$site_no[missing],sep="/r",file="gages_missing_all_data.txt")
               write.table(gages$site_no[-missing],sep="\r",file="gages_site_no.txt",row.names=F,col.names=F)
               }
          
          else {
               log<-c(log,             
                    paste(length(gages$site_no),"sites"),
                    "\r","\r",
                    "sites",
                    gages$site_no)
               write.table(gages$site_no,sep="\r",file="gages_site_no.txt",row.names=F,col.names=F)
               }
          
          }
          writeLines(log,"flow_retrieval_log.txt")
          
     }

#      q.matrices[[nrow(template.period)+1]]<-gages.temp  #save counts of how many records each gages has, for each period aggregated
     q.matrices[["records"]]<-gages.temp  #save counts of how many records each gages has, for each period aggregated
     
     return(q.matrices)
     
     
}

