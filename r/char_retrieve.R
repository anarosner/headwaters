
## ------------------------------------------------------------------------
#' @title x
#' @description x
#' @param  x \code{x} 
#' @param  x \code{x} 
#' @param  x \code{x} 
#' @return x \code{x}
#' @seealso x \code{\link{conteStreamflow::}}, \code{\link{conteStreamflow::}}
#' @export
char.columns.retrieve <- function(id=F,impoundment=F) {     
     # placeholder function until basin characteristics are available through rest api
     # returns list of available columns, or the name of the id column

     if ( id ) 
          return("FEATUREID")
     else if ( impoundment==T )
          return( c("TNC_DamCount", "deg_barr_1", "deg_barr_2", "deg_barr_3", "deg_barr_4", "deg_barr_6", "deg_barr_7", 
                  "OnChannelWaterSqKM", "OnChannelWetlandSqKM", "OffChannelWaterSqKM", "OffChannelWetlandSqKM") )
     else 
          return( c("TotDASqMI", "TotDASqKM", "ReachLengthKM",         
               "Forest", "Herbacious","Agriculture","Developed","DevelopedNotOpen", "Impervious",  
               "CONUSOpenWater","CONUSWetland",
               "DrainageClass","HydrologicGroupA","HydrologicGroupAB","HydrologicGroupCD","HydrologicGroupD4","HydrologicGroupD1",
               "SurficialCoarseA","SurficialCoarseB", "SurficialCoarseC","SurficialCoarseD","PercentSandy", 
               "AnnualTmaxC","AnnualTminC","AnnualPrcpMM",
               "JanPrcpMM","FebPrcpMM","MarPrcpMM","AprPrcpMM","MayPrcpMM","JunPrcpMM",
               "JulPrcpMM","AugPrcpMM","SepPrcpMM","OctPrcpMM","NovPrcpMM","DecPrcpMM",
               "AtmDepositionNO3","AtmDepositionSO4",
               "ReachElevationM","BasinElevationM","ReachSlopePCNT","BasinSlopePCNT") )
     
}



## ------------------------------------------------------------------------
#' @title char columns select
#' @export
char.columns.select <- function(impoundment=F) {
     # placeholder function until basin characteristics are available through rest api
     # interactive selection of columns to include
     # clunky, but at least provides an option to make it a little easier for the user

#      if ( !impoundment ) 
#           col=char.columns.retrieve()
#      else 
#           col=char.columns.retrieve(impoundment=T)
     col=char.columns.retrieve(impoundment=impoundment)
          
     
     all <- data.frame( col=col, include=rep(x=NA, times=length(col)), stringsAsFactors=F ) 
     for ( i in 1:nrow(all) ) {
          all[i,"include"] <- readline( paste0("\'", all[i,"col"], "\': ","Include this variable? (y/n)  ") ) 
     }
     all[,"include"] <- as.logical.y.n(all[,"include"])
     
     return(c( char.columns.retrieve(id=T), all[ all$include==T, "col" ] ))
}



## ------------------------------------------------------------------------
#' @title Get basin characteristics associated with each gage
#' @description Get basin characteristics associated with each gage, or more precisely, with the NHDplus stream reach each gage is plotted to.
#' @param gages.spatial \code{SpatialPointsDataFrame} of gage info from plot.gages.nhdplus()
#' @param basin.char.file \code{character} location andname of .rdata file that contains UpstreamStats, by NHDplus FEATUREID
#' @return \code{SpatialPointsDataFrame} of gage info including upstream basin characteristics
#' @seealso plot.gages.nhdplus, load.gage.impound
#' @export

char.retrieve<-function(gages.spatial, cols=NULL, 
                        log.cols=NULL, std.cols=NULL, log.std.cols=NULL, return.scaling=F, 
                        impound=F, server.url="http://felek.cns.umass.edu:9283") {
     
     cache.check()

     if ( !("FEATUREID" %in% names(gages.spatial)) )
          stop("Please first place gages in NHDplus catchments")
     
     #ensure all columns indicated for transformation are included in full set of selected columns
     cols <- unique(c(cols, log.cols, std.cols, log.std.cols ))
     
     ### retrieve data from server
     # this is temporary until basin characteristics are available through rest api
     # just loads rdata w/ data.frame's given to me by kyle
     
     if ( !impound ) {
          cache.load.data( object="UpstreamStats", dir="basin_char", file="NENY_CovariateData_2014-01-23_upstream.rdata")
          all.char<-get("UpstreamStats")
          names(all.char)[names(all.char)=="TotDASqKM"]<-"NHDplusTotDASqKM"
          names(all.char)[names(all.char)=="TotDASqMI"]<-"NHDplusTotDASqMI"
     }

     else { 
          cache.load.data( object="UpstreamStats", dir="basin_char", file="NENY_Impound_renamed_2014-01-23.rdata")
          all.char<-get("UpstreamStats")          
     }
     
     # selects only needed features (also temporary until rest api)
     all.char <- all.char[all.char$FEATUREID %in% gages.spatial$FEATUREID,]
     
     if ( !return.scaling ) {
          # selects only desired columns
          if ( !is.null(cols) ) #(also temporary until rest api)
               all.char <- all.char[,unique(c("FEATUREID",cols))]
          # Standardize and/or log transform options
          if (!is.null(log.cols) & length(log.cols)>0 )
               all.char <- char.transform( char=all.char, cols=log.cols, trans="log" )
          if (!is.null(std.cols) & length(std.cols)>0 )
               all.char <- char.transform( char=all.char, cols=std.cols, trans="std" )
          if (!is.null(log.std.cols) & length(log.std.cols)>0 )
               all.char <- char.transform( char=all.char, cols=log.std.cols, trans="log.std" )

          # merge and return     
          # merges w/ spatial.gages object 
          gages.spatial.char<-merge.sp( gages.spatial, all.char, by="FEATUREID", all.x=T, all.y=F, sort=F)
     
          return(gages.spatial.char)
     }
     
     else if (return.scaling) {
          scaling <- data.frame(matrix(nrow=2, ncol=0))
          if (!is.null(std.cols) & length(std.cols)>0 )
               scaling <- cbind( char.transform( return.scaling=T, char=all.char, cols=std.cols, trans="std" ) )
          if (!is.null(log.std.cols) & length(log.std.cols)>0 )
               scaling <- cbind( char.transform( return.scaling=T, char=all.char, cols=log.std.cols, trans="log.std" ) )
          return( scaling )
     }
     


     #to improve later: 
     #also, see if i can find a better solution to sp merge issue

}



## ------------------------------------------------------------------------

char.transform <- function( char, cols, id.col="FEATUREID", trans="log", return.scaling=F) {
     
     if (trans=="log" | trans=="log.std") {
          char2 <- apply( char[,cols], MARGIN=c(1,2), FUN=function(x) log(non.zero(x)) )
          char2 <- as.data.frame( cbind( char[,id.col], char2 ), stringsAsFactors=F )
#           cols2 <- paste0("log",".",cols)
          names(char2)[[1]] <- id.col
     }
     else {
          char2<-char
#           cols2<-cols
     }
     
     
     if ( trans=="std" | trans=="log.std" ) {
          char3 <- scale( char2[,cols], center=T, scale=T )
#           print("center")
#           print( attr(char3, "scaled:center") )
#           print("scale")
#           print( attr(char3, "scaled:scale") )
          char2 <- as.data.frame( cbind( char2[,id.col], char3 ), stringsAsFactors=F )
     }

     
     if ( !return.scaling ) {
          names(char2) <- c( id.col, paste0(trans,".",cols) )
          char4 <- merge( char, char2, by=id.col, all.x=T, all.y=F, sort=F )
          return( char4 )
     }

     else {
          if (trans=="log")
               stop( "Cannot return scaling on non-standardization transformation" )
          scaling <- as.data.frame( rbind(attr(char3, "scaled:center"),attr(char3, "scaled:scale")) )  
          row.names(scaling) <- c("mean","sd")
          return(scaling)
     }
}



## ------------------------------------------------------------------------
#' @title impound retrieve
#' @description wrapper function for char.retrieve
#' @export
impound.retrieve <- function(gages.spatial, server.url="http://felek.cns.umass.edu:9283", cols=NULL) {

     char.retrieve( gages.spatial=gages.spatial, impound=T, server.url=server.url, cols=cols )                    
}



## ------------------------------------------------------------------------
#' @title default char columns 
#' @export
char.columns.default <- function( impound=F ) {
     if ( !impound ) 
          return( c( char.columns.retrieve(id=T), 
               "ReachLengthKM",         
               "Forest", "Herbacious","Agriculture","Developed","DevelopedNotOpen", "Impervious",  
               "CONUSOpenWater","CONUSWetland",
               "DrainageClass","HydrologicGroupAB","SurficialCoarseC","PercentSandy", 
               "ReachElevationM","BasinElevationM","ReachSlopePCNT","BasinSlopePCNT", "NHDplusTotDASqKM") )
     else 
          return( c( char.columns.retrieve(id=T), 
                      "TNC_DamCount", "OnChannelWaterSqKM", "OnChannelWetlandSqKM"))
}
     


## ------------------------------------------------------------------------
#      load("C:/ALR/Models_from_others/Kyle/NH NY NHDplus basin zonal stats KO/NENY_CovariateData_2014-01-23.RData", verbose = T)
#      save(UpstreamStats, file="C:/ALR/Models_from_others/Kyle/NH NY NHDplus basin zonal stats KO/NENY_CovariateData_2014-01-23_upstream.RData")
#      ls.objects()
#      load("C:/ALR/Models_from_others/Kyle/NH NY NHDplus basin zonal stats KO/NENY_CovariateData_2014-03-13.RData", verbose=T)
#      names(UpstreamStats)



## ------------------------------------------------------------------------
# basin.char.retrieve<-function(type="char",
#                             basin.impound.file="C:/ALR/Models_from_others/Kyle/NH NY NHDplus basin zonal stats KO/NENY_CovariateData_Impoundments_2014-01-23.RData",
#                             basin.char.file="C:/ALR/Models_from_others/Kyle/NH NY NHDplus basin zonal stats KO/NENY_CovariateData_2014-01-23.RData") {
#      if (type == "char") {
#           load(basin.char.file) 
#           temp<-get("UpstreamStats")
#      }
#      
#      else if (type == "impound") {
#           load(basin.impound.file)
#           temp <- get("UpstreamStats")
#      }
#      
#      return(temp)
# }



## ----impoundments--------------------------------------------------------


# @title Get information on upstream barriers and impoundments
# @description Load and attach information for each gage on upstream barriers and impoundments.  Uses TNC barrier inventory and FWS survey openwater and wetlands. 
# (Open water and wetlands are intersected with NHDplus med res flowlines, to differentiate on- and off-network wetlands and waters)
# @param gages.spatial \code{SpatialPointsDataFrame} of gage info from plot.gages.nhdplus()
# @param basin.impound.file \code{character} location and name .Rdata file that contains UpstreamStatsImpounded, by NHDplus FEATUREID
# @return \code{SpatialPointsDataFrame} of gage info including TNC barriers and area of wetlands and open water
# @seealso plot.gages.nhdplus, load.gage.char, filter.gage.impound

# 
# gage.load.impound<-function(gages.spatial, basin.impound.file=NULL)
#           {
#        #      gage.load.impound<-function(gages.spatial, 
#        # basin.impound.file="C:/ALR/Models_from_others/Kyle/NH NY NHDplus basin zonal stats KO/NENY_CovariateData_Impoundments_2014-01-23.RData") 
# 
# 
#      #load Kyle's zonal stats
# #      load(basin.impound.file, verbose = T)
# #      basin.impound<-get(x = "UpstreamStatsImpounded")
# #      if (is.null(basin.impound.file))
# #           basin.impound<-basin.char.retrieve(type="impound")
# #      else
#      basin.impound<-basin.char.retrieve(type="impound",basin.impound.file=basin.impound.file)          
#      
# 
#      #info on column names and what they represent, from Kyle's zonal stats
#      # based on intersection between FWS wetlands survey (CONUS) and med rez flowlines 
#      # open=open water, 
#      # all=wetlands and open water  (so subtract open from all to get just wetlands...)
#           # "ImpoundmentsOpenSqKM"    "ImpoundmentsAllSqKM"      areas intersecting w/ stream network 
#           # "OffChannelOpenSqKM"      "OffChannelAllSqKM"        areas not intersecting w/ stream network
#           # "WetlandsOpenSqKM"        "WetlandsAllSqKM"          areas in drainage area, whether or not on network (should be same as CONUS, but in area instead of %?)
#           # "PercentImpoundedOpen"    "PercentImpoundedAll"      percentage of wetlands or open water that is on the stream network (should be able to derive from above)
#      
#      #rename impounded area stats, 
#      basin.impound$OnChannelWaterSqKM<-basin.impound$ImpoundmentsOpenSqKM
#      basin.impound$OnChannelWetlandSqKM<-basin.impound$ImpoundmentsAllSqKM-basin.impound$ImpoundmentsOpenSqKM
#      basin.impound$OffChannelWaterSqKM<-basin.impound$OffChannelOpenSqKM
#      basin.impound$OffChannelWetlandSqKM<-basin.impound$OffChannelAllSqKM-basin.impound$OffChannelOpenSqKM
#      basin.impound<-basin.impound[,-which(names(basin.impound) %in% c("ImpoundmentsOpenSqKM","ImpoundmentsAllSqKM","WetlandsOpenSqKM","WetlandsAllSqKM",
#                                                                 "PercentImpoundedOpen","PercentImpoundedAll","OffChannelOpenSqKM","OffChannelAllSqKM"))]
# 
#      #classify large and small barriers
#      
#      # TNC codes, and their explanations
#      # 1 = Complete barrier to all fish (12+ feet)
#      # 2 = Small dam barrier (1-12 feet)
#      # 3 = Partial breach
#      # 4 = Barrier with fish ladder
#      # 5 = Unlikely barrier - fully breached, weir, under 1ft dam (also COND=NO DAM or COND=REM or COND=DEL)
#      # 6 = Unknown, assumed full barriers
#      # 7 = Locks
#      basin.impound$large_barriers<-basin.impound$deg_barr_1+basin.impound$deg_barr_4+
#           basin.impound$deg_barr_6+basin.impound$deg_barr_7
#      basin.impound$small_barriers<-basin.impound$deg_barr_2+basin.impound$deg_barr_3
# 
# 
#      #merge with gages spatial info
#      gages.impound.spatial<-merge.sp(gages.spatial,basin.impound,by="FEATUREID",all.x=T,all.y=F,sort=F)
# 
#      return(gages.impound.spatial)
#      
# }




## ----filter gages by impoundments----------------------------------------
# #' @title Filter out gages based on upstream barriers and impoundments
# #' @description Using barrier and impoundment info returned by load.gage.impound()
# #' @param gages.spatial \code{SpatialPointsDataFrame} of gage info including column names "TNC_DamCount" and "OnChannelWaterSqKM"
# #' @return \code{SpatialPointsDataFrame} of gage info that meet default barrier and impoundment criteria
# #' @seealso load.gage.impound
# #' @export
# 
# #uses default requirements for unregulated streams
# #placeholder to eventually allow customization of requirements, but that doesn't seem worth the time right now
# gage.filter.impound<-function(gages.spatial) {
# 
#      gages.spatial.unimpound<-subset(gages.spatial,TNC_DamCount==0 & OnChannelWaterSqKM<.5)
#      return(gages.spatial.unimpound)
# 
# }



## ----trace tnc dams ONLY for basins that dont have impoundment and dam count calculated in basin characteristics----

# internal.count.dams<-function(FEATUREID,plusflow=NULL,dams=NULL) {
#      #trace tnc dams ONLY for basins that dont have impoundment and dam count calculated in basin characteristics
#      small<-large<-0
#      if (is.null(plusflow))
#           plusflow<-read.dbf("C:/ALR/Data/StreamData/NHDplus/NHDPlusAttributes/0205/PlusFLow.dbf")
#      if (is.null(dams))
#           dams<-read.dbf("C:/ALR/Data/ConnectivityData/TNC_100k/dams_on_med_rez.dbf")
#      max.length<-0
#      
#      #iteratively select all features upstream of user chosen feature
#      queue<-c(FEATUREID) #queue of flowline segments that need to be traced upstream      
#      while (length(queue)>0) {
#           large<-large+sum(dams[dams$COMID==queue[1], "large"])
#           small<-small+sum(dams[dams$COMID==queue[1], "small"])
#           
#           queue<-c(queue[-1],plusflow[plusflow$TOCOMID==queue[1],"FROMCOMID"]) 
#                #remove 1st segment from the queue, 
#                #and add all segments that flow into it to the queue
#           
#           #clean up to eliminate uneccesary looping due to duplicates or terminal headwater features
#           queue<-queue[!(queue==0)]  #in the NHDplus tables, features with FROMCOMID==0 have no inflowing tribs
#           queue<-unique(queue)     #eliminate duplicates
#           max.length<-max(max.length,length(queue))          
#      }#end while loop
# #      print(max.length)
#      return( data.frame(FEATUREID=FEATUREID, large=large, small=small) )
# }



## ------------------------------------------------------------------------
# 
# #' @title iteratively trace nhdplus network upstream and cound # and size of dams
# #' @description for gages that don't have dam counts and impounded area in basin characteristics only
# #' @export
# impound.trace<-function(gages.spatial, 
#                          dam.file="C:/ALR/Data/ConnectivityData/TNC_100k/dams_on_med_rez.dbf", 
#                          plusflow.file="C:/ALR/Data/StreamData/NHDplus/NHDPlusAttributes/0205/PlusFLow.dbf") {
#      cat("Loading dam information...\r")
#           
#      plusflow<-read.dbf(plusflow.file)
#      
#      deg.barr.large<- c(1,4,6,7)
#      deg.barr.small<-c(2,3)
#      
#      dams<-read.dbf(dam.file)
#      dams<-dams[,c("UNIQUE_ID","COMID","deg_barr","NotOnHydro")]
#      dams$large<-as.numeric(dams$deg_barr %in% deg.barr.large)
#      dams$small<-as.numeric(dams$deg_barr %in% deg.barr.small)
#      
#      f<-unique(gages.spatial$FEATUREID)
#      f<-f[!is.na(f)]
#      
#      f.d<-c()
#      cat(paste("Starting to trace network upstream for",length(f),"gages...","\r","    "))
#      for (i in 1:length(f)) {
#           cat( paste( "... ", i ))
#           f.d<-rbind( f.d,internal.count.dams( FEATUREID=f[i], plusflow=plusflow, dams=dams ) )
#      
#      }
# 
#      gages.spatial<-merge.sp(gages.spatial, f.d, by = "FEATUREID")
# #      gages.spatial<-merge(gages.spatial, f.d, all.x=T, all.y=F, by="FEATUREID", sort=F) 
# #      gages.spatial@data<-merge(gages.spatial@data, f.d, all.x=T, all.y=F, by="FEATUREID", sort=F) 
# 
#      return(gages.spatial)
# 
# }


## ------------------------------------------------------------------------
# #' @title filter by dams traced through gage.trace.dams
# #' @description filter by dams traced through gage.trace.dams
# #' @export
# 
# gage.filter.dams<-function(gages.spatial) {
#      gages.spatial<-subset(g.spatial,!is.na(FEATUREID))
#      gages.spatial<-subset(gages.spatial,large==0 & small==0)
#      return(gages.spatial)
# }



## ----gage basin characteristics------------------------------------------
# 
# #' @title Get basin characteristics associated with each gage
# #' @description Get basin characteristics associated with each gage, or more precisely, with the NHDplus stream reach each gage is plotted to.
# #' @param gages.spatial \code{SpatialPointsDataFrame} of gage info from plot.gages.nhdplus()
# #' @param basin.char.file \code{character} location andname of .Rdata file that contains UpstreamStats, by NHDplus FEATUREID
# #' @return \code{SpatialPointsDataFrame} of gage info including upstream basin characteristics
# #' @seealso plot.gages.nhdplus, load.gage.impound
# #' @export
# 
# gage.load.char<-function(gages.spatial, basin.char.file=NULL, use.default.cols=F) {
#      #something to improve later: if there are basins that don't match, outputs generic warning
#      #   ## Warning: 101 records in y cannot be matched to x
#      # but should customize it so that it's specific to our x and y
#      
#      
#      #also, see if i can find a better solution to sp merge issue
#      
#      #load Kyle's zonal stats
# #      load(basin.char.file)
# #      basin.char<-get("UpstreamStats")
# #      if (is.null(basin.char.file))
# #           basin.char<-basin.char.retrieve( type="char" )
# #      else
#      basin.char<-basin.char.retrieve( type="char", basin.char.file=basin.char.file )
#      
#      if (use.default.cols){
#           basin.char<-basin.char[,c("FEATUREID","Forest","Herbacious","Agriculture",
#                                    "Developed","DevelopedNotOpen","Impervious",
#                                    "AnnualTmaxC","AnnualTminC",
#                                    "AnnualPrcpMM","SummerPrcpMM","WinterPrcpMM",
#                                    "DrainageClass","HydrologicGroupAB","HydrologicGroupCD","SurficialCoarseC","PercentSandy",
#                                    "ReachElevationM","BasinElevationM","ReachSlopePCNT","BasinSlopePCNT","TotDASqKM")]
#                                    #right now, limiting the number of columns, for readability. 
#                                    #can change this later, once we figure out how basin char will be accessed
#      }
# 
#      gages.char.spatial<-merge.sp( gages.spatial, basin.char, by="FEATUREID", all.x=T, all.y=F, sort=F)
# 
#      return(gages.char.spatial)
# }


