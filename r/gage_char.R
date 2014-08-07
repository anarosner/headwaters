
## ------------------------------------------------------------------------
#this is a placeholder
#right now, just loads rdata w/ data.frame's given to me by kyle
#in the future, maybe this will be replaced by a rest api call, or something similar?
basin.char.retrieve<-function(type="char",
                            basin.impound.file="C:/ALR/Models_from_others/Kyle/NH NY NHDplus basin zonal stats KO/NENY_CovariateData_Impoundments_2014-01-23.RData",
                            basin.char.file="C:/ALR/Models_from_others/Kyle/NH NY NHDplus basin zonal stats KO/NENY_CovariateData_2014-01-23.RData") {
     if (type == "char")
          load(basin.char.file)
     else if (type == "impound")
          load(basin.impound.file)
     
     temp<-get("UpstreamStats")
     return(temp)
}


## ----impoundments--------------------------------------------------------


#' @title Get information on upstream barriers and impoundments
#' @description Load and attach information for each gage on upstream barriers and impoundments.  Uses TNC barrier inventory and FWS survey openwater and wetlands. 
#' (Open water and wetlands are intersected with NHDplus med res flowlines, to differentiate on- and off-network wetlands and waters)
#' @param gages.spatial \code{SpatialPointsDataFrame} of gage info from plot.gages.nhdplus()
#' @param basin.impound.file \code{character} location and name .Rdata file that contains UpstreamStatsImpounded, by NHDplus FEATUREID
#' @return \code{SpatialPointsDataFrame} of gage info including TNC barriers and area of wetlands and open water
#' @seealso plot.gages.nhdplus, load.gage.char, filter.gage.impound
#' @export

gage.load.impound<-function(gages.spatial, basin.impound.file=NULL)
          {
       #      gage.load.impound<-function(gages.spatial, 
       # basin.impound.file="C:/ALR/Models_from_others/Kyle/NH NY NHDplus basin zonal stats KO/NENY_CovariateData_Impoundments_2014-01-23.RData") 


     #load Kyle's zonal stats
#      load(basin.impound.file, verbose = T)
#      basin.impound<-get(x = "UpstreamStatsImpounded")
     if (is.null(basin.impound.file))
          basin.impound<-basin.char.retrieve(type="impound")
     else
          basin.impound<-basin.char.retrieve(type="impound",basin.impound.file=basin.impound.file)          
     

     #info on column names and what they represent, from Kyle's zonal stats
     # based on intersection between FWS wetlands survey (CONUS) and med rez flowlines 
     # open=open water, 
     # all=wetlands and open water  (so subtract open from all to get just wetlands...)
          # "ImpoundmentsOpenSqKM"    "ImpoundmentsAllSqKM"      areas intersecting w/ stream network 
          # "OffChannelOpenSqKM"      "OffChannelAllSqKM"        areas not intersecting w/ stream network
          # "WetlandsOpenSqKM"        "WetlandsAllSqKM"          areas in drainage area, whether or not on network (should be same as CONUS, but in area instead of %?)
          # "PercentImpoundedOpen"    "PercentImpoundedAll"      percentage of wetlands or open water that is on the stream network (should be able to derive from above)
     
     #rename impounded area stats, 
     basin.impound$OnChannelWaterSqKM<-basin.impound$ImpoundmentsOpenSqKM
     basin.impound$OnChannelWetlandSqKM<-basin.impound$ImpoundmentsAllSqKM-basin.impound$ImpoundmentsOpenSqKM
     basin.impound$OffChannelWaterSqKM<-basin.impound$OffChannelOpenSqKM
     basin.impound$OffChannelWetlandSqKM<-basin.impound$OffChannelAllSqKM-basin.impound$OffChannelOpenSqKM
     basin.impound<-basin.impound[,-which(names(basin.impound) %in% c("ImpoundmentsOpenSqKM","ImpoundmentsAllSqKM","WetlandsOpenSqKM","WetlandsAllSqKM",
                                                                "PercentImpoundedOpen","PercentImpoundedAll","OffChannelOpenSqKM","OffChannelAllSqKM"))]

     #classify large and small barriers
     
     # TNC codes, and their explanations
     # 1 = Complete barrier to all fish (12+ feet)
     # 2 = Small dam barrier (1-12 feet)
     # 3 = Partial breach
     # 4 = Barrier with fish ladder
     # 5 = Unlikely barrier - fully breached, weir, under 1ft dam (also COND=NO DAM or COND=REM or COND=DEL)
     # 6 = Unknown, assumed full barriers
     # 7 = Locks
     basin.impound$large_barriers<-basin.impound$deg_barr_1+basin.impound$deg_barr_4+
          basin.impound$deg_barr_6+basin.impound$deg_barr_7
     basin.impound$small_barriers<-basin.impound$deg_barr_2+basin.impound$deg_barr_3


     #merge with gages spatial info
     gages.impound.spatial<-merge.sp(gages.spatial,basin.impound,by="FEATUREID",all.x=T,all.y=F,sort=F)

     return(gages.impound.spatial)
     
}




## ----filter gages by impoundments----------------------------------------
#' @title Filter out gages based on upstream barriers and impoundments
#' @description Using barrier and impoundment info returned by load.gage.impound()
#' @param gages.spatial \code{SpatialPointsDataFrame} of gage info including column names "TNC_DamCount" and "OnChannelWaterSqKM"
#' @return \code{SpatialPointsDataFrame} of gage info that meet default barrier and impoundment criteria
#' @seealso load.gage.impound
#' @export

#uses default requirements for unregulated streams
#placeholder to eventually allow customization of requirements, but that doesn't seem worth the time right now
gage.filter.impound<-function(gages.spatial) {

     gages.spatial.unimpound<-subset(gages.spatial,TNC_DamCount==0 & OnChannelWaterSqKM<.5)
     return(gages.spatial.unimpound)

}



## ----trace tnc dams ONLY for basins that dont have-----------------------

internal.count.dams<-function(FEATUREID,plusflow=NULL,dams=NULL) {
     small<-large<-0
     if (is.null(plusflow))
          plusflow<-read.dbf("C:/ALR/Data/StreamData/NHDplus/NHDPlusAttributes/0205/PlusFLow.dbf")
     if (is.null(dams))
          dams<-read.dbf("C:/ALR/Data/ConnectivityData/TNC_100k/dams_on_med_rez.dbf")
     max.length<-0
     
     #iteratively select all features upstream of user chosen feature
     queue<-c(FEATUREID) #queue of flowline segments that need to be traced upstream      
     while (length(queue)>0) {
          large<-large+sum(dams[dams$COMID==queue[1], "large"])
          small<-small+sum(dams[dams$COMID==queue[1], "small"])
          
          queue<-c(queue[-1],plusflow[plusflow$TOCOMID==queue[1],"FROMCOMID"]) 
               #remove 1st segment from the queue, 
               #and add all segments that flow into it to the queue
          
          #clean up to eliminate uneccesary looping due to duplicates or terminal headwater features
          queue<-queue[!(queue==0)]  #in the NHDplus tables, features with FROMCOMID==0 have no inflowing tribs
          queue<-unique(queue)     #eliminate duplicates
          max.length<-max(max.length,length(queue))          
     }#end while loop
#      print(max.length)
     return( data.frame(FEATUREID=FEATUREID, large=large, small=small) )
}



## ------------------------------------------------------------------------

#' @title iteratively trace nhdplus network upstream and cound # and size of dams
#' @description for gages that don't have dam counts and impounded area in basin characteristics only
#' @export
gage.trace.dams<-function(gages.spatial, 
                         dam.file="C:/ALR/Data/ConnectivityData/TNC_100k/dams_on_med_rez.dbf", 
                         plusflow.file="C:/ALR/Data/StreamData/NHDplus/NHDPlusAttributes/0205/PlusFLow.dbf") {
     cat("Loading dam information...\r")
          
     plusflow<-read.dbf(plusflow.file)
     
     deg.barr.large<- c(1,4,6,7)
     deg.barr.small<-c(2,3)
     
     dams<-read.dbf(dam.file)
     dams<-dams[,c("UNIQUE_ID","COMID","deg_barr","NotOnHydro")]
     dams$large<-as.numeric(dams$deg_barr %in% deg.barr.large)
     dams$small<-as.numeric(dams$deg_barr %in% deg.barr.small)
     
     f<-unique(gages.spatial$FEATUREID)
     f<-f[!is.na(f)]
     
     f.d<-c()
     cat(paste("Starting to trace network upstream for",length(f),"gages...","\r","    "))
     for (i in 1:length(f)) {
          cat( paste( "... ", i ))
          f.d<-rbind( f.d,internal.count.dams( FEATUREID=f[i], plusflow=plusflow, dams=dams ) )
     
     }

     gages.spatial<-merge.sp(gages.spatial, f.d, by = "FEATUREID")
#      gages.spatial<-merge(gages.spatial, f.d, all.x=T, all.y=F, by="FEATUREID", sort=F) 
#      gages.spatial@data<-merge(gages.spatial@data, f.d, all.x=T, all.y=F, by="FEATUREID", sort=F) 

     return(gages.spatial)

}


## ------------------------------------------------------------------------
#' @title filter by dams traced through gage.trace.dams
#' @description filter by dams traced through gage.trace.dams
#' @export

gage.filter.dams<-function(gages.spatial) {
     gages.spatial<-subset(g.spatial,!is.na(FEATUREID))
     gages.spatial<-subset(gages.spatial,large==0 & small==0)
     return(gages.spatial)
}


## ----gage basin characteristics------------------------------------------

#' @title Get basin characteristics associated with each gage
#' @description Get basin characteristics associated with each gage, or more precisely, with the NHDplus stream reach each gage is plotted to.
#' @param gages.spatial \code{SpatialPointsDataFrame} of gage info from plot.gages.nhdplus()
#' @param basin.char.file \code{character} location andname of .Rdata file that contains UpstreamStats, by NHDplus FEATUREID
#' @return \code{SpatialPointsDataFrame} of gage info including upstream basin characteristics
#' @seealso plot.gages.nhdplus, load.gage.impound
#' @export

gage.load.char<-function(gages.spatial, basin.char.file=NULL, use.default.cols=F) {
     #something to improve later: if there are basins that don't match, outputs generic warning
     #   ## Warning: 101 records in y cannot be matched to x
     # but should customize it so that it's specific to our x and y
     
     
     #also, see if i can find a better solution to sp merge issue
     
     #load Kyle's zonal stats
#      load(basin.char.file)
#      basin.char<-get("UpstreamStats")
     if (is.null(basin.char.file))
          basin.char<-basin.char.retrieve( type="char" )
     else
          basin.char<-basin.char.retrieve( type="char", basin.char.file=basin.char.file )
     
     if (use.default.cols){
          basin.char<-basin.char[,c("FEATUREID","Forest","Herbacious","Agriculture",
                                   "Developed","DevelopedNotOpen","Impervious",
                                   "AnnualTmaxC","AnnualTminC",
                                   "AnnualPrcpMM","SummerPrcpMM","WinterPrcpMM",
                                   "DrainageClass","HydrologicGroupAB","HydrologicGroupCD","SurficialCoarseC","PercentSandy",
                                   "ReachElevationM","BasinElevationM","ReachSlopePCNT","BasinSlopePCNT","TotDASqKM")]
                                   #right now, limiting the number of columns, for readability. 
                                   #can change this later, once we figure out how basin char will be accessed
     }

     gages.char.spatial<-merge.sp( gages.spatial, basin.char, by="FEATUREID", all.x=T, all.y=F, sort=F)

     return(gages.char.spatial)
}

