
## ------------------------------------------------------------------------
#' @title Get gage location and information from NWIS
#' @description Get gage information, including site_no and lat/long coordinates from NWIS, by specifying a list of states to search within.
#' @param states \code{character vector} of state abbreviations
#' @param max.da.sqkm \code{numeric} filter gages by min and max drainage area
#' @param min.da.sqkm
#' @param temp.dir \code{character} directory to save temporary files and gage retrieval logs/metadata
#' @param save.log \code{boolean} whether to save a log
#' @return \code{data.frame}
#' @keywords nwis, gage

gage.get<-function(states=c( "CT", "ME", "MA", "NH", "NY", "RI", "VT"), max.da.sqkm=50, min.da.sqkm=0, 
                    temp.dir="C:/ALR/Models_processed_data/flow_timeseries", save.log=T, log.dir="C:/ALR/Models_processed_data/flow_timeseries") {     
                              #right now, using my local directory as the defaults.  this is bad, will replace later, etc.

     
     # #get gage info, sites w/ daily flow
#              (select by list of states, because something is 
#                weird w/ hydr unit codes in nw vt)
#    by default, states included are CT, ME, MA, NH, NY, RI, VT

     # paste0("&state_cd=",states,collapse="")
     # write url to retrieve gage info from nwis 
     # (at time of writing, can't do this using waterData package or other webservices I know of, so we're creating/using a url)
     gage.info.url<-paste0("http://nwis.waterdata.usgs.gov/nwis/dvstat?referred_module=sw",
     #      "&state_cd=ct&state_cd=me&state_cd=ma&state_cd=nh&state_cd=ny&state_cd=ri&state_cd=vt",
          paste0("&state_cd=",states,collapse=""),
          "&site_tp_cd=OC&site_tp_cd=OC-CO&site_tp_cd=ES&site_tp_cd=LK&site_tp_cd=ST&site_tp_cd=ST-CA&site_tp_cd=ST-DCH&site_tp_cd=ST-TS&index_pmcode_00060=1&group_key=NONE&format=sitefile_output&sitefile_output_format=rdb&column_name=agency_cd&column_name=site_no&column_name=station_nm&column_name=dec_lat_va&column_name=dec_long_va&column_name=dec_coord_datum_cd&column_name=huc_cd&column_name=drain_area_va&column_name=sv_begin_date&column_name=sv_end_date&column_name=sv_count_nu&list_of_search_criteria=state_cd%2Csite_tp_cd%2Crealtime_parameter_selection")

     #use wget to save into temporary folder, and then read it in and parse.  probably there's a better way to do this, but this works for now.  
     setwd(temp.dir)
     system(paste("wget -O ./raw_gage_info.txt",gage.info.url) )

     #remove extra header 
     raw<-readLines("raw_gage_info.txt")
     line<-max(grep("agency_cd",raw))
     clean<-raw[c(line,(line+2):length(raw))]

     #save metadata from raw nwis file, and 
     if (save.log) {
          if (is.null(log.dir)) 
               warning("Must specify directory to save log, if save.log==T")
          else {
               #save metadata as file and list of gage site_no's
               writeLines(raw[1:(line-1)],"gages_meta.txt")
               # [13] "#  agency_cd       -- Agency"                                              
               # [14] "#  site_no         -- Site identification number"                          
               # [15] "#  station_nm      -- Site name"                                           
               # [16] "#  dec_lat_va      -- Decimal latitude"                                    
               # [17] "#  dec_long_va     -- Decimal longitude"                                   
               # [18] "#  coord_acy_cd    -- Latitude-longitude accuracy"                         
               # [19] "#  dec_coord_datum_cd -- Decimal Latitude-longitude datum"                 
               # [20] "#  huc_cd          -- Hydrologic unit code"                                
               # [21] "#  drain_area_va   -- Drainage area"                                       
               # [22] "#  sv_begin_date   -- Site-visit data begin date"                          
               # [23] "#  sv_end_date     -- Site-visit data end date"                            
               # [24] "#  sv_count_nu     -- Site-visit data count"
          }
     }
     
     #read back in raw file saved in temp directory
     #change from raw "lines" to table
     setwd(temp.dir)
     gages.all<-read.table(text=clean,header=T,sep="\t",fill=T,
                       colClasses=c("site_no"="character","huc_cd"="character",
                                    "sv_begin_date"="Date", "sv_end_date"="Date"))
     
     #make sure all gage info includes IDs, coords
     if (sum(is.na(gages.all$site_no))>0)
          warning(paste("Missing site identifiers:\n", sum(is.na(gages.all$site_no)), "sites do not have site_no values and are being ignored"))
     if (sum(is.na(gages.all$dec_lat_va),is.na(gages.all$dec_long_va))>0)
          warning(paste("Missing geographic coordinates:\n", 
                        sum(is.na(gages.all$dec_lat_va)), "sites do not have lat coordinates and are being ignored\n",
                        sum(is.na(gages.all$dec_long_va)), "sites do not have long coordinates and are being ignored"))

     gages.all<-subset(gages.all,
                       subset=(!is.na(gages.all$site_no) & !is.na(gages.all$dec_lat_va) & !is.na(gages.all$dec_long_va) ))
     
     #convert to sq km, and filter by size
     gages.all$da_sqkm<-gages.all$drain_area_va*2.58999
     gages.subset<-subset(x=gages.all,  gages.all$da_sqkm<=max.da.sqkm & gages.all$da_sqkm>min.da.sqkm )

     print("==========================")
     print("Gage retrieval complete")
     print(paste0(nrow(gages.subset),  " gages identified\n"))
     print(paste0("   (", nrow(gages.all)-nrow(gages.subset), " eliminated due to specified size requirements:"))
     print(paste0(" >", min.da.sqkm, " and <=", max.da.sqkm, " sq km)" ))
     print("==========================")
     
     return(gages.subset)
}






## ----plot gages spatially------------------------------------------------

#' @title Create SpatialPointsDataFrame from data.frame of gage information
#' @description Uses gage info created by get.gages, uses the columns with lat and long coordinate info to turn it into a spatial object (SpatialPointsDataFrame), preserving all data in the data.frame
#' @param gages.df \code{data.frame} of gage information, including columns titled "dec_long_va" and "dec_lat_va"
#' @param proj4 \code{character}  coordinate system, in proj4 syntax.  Defaults to GCS NAD83, which is used by NHDplus
#' @return \code{SpatialPointsDataFrame}
#' @keywords gage, SpatialPointsDataFrame
#' @seealso get.gages

gage.plot<-function(gages.df, proj4="+proj=longlat +ellps=GRS80 +datum=NAD83 +no_defs") {
     if (!("dec_long_va" %in% names(gages.df) & "dec_lat_va" %in% names(gages.df) ))
          stop("Input \"coords_df\" must include columns \"dec_long_va\" and \"dec_lat_va\"")
     gages.spatial<-SpatialPointsDataFrame(coords=as.matrix(gages_df[,c("dec_long_va","dec_lat_va")]), 
                      data=gages,
                      proj4string=CRS(proj4),
                      match.ID=F)

     return(gages.spatial)
}


## ----refine gages within a buffer----------------------------------------

#' @title Filter gages based on whether they fall within a geographic buffer
#' @description Uses spatial gage object (SpatialPointsDataFrame) created by plot.gages(), 
#' and filters them based on whether they fall within a buffer.  Default buffer around CT River basin states is specified.
#' @param gages.spatial SpatialPointsDataFrame produced by plot.gages() function
#' @param proj4 character  coordinate system, in proj4 syntax.  Defaults to GCS NAD83, which is used by NHDplus
#' @return SpatialPointsDataFrame
#' @keywords gage, SpatialPointsDataFrame
#' @seealso get.gages


gage.buffer<-function(gages.spatial, plot=F, proj4="+proj=longlat +ellps=GRS80 +datum=NAD83 +no_defs",
                            buffer.file="C:/ALR/Model_processed_dataflow_gages/ctr_states_buffer_no_islands2") {
                         #right now, using my local directory as the defaults.  this is bad, will replace later, etc.

     orig.n.gages<-nrow(gages.spatial)
     
     #eliminate gages outside a defined buffer
     #    original buffer is a buffer around the CT River states (i.e. not all of NY and ME, even though they're included in the request to nwis)
     #    excluding Cape Cod and Long Island

     
#           #I'm thinking about putting this buffer (probably an rdata file, maybe a shapefile)
#           #in the package's data directory, as a sample data set... is that a good idea?
#           {
#           if (is.null(buffer.file)) {}
#           else {
#           }}
          
     
     #load buffer
     #    default is a 100 km buffer around CT R states, but does not include Cape Cod and Long Island     
     buffer<-readShapePoly(buffer.file,proj4string=CRS(proj4))

     in.buffer<-!is.na(over(gages.spatial,buffer)[,1])
     gages.spatial<-gages.spatial[in.buffer,]


     if (plot) {
          #create spatialpoint object from coordinates (coordinates are listed in the order long, lat)
          par(mfrow=c(1,1))
          plot(gages.spatial)
          plot(gages.spatial,add=T,col="red")
          print(paste(nrow(gages.spatial), "gages"))
          print(paste0("     (", orig.n.gages-nrow(gages.spatial)," gages eliminated)"))
     }

     return(gages.spatial)

}


## ----plot gages to nhdplus catchments and assign featureid---------------

#' @title Assigns each gage to a NHDplus catchment/stream reach
#' @description Uses spatial gage object (SpatialPointsDataFrame) created by plot.gages() and optionally plot.gages.buffer().  
#' Loads NHDplus catchment file, determines which catchment each gages falls within, and assigns a FEATUREID
#' @param gages.spatial \code{SpatialPointsDataFrame} produced by plot.gages() function.  Optionally, can be first filtered using plot.gages.buffer() before assigning NHDplus IDs
#' @param proj4 \code{character} coordinate system, in proj4 syntax.  Defaults to GCS NAD83, which is used by NHDplus
#' @param catchment.file \code{character} location and name shapefile of NHDplus catchments (that cover regions that the gages are in)
#' @return \code{SpatialPointsDataFrame}
#' @keywords gage, NHDplus
#' @seealso plot.gages, plot.gages.buffer


gage.plot.nhdplus<-function(gages.spatial, proj4="+proj=longlat +ellps=GRS80 +datum=NAD83 +no_defs", 
                             catchment.file="C:/ALR/Data/StreamData/NHDplus/NHDPlusCatchment/NENY/Catchment", save.catch=T) {
                              #right now, using my local directory as the defaults.  this is bad, will replace later, etc.

     catchments<-readShapePoly(catchment.file,proj4string=CRS(proj4),IDvar="FEATUREID")
     if (save.catch)
          assign(x = "catchments",value = catchments,envir = .GlobalEnv)
     match<-over(gages.spatial,catchments)
     
     gages.spatial@data$FEATUREID<-match$FEATUREID

     if ( sum(is.na(gages.spatial$FEATUREID))>0 ) 
          warning(paste(sum(is.na(gages.spatial$FEATUREID)), "gages did not map to a NHDplus catchement:\n",
                        gages.spatial[is.na(gages.spatial$FEATUREID),"site_no"]))
     
     return(gages.spatial)
}



