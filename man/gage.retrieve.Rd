% Generated by roxygen2 (4.0.1): do not edit by hand
\name{gage.retrieve}
\alias{gage.retrieve}
\title{Get gage location and information from NWIS}
\usage{
gage.retrieve(buffer.file = NULL,
  proj4 = "+proj=longlat +ellps=GRS80 +datum=NAD83 +no_defs",
  states = c("CT", "ME", "MA", "NH", "NY", "RI", "VT"), max.da.sqkm = 50,
  min.da.sqkm = 0,
  temp.dir = "C:/ALR/Models_processed_data/flow_timeseries", log.dir = NULL)
}
\arguments{
\item{buffer.file}{\code{character} file name and location of a shapefile (polygon) that outlines area to find gages within}

\item{states}{\code{character vector} of state abbreviations to search within.  This will only be used if buffer.file is not provided.}

\item{max.da.sqkm}{\code{numeric} filter gages by min and max drainage area}

\item{min.da.sqkm}{}

\item{temp.dir}{\code{character} directory to save temporary files and gage retrieval logs/metadata}

\item{log.dir}{\code{character} if provided, will save a log file into the specified directory}
}
\value{
\code{SpatialPointsDataFrame} of gages within the buffer, with gage info from NWIS
}
\description{
Get gage information, including site_no and lat/long coordinates from NWIS, by specifying either
            1) a shapefile outlining an area in the eastern U.S., or
            2) a list of states to search within.
}
\keyword{gage}
\keyword{nwis,}
