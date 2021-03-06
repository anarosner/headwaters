% Generated by roxygen2 (4.0.1): do not edit by hand
\name{gage.place.weather}
\alias{gage.place.weather}
\title{plot gages to weather grid}
\usage{
gage.place.weather(gages.spatial, plot = F)
}
\arguments{
\item{gages.spatial}{\code{SpatialPolygonsDataFrame}}

\item{plot}{\code{boolean} if true, will plot a map of gages with the weather grid overlaid}
}
\value{
\code{SpatialPolygonsDataFrame}
}
\description{
plot to a weather grid polygon, based on all the files in the mauer daily east data set
}
\seealso{
\code{\link{conteStreamflow::gage.retrieve}}, \code{\link{conteStreamflow::weather.retrieve}}
}

