% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/csquare.R
\name{csquare_to_dd}
\alias{csquare_to_dd}
\title{csquare code conversion to decimal degrees}
\usage{
csquare_to_dd(code)
}
\arguments{
\item{code}{csquare code}
}
\value{
a list with decimal degrees lat and lon and resolution
}
\description{
The csquare code is explained here:
http://www.cmar.csiro.au/csquares/csq-faq.htm
}
\examples{
\dontrun{
csquare_to_dd("7307:487:380:383")
}
}
\seealso{
Other spatial: 
\code{\link{am_csc_from_extent}()},
\code{\link{am_map_leaflet}()},
\code{\link{am_raster}()},
\code{\link{am_species_in_csc}()},
\code{\link{am_species_per_csc}()},
\code{\link{csquare_from_dd}()},
\code{\link{rasterize_coords}()},
\code{\link{which_cells_in_raster}()}
}
\concept{spatial}
