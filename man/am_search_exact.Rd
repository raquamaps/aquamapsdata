% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data.R
\name{am_search_exact}
\alias{am_search_exact}
\title{Exact search for taxonomic names}
\usage{
am_search_exact(
  SpeciesID = NULL,
  SpecCode = NULL,
  Genus = NULL,
  Species = NULL,
  FBname = NULL,
  OccurRecs = NULL,
  OccurCells = NULL,
  StockDefs = NULL,
  Kingdom = NULL,
  Phylum = NULL,
  Class = NULL,
  Order = NULL,
  Family = NULL,
  deepwater = NULL,
  angling = NULL,
  diving = NULL,
  dangerous = NULL,
  m_invertebrates = NULL,
  highseas = NULL,
  invasive = NULL,
  resilience = NULL,
  iucn_id = NULL,
  iucn_code = NULL,
  iucn_version = NULL,
  provider = NULL
)
}
\arguments{
\item{SpeciesID}{AquaMaps unique identifier for a valid species used by the
Catalogue of Life Annual Checklist (www.catalogueoflife.org). Example for
the whale shark: Fis-30583}

\item{SpecCode}{Species identifier used in FishBase or SeaLifeBase}

\item{Genus}{Genus name of the species}

\item{Species}{Specific epithet of the species}

\item{FBname}{Common name suggested by FishBase or SeaLifeBase}

\item{OccurRecs}{Number of point records used to generate good cells}

\item{OccurCells}{Number of good cells used to generate species envelope}

\item{StockDefs}{Distribution of the species as recorded in
FishBase or SeaLifeBase}

\item{Kingdom}{Kingdom to which the species belongs}

\item{Phylum}{Phylum to which the species belongs}

\item{Class}{Class to which the species belongs}

\item{Order}{Order to which the species belongs}

\item{Family}{Family to which the species belongs}

\item{deepwater}{Does the species occur in the deep-sea (i.e. tagged
bathypelagic or bathydemersal in FishBase or SeaLifeBase)? 0=No, 1=Yes}

\item{angling}{Is the species a sport fish (i.e. tagged as a GameFish in
FishBase)? 0=No, 1=Yes}

\item{diving}{Is the species found on a dive (i.e. where DepthPrefMin in
HSPEN < 20 meters)? 0=No, 1=Yes}

\item{dangerous}{Is the species dangerous (i.e. tagged as traumatogenic or
venonous in FishBase or SeaLifeBase)? 0=No, 1=Yes}

\item{m_invertebrates}{Is the species a marine invertebrate? 0=No, 1=Yes}

\item{highseas}{Is the species an open ocean fish species (i.e. tagged as
pelagic-oceanic in FishBase)? 0=No, 1=Yes}

\item{invasive}{Is the species recorded to be invasive (i.e. in FishBase
or SeaLifeBase)? 0=No, 1=Yes}

\item{resilience}{Resilience of the species (i.e. as recorded in
FishBase/SeaLifeBase)}

\item{iucn_id}{IUCN species identifier}

\item{iucn_code}{IUCN Red list classification assigned to the species}

\item{iucn_version}{IUCN version}

\item{provider}{FishBase (FB) or SeaLifeBase (SLB)?}
}
\value{
tibble with results...
}
\description{
Search taxonomic names with or without parameters specified.
\itemize{
\item If parameters are given, the intersection of matching records
are returned.
\item If no parameters are given, all records are returned.
\item If parameters are specified with NULL specified, examples of
valid combinations of values are returned.
}
}
\examples{
\dontrun{
am_search_exact()
am_search_exact(Species = "bucculentus")
am_search_exact(FBname = NULL, provider = NULL)
}
}
\seealso{
Other general: 
\code{\link{\%>\%}()},
\code{\link{am_hcaf}()},
\code{\link{am_hspen}()},
\code{\link{am_search_fuzzy}()},
\code{\link{download_db}()}
}
\concept{general}
