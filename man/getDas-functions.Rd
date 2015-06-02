\name{getDas-functions}
\alias{getDas-functions}
\alias{setDasServer}
\alias{getDasServer}
\alias{getDasSource}
\alias{getDasDsn}
\alias{getDasEntries}
\alias{getDasTypes}
\title{
	DAS metadata information handling
}
\description{
	Functions to query metadata from the DAS server, 
}
\usage{
setDasServer(server)
getDasServer()
getDasSource()
getDasDsn()
getDasEntries(source, as.GRanges=TRUE)
getDasTypes(source)
}
\arguments{
  \item{server}{
		URL of the DAS server.
	}
	\item{source}{
  	The data source id to get the metadata from. Be sure this information is coherent with those
		returned by \code{\link{getDasSource}}. In somes cases, depending on the server, you might need
		to use the source title, instead of the source id. 
	}
	\item{as.GRanges}{
		For \code{getDasEntries}, return a GRanges object instead of a data.frame (if possible)
	}
}
\details{
	The functions documented in this man page are used to set the session's DAS server and retrieve metadata from it.

	\itemize{
		\item{setDasServer} sets the URL of the DAS server to be used.
		\item{getDasServer} returns the URL of the DAS server used in this session
    \item{getDasSource} returns the id's, the titles and the capabilities of the data sources available in the server
		\item{getDasDsn} returns the id's of the dsn data sources available in the server
		\item{getDasEntries} returns the entry points  of the given data source
		\item{getDasTypes} returns the types (a.k.a. tables) of the given data source
	}
  
  Please, be sure to pass the identifiers EXACTLY as the values returned by the functions 
  available here (DAS is case-sensitive). It is also recommended that you check the capabilities 
  for each of the data sources.
  
	For performance reasons, the values in the main calls \code{\link{getDasFeature},
	\link{getDasSequence},\link{getDasStructure}} are not validated against the server. 
	If you pass wrong values for \code{source, ranges or types} it will result in a NULL
	 output of the function, but no error will rise.

	
}
\value{
	For \code{getDasSever, getDasDsn and getDasTypes} a \code{character} vector

	For \code{getDasSource} a \code{data.frame} with the information of the DBs

	For \code{getDasEntries} a \code{\link[GenomicRanges]{GRanges}} (\code{as.GRanges=TRUE})
		 or \code{data.frame} otherwise
}
\author{
	Oscar Flores <oflores@mmb.pcb.ub.es>
  
  Anna Mantsoki <anna.mantsoki@bsc.es>
}
\seealso{
	\code{\link{getDasFeature} \link{getDasSequence} \link{getDasStructure}}
}
\examples{
	#Set session server to EMBL-EBI Genomic DAS Server
  
  setDasServer(server="http://www.ebi.ac.uk/das-srv/genomicdas/das")
	
  print(getDasServer())
  
  sources=getDasSource()
  print(sources)
  
  dsn_sources = getDasDsn()
  print(dsn_sources)
  
  entries = getDasEntries(sources$id[[1]])
  print(entries)
	
  types = getDasTypes(sources$id[[1]])
  print(types)
}
\keyword{ misc }
