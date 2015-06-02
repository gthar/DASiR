\name{getDasFeature}
\alias{getDasFeature}
\title{
	Query for features to session's DAS server
}
\description{
	This function retrieves information for the features for the given ranges and types using the
	corresponding data source id (or title) in the session's DAS server
}
\usage{
getDasFeature(source, ranges, types)
}
\arguments{
  \item{source}{
		Data source id (or title). See \code{\link{getDasSource}}
	}
  \item{ranges}{
		\code{\link{GRanges}} object containing the seqname(s) and range(s) to query.
		Be sure this information is coherent with those returned by \code{\link{getDasEntries}}
	}
  \item{types}{
		Names of the features to retrieve. They must match one or more of the values returned
		by \code{\link{getDasTypes}}
	}
}
\details{
	This function queries the DAS server and returns the available information for the  type(s) at the given range(s).
  
  Please, be sure to pass the identifiers EXACTLY as the values returned by the functions 
  available here (DAS is case-sensitive). It is also recommended that you check the capabilities 
  for each of the data sources.

	Please, notice that this function codes the parameters in a URL to query the server.
	This function could fail if you ask for many ranges or types. Also, notice that the
	speed of the query depends on the number of elements and the load in the server.

	Please, use this function smartly and do not overload servers. 
}
\value{
	\code{data.frame} with the following columns: \code{id, start, stop, version, label, type,
	method, score, orientation, phase, note, link, target, parent, part}, which match the contents
	of the specific annotation for the given ranges and types in the server (some of the fields are 
	optional, so not all the servers provide information for all the above columns).

	If there are not matching results, output is \code{NULL}. 
}
\author{
	Oscar Flores <oflores@mmb.pcb.ub.es>
  
  Anna Mantsoki <anna.mantsoki@bsc.es>
}
\seealso{
	\code{\link{getDas-functions}}
}
\examples{
	#Set session server to UniProt DAS Reference Server
  
  setDasServer(server="http://www.ebi.ac.uk/das-srv/uniprot/das")
  source = "uniprot-summary"
  ranges=GRanges(seqnames=c("A0A000","A0A001"),IRanges(start=c(1,1),end=c(394,591)))
  types=c("description","sequence-summary")
  
  features = getDasFeature(source, ranges, types)
  print(features)
}
\keyword{ manip }
