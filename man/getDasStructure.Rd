\name{getDasStructure}
\alias{getDasStructure}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Query a protein 3D structure to session's DAS server
}
\description{
  This function retrieves the 3D structure,including metadata and coordinates for the given query(ID 
  of the reference structure) using the data source id (or title) in the session's DAS server.
}
\usage{
getDasStructure(source,query)
}

\arguments{
  \item{source}{
	  Data source id (or title). See \code{\link{getDasSource}}
	}
	\item{query}{
  	Query id
	}
}
\details{
  This function allows to query the DAS server and returns the the 3D structure,including metadata and coordinates for the given query (id of the reference structure).
  
  Please, be sure to pass the identifiers EXACTLY as the values returned by the functions 
  available here (DAS is case-sensitive). It is also recommended that you check the capabilities 
  for each of the data sources.

  Please, notice that this function codes the parameters in a URL to query the server.
	This function could fail if you ask for a reference structure that does not exist. 
  Also, notice that the speed of the query depends on the number of elements and the load in the server.

	Please, use this function smartly and do not overload servers. 
}
\value{
\code{data.frame} with the following columns: \code{x,y,z,atomName,atomID,occupancy,tempFactor,type,groupID,name}, which match with the contents of the reference structure of the given query.

  If there are not matching results, output is \code{NULL}. 
}

\author{
 Oscar Flores <oflores@mmb.pcb.ub.es>
  
 Anna Mantsoki <anna.mantsoki@bsc.es>
}
\seealso{
\code{\link{getDasFeature} \link{getDas-functions} \link{getDasSequence}}
}
\examples{
	#Set DAS server 
	setDasServer(server="http://das.sanger.ac.uk/das")

	#Get the sources with "structure" capability...
	sources = getDasSource()
	sources[grep("structure", sources$capabilities),]
	source="structure" #...which name is also "structure"

	query="1HCK" #PDB code

	structure=getDasStructure(source,query)

	head(structure)
}
\keyword{ manip }
