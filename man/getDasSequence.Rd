\name{getDasSequence}
\alias{getDasSequence}
\title{
Query nucleotide or amino acid sequence to session's DAS server
}
\description{
This function retrieves the nucleotide or amino acid sequence for the given ranges using the
  corresponding data source id (or title) in the session's DAS server
}
\usage{
	getDasSequence(source,ranges, class=c("character", "DNAStringSet", "RNAStringSet", "AAStringSet"))
}
\arguments{
  \item{source}{
    Data source id (or title). See \code{\link{getDasSource}}
	}

 \item{ranges}{
  \code{\link{GRanges}} object containing the seqname(s) and range(s) to query.
	Be sure this information is coherent with those returned by \code{\link{getDasEntries}}
	}
	\item{class}{
		Instance of the class to be used in the output. By default it returns a regular
		\code{character} vector, but we recomend using Biostring classes for retrieving large
		sequences. Most usual Biosting classes are \code{\link[Biostrings]{DNAStringSet}} for
		genomic	sequences, \code{\link[Biostrings]{RNAStringSet}} for transcripts and 
		\code{\link[Biostrings]{AAStringSet}} for aminoacids.
	}
}
\details{
  This function allows to query the DAS server and returns the nucleotide or amino acid 
  sequence available at the given ranges.
  
  Please, be sure to pass the identifiers EXACTLY as the values returned by the functions 
  available here (DAS is case-sensitive). It is also recommended that you check the capabilities 
  for each of the data sources.

	Please, notice that this function codes the parameters in a URL to query the server.
	This function could fail if you ask for many ranges. Also, notice that the speed of 
  the query depends on the number of elements and the load in the server.

	Please, use this function smartly and do not overload servers. 
}

\value{
	Nucleotide or amino acid sequences that match the content of the annotation for the given ranges in the server.

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
  #Set session server to the UniProt DAS Reference Server
  
   setDasServer(server="http://www.ebi.ac.uk/das-srv/uniprot/das")       
  
   print(getDasServer())
	
	 source = "uniprot" #Uniprot
  	
   ranges = GRanges(c("A0A000","A0A001"), IRanges(start=c(1,1),end=c(394,591)))

	 sequences = getDasSequence(source, ranges)        
  
   print(sequences)
}
\keyword{ manip }
