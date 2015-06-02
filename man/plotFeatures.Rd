\name{plotFeatures}
\alias{plotFeatures}
\title{
	Basic plotting function for obtained features
}
\description{
	This functions creates a basic plot with the features retrieved by \code{\link{getDasFeature}}
	or adds them to an existing plot
}
\usage{
plotFeatures(df, col=NULL, col.start="start", col.end="end", col.id ="label", 
	 col.type="type", box.height=4, box.sep=2, pos.label=c("middle", "top", "bottom", "no"),
	 new=TRUE, legend=TRUE, ...)
}
\arguments{
  \item{df}{
	\code{\link{data.frame}} with the features. Columns \code{col.start, col.end, col.id and
	 col.type} must be present.
}
	\item{col}{
	Vector of colors for each box in \code{df}. If NULL, automatic rainbow coloring for
	different types will be used.
}
	\item{col.start, col.end, col.id, col.type}{
	Names of the columns of the data.frame from which retrieve the values
}
	\item{box.height, box.sep}{
  Height and separation between boxes (in percentual points respect y-axis)
}
	\item{pos.label}{
	Position of the label respect the box. If \code{pos.label=="no"}, labels
	are disabled.
}
	\item{new}{
	Open a new graphical device. If \code{new=FALSE}, boxes will be added to an existing plot
	(so, coordinate system must be coherent). 
}
	\item{legend}{
	Add legend to the plot
}
  \item{\dots}{
	Other graphical parameters passed to \code{\link{plot.default}}
}
}
\details{
	This function provides a quick way to view (non-overlapped) boxes in a new or an
	existing plot with the features retrieved.
	
	Notice that boxes will can be plotted overlaping the current open graphical device,
	so the x-coordinates must match.
}
\value{
	(none)
}
\author{
	Oscar Flores <oflores@mmb.pcb.ub.es>
}
\seealso{
	\code{\link{getDasFeature}}
}
\examples{
	 #This is UCSC Genome browser
  setDasServer(server="http://genome.ucsc.edu/cgi-bin/das")

  source = "sacCer3" #Notice that id now changes, we can retrieve it from getDasDsn()
  range = GRanges(c("I"), IRanges(start=1, end=2500))
	type  = c("sgdGene")
	
	#We want the official genes from 'sdgGene' in the range I:1-2500
  features = getDasFeature(source, range, type)
  print(features)

	plotFeatures(features)
}
\keyword{ plot }
