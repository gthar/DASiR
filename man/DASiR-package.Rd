\name{DASiR-package}
\alias{DASiR-package}
\alias{DASiR}
\docType{package}
\title{
    Distributed Annotation System in R
}
\description{
    R package for programmatic retrieval of information from DAS servers
}
\details{
\tabular{ll}{
    Package: \tab DASiR\cr
    Type: \tab Package\cr
    License: \tab LGPL(>= 3)\cr
}

Distributed Annotation System (DAS) is a protocol for information exchange
between a server and a client. Is widely used in bioinformatics and the most
important repositories include a DAS server parallel to their main front-end.

DAS uses XML and HTTP protocols, so the requirements are much less than using
MySQL or BioMart based alternatives.

This package provides a convenient R-DAS interface to programmatically access
DAS servers available from your network. It supports only the basic features of
DAS 1.6 protocol (DAS/2 is not supported) but these should be enough for most
of the users.

Despite a large number (>1200 according official numbers on February 2013) are
available, this package is designed with range-features (neither coverages nor
sequences). DAS also supports querying genomic sequences and protein
structures, but there are already better ways to access such static data in R.

In order to start with this package, you can give a look to basic metadata
functions in \code{\link{getDas-functions}} man page and to the main functions
\code{\link{getDasFeature}, \link{getDasSequence}, \link{getDasStructure}}.

Basic support for graphical representation is also provided through
\code{\link{plotFeatures}}.

As a final consideration, you should have in mind that public available DAS
servers are a valuable service and you should give them a reasonable use.
Please, don't overload the servers with many or large queries.
}

\author{
    Oscar Flores <oflores@mmb.pcb.ub.es>

    Anna Mantsoki <anna.mantsoki@bsc.es>
}

\references{
    BioDas Wiki - http://www.biodas.org/

    DAS 1.6 Specification - http://www.ebi.ac.uk/~aj/1.6_draft7/documents/spec.html

    DAS servers catalog - http://www.dasregistry.org/
}

\keyword{ package }
\examples{
    setDasServer(server="http://www.ensembl.org/das")
    print(getDasServer())

    # See the sources (supported organisms)
    sources <- getDasSource()
    head(sources)

    dsn_sources <- getDasDsn()
    head(dsn_sources)

    # Notice that sources returns more information but we need the source ID
    # given by getDasDsn, that corresponds to getDasSource$title in this
    # server.

    # The server we are quering is a genomic sequence server, so no gene
    # information nor structure is available here

    # Let's see what this sever supports for SCerevisiae
    source <- "Saccharomyces_cerevisiae.EF4.reference"
    # das1:entry_points das1:sequence das1:features
    sources[grep(source, sources$title), ]

    # Ask for the entries
    entries <- getDasEntries(source)
    head(entries)

    # Retrive first 1000nts in the beginning of chromosomes I and II
    range <- GRanges(c("I", "II"), IRanges(start=1, end=1000))
    seq <- getDasSequence(source, range, class="DNAStringSet")

    # Server supports features, but not types...    
    types <- getDasTypes(source)
    # Server returns null... So we will perform a query without types
    print(types)

    # Let's see which features is returning the server
    features <- getDasFeature(source, range, NULL)
    print(features)
    # Features here are not genomic features (like genes), this is only
    # sequence info

    # In the webpage http://www.dasregistry.org/listSources.jsp
    # We can see all the registred DAS servers and the information they provide

    # Remember that DASiR is only an interface to DAS servers! You should know
    # what the server you are querying contains and if it supports your desired
    # features!

    # Now let's retrieve genes in that region from another source
    # This is UCSC Genome browser, which has access to all the features
    # displayed in the webpage
    setDasServer(server="http://genome.ucsc.edu/cgi-bin/das")
    # Notice that id now changes, we can retrieve it from getDasDsn()
    source <- "sacCer3"

    # Retrieve the types
    types <- getDasTypes(source)

    # We want the official genes from 'sdgGene' and the ENSEMBL predicted genes
    # from 'ensGene'
    features <- getDasFeature(source, range, c("sgdGene","ensGene"))
    print(features)

    # We obtain a total of 4 genes, 2 in chromosome I and 2 in chromosome II
    # Notice each gene appears 2 times, one as a sgdGene and another as ensGene

    # Now plot the features (but only from one range at a time!)
    # Notice that with the default parameters each feature will appear with a
    # different color
    plotFeatures(features[features$segment.range == 1,])
}
