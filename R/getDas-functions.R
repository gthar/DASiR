setDasServer <- function(server)
{
    assign(".DASiR_server", server, envir=.GlobalEnv)
    message("DAS server for this session is: ",
            get(".DASiR_server", envir=.GlobalEnv))
}

getDasServer <-  function()
{
    if (!exists(".DASiR_server", envir=.GlobalEnv)) {
        stop("Undefined DAS server. Use setDasServer() prior to this call")
    }
    return(get(".DASiR_server", envir=.GlobalEnv))
}

getDasSource <- function()
{
    url <- paste(getDasServer(), "/sources", sep="")
    message("Querying url: ", url)

    doc <- tryCatch(
        xmlTreeParse(url, useInternalNodes = TRUE,error=NULL),
        XMLError=function(e) {
            message("Error: ", e$message)
            if (e$code == 4) {
                message("Invalid XML document. ",
                        "Maybe the server does not support the sources.")
            }
        },
        error=function (e) {
            message("Error: ",
                    substr(e$message, 1, 500),
                    ifelse(nchar(e$message>500), "...", ""))
        },
        finally=function() {
            return(NULL)
        }
    )

    if (is.null(doc)) {
        return(NULL)
    }

    top <- xmlRoot(doc)
    source_id <- unname(xmlSApply(top, function(x) xmlAttrs(x)["uri"]))
    title <- unname(xmlSApply(top, function(x) xmlAttrs(x)["title"]))
    capabilities <- NULL

    for (isource in seq_along(names(top))) {
      source <- top[[isource]]
      version <- source[["VERSION"]]
      capabilities[[isource]] <- .getCapabilities(version)
    }

    message("For additional info visit ", url)
    res <- data.frame(id=source_id, title, capabilities)

    return(res)
}

.getCapabilities <- function(version) {
    capability <- NULL
    for (icap in  seq_along(names(version))) {
        if (xmlSize(version) > 1) {
          capability[[icap]] <- ifelse(names(version)[[icap]] == "CAPABILITY",
                                       unname(xmlAttrs(version[[icap]])["type"]),
                                       NA)
        }
    }
    capability <- as.data.frame(capability)
    capability <- capability[!is.na(capability),]
    capability <- as.vector(capability)
    capability <- paste(capability, collapse=" ")
    return (capability)
}

getDasDsn <- function()
{
    url <- paste(getDasServer(), "/dsn", sep="")
    message("Querying url: ", url)
    doc <- xmlTreeParse(url, useInternalNodes = TRUE)
    top <- xmlRoot(doc)
    res <- unname(xmlSApply(top, function(x) xmlAttrs(x[["SOURCE"]])[["id"]]))
    return(res)
}

getDasEntries<- function(source, as.GRanges=TRUE)
{
    url <- paste(getDasServer(), "/", source, "/entry_points", sep="")
    message("Querying url: ", url) 

    doc <- tryCatch(
        xmlTreeParse(url, useInternalNodes=TRUE, error=NULL),
        XMLError=function(e) {
            # Some servers return a custom HTML/XML mix, which makes us sad,
            # but try to recover
            if (e$code == 65) {
                htmlTreeParse(url, ignoreBlanks=TRUE)
            }
        },
        error = function(e) {
            message("Error: ",
                    substr(e$message, 1, 500),
                    ifelse(nchar(e$message > 500), "...", ""))
        },
        finally=function() {
            return(NULL)
        }
    )

    if (is.null(doc)) {
        return(NULL)
    }

    # Here there is a little mess with XML/HTML classes due to versions of XML
    # library and server implementation of (wrong) DAS specification

    if (class(doc)[1] == "XMLInternalDocument") {
        top <- xmlRoot(doc)
        res <- do.call(rbind, xmlApply(top[[1]], xmlAttrs))

    } else if (class(doc)[1] %in% c("XMLDocumentContent", "HTMLDocument")) {

        top <- doc$children$html[["body"]][["dasep"]]

        if (is.null(doc$children$htm) || is.null(top)) {
            stop("Unknown format recieved from the DAS Server. ",
                 "Check entry points in the above url")
        }

        res <- do.call(rbind, xmlApply(top[[1]], xmlAttrs))

    } else {
        stop("Unknown XML class (Did you update your XML package recently?)")
    }

    rownames(res) <- NULL
    res <- as.data.frame(res)
    names(res) <- tolower(names(res))

    if (as.GRanges) {
        nam <- names(res)

        if (!all(c("start","stop", "id")  %in% nam)) {
            warning("Unable to create GRanges object. Returning data.frame") 
            return(res)
        }

        .f2n <- function(n) as.numeric(as.character(n))
        res2 <- GRanges(res$id,
                        IRanges(start=.f2n(res$start),
                                end=.f2n(res$stop)))

        others <- which(!nam %in% c("start", "stop", "id"))
        if (length(others > 0)) {
            elementMetadata(res2) <- res[, others]
        }

        return(res2)
    }

    return(res)
}

getDasTypes <- function(source)
{
    url <- paste(getDasServer(), "/", source, "/types", sep="")
    message("Querying url: ", url)
    doc <- xmlTreeParse(url, useInternalNodes = TRUE)
    top <- xmlRoot(doc)
    type <- unname(xmlSApply(top[[1]][[1]],
                             function(x) xmlAttrs(x)[["id"]]))

    if (is.null(unlist(type))) {
        return(NULL)
    } else {
        return(type)
    }
}

.makeSegments <- function(ranges)
{
    res <- NULL
    for(i in 1:length(ranges)) {
        rtmp <- ranges[i]
        id <- as.character(seqnames(rtmp))
        start <- start(rtmp)
        end <- end(rtmp)
        res <- c(res,
                 paste("segment=", id, ":", start, ",", end, ";", sep=""))
    }
    return(paste(res, sep="", collapse=""))
}
