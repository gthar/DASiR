.getFeature <- function(feature)
{
	attrs = xmlAttrs(feature)
	feats = xmlSApply(feature, xmlValue)
    
	tmp = t(data.frame(c(attrs, feats)))
	rownames(tmp) = NULL
  
	return(tmp)
}

.makeTypes <- function(types)
{
	res = sapply(types, function(x) paste("type=", x, ";", sep=""))
  return(paste(res, sep="", collapse=""))
}

getDasFeature <- function(source, ranges, types=NULL)
{
	str_ran = .makeSegments(ranges)
	str_typ = .makeTypes(types)
    
	url = paste(getDasServer(), "/", source, "/features?", str_ran, str_typ, sep="")
	message("Querying url: ", paste(getDasServer(), "/", source, "/features", sep=""))
  isLong = nchar(url) > 2000

	doc = tryCatch(xmlTreeParse(url, useInternalNodes = TRUE, error=NULL),
					XMLError = function(e){
						message("Error: ", e$message)
						if(e$code == 1549 & isLong) message("Maybe URL is too long. Try querying less ranges/types");
					},
					error = function(e){
						message("Error: ", substr(e$message, 1, 500), ifelse(nchar(e$message>500), "...", ""))
					},
					finally=function(){ return(NULL) })
    
	if(is.null(doc)) return(NULL)
    
	top = xmlRoot(doc)[[1]]
    
	res = NULL
	for(iseg in seq_along(names(top))) #Each Segment
  {
  	seg = top[[iseg]]
            
		if(xmlSize(seg) >=1) #More than the SEGMENT node
		{
			fets = do.call("rbind", xmlApply(seg, function(x) .getFeature(x)))
	    res[[iseg]]=data.frame(SEGMENT.RANGE=iseg, fets)
    }    
	}

  all = do.call("rbind",res)
	names(all) = tolower(names(all))
  return(all)
}

