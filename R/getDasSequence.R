.getSequence <- function(sequence)
{
	seq=xmlValue(sequence)
	pattern="\n"
	seq=gsub(pattern,"",seq)
	return(seq)
}

getDasSequence<-function(source, ranges, class=c("character", "DNAStringSet", "RNAStringSet", "AAStringSet"))
{
	class = match.arg(class)
 
  str_ran = .makeSegments(ranges)
  
  url = paste(getDasServer(), "/", source, "/sequence?",str_ran,sep="")
	message("Quering url: ", paste(getDasServer(), "/", source, "/sequence", sep=""))	
	isLong = nchar(url) > 2000

  doc = tryCatch(xmlTreeParse(url, useInternalNodes = TRUE, error=NULL),
                 XMLError = function(e){
                   message("Error: ", e$message)
                   if(e$code == 1549 & isLong) message("Maybe URL is too long. Try querying less ranges");
                 },
                 error = function(e){
                   message("Error: ", substr(e$message, 1, 500), ifelse(nchar(e$message>500), "...", ""))
                 },
                 finally=function(){ return(NULL) })
  
  if(is.null(doc)) return(NULL)
  
  top = xmlRoot(doc)
  
  res = list()
  for(iseg in seq_along(names(top))) #Each Segment
  {
    seg = top[[iseg]]
    res[[iseg]]=unlist(xmlApply(seg, function(x) .getSequence(x)), use.names=FALSE)
  }
    
  all=as(unlist(res), class)
  return(all)
}
  
