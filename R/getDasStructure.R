.getGroup<-function(chain)
{
	type=unname(xmlAttrs(chain)["type"])
  groupID=unname(xmlAttrs(chain)["groupID"])
  name=unname(xmlAttrs(chain)["name"])
  return(data.frame(type,groupID,name))
}

.getStructure<-function(group)
{
  x=xmlAttrs(group[["atom"]])[["x"]]
  y=xmlAttrs(group[["atom"]])[["y"]]
  z=xmlAttrs(group[["atom"]])[["z"]]
  atomName=xmlAttrs(group[["atom"]])[["atomName"]]
  atomID=xmlAttrs(group[["atom"]])[["atomID"]]
  occupancy=xmlAttrs(group[["atom"]])[["occupancy"]]
  tempFactor=xmlAttrs(group[["atom"]])[["tempFactor"]]
  #return(data.frame(x,y,z,atomName,atomID,occupancy,tempFactor))
}

getDasStructure <- function(source,query)
{
	url = paste(getDasServer(), "/",source, "/structure?","query=",query,sep="")
	message("Quering url: ", paste(getDasServer(), "/", source, "/structure?query=", query, sep=""))

	#To avoid problems with large structures we will download the file
	tf = tempfile(); 
	download.file(url, destfile=tf, quiet=TRUE)
	
	doc = tryCatch(xmlTreeParse(tf, useInternalNodes = TRUE,error=NULL),
					XMLError = function(e){
						message("Error: ", e$message)
            if(e$code == 4) message("Invalid XML document.Maybe the server does not support the sources");
					},
          error = function(e){
          	message("Error: ", substr(e$message, 1, 500), ifelse(nchar(e$message>500), "...", ""))
          },
          finally=function(){ return(NULL) })
    
	if(is.null(doc)) return(NULL)
    
	top=xmlRoot(doc)[["chain"]]
	if(is.null(top)) return(NULL)  

	res_group=NULL
	res_atom=NULL
	all_col=NULL
	all=NULL

	for(igroup in seq_along(names(top))) #Each Group
	{
		group = top[[igroup]]
    res_group[[igroup]]=.getGroup(group)
    res_atom [[igroup]]=data.frame(xmlSApply(group, xmlAttrs))
    res_atom[[igroup]]=t(res_atom [[igroup]])
    all_col[[igroup]]=data.frame(cbind(res_atom [[igroup]],res_group[[igroup]]))
	}
    
	all = do.call("rbind",all_col)
	rownames(all)=NULL    
	return(all)
}  
    
