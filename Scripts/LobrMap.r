#' LobsterMap
#' @export

LobrMap<-function(ylim=c(40,48),xlim=c(-70,-54),land.col='wheat',coast=coa,LFAs=lfas){

options(stringsAsFactors=F)		
	require(PBSmapping)|| stop("Install PBSmapping Package")
	
	attr(coast,"projection")<-"LL"
	
	plotMap(coast,xlim=xlim,ylim=ylim,border=NA)
		
			
			#browser()
		addPolys(LFAs, lwd=2,col=NULL)
		addPolys(coast, lwd=2,col=land.col)
		
			box(lwd=2)
			}

