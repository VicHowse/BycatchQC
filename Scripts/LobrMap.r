#' LobsterMap
#' @export

LobrMap<-function(ylim=c(40,48),xlim=c(-70,-54),land.col='wheat'){

options(stringsAsFactors=F)		
	require(PBSmapping)|| stop("Install PBSmapping Package")
	
	coast<- read.csv(file.path("MapFiles","shorelineHR.csv"))
	attr(coast,"projection")<-"LL"
	
	plotMap(coast,xlim=xlim,ylim=ylim,border=NA)
		
		LFAs<-    read.csv(file.path("MapFiles","LFAPolys.csv"))
			
			#browser()
		addPolys(LFAs, lwd=2,col=NULL)
		addPolys(coast, lwd=2,col=land.col)
		
			box(lwd=2)
			}

