
#############################################################################
#Name       : RunScript.r
#Developed by   : Lisa Cook
#Date Developed : Sept 17, 2019
#Usage      : Creating pdfs for data validation
#TODOS      : 
#############################################################################


#Rmarkdown Script
	require(tint)
	require(rmarkdown)
	require(PBSmapping)
	require(bio.lobster)
	require(bio.utilities) 
	require(rio)
	options(stringsAsFactors = F)

fd = file.path('C:/Users/cooka')
	setwd(fd)

   		LFAS<-read.csv(file.path("MapFiles","LFAPolys.csv"))
    	LFAGrid<-read.csv(file.path("MapFiles","GridPolys.csv"))
 		source('Scripts/LobrMap.r')

cleanRmd <- function(RmdName = 'BycatchTripValidation', RmdFolder = 'Markdown') {
					unlink(file.path(RmdFolder,paste(RmdName,'_cache',sep="")),recursive=T)
					unlink(file.path(RmdFolder,paste(RmdName,'_files',sep="")),recursive=T)
					unlink(file.path(RmdFolder,paste(RmdName,'.tex',sep="")))
					cat( paste('Clean',RmdName,"\n",sep=" "))
					}


	cleanRmd()
  
  	Fi = list.files('NewTrips')
  	for(i in 1:length(Fi)) {
  	  ii=i
  		if(grepl("xls",Fi[i])){
  			tmp<-import(file.path("NewTrips",Fi[i]),sheet=1)
  			unlink(file.path("NewTrips",Fi[i]))
  			Fi[i]<-gsub("xls", "csv", Fi[i])
  			Fi[i]<-gsub("csvx", "csv", Fi[i])
  			
  			write.csv(tmp, file = file.path(fd,"NewTrips",Fi[i]), row.names=FALSE)
  			rm(tmp)
  		}
  				dat<- read.csv(file.path(fd,"NewTrips",Fi[i]))
  				grp<-unique(dat$OWNER_GROUP)
  				if(!dir.exists(file.path('Reports',grp[1]))) dir.create(file.path('Reports',grp[1]))
  				if(!dir.exists(file.path('TripsRun',grp[1]))) dir.create(file.path('TripsRun',grp[1]))
  				nm = paste0(gsub(".csv","", Fi[i]), ".pdf")
  				print(paste0('Running files ',Fi[i]))
  				dat$COMAREA_ID = toupper(dat$COMAREA_ID)
  				rmarkdown::render('Markdown/BycatchTripValidation.Rmd',quiet=T)
  				file.rename(from = file.path('Markdown','BycatchTripValidation.pdf'), to = file.path('Reports',grp[1],nm))
  				cleanRmd()
  				file.rename(from = file.path('NewTrips',Fi[ii]), to = file.path('TripsRun',grp[1],Fi[ii]))
  				rm(dat)
  			}
  		
  	
  	
  	