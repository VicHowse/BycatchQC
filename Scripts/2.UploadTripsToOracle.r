
#############################################################################
#Name 			: UploadTripsToOracle.r
#Developed by 	: Lisa Cook
#Date Developed	: Sept 17, 2019
#Usage			: Upload Validated Data to Oracle Table COOKA.LOBSTER_BYCATCH_ASSOC
#TODOS			: 
#############################################################################

#load packages
	require(ROracle)
	require(bio.lobster) #github.com/LobsterScience/bio.lobster
	require(bio.utilities) #github.com/amcook/bio.utilities
	require(lubridate)
  options(stringsAsFactors = F)

oracle.username=""
oracle.password=""
		

#set working directory
local=T
fd = file.path('C:/Users/cooka/')
setwd(fd)
dataGroup = 'SWLSS' #or whichever group has submitted the data and is considered the 'data owner'

##################################################################################################################################
#THIS BLOCK IS FOR EDITED and Unedited DATA###
#get a list of the trips currently available 
		Fi = list.files(file.path('TripsRun',dataGroup),full.names=T, recursive=T)
		
source('Scripts/addWeights.r')	

#combine files into one dataset
		collector = list()
		for(i in 1:length(Fi)){
			collector[[i]] = read.csv(Fi[i])
				}
		datafile = do.call(rbind,collector)
		datafile$CALWT_G = NA
		datafile$CREATED_DATE1=datafile$LANDING_DATE1 = datafile$BOARD_DATE1 = as.Date('2001-01-01')
	  if(any(is.na(datafile$CREATED_DATE))){
	    v = which(is.na(datafile$CREATED_DATE))
	    datafile$CREATED_DATE[v] = datafile$LANDING_DATE[v]
	  }
		
for(i in 1:nrow(datafile)){
			g = datafile[i,]
			if(!is.na(g$TRIP_ID)){
			h = addWeights(spec = g$SPECCD_ID, len= g$FISH_LENGTH)
			datafile[i,'CALWT_G'] <- h[2]
			datafile[i,'FISH_LENGTH'] <- h[1]
      
			if(nchar(g$BOARD_DATE)==10 & grepl("/",g$BOARD_DATE)) datafile$BOARD_DATE1[i]  = as.Date(g$BOARD_DATE,"%m/%d/%Y")
			if(nchar(g$LANDING_DATE)==10 & grepl("/",g$LANDING_DATE)) datafile$LANDING_DATE1[i]  = as.Date(g$LANDING_DATE,"%m/%d/%Y")
			if(nchar(g$CREATED_DATE)==10 & grepl("/",g$CREATED_DATE)) datafile$CREATED_DATE1[i]  = as.Date(g$CREATED_DATE,"%m/%d/%Y")
			
			if(nchar(g$BOARD_DATE)==8 & grepl("/",g$BOARD_DATE)) datafile$BOARD_DATE1[i]  = as.Date(g$BOARD_DATE,"%m/%d/%Y")
			if(nchar(g$LANDING_DATE)==8 & grepl("/",g$LANDING_DATE)) datafile$LANDING_DATE1[i]  = as.Date(g$LANDING_DATE,"%m/%d/%Y")
			if(nchar(g$CREATED_DATE)==8 & grepl("/",g$CREATED_DATE)) datafile$CREATED_DATE1[i]  = as.Date(g$CREATED_DATE,"%m/%d/%Y")
			
			
			if(nchar(g$BOARD_DATE)==20) datafile$BOARD_DATE1[i] =  as.Date(unlist(strsplit(g$BOARD_DATE,'T'))[1])
			if(nchar(g$LANDING_DATE)==20) datafile$LANDING_DATE1[i] =  as.Date(unlist(strsplit(g$LANDING_DATE,'T'))[1])
			if(nchar(g$CREATED_DATE)==20) datafile$CREATED_DATE1[i] =  as.Date(unlist(strsplit(g$CREATED_DATE,'T'))[1])
			
			if(nchar(g$BOARD_DATE)==10 & grepl("-",g$BOARD_DATE)) datafile$BOARD_DATE1[i] =  as.Date(g$BOARD_DATE,format="%Y-%m-%d")
			if(nchar(g$LANDING_DATE)==10 & grepl("-",g$LANDING_DATE)) datafile$LANDING_DATE1[i] =  as.Date(g$LANDING_DATE,format="%Y-%m-%d")
			if(nchar(g$CREATED_DATE)==10 & grepl("-",g$CREATED_DATE)) datafile$CREATED_DATE1[i] =  as.Date(g$CREATED_DATE,format="%Y-%m-%d")
			}
}

			datafile$BOARD_DATE = datafile$BOARD_DATE1
			datafile$LANDING_DATE = datafile$LANDING_DATE1
			datafile$CREATED_DATE = datafile$CREATED_DATE1
			datafile$CREATED_DATE1 = datafile$LANDING_DATE1 = datafile$BOARD_DATE1 = NULL
		datafile$BOARD_DATE =  as.Date(datafile$BOARD_DATE,format="%Y-%m-%d")
		datafile$LANDING_DATE =  as.Date(datafile$LANDING_DATE,format="%Y-%m-%d")
		unique(datafile$BOARD_DATE)
		length(unique(datafile$TRIP))
			
	fname = paste('CompiledData',dataGroup,Sys.Date(),'csv',sep='.')
	write.csv(datafile,file=file.path('SummaryOfTrips',fname))
  
	datafile = read.csv(file=file.path('SummaryOfTrips',fname))
	
	i = which(is.na(datafile$FISH_NO))
	w = which(is.na(as.numeric(datafile$FISH_NO)))
	datafile$FISH_NO[w] = w

	i = which((datafile$BAIT_CD3=='60/70'))
	datafile$BAIT_CD3[i] = 60.70
	
	i = grep('-',datafile$MARFIS_LICENSE_NO)
	datafile$MARFIS_LICENSE_NO[i]= as.numeric(do.call(rbind,strsplit(datafile$MARFIS_LICENSE_NO[i],'-'))[,1])
	datafile$MARFIS_LICENSE_NO = as.numeric(datafile$MARFIS_LICENSE_NO)
	###################
	
	
	
	bio.lobster::db.setup(un=oracle.username,pw=oracle.password)  
	tablenm = paste('BYCATCH_',dataGroup,'_2023',sep="")
	
	dbSendQuery(conn=con, statement = paste("create table ",tablenm,
	                                        "(TRIP_ID NUMBER(38,0),
	                                        OWNER_GROUP VARCHAR2(255 BYTE),
	                                        TRIP VARCHAR2(255 BYTE),
	                                        TRIPCD_ID VARCHAR2(5 BYTE),
	                                        VESSEL_NAME VARCHAR2(255 BYTE),
	                                        LICENSE_NO NUMBER(38,0),
	                                        BOARD_DATE DATE,
	                                        LANDING_DATE DATE,
	                                        SAMPLER_NAME VARCHAR2(255 BYTE),
	                                        COMAREA_ID VARCHAR2(255 BYTE),
	                                        CAPTAIN VARCHAR2(255 BYTE),
	                                        MARFIS_LICENSE_NO NUMBER(38,0),
	                                        CREATED_BY VARCHAR2(255 BYTE),
	                                        CREATED_DATE DATE,
	                                        TRAP_ID NUMBER(38,0),
	                                        TRAP_NO NUMBER(38,0),
	                                        BAIT_CD NUMBER(38,0),
	                                        BAIT_CD2 NUMBER(38,0),
	                                        BAIT_CD3 NUMBER(38,0),
	                                        BAIT_TYPE1 NUMBER(38,0),
	                                        BAIT_TYPE2 NUMBER(38,0),
	                                        BAIT_TYPE3 NUMBER(38,0),
	                                        VENT_CD NUMBER(38,0),
	                                        TRAP_TYPE NUMBER(38,0),
	                                        FISHSET_ID NUMBER(38,0),
	                                        SET_NO NUMBER(38,0),
	                                        SETCD_ID NUMBER(38,0),
	                                        GEAR_ID VARCHAR2(5 BYTE),
	                                        SPECSCD_ID NUMBER(38,0),
	                                        STRATUM_ID NUMBER(38,0),
	                                        EST_CATCH VARCHAR2(5 BYTE),
	                                        LATDDMM BINARY_DOUBLE,
	                                        LONGDDMM BINARY_DOUBLE,
	                                        DEPTH NUMBER(38,0),
	                                        SOAK_DAYS NUMBER(38,0),
	                                        SOURCE NUMBER(38,0),
	                                        NUM_HOOK_HAUL NUMBER(38,0),
	                                        FISH_NO NUMBER(38,0),
	                                        SPECCD_ID NUMBER(38,0),
	                                        SEXCD_ID NUMBER(38,0),
	                                        FISH_LENGTH BINARY_DOUBLE,
	                                        ABUNDANCE NUMBER(38,0),
	                                        CONDITION_CD NUMBER(38,0),
	                                        KEPT NUMBER(38,0),
	                                        RELEASE_CD VARCHAR2(5 BYTE),
	                                        SHELL NUMBER(38,0),
	                                        EGG_STAGE NUMBER(38,0),
	                                        CLUTCH NUMBER(38,0),
	                                        VNOTCH NUMBER(38,0),
	                                        DISEASE NUMBER(38,0),
	                                        CALWT_G BINARY_DOUBLE)",SEP=" "))
	
	Sys.setenv(TZ = "America/Curacao")
	Sys.setenv(ORA_SDTZ = "America/Curacao")   
	
	dbWriteTable(conn=con,name=tablenm,append=T, value=datafile)
		
	
