
#############################################################################
#Name 			: UploadTripsSWLSS.r
#Developed by 	: Lisa Cook
#Date Developed	: Sept 17, 2019
#Usage			: Upload Validated Data to Oracle Table COOKA.LOBSTER_BYCATCH_ASSOC
#Important Notes: Make sure you use the 32bit R - still issues with 64bit
#TODOS			: 
#############################################################################

#load packages
	require(ROracle)
	require(bio.lobster) #github.com/LobsterScience/bio.lobster
	require(bio.utilities) #github.com/amcook/bio.utilities
	require(lubridate)
  options(stringsAsFactors = F)
	

#set working directory
local=T
fd = file.path('C:/Users/cooka/')
setwd(fd)
dataGroup = 'SWLSS' #or whichever group has submitted the data and is considered the 'data owner'

##################################################################################################################################
#THIS BLOCK IS FOR EDITED and Unedited DATA###
#get a list of the trips currently available 
		Fi = list.files(file.path('TripsRun',dataGroup),full.names=F, recursive=F)
		Fii = list.files(file.path('TripsRun',dataGroup,'EditedTripsforUpload'),full.names=F, recursive=F)

#make sure this is false
		any(toupper(Fi) %in% tolower(Fii))

		##only if false	
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
			
	fname = paste('CompiledData','SWLSS',Sys.Date(),'csv',sep='.')
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
	tablenm = 'BYCATCH_SWLSS_2023'
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
		
	
	df = datafile
	GG = read.csv('MapFiles/Grids.csv')

#make a Map to put all trips on as we loop through
    LobsterMap(ylim=c(42.5,46) ,xlim=c(-67.8,-62.2), labels = 'lfa')
	df$Y<- convert.dd.dddd(df$LATDDMM, format="dec.deg")
    df$X<- convert.dd.dddd(df$LONGDDMM, format="dec.deg")*-1
    df$EID<- 1:nrow(df)
    attr(df,"projection") <- "LL"
   	addPoints(na.omit(df[,c('X','Y','EID')]), pch = 21, cex=.5, bg='red')
	
	mapName = paste('SWLSS',Sys.Date(),'png',sep='.')
	savePlot(file.path('SummaryOfTrips',mapName),type='png')	
	
	#convert COMAREA_ID to LFA number
				df$LFA = NA
				df$mon = month(df$BOARD_DATE)				
				df$GRID_NUM = df$STRATUM_ID
				df$period = NA
				df$GridGrouping = NA
				for(i in 1:nrow(df)){
				df$LFA[i] = as.numeric(strsplit(unique(df$COMAREA_ID[i]),"L")[[1]][2])
					gg = subset(GG,LFA==df$LFA[i])
					g = gg[1,]
						df$period[i] = 
					 ifelse(df$mon[i]>=g$Period1.Start & df$mon[i]<=g$Period1.End, 1, 
					 ifelse(df$mon[i]>=g$Period2.Start & df$mon[i]<=g$Period2.End, 2, 
					 ifelse(df$mon[i]>=g$Period3.Start & df$mon[i]<=g$Period3.End, 3, 
					 ifelse(df$mon[i]>=g$Period4.Start & df$mon[i]<=g$Period4.End, 4, NA))))			
					lop = try(gg$GridGrouping[which(df$GRID_NUM[i] == gg$GRID_NUM)] )
					df$GridGrouping[i] = ifelse(length(lop)==0,NA,lop)
					}

				#need to put trips to targets based on level of effort within a grid within a trip

				traps = aggregate(paste(TRAP_NO,SET_NO,sep='_')~TRIP+LFA+period , data=df, FUN=function(x) length(unique(x)))
				names(traps)[4] = 'UID'
		#		traps = aggregate(UID~LFA+period,data=traps,FUN=mean)

				#How many actual trips per grid grouping and period
				trapsg = aggregate(cbind(paste(TRAP_NO,SET_NO,sep='_'),paste(TRAP_NO,SET_NO,FISH_NO,sep='_'))~TRIP+LFA+period+GridGrouping , data=df, FUN=function(x) length(unique(x)))
				names(trapsg)[5:6] = c('UID','UFID')
				
				#removing trips from counts that did not have enough traps and or fish sampled
				idrop = which(trapsg$UID<25 & trapsg$UFID<300)
				idrop2 = which(trapsg$UID<50 & trapsg$UFID<300 & trapsg$period==2 & trapsg$LFA==35)
				idrop3 = which(trapsg$UID<50 & trapsg$UFID<300 & trapsg$period==2 & trapsg$LFA %in% c(33,34))
				drops = union(union(idrop,idrop2),idrop3)

				trapsg = trapsg[-drops,]

				#ACTUAL NUMBERS OF TRIPS RELATIVE TO TARGETS
				TotTrips = aggregate(TRIP~LFA+GridGrouping+period,data=trapsg,FUN=length)
				

						GG = subset(GG,LFA %in% c(33:35))
						GG$ids = paste(GG$LFA,GG$GridGrouping,sep='-')
						ggg = GG[,c('ids', 'Period1','Period2','Period3','Period4')]
						targets = reshape(ggg,varying=list(2:5),direction = 'long')
						targets = as.data.frame(unique(cbind(targets$ids,targets$time, targets$Period1)))
						names(targets) = c('ids','period','target')
						targets$LFA = substr(targets$ids,1,2)
						targets$GridGrouping = do.call(rbind,strsplit(targets$ids,"-"))[,2]
						targets = targets[,c('LFA','GridGrouping','period','target')]
						targets = na.omit(targets)

				Total2Targets = merge(targets,TotTrips, all=T)
				Total2Targets = na.zero(Total2Targets)
				 write.csv(Total2Targets , "Trips2TotalSWLSSMarch252022.csv")



				fish = aggregate(paste(TRAP_NO,SET_NO,FISH_NO,sep='_')~TRIP+LFA+period , data=df, FUN=function(x) length(unique(x)))
				names(fish)[4] = 'UID'
				fish = aggregate(UID~LFA+period,data=fish,FUN=mean)

				lobster = aggregate(paste(TRAP_NO,SET_NO,FISH_NO,sep='_')~TRIP+LFA+period , data=subset(df,SPECCD_ID==2550), FUN=function(x) length(unique(x)))
				names(lobster)[4] = 'UID'
				lobster = aggregate(UID~LFA+period,data=lobster,FUN=mean)

				nonlobster = aggregate(paste(TRAP_NO,SET_NO,FISH_NO,sep='_')~TRIP+LFA+period , data=subset(df,!SPECCD_ID ==2550), FUN=function(x) length(unique(x)))
				names(nonlobster)[4] = 'UID'
				nonlobster = aggregate(UID~LFA+period,data=nonlobster,FUN=mean)

#add in goals to grid grouping to summary tripo

	gh = GG[,c('LFA','GridGrouping','Period1','Period2','Period3','Period4')]
	