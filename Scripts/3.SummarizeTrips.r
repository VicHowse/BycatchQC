
#############################################################################
#Name 			: SummarizeTrips.r
#Developed by 	: Lisa Cook
#Date Developed	: Sept 17, 2019
#Usage			: Upload Validated Data to Oracle Table COOKA.LOBSTER_BYCATCH_ASSOC
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
	fname = paste('CompiledData',dataGroup,Sys.Date(),'csv',sep='.')
	
	datafile = read.csv(file=file.path('SummaryOfTrips',fname))
	
	i = which(is.na(datafile$FISH_NO))
	w = which(is.na(as.numeric(datafile$FISH_NO)))
	datafile$FISH_NO[w] = w

	i = which((datafile$BAIT_CD3=='60/70'))
	datafile$BAIT_CD3[i] = 60.70
	
	i = grep('-',datafile$MARFIS_LICENSE_NO)
	datafile$MARFIS_LICENSE_NO[i]= as.numeric(do.call(rbind,strsplit(datafile$MARFIS_LICENSE_NO[i],'-'))[,1])
	datafile$MARFIS_LICENSE_NO = as.numeric(datafile$MARFIS_LICENSE_NO)
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



				fish = aggregate(paste(TRAP_NO,SET_NO,FISH_NO,sep='_')~TRIP+LFA+period , data=df, FUN=function(x) length(unique(x)))
				names(fish)[4] = 'UID'
				fish = aggregate(UID~LFA+period,data=fish,FUN=mean)

				lobster = aggregate(paste(TRAP_NO,SET_NO,FISH_NO,sep='_')~TRIP+LFA+period , data=subset(df,SPECCD_ID==2550), FUN=function(x) length(unique(x)))
				names(lobster)[4] = 'UID'
				lobster = aggregate(UID~LFA+period,data=lobster,FUN=mean)

				nonlobster = aggregate(paste(TRAP_NO,SET_NO,FISH_NO,sep='_')~TRIP+LFA+period , data=subset(df,!SPECCD_ID ==2550), FUN=function(x) length(unique(x)))
				names(nonlobster)[4] = 'UID'
				nonlobster = aggregate(UID~LFA+period,data=nonlobster,FUN=mean)
