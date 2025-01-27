
#############################################################################
#Upload Validated Data to Oracle Table COOKA.LOBSTER_BYCATCH_ASSOC
#############################################################################

#load packages
	require(ROracle)
	require(bio.lobster) #github.com/LobsterScience/bio.lobster
	require(bio.utilities) #github.com/amcook/bio.utilities
	require(lubridate)
  require(dplyr)
 require(readr)
  options(stringsAsFactors = F)

  
#  oracle.username="COOKA"
 # oracle.password="bzz7plf"

#set working directory
local=T
fd = file.path('C:/Users/HowseVJ/Documents/GitHub/BycatchQC')
setwd(fd)
dataGroup = 'SWLSS' #or whichever group has submitted the data and is considered the 'data owner'
columnOrder<-read.csv('C:/Users/HowseVJ/Documents/GitHub/BycatchQC/columnOrder.csv')
##################################################################################################################################
Fi = list.files(file.path('TripsRun',dataGroup),full.names=T, recursive=T)


# Check column names of each file

column_names_list <- list()
for(i in 1:length(Fi)){
  temp_df <- read.csv(Fi[i], nrows = 1)  # Read only the first row to get column names
  column_names_list[[i]] <- colnames(temp_df)
}

#  Identify unique columns across all files
unique_columns <- unique(unlist(column_names_list))

# Check which columns are missing in each file
missing_columns <- lapply(column_names_list, function(cols) setdiff(unique_columns, cols))
names(missing_columns) <- Fi  # Assign file names to the list

# Print the results
for(i in 1:length(missing_columns)){
  cat("File:", names(missing_columns)[i], "\n")
  cat("Missing columns:", missing_columns[[i]], "\n\n")
}
# 
# ### Add missing columns and fill with NA###
# for (i in 1:length(Fi)) {
#   if ("NUM_VENTS" %in% missing_columns[[i]] || "CULL_ID" %in% missing_columns[[i]]) {
#     temp_df <- read.csv(Fi[i])
#     if (!"NUM_VENTS" %in% colnames(temp_df)) {
#       temp_df$NUM_VENTS <- NA
#     }
#     if (!"CULL_ID" %in% colnames(temp_df)) {
#       temp_df$CULL_ID <- NA
#     }
#     write.csv(temp_df, Fi[i], row.names = FALSE)
#   }
# }


### Merge Files into one large dataset and Add the Calculated Weights of individuals 
source('Scripts/addWeights.r')

# Function to read and convert columns
read_and_convert <- function(file) {
  df <- read_csv(file, col_types = cols(
    MARFIS_LICENSE_NO = col_double(),  # Change to numeric
    LICENSE_NO = col_double(),         # Change to numeric
    BOARD_DATE = col_date(format = ""),
    LANDING_DATE = col_date(format = ""),
    CREATED_DATE = col_date(format = "")
  ))
  return(df)
}

# Read and combine all files
combined_df1 <- Fi %>%
  lapply(read_and_convert) %>%
  bind_rows()

#check if there are parsing issues
parsing_problems <- problems(combined_df1)
print(parsing_problems)


# Add CALWT_G column fill with NAs
combined_df1$CALWT_G <- NA

combined_df1$PORT_NAME <-NA

combined_df1$GEAR_ID <-NA

# Loop through each row to populate CALWT_G for each individual animal 
for (i in 1:nrow(combined_df1)) {
  g <- combined_df1[i, ]
  if (!is.na(g$TRIP_ID)) {
    h <- addWeights(spec = g$SPECCD_ID, len = g$FISH_LENGTH)
    combined_df1[i, 'CALWT_G'] <- h[2]
    combined_df1[i, 'FISH_LENGTH'] <- h[1]
  }
}

datafile<- as.data.frame(combined_df1)

##Reorder the data to match database
desired_order <- columnOrder[[1]]
# Reorder the columns of datafile to match the desired order
datafile <- datafile[, desired_order]

# Convert columns to match ORacle Tables
datafile$EST_CATCH <- as.character(datafile$EST_CATCH)
datafile$GEAR_ID <- as.character(datafile$GEAR_ID)
datafile$RELEASE_CD <- as.character(datafile$RELEASE_CD)
datafile$PORT_NAME <-as.character(datafile$PORT_NAME)


# Ensure NA values remain as NA
datafile$EST_CATCH[is.na(datafile$EST_CATCH)] <- NA
datafile$GEAR_ID[is.na(datafile$GEAR_ID)] <- NA
datafile$RELEASE_CD[is.na(datafile$RELEASE_CD)] <- NA

datafile$LANDING_DATE <- as.Date(datafile$LANDING_DATE)
datafile$BOARD_DATE <- as.Date(datafile$BOARD_DATE)
datafile$CREATED_DATE  <- as.Date(datafile$CREATED_DATE)



# Check the result
str(datafile)

####UPLOAD TO ORACLE - Lobster BYCATCH_SWLSS_BACKLOG ####

oracle.username="LOBSTER"
oracle.password="X98HK4"


bio.lobster::db.setup(un=oracle.username,pw=oracle.password)  
tablenm = paste('BYCATCH_',dataGroup, '_BACKLOG',sep="")
Sys.setenv(TZ = "America/Curacao")
Sys.setenv(ORA_SDTZ = "America/Curacao")  

dbWriteTable(conn = con, name = tablenm, value = datafile, append = TRUE)
dbCommit(conn=con)






####Build Table and upload in  ORACLE ####
# 
# bio.lobster::db.setup(un=oracle.username,pw=oracle.password)  
# tablenm = paste('BYCATCH_',dataGroup, '_BACKLOG',sep="")
# 
# dbSendQuery(conn = con, statement = paste("DROP TABLE ", tablenm, " CASCADE CONSTRAINTS", sep = ""))
# 
# 
# dbSendQuery(conn=con, statement = paste("create table ",tablenm,
#                                         "(TRIP_ID NUMBER(38,0),
# 	                                        OWNER_GROUP VARCHAR2(255 BYTE),
# 	                                        TRIP VARCHAR2(255 BYTE),
# 	                                        TRIPCD_ID VARCHAR2(5 BYTE),
# 	                                        VESSEL_NAME VARCHAR2(255 BYTE),
# 	                                        LICENSE_NO NUMBER(38,0),
# 	                                        BOARD_DATE DATE,
# 	                                        LANDING_DATE DATE,
# 	                                        SAMPLER_NAME VARCHAR2(255 BYTE),
# 	                                        COMAREA_ID VARCHAR2(255 BYTE),
# 	                                        CAPTAIN VARCHAR2(255 BYTE),
# 	                                        MARFIS_LICENSE_NO NUMBER(38,0),
# 	                                        CREATED_BY VARCHAR2(255 BYTE),
# 	                                        CREATED_DATE DATE,
# 	                                        TRAP_ID NUMBER(38,0),
# 	                                        TRAP_NO NUMBER(38,0),
# 	                                        BAIT_CD NUMBER(38,0),
# 	                                        BAIT_CD2 NUMBER(38,0),
# 	                                        BAIT_CD3 NUMBER(38,0),
# 	                                        BAIT_TYPE1 NUMBER(38,0),
# 	                                        BAIT_TYPE2 NUMBER(38,0),
# 	                                        BAIT_TYPE3 NUMBER(38,0),
# 	                                        VENT_CD NUMBER(38,0),
# 	                                        TRAP_TYPE NUMBER(38,0),
# 	                                        FISHSET_ID NUMBER(38,0),
# 	                                        SET_NO NUMBER(38,0),
# 	                                        SETCD_ID NUMBER(38,0),
# 	                                        GEAR_ID NUMBER(38,0),
# 	                                        SPECSCD_ID NUMBER(38,0),
# 	                                        STRATUM_ID NUMBER(38,0),
# 	                                        EST_CATCH NUMBER(38,0),
# 	                                        LATDDMM BINARY_DOUBLE,
# 	                                        LONGDDMM BINARY_DOUBLE,
# 	                                        DEPTH NUMBER(38,0),
# 	                                        SOAK_DAYS NUMBER(38,0),
# 	                                        SOURCE NUMBER(38,0),
# 	                                        NUM_HOOK_HAUL NUMBER(38,0),
# 	                                        FISH_NO NUMBER(38,0),
# 	                                        SPECCD_ID NUMBER(38,0),
# 	                                        SEXCD_ID NUMBER(38,0),
# 	                                        FISH_LENGTH BINARY_DOUBLE,
# 	                                        ABUNDANCE NUMBER(38,0),
# 	                                        CONDITION_CD NUMBER(38,0),
# 	                                        KEPT NUMBER(38,0),
# 	                                        RELEASE_CD NUMBER(38,0),
# 	                                        SHELL NUMBER(38,0),
# 	                                        EGG_STAGE NUMBER(38,0),
# 	                                        CLUTCH NUMBER(38,0),
# 	                                        VNOTCH NUMBER(38,0),
# 	                                        DISEASE NUMBER(38,0),
# 	                                        CALWT_G BINARY_DOUBLE,
# 	                                        CULL_ID NUMBER(38,0),
# 	                                        NUM_VENTS NUMBER)",SEP=" "))
# 
# dbWriteTable(conn=con, name=tablenm, value=datafile, overwrite = TRUE, field.types = list(
#   BOARD_DATE = "DATE",
#   LANDING_DATE = "DATE",
#   LATDDMM = "BINARY_DOUBLE",
#   LONGDDMM = "BINARY_DOUBLE",
#   FISH_LENGTH = "BINARY_DOUBLE",
#   CALWT_G = "BINARY_DOUBLE"
# ))


