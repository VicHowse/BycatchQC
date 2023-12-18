# BycatchQC
QC checks and data uploading for bycatch data from DFO

# Folder structure
Folders are structured to maintain a clean workflow
MapFiles/      contains the polygons for mapping and ensuring the coordinates from the data submission are within the allocated Lobster Fishing Area
Markdown/      contains the .rmd for evaulating each trip and producing a .pdf
NewTrips/      this folder is where you paste new trips that need to be run through scripts
Reports/       location of Markdown reports
Scripts/       R scripts for running reports and uploading. There are also a couple of helper scripts
SummaryOfTrips/ Compiled data ready for uploading to oracle
TripsRun/       All trips that have been run through the vaidation scripts (from NewTrips) migrate to this folder

# WorkFlow
Step 1: download or copy all trips that need to be evaulated into the NewTrips folder. Each trip should be its own file, ideally a .csv but if not the code will generate a .csv from an .xlsx (so long as it is only one sheet)
Step 2: Open Scripts/1.RunScript.r this file needs to be run from top to bottom. You will need to fill in the file path where the folder structure of BycatchQC lives. This script loops through each trip file (.csv) and produces a report. It also moves the files from NewTrips to TripsRun folders.

Step 3: Open 2.UploadTripsToOracle.r You will need to have an Oracle account for this part to work. This script combines all files in TripsRun to make a single file for uploading. The compiled file is saved in SummaryOfTrips folder. This script deals with a few common data issues that have arisen, but each compiled file should be checked for internal consistency (dates, LFAs, OwnerGroups etc) prior to uploading. The uploading currently is done to a temporary table which is named within this file, but could be setup for appending data in the future.

Optional Step 4: Open 3.SummarizeTrips.r Prepare data summaries if desired. More could be added here

# Dependencies
These scripts require several packages including:

	(ROracle)
	(bio.lobster) #github.com/LobsterScience/bio.lobster
	(bio.utilities) #github.com/amcook/bio.utilities
	(lubridate)
  (tint)
	(rmarkdown)
	(PBSmapping)
	(rio)
	
