#convertingXLS to csv
require(tint)
require(rmarkdown)
require(PBSmapping)
require(bio.lobster)
require(bio.base)
require(bio.utilities) 
require(rio)
options(stringsAsFactors = F)

fd = file.path('C:/Users/cooka/Desktop/Bycatch Validation 19-20/TripsRun/SWLSS')
setwd(fd)
Fi = list.files()
Fii = list.files(file.path('EditedTripsforUpload'))
for(i in 1:length(Fii)){
    if(grepl("xls",Fii[i])){
        tmp<-import(file.path("EditedTripsforUpload",Fii[i]),sheet=1)
        unlink(file.path("EditedTripsforUpload",Fii[i]))
  Fii[i]<-gsub("xls", "csv", Fii[i])
  write.csv(tmp, file = file.path(fd,"EditedTripsforUpload",Fii[i]), row.names=FALSE)
  rm(tmp)
    }
  if(Fii[i] %in% Fi) {
      unlink(Fii[i])
  }
}