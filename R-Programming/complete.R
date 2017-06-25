#John Hopkins coursera R programming course week 2 assignment

#reads a directory full of files and reports number of completely observed cases in each file

complete<-function(directory,id=1:332) {
    TmpDat<-data.frame()
    #loop through monitor ids
    for (monitor in id) {
        #convert integers to 3 digits
        if (monitor <10) {
            formatmonitor<-paste0("00",monitor)
        }
        else if (monitor>9 & monitor<100) {
            formatmonitor<-paste0("0",monitor)
        }
        else {formatmonitor<-paste0(monitor)}
        #read in files
        IDdat<-read.csv(paste0(directory,formatmonitor,".csv"))
       #count non NAs
       cnt<- length(which(complete.cases(IDdat)==TRUE))
         #combine data into one frame
        TmpDat<-rbind(TmpDat,cbind(id=monitor,nobs=cnt))
    }
    
    TmpDat
}