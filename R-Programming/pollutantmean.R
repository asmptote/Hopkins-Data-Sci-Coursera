#John Hopkins coursera R programming course week 2 assignment

#function uses specdata directory which contains csv files representing pollution particulate matter observed at different times from different monitors throughout the US
#particulate matter is measured in micrograms per cubic meter
# calculates mean of a pollutant across list of monitors
#arguments are the directory path to find the files, the type of pollutant to average and the monitor id numbers to consider

pollutantmean <-function(directory,pollutant, id=1:332) {
   CombineIDdat<-data.frame()
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
       #combine data into one frame
         CombineIDdat<-rbind(CombineIDdat,IDdat)
   }
#calculate mean
mean(CombineIDdat[[pollutant]],na.rm=TRUE)   
      
}


