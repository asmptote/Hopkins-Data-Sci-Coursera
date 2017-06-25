#John Hopkins coursera R programming course week 2 assignment

#takes a dir of polution data files and threshold for complete cases and calculates 
#correlation between sulfate and nitrate wherever the threshold is met. returns a vector of correlations

corr<-function(directory, threshold=0) {
  corrs=numeric()
    #loop through all monitor ids
    for (monitor in c(paste0("00",1:9),paste0("0",10:99),paste0(100:332))) {
        IDdat<-read.csv(paste0(directory,monitor,".csv"))
        #check threshold
        if (length(which(complete.cases(IDdat)==TRUE))<=threshold) {
            next
        } 
        corrs=c(corrs,cor(IDdat$sulfate,IDdat$nitrate,use="complete.obs"))
        
    }
        corrs
}
