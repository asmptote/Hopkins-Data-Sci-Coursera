#function finds best hospital in a US state     
#reads in outcome of care measures dataset from US dept of health and human services
#returns lowest 30-day mortality for the specified outcome

best <- function(state, outcome_type) {
   LoadDat = read.csv("Datasets/outcome-of-care-measures.csv",na.strings = "Not Available")
   
   #check that state and outcome are valid 
   allowState=unique(LoadDat$State)
   allowOutcome=c("heart attack", "heart failure", "pneumonia")
   if (!is.element(state, allowState) | length(state)>1) {
       stop("invalid state argument") 
   }
   if (!is.element(outcome_type,allowOutcome) | length(outcome_type)>1) {
       stop("invalid outcome_type argument")
   }
  
   #find name of hospital with lowest 30 day mortality
   ColList = list("heart attack"=11,"heart failure"=17,"pneumonia"=23)
   workDat=na.omit(LoadDat[LoadDat$State==state,c(2,ColList[[outcome_type]])])
   minVal=min(workDat[,2])
   minDat=workDat[workDat[,2]==minVal,]
   Output = as.character(sort(minDat[,1])[1])
   print(Output)
 
}