# function returns dataframe showing the hospital in each state with specified rank for specified outcome type

rankall <- function(outcome_type, num = "best") {
    ## Read outcome data
    LoadDat = read.csv("Datasets/outcome-of-care-measures.csv",na.strings = "Not Available")
    ## Check that num and outcome are valid
    allowOutcome=c("heart attack", "heart failure", "pneumonia")
    allowNum=c("best","worst")
    if (!is.element(outcome_type,allowOutcome) | length(outcome_type)>1) {
        stop("invalid outcome_type argument")
    }
    if (!is.element(num,allowNum) & !is.numeric(num)) {
        stop("invalid num argument")
    }
    
    ## For each state, find the hospital of the given rank
    OutcomeDF<-data.frame()
    ColList = list("heart attack"=11,"heart failure"=17,"pneumonia"=23)
    #segment data frame by outcome_type
    workDat=LoadDat[,c(2,7,ColList[[outcome_type]])]
    #loop through states
    for (state in sort(unique(LoadDat$State))) {
       #segment data frame by state
       workDat.state=na.omit(workDat[workDat$State==state,])
       #sort data frame
       alpha.order.workDat=workDat.state[order(workDat.state[,1]),]
       numeric.order.workDat=alpha.order.workDat[order(alpha.order.workDat[,3]),]
       #change "best" and "worst" to numbers
       if (num == "best") {
           index=1
       } else if (num == "worst") {
           index=nrow(numeric.order.workDat)
       } else 
           index=num
           
       #store result in dataframe
       hosp = as.character(numeric.order.workDat[index,1])
       OutcomeDF=rbind(OutcomeDF,data.frame(hospital=hosp, state=state))
    }
    OutcomeDF
}


