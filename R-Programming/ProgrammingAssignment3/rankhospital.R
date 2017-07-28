#function ranks hospitals by outcome in a US state

rankhospital <- function(state, outcome_type, num = "best") {
    LoadDat = read.csv("Datasets/outcome-of-care-measures.csv",na.strings = "Not Available")
    ## Check that state and outcome are valid
    allowState=unique(LoadDat$State)
    allowOutcome=c("heart attack", "heart failure", "pneumonia")
    allowNum=c("best","worst")
    if (!is.element(state, allowState) | length(state)>1) {
        stop("invalid state argument") 
    }
    if (!is.element(outcome_type,allowOutcome) | length(outcome_type)>1) {
        stop("invalid outcome_type argument")
    }
    if (!is.element(num,allowNum) & !is.numeric(num)) {
        stop("invalid num argument")
    }
    
    #segment data frame
    ColList = list("heart attack"=11,"heart failure"=17,"pneumonia"=23)
    workDat=na.omit(LoadDat[LoadDat$State==state,c(2,ColList[[outcome_type]])])
   #sort data frame
    alpha.order.workDat=workDat[order(workDat[,1]),]
    numeric.order.workDat=alpha.order.workDat[order(alpha.order.workDat[,2]),]
    #change "best" and "worst" to numbers
    if (num=="best") {
        num=1
    }
    if (num == "worst") {
        num=nrow(numeric.order.workDat)
    }
  
    #return name of hospital of given rank of 30 day mortality
    Output = as.character(numeric.order.workDat[num,1]) 
    print(Output)
    
}


   
    
