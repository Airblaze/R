best1 <- function(state, outcome) {
  ## Read outcome data
  outputFile=read.csv('rprog_data_ProgAssignment3-data/outcome-of-care-measures.csv')
  possible_outcomes<-c("heart attack","heart failure","pneumonia")
  outcome_col<-c("heart attack"=11,"heart failure"=17,"pneumonia"=23)
  
  if (!(state %in% outputFile[,7]))
    stop('invalid state')
  if(!(outcome %in% possible_outcomes))
    stop('invalid outcome')
  else 
    colnumber<-outcome_col[outcome]
  selectDataframe<-data.frame()
  selectDataframe<-rbind(selectDataframe,outputFile[,2],outputFile[,7],outputFile[,colnumber]) ##single list is formed in the dataframe, needs to be corrected
  s1<-split(selectDataframe,selectDataframe$State)
  selectDataframeState<-s1[state]
  index<-which(!(selectDataframeState[[1]][,3]=="Not Available"))
  computationFrame<-selectDataframeState[[1]][index,]
  
  as.numeric(computationFrame[,3])
  
  
  
  
}


