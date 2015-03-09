best <- function(state, outcome) {
  ## Read outcome data
  outputFile=read.csv('rprog_data_ProgAssignment3-data/outcome-of-care-measures.csv')
  possible_outcomes<-c("heart attack","heart failure","pneumonia")
  outcome_col<-c("heart attack"=11,"heart failure"=17,"pneumonia"=23)
  
  if (!(state %in% outputFile[,7]))
    stop('invalid state')
  if(!(outcome %in% possible_outcomes))
    stop('invalid outcome')
  else 
    colnumber=outcome_col[outcome]
  selectDataframe=data.frame(outputFile[,c(2,7,colnumber)])  ##single list is formed in the dataframe, needs to be corrected
  s1=split(selectDataframe,selectDataframe$State)
  selectDataframeState=s1[state]
  index<-which(!(selectDataframeState[[1]][,3]=="Not Available"))
  computationFrame<-selectDataframeState[[1]][index,]
  colnames(computationFrame)<-c("HospitalName","State","Outcome")
  computationFrame[,3]<-sapply(computationFrame[,3],as.character)    ##Need to find a better way for conversion
  computationFrame[,3]<-sapply(computationFrame[,3],as.double)
  
  filterindex<-which(computationFrame[,3]==min(computationFrame[,3]))
  hospitalName<-as.character(computationFrame[filterindex,]$HospitalName) ##if as.character is removed then hospitalName is considered integer ! why !?
  sort(hospitalName)[1]
}


