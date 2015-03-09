rankall <- function(outcome,num="best") {
  
  ## Read outcome data
  outputFile=read.csv('rprog_data_ProgAssignment3-data/outcome-of-care-measures.csv')
  possible_outcomes<-c("heart attack","heart failure","pneumonia")
  outcome_col<-c("heart attack"=11,"heart failure"=17,"pneumonia"=23)
  totalstates<-unique(outputFile$State)
  if(!(outcome %in% possible_outcomes))
    stop('invalid outcome')
  else 
    colnumber=outcome_col[outcome]
  
  
  rankallFrame<-data.frame("hospital"=character(),"state"=character(),stringsAsFactors=FALSE)
  
  selectDataframe=data.frame(outputFile[,c(2,7,colnumber)])  ##single list is formed in the dataframe, needs to be corrected
  s1=split(selectDataframe,selectDataframe$State)
  numOrig=num
  for(state in sort(totalstates))
  {
    num=numOrig
    #print(state)
    selectDataframeState=s1[state]
    index<-which(!(selectDataframeState[[1]][,3]=="Not Available"))
    #print(index)
    computationFrame<-selectDataframeState[[1]][index,]
    colnames(computationFrame)<-c("HospitalName","State","Outcome")
    computationFrame[,3]<-sapply(computationFrame[,3],as.character)    ##Need to find a better way for conversion
    computationFrame[,3]<-sapply(computationFrame[,3],as.double)
    
    if(num=="best")num<-1
    else if(num=="worst"){
      num<-nrow(computationFrame)
      #print(num)
    }
    
    filterindex <- with(computationFrame,  computationFrame[order(Outcome,HospitalName),])
    #c(as.character(filterindex[num,1]),state)
    filterindex[,1] <- sapply(filterindex[,1],as.character)
    df=data.frame("hospital"=filterindex[num,1],"state"=state)
    rankallFrame<-rbind(rankallFrame,df)   
  }
  rankallFrame
}