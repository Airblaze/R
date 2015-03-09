best <- function(state, outcome) {
  ## Read outcome data
  output=read.csv('rprog_data_ProgAssignment3-data/outcome-of-care-measures.csv')
  possible_outcomes<-c("heart attack","heart failure","pneumonia")
  outcome_col<-c("heart attack"=11,"heart failure"=17,"pneumonia"=23)
  
  if (!(state %in% output[,7]))
    stop('invalid state')
  if(!(outcome %in% possible_outcomes))
    stop('invalid outcome')
  else 
    colnumber=outcome_col[outcome]
  
  #output.sub=subset(output,output[,7]==state,na.rm=T)
  s<-split(output,output$State)
  takeMax <- function(x) max(x[, c("Hospital 30-Day Death (Mortality) Rates from Heart Attack", "Hospital 30-Day Death (Mortality) Rates from Heart Failure", "Hospital 30-Day Death (Mortality) Rates from Pneumonia"), na.rm = TRUE)
  tapply(s,takeMax)
  #print(output.sub[colnumber])
  #maxval<-max(output.sub[colnumber])
  #print(maxval)
}









#sort dataframe by col
sort.df <- with(df,  df[order(sortbythiscolumn) , ])

#can also sort by more than one variable: sort by col1 and then by col2
sort2.df <- with(df, df[order(col1, col2) , ])

#sort in reverse order
sort2.df <- with(df, df[order(col1, -col2) , ])









best <- function(state, outcome) {
  ## Read outcome data
  outputFile=read.csv('rprog_data_ProgAssignment3-data/outcome-of-care-measures.csv')
  possible_outcomes<-c("heart attack","heart failure","pneumonia")
  outcome_col<-c("heart attack"=11,"heart failure"=17,"pneumonia"=23)
  
  if (!(state %in% output[,7]))
    stop('invalid state')
  if(!(outcome %in% possible_outcomes))
    stop('invalid outcome')
  else 
    colnumber=outcome_col[outcome]
  
  Processed_dataframe<-c(NULL)
  outputFrame<-outputFile[which(outputFile$State==state & !(toString(outputFile[row,colnumber])=='Not Available')),c(2,7,colnumber)]
  print(colnumber)
  for( row in 1:nrow(outputFrame))
  {
    #print(outputFile[row,colnumber])
    if(!(toString(outputFile[row,colnumber])=='Not Available'))
    {
      #print(row)
      #print(outputFile[row,colnumber])
      print(outputFrame[row,])
      Processed_dataframe<-c(Processed_dataframe,outputFrame[row,])
    }
  }
  
  #print(Processed_dataframe)
  #max(Processed_dataframe[,colnumber])
  #print(Processed_dataframe)
  #print(Processed_dataframe)
  #s<-split(Processed_dataframe,outputFrame$State)
  #takeMax <- function(x) max(x[,colnumber], na.rm = TRUE)
  #lapply(s,takeMax)
  
}



makeVector <- function(x = numeric()) {
  m <- NULL
  set <- function(y) {
    x <- y
    m <- NULL
  }
  get <- function() x
  setmean <- function(mean) m <- mean
  getmean <- function() m
  list(set = set, get = get,
       setmean = setmean,
       getmean = getmean)
}



