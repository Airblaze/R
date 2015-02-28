source("complete.R")
corr <- function(directory, threshold = 400) 
{
filedata=data.frame()
filedata<-rbind(filedata,complete(directory))
idlist<-c(NULL)
correlation<-c(NULL)
for (row in 1:nrow(filedata))
{
	if (filedata[row,2] > threshold)
	   idlist<-c(idlist,filedata[row,1])
}
#idlist
for(id in idlist)
{
	fid=sprintf("%03d",id)
    files_path=paste0(directory,"/",fid,sep=".csv")
	filecontent=read.csv(files_path ,header=TRUE)
    f1<-c(NULL)
    f2<-c(NULL)
    for (cols in 1:nrow(filecontent))
   {
        	flag<-T
        	for (row in 2:3)
          		{
          	         if(is.na(filecontent[[row]][[cols]]))
        			   flag<-F
        	     }
        	 if(flag==T)
             {
             	f1<-c(f1,filecontent[[2]][[cols]])	
                f2<-c(f2,filecontent[[3]][[cols]])
             }
    }
     #print(f1)
     #print(f2)
     correlation<-c(correlation,cor(f1,f2)) 
}
	 print(threshold)
	 print(correlation)
	 correlation
	 #summary(correlation)
}