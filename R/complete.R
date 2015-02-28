complete<-function(directory,id=1:332){
fid=sprintf("%03d",id)
files_full=paste0(directory,"/",fid,sep=".csv")
tmp <- vector(mode = "list", length = length(id))
output=data.frame()
for (i in seq_along (files_full)) 
{
     tmp[[i]] <- read.csv(files_full[[i]],header=TRUE)
     filecontent<-tmp[[i]]
     nobs<-0
     for (cols in 1:nrow(filecontent))
        {
        	flag<-T
        	for (row in 1:ncol(filecontent))
          		{
          	         if(is.na(filecontent[[row]][[cols]]))
        			   flag<-F
        	      }
        	 if(flag==T)
             nobs<-nobs+1	
        }
output <-rbind(output,c(id[i],nobs))
}
colnames(output)<-c('id','nobs')
output
}