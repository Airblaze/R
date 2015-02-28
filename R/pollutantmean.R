pollutantmean<-function(directory,pollutant,id=1:2){
fid=sprintf("%03d",id)
files_full=paste0(directory,"/",fid,sep=".csv")
tmp <- vector(mode = "list", length = length(id))
for (i in seq_along (files_full)) {
        tmp[[i]] <- read.csv(files_full[[i]],header=TRUE)
    }
output <- do.call(rbind, tmp)
#str(output)
print(pollutant)
print(id)
print(round(mean(output[[pollutant]],na.rm=T),4))
#mean(output[[pollutant]],na.rm=T)
}
