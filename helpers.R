fSave <- function(data){
  save(data, file="~/Documents/Projects/fantasy-hockey/Output/data.Rda")
}

fLoad <- function(){
  load(file="~/Documents/Projects/fantasy-hockey/Output/data.Rda")
  
  return(data)
}

fBackup <- function(period){
  load(file="~/Documents/Projects/fantasy-hockey/Output/data.Rda")
  
  save(data, file=paste("~/Documents/Projects/fantasy-hockey/Output/Backup/data-",period,".Rda", sep=""))
}
