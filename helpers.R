optimize <- function(data){
  TEAMS<-c("Devin", "Matt", "Evan", "Dave", "Brett", "Jory", "Gary", "Carl", "Kyle", "Ryan", "Tanner") 
  
  table <- data.frame(matrix(nrow=0,ncol=5))
  colnames(table)<-c("day","team","skater","goalie","total")
  
  for(row in 1:nrow(data)){
    d <- data[row,1]
    for(team in 1:length(TEAMS)){
      append <- data.frame(matrix(nrow=1,ncol=5))
      colnames(append)<-c("day","team","skater","goalie","total")
      
      append[1,]$day<-d
      append[1,]$team<-TEAMS[team]
      append[1,]$skater<-data[row,1+team+11]
      append[1,]$goalie<-data[row,1+team+22]
      append[1,]$total<-data[row,1+team]
      
      table<-rbind(table, append)
    }
  }
  return(table)
}

fLoad <- function(){
  load(file="~/Documents/Projects/fantasy-hockey/Output/data.Rda")
  load(file="~/Documents/Projects/fantasy-hockey/Output/table.Rda")
  return(data)
}

fSave <- function(data){
  table <- optimize(data)
  
  save(data, file="~/Documents/Projects/fantasy-hockey/Output/data.Rda")
  save(table, file="~/Documents/Projects/fantasy-hockey/Output/table.Rda")
}

fBackup <- function(period){
  load(file="~/Documents/Projects/fantasy-hockey/Output/data.Rda")
  load(file="~/Documents/Projects/fantasy-hockey/Output/table.Rda")
  save(data, file=paste("~/Documents/Projects/fantasy-hockey/Output/Backup/data-",period,".Rda", sep=""))
  save(table, file=paste("~/Documents/Projects/fantasy-hockey/Output/Backup/table-",period,".Rda", sep=""))
}

