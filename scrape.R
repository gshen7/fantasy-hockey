# scrape.R

# install.packages("rvest")
# install.packages("XML")

source("~/Documents/Projects/fantasy-hockey/helpers.R")

#* @get /initialize
initialize <- function(period=10){
  library("rvest")
  library("XML")

  TEAMS<-c("Devin", "Matt", "Evan", "Dave", "Brett", "Jory", "Gary", "Carl", "Kyle", "Ryan", "Tanner")

  period<-as.numeric(period)
  
  pointsTotal <- data.frame(matrix(nrow=period+1, ncol=1))
  pointsSkater <- data.frame(matrix(nrow=period+1, ncol=1))
  pointsGoalie <- data.frame(matrix(nrow=period+1, ncol=1))
  
  colnames(pointsTotal)<-c("day")
  colnames(pointsSkater)<-c("day")
  colnames(pointsGoalie)<-c("day")
  
  pointsTotal$day <- 0:period
  pointsSkater$day <- 0:period
  pointsGoalie$day <- 0:period
  
  for(team in 1:11){
    pointsTotal <- cbind(pointsTotal,NA)
    pointsSkater<- cbind(pointsSkater,NA)
    pointsGoalie<- cbind(pointsGoalie,NA)
  }
  
  colnames(pointsTotal)<-c("day",TEAMS)
  colnames(pointsSkater)<-c("day",TEAMS)
  colnames(pointsGoalie)<-c("day",TEAMS)
  
  pointsTotal[1,] <- c(0)
  pointsSkater[1,] <- c(0)
  pointsGoalie[1,] <- c(0)
  
  for(scoringPeriod in 1:period){
    for(teamId in 1:11){
      url <- paste("http://games.espn.com/fhl/clubhouse?leagueId=34619&teamId=",teamId,"&seasonId=2018&scoringPeriodId=", scoringPeriod, sep="")
    
      day <- read_html(url)
      
      skaters <- html_nodes(day, xpath = '//*[@id="playertable_0"]')
      skaters<-as(skaters, "character")
      skaters<-readHTMLTable(skaters)
      skaters<-as.data.frame(skaters)
      skaters <- data.frame(lapply(skaters, as.character), stringsAsFactors=FALSE)
      colnames(skaters)<-skaters[1,]
      skaters<-skaters[-1,]
      skaters<-Filter(function(x) !(all(x=="")), skaters)
      
      goalies <- html_nodes(day, xpath = '//*[@id="playertable_1"]')
      goalies<-as(goalies, "character")
      goalies<-readHTMLTable(goalies)
      goalies<-as.data.frame(goalies)
      goalies <- data.frame(lapply(goalies, as.character), stringsAsFactors=FALSE)
      colnames(goalies)<-goalies[1,]
      goalies<-goalies[-1,]
      goalies<-Filter(function(x) !(all(x=="")), goalies)
      
      skatersTotals<-skaters[skaters$SLOT == "",]
      colnames(skatersTotals)<-colnames(skatersTotals)[-(1:2)]
      skatersTotals<-skatersTotals[,-(c(1:2,(ncol(skatersTotals)-3):ncol(skatersTotals)))]
      colnames(skatersTotals)[1:2]<-""
      # skatersTotals[1,]$PTS
      pointsSkater[scoringPeriod+1,teamId+1]<-as.numeric(skatersTotals[1,]$PTS)+as.numeric(pointsSkater[scoringPeriod,teamId+1])
      
      goaliesTotals<-goalies[goalies$SLOT == "",]
      colnames(goaliesTotals)<-colnames(goaliesTotals)[-(1:2)]
      goaliesTotals<-goaliesTotals[,-(c(1:2,(ncol(goaliesTotals)-3):ncol(goaliesTotals)))]
      colnames(goaliesTotals)[1:2]<-""
      # goaliesTotals[1,]$PTS
      pointsGoalie[scoringPeriod+1,teamId+1]<-as.numeric(goaliesTotals[1,]$PTS)+as.numeric(pointsGoalie[scoringPeriod,teamId+1])
      
      pointsTotal[scoringPeriod+1,teamId+1]<-pointsSkater[scoringPeriod+1,teamId+1]+pointsGoalie[scoringPeriod+1,teamId+1]
    }
  }
  data <- cbind(pointsTotal, pointsSkater, pointsGoalie)
  fBackup(period)
  fSave(data)
  
  paste("initialized to scoring period ", period)
}

#* @get /update
update <- function(newPeriod=10){
  library("rvest")
  library("XML")
  
  TEAMS<-c("Devin", "Matt", "Evan", "Dave", "Brett", "Jory", "Gary", "Carl", "Kyle", "Ryan", "Tanner")
  
  newPeriod<-as.numeric(newPeriod)
  
  data <- fLoad()
  pointsTotal <- data[,1:12]
  pointsSkater <- data[,13:24]
  pointsGoalie <- data[,25:36]
  
  currentPeriod <- nrow(pointsTotal)-1
  
  for(scoringPeriod in (currentPeriod+1):newPeriod){
    pointsTotal <- rbind(pointsTotal,c(scoringPeriod, NA))
    pointsSkater <- rbind(pointsSkater,c(scoringPeriod, NA))
    pointsGoalie <- rbind(pointsGoalie,c(scoringPeriod, NA))
    
    for(teamId in 1:11){    
      url <- paste("http://games.espn.com/fhl/clubhouse?leagueId=34619&teamId=",teamId,"&seasonId=2018&scoringPeriodId=", scoringPeriod, sep="")
      
      day <- read_html(url)
      
      skaters <- html_nodes(day, xpath = '//*[@id="playertable_0"]')
      skaters<-as(skaters, "character")
      skaters<-readHTMLTable(skaters)
      skaters<-as.data.frame(skaters)
      skaters <- data.frame(lapply(skaters, as.character), stringsAsFactors=FALSE)
      colnames(skaters)<-skaters[1,]
      skaters<-skaters[-1,]
      skaters<-Filter(function(x) !(all(x=="")), skaters)
      
      goalies <- html_nodes(day, xpath = '//*[@id="playertable_1"]')
      goalies<-as(goalies, "character")
      goalies<-readHTMLTable(goalies)
      goalies<-as.data.frame(goalies)
      goalies <- data.frame(lapply(goalies, as.character), stringsAsFactors=FALSE)
      colnames(goalies)<-goalies[1,]
      goalies<-goalies[-1,]
      goalies<-Filter(function(x) !(all(x=="")), goalies)
      
      skatersTotals<-skaters[skaters$SLOT == "",]
      colnames(skatersTotals)<-colnames(skatersTotals)[-(1:2)]
      skatersTotals<-skatersTotals[,-(c(1:2,(ncol(skatersTotals)-3):ncol(skatersTotals)))]
      colnames(skatersTotals)[1:2]<-""
      # skatersTotals[1,]$PTS
      pointsSkater[scoringPeriod+1,teamId+1]<-as.numeric(skatersTotals[1,]$PTS)+as.numeric(pointsSkater[scoringPeriod,teamId+1])
      
      goaliesTotals<-goalies[goalies$SLOT == "",]
      colnames(goaliesTotals)<-colnames(goaliesTotals)[-(1:2)]
      goaliesTotals<-goaliesTotals[,-(c(1:2,(ncol(goaliesTotals)-3):ncol(goaliesTotals)))]
      colnames(goaliesTotals)[1:2]<-""
      # goaliesTotals[1,]$PTS
      pointsGoalie[scoringPeriod+1,teamId+1]<-as.numeric(goaliesTotals[1,]$PTS)+as.numeric(pointsGoalie[scoringPeriod,teamId+1])
      
      pointsTotal[scoringPeriod+1,teamId+1]<-pointsSkater[scoringPeriod+1,teamId+1]+pointsGoalie[scoringPeriod+1,teamId+1]
    }
  }  
  data <- c(pointsTotal, pointsSkater, pointsGoalie)
  fBackup(newPeriod)
  fSave(data)
  
  paste("updated to scoring period ", newPeriod)
}
