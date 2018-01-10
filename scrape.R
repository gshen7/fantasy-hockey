# install.packages("rvest")
# install.packages("XML")

library("rvest")
library("XML")

sPoints<-list()
gPoints<-list()

currentPeriod<-97

# automatically get current day - starting day
for(teamId in 1:11){
  s<-NA
  g<-NA
  for(scoringPeriod in 1:currentPeriod){
    url <- paste("http://games.espn.com/fhl/clubhouse?leagueId=34619&teamId=",teamId,"&seasonId=2018&scoringPeriodId=", scoringPeriod, sep="")
    
    day <- read_html(url)
    
    skaters <- html_nodes(day, xpath = '//*[@id="playertable_0"]')
    head(skaters)
    skaters<-as(skaters, "character")
    skaters<-readHTMLTable(skaters)
    skaters<-as.data.frame(skaters)
    skaters <- data.frame(lapply(skaters, as.character), stringsAsFactors=FALSE)
    colnames(skaters)<-skaters[1,]
    skaters<-skaters[-1,]
    skaters<-Filter(function(x) !(all(x=="")), skaters)
    
    goalies <- html_nodes(day, xpath = '//*[@id="playertable_1"]')
    head(goalies)
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
    s<-c(s,skatersTotals[1,]$PTS)
    
    goaliesTotals<-goalies[goalies$SLOT == "",]
    colnames(goaliesTotals)<-colnames(goaliesTotals)[-(1:2)]
    goaliesTotals<-goaliesTotals[,-(c(1:2,(ncol(goaliesTotals)-3):ncol(goaliesTotals)))]
    colnames(goaliesTotals)[1:2]<-""
    # goaliesTotals[1,]$PTS
    g<-c(g,goaliesTotals[1,]$PTS)
  }
  sPoints[[teamId]]<-s[-1]
  gPoints[[teamId]]<-g[-1]
}

for(team in 1:11){
  sPoints[[team]]<-cumsum(sPoints[[team]])
  gPoints[[team]]<-cumsum(gPoints[[team]])
}

COLOURS<-c("#f44242", "#f4a941", "#f2ec3e", "#259b31", "#4fc1ea", "#1627a3", "#aa76f7", "#ed5ac8", "black", "grey", "brown")
TEAMS<-c("Devin", "Matt", "Evan", "Dave", "Brett", "Jory", "Gary", "Carl", "Kyle", "Ryan", "Tanner")
plot(1:currentPeriod, sPoints[[1]]+gPoints[[1]], type="l", col=COLOURS[1],xlim=c(0,100),ylim=c(0,3000))
for(team in 2:11){
  lines(1:currentPeriod, sPoints[[team]]+gPoints[[team]], type="l", col=COLOURS[team])
}
legend(0, 3000, legend=TEAMS, col=COLOURS, lty=1, cex=0.8)

