##Start by importing CSV Data
##Modified data from data.world/makeovermonday/2018w29-historical-nba-team-spending-against-the-cap
`NBASalary+Standing+PlayoffRecord` <- read.csv("C:/Users/dpesl/Desktop/NBASalary+Standing+PlayoffRecord.csv")

##Add indicator for playoff rankings
##Note we separate them, since the NBA changed from Best-Of-3 to Best-Of-4 in the
##first round in 2002-03
finish <- c()
for (j in 1:338) {
      if(`NBASalary+Standing+PlayoffRecord`$Playoff.Wins[j] == 0){
        finish <- c(finish, "MISSED")
      }
      if(`NBASalary+Standing+PlayoffRecord`$Playoff.Wins[j] <= 2 &&
         `NBASalary+Standing+PlayoffRecord`$Playoff.Wins[j] > 0){
        finish <- c(finish, "1R")
      }
      if(`NBASalary+Standing+PlayoffRecord`$Playoff.Wins[j] <= 6 &&
         `NBASalary+Standing+PlayoffRecord`$Playoff.Wins[j] > 2){
        finish <- c(finish, "2R")
      }
      if(`NBASalary+Standing+PlayoffRecord`$Playoff.Wins[j] <= 9 &&
         `NBASalary+Standing+PlayoffRecord`$Playoff.Wins[j] > 6){
        finish <- c(finish, "CFINALS")
      }
      if(`NBASalary+Standing+PlayoffRecord`$Playoff.Wins[j] <= 14 &&
         `NBASalary+Standing+PlayoffRecord`$Playoff.Wins[j] > 9){
        finish <- c(finish, "FINALS")
      }
      if(`NBASalary+Standing+PlayoffRecord`$Playoff.Wins[j] == 15){
        finish <- c(finish, "CHAMPIONS")
      }
}
for (j in 339:846) {
    if(`NBASalary+Standing+PlayoffRecord`$Playoff.Wins[j] == 0){
      finish <- c(finish, "MISSED")
    }
    if(`NBASalary+Standing+PlayoffRecord`$Playoff.Wins[j] <= 3 &&
       `NBASalary+Standing+PlayoffRecord`$Playoff.Wins[j] > 0){
      finish <- c(finish, "1R")
    }
    if(`NBASalary+Standing+PlayoffRecord`$Playoff.Wins[j] <= 7 &&
       `NBASalary+Standing+PlayoffRecord`$Playoff.Wins[j] > 3){
      finish <- c(finish, "2R")
    }
    if(`NBASalary+Standing+PlayoffRecord`$Playoff.Wins[j] <= 10 &&
       `NBASalary+Standing+PlayoffRecord`$Playoff.Wins[j] > 7){
      finish <- c(finish, "CFINALS")
    }
    if(`NBASalary+Standing+PlayoffRecord`$Playoff.Wins[j] <= 15 &&
       `NBASalary+Standing+PlayoffRecord`$Playoff.Wins[j] > 10){
      finish <- c(finish, "FINALS")
    }
    if(`NBASalary+Standing+PlayoffRecord`$Playoff.Wins[j] == 16){
      finish <- c(finish, "CHAMPIONS")
    }
}
Df <- cbind(`NBASalary+Standing+PlayoffRecord`, finish)

##Note we use a function from the nbastatR package to get data from basketball-reference.com
##devtools::install_github("abresler/nbastatR")
##Note that we rename New Orleans/Oklahoma City Hornets to New Orleans Hornets 
##for dataset matching purposes
library(nbastatR)
bref_teams_stats(seasons = 1991:2019)
dataBREFMiscTeams[450,5] <- 'New Orleans Hornets'
dataBREFMiscTeams[473,5] <- 'New Orleans Hornets'
dataBREFMiscTeams$slugSeason <- as.factor(dataBREFMiscTeams$slugSeason)
dataBREFMiscTeams$nameTeam <- as.factor(dataBREFMiscTeams$nameTeam)
dataBREFPerGameTeams[451,5] <- 'New Orleans Hornets'
dataBREFPerGameTeams[481,5] <- 'New Orleans Hornets'
dataBREFPerGameTeams$slugSeason <- as.factor(dataBREFPerGameTeams$slugSeason)
dataBREFPerGameTeams$nameTeam <- as.factor(dataBREFPerGameTeams$nameTeam)
dataBREFPerPossTeams[451,5] <- 'New Orleans Hornets'
dataBREFPerPossTeams[481,5] <- 'New Orleans Hornets'
dataBREFPerPossTeams$slugSeason <- as.factor(dataBREFPerPossTeams$slugSeason)
dataBREFPerPossTeams$nameTeam <- as.factor(dataBREFPerPossTeams$nameTeam)

##Use multiple for loops to join all datasets together
datafr <- 0
for (i in 1:dim(Df)[1]) {
  for (j in 1:dim(dataBREFMiscTeams)[1]) {
    if((Df$Season[i] == dataBREFMiscTeams$slugSeason[j]) &&
       (Df$Team[i] == dataBREFMiscTeams$nameTeam[j])){
      datafr <- rbind(datafr, cbind(Df[i,], dataBREFMiscTeams[j,]))
    }
  }
}
datafr <- datafr[-1,]
datafr2 <- 0
for (i in 1:dim(datafr)[1]) {
  for (j in 1:dim(dataBREFPerGameTeams)[1]) {
    if((datafr$Season[i] == dataBREFPerGameTeams$slugSeason[j]) &&
       (datafr$Team[i] == dataBREFPerGameTeams$nameTeam[j])){
      datafr2 <- rbind(datafr2, cbind(datafr[i,], dataBREFPerGameTeams[j,]))
    }
  }
}
datafr2 <- datafr2[-1,]
datafr3 <- 0
for (i in 1:dim(datafr2)[1]) {
  for (j in 1:dim(dataBREFPerPossTeams)[1]) {
    if((datafr2$Season[i] == dataBREFPerPossTeams$slugSeason[j]) &&
       (datafr2$Team[i] == dataBREFPerPossTeams$nameTeam[j])){
      datafr3 <- rbind(datafr3, cbind(datafr2[i,], dataBREFPerPossTeams[j,]))
    }
  }
}
datafr3 <- datafr3[-1,]

##Export into a master data frame
##Remove unimportant variables and repeated variables
masterdf <- datafr3
masterdf <- masterdf[,-(10:14)]
masterdf <- masterdf[,-(18:24)]
masterdf <- masterdf[,-(34:45)]
masterdf <- masterdf[,-(78:90)]

write.csv(masterdf, file = "c:/Users/dpesl/Desktop/NBASalaryAnalysisData.csv")
