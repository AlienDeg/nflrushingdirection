library(nflscrapR)
library(dplyr)
library(ggplot2)
library(lubridate)
library(viridis)
library(ggthemes)
library(grid)

allGameIDs<-c(extracting_gameids(2009:2016, playoffs = FALSE),
              extracting_gameids(2009:2016, playoffs = TRUE))




for(i in allGameIDs) {
  x<- game_play_by_play(i)
  playByPlay2009 <- rbind(playByPlay2009, x)
}


rush <- all[which(all$PlayType == 'Run'),]
rush$gain <- ifelse(rush$Yards_Gained < 0 ,'negative', ifelse(rush$Yards_Gained == 0 ,'no gain', ifelse(rush$Yards_Gained  > 0  & rush$Yards_Gained <4 ,'small gain',  ifelse(rush$Yards_Gained  > 3  & rush$Yards_Gained <7 ,'medium gain', 'big gain' ) ) ))

rushes <- rush %>% select(Date,GameID,posteam, Rusher,RunLocation,RunGap,Yards_Gained,gain)
rushes$RunGap<-ifelse(rushes$RunLocation == 'middle', '',rushes$RunGap)


rushes$season <- ifelse(format(as.Date(rush$Date), "%m") == '01' | 
                        format(as.Date(rush$Date), "%m") == '02' , 
                      format(as.Date(rush$Date - dyears(1)), "%Y"), 
                      format(as.Date(rush$Date), "%Y"))


rushes <- within(rushes,  direction <- paste(RunLocation, RunGap, sep=" "))
rushes$direction <- gsub('middle ','middle',rushes$direction)
rushes <- rushes %>% filter(!grepl('NA', direction))
rushes <- rushes %>% filter(grepl('[[:alpha:]]',posteam))
newRushes <- right_join(rushes,nflteams, by = c('posteam' = 'Abv'))
newRushes$posteam <- newRushes$posteam.y
newRushes$posteam.y <- NULL
newRushes$gain <- factor(newRushes$gain, levels = c('negative', 'no gain', 'small gain', 'medium gain', 'big gain'))
newRushes$direction <- factor(newRushes$direction, levels = c('left end','left tackle', 'left guard', 'middle', 'right guard', 'right tackle', 'right end'))
newRushes$season <- factor(newRushes$season, levels = c(sort(unique(newRushes$season))))


rusherData <- newRushes %>% group_by(Rusher,posteam,season,direction, gain) %>% summarise(count = n())
rusherData1 <- newRushes %>% group_by(Rusher,posteam,season,direction) %>% summarise(count = n(), sum = sum(Yards_Gained))
teamData <- newRushes %>% group_by(posteam,season,direction, gain) %>% summarise(count = n())
teamData1 <- newRushes %>% group_by(posteam,season,direction) %>% summarise(count = n(), sum = sum(Yards_Gained))







ggplot(teamData, aes(x=direction, y=gain, fill=count)) + 
  geom_tile(color="white", size=0.1) 


ggplot(teamData1, aes(x=direction,y= sum)) + geom_col(fill = 'red', width = .6) + geom_col( aes(y=count), fill = 'green', width = .4)
