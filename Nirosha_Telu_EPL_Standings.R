## Author: Nirosha Telu
# EPL_Standings final project 

library(tidyverse)
library(lubridate)
library(dbplyr)


EPL_Standings <- function(date,season){
  # Convertion of input date from character type to date type 
  date <- as.Date(mdy(date),format = '%m/%d/%Y')
  # Change season format: Example: '2020/21' to '2120/'  
  url_season <- paste0(substr(season,3,4), substr(season,6,7),'/')
  # creating url
  url <- paste0('http://www.football-data.co.uk/mmz4281/',url_season,'E0.csv')
  # assign csv into a data frame
  epl_df <- read_csv(url)
  
  # creating a subset from epl_df, only included necessary columns
  df <- epl_df %>%
    select(Date, 
           HomeTeam, 
           AwayTeam, 
           FTHG, 
           FTAG,
           FTR) %>%
    # extract year from Date column to take care of 'YYYY' format and 'yy' format.
           mutate(year = ifelse(nchar(Date) == 10,substring(Date,9,10),
                         ifelse(nchar(Date) == 8,substring(Date,7,8),'Check Date format')),
           Date = paste0(substring(Date,1,6),year),
           Date1 = as.Date(Date,format = '%d/%m/%y')) %>% # Convert to Date format 
    # subset of epl_df only includes matches played up to and including the specified  date input 
    filter(Date1 <= date) %>% 
    mutate(point_home = ifelse(FTR == 'D', 1, ifelse(FTR == 'H', 3, ifelse(FTR == 'A', 0,NA))), # point earned played at home city
           point_away = ifelse(FTR == 'D', 1, ifelse(FTR == 'H', 0, ifelse(FTR == 'A', 3,NA))), # point earned played at opponent's city 
           win_home = ifelse(FTR == 'H',1,0), # if win at home, count 1
           win_away = ifelse(FTR == 'A', 1,0), # if win away, count 1
           draw_home = ifelse(FTR == 'D',1,0), # if draw at home, count 1
           draw_away = ifelse(FTR == 'D',1,0), # if draw away, count 1
           losse_home = ifelse(FTR == 'A',1,0), # if lost at home, count 1
           losse_away = ifelse(FTR == 'H',1,0)
    ) # if lost away, count 1
  
  # Creating a subset of only matches played at home 
  home <- df %>%
    select(Date, HomeTeam, FTHG, FTAG, FTR, Date1, point_home, win_home,draw_home,losse_home) %>%
    group_by(TeamName = HomeTeam) %>% # Group by Team name and calculate: (see below)
    summarise(count_home = n(), # total game played 
              point_home = sum(point_home), # total points earned
              wins_home = sum(win_home), # total wins
              draws_home = sum(draw_home), # total draws
              losses_home = sum(losse_home), # total losses
              goals_for_home = sum(FTHG), # total goals scored
              goals_against_home = sum(FTAG)) # total goals opponents scored against 
  
  # Creating a subset of only matches played away
  away <- df %>%
    select(Date, Date1, AwayTeam, FTHG,FTAG, FTR, point_away, win_away, draw_away, losse_away) %>%
    group_by(TeamName = AwayTeam) %>% # Group by Team name and calculate: (see below)
    summarise(count_away = n(), # total game played 
              point_away = sum(point_away),  # total points earned
              wins_away = sum(win_away), # total wins
              draws_away = sum(draw_away), # total draws
              losses_away = sum(losse_away), # total losses
              goals_for_away = sum(FTAG), # total goals scored
              goals_against_away = sum(FTHG)) # total goals opponents scored against
  
  # join home subset and away subset by 'TeamName'
  join_df1 <- home %>%
    full_join(away, by = c('TeamName'))
  
  # Convert NA to 0
  join_df1[is.na(join_df1)] <- 0
  
  # Create calculated columns to answer all the questions required
  join_df <- join_df1 %>%    
    mutate(MatchesPlayed = count_home + count_away, # total matches played
           Points = point_home + point_away, # total points earned
           PPM = round(Points/MatchesPlayed,3), # points per match 
           PtPct = round(Points/(3*MatchesPlayed),2), # point percentage = points / 3 * the number of games played
           Wins = wins_home + wins_away, # total wins, need this for Record(wins-loses-ties)
           Draws = draws_home + draws_away, # total draws, need this for Record(wins-loses-ties)
           Losses = losses_home + losses_away, # total loses, need this for Record(wins-loses-ties)
           Record = paste0(Wins,'-',Losses,'-',Draws), # Record(wins-loses-ties)
           HomeRec = paste0(wins_home,'-',losses_home,'-',draws_home), # home record
           AwayRec = paste0(wins_away,'-',losses_away,'-',draws_away), # away record
           GS = goals_for_home + goals_for_away, # goals scored 
           GSM = round(GS/MatchesPlayed,2), # goals scored per match 
           GA = goals_against_home + goals_against_away, # goals allowed 
           GAM = round(GA/MatchesPlayed,2)) # goals allowed per match 
  
  
  # Creat a subset to calculate the team’s record over the last 10 games played (Last10)
  
  # last 10 home
  last10_home <- df %>%
    select(Date1, TeamName = HomeTeam, win = win_home,draw = draw_home,losse = losse_home)
  # last 10 away
  last10_away <- df %>%
    select(Date1, TeamName = AwayTeam, win = win_away, draw = draw_away,losse = losse_away)
  # join last 10 home and last 10 away
  last_10df <- rbind(last10_home, last10_away) 
  # from last 20 games, only get lasted 10 games played
  join_df <- last_10df %>%
    group_by(TeamName) %>%
    #arrange(desc(Date1)) %>%
    top_n(10,wt = Date1) %>%
    summarise(Wins = sum(win),
              Losses = sum(losse),
              Draws = sum(draw)) %>%
    mutate(Last10 = paste0(Wins,'-',Losses,'-',Draws)) %>%
    select(TeamName,Last10) %>%
    inner_join(join_df, by = c('TeamName')) %>% # Join last 10 subset with join_df 
    arrange(TeamName) # arrange Team Name by alphabetical order
  
  
  # Create a subset to calculate the team’s current streak 
  
  # streak home
  streak_home <- df %>%
    select(Date1,TeamName = HomeTeam,FTR) %>%
    mutate(result = ifelse(FTR == 'D', 'D', ifelse(FTR == 'H', 'W','L'))) %>%
    select(-c(FTR))
  # streak away
  streak_away <- df %>%
    select(Date1,TeamName = AwayTeam,FTR) %>%
    mutate(result = ifelse(FTR == 'D', 'D', ifelse(FTR == 'A', 'W','L'))) %>%
    select(-c(FTR)) # remove FTR columns 
  # join streak home and streak away
  streak <- rbind(streak_home,streak_away)
  # spread data frame to have date columns
  streak <- rbind(streak_home,streak_away) %>%
    spread(key = 'Date1', value = result) %>%
    arrange(TeamName)
  # subset of TeamName
  TeamName <- streak$TeamName
  # subset of streak without Team Name but the order of this streak match Team names alphabetical order
  # Use function rle() to count consecutive win, draw, lost
  streak1 <- apply(streak, 1, function(x) {
    x <- x[!is.na(x)] # remove NAs
    r <- rle(x)
    streak1 = tail(unlist(r[1]),n=1)
    streak2 =tail(unlist(r[2]),n=1)
    paste0(streak1,streak2)
  })
  streak1 <- as.vector(unlist(streak1)) # convert streak subset to a vector
  # Join team names with its streak
  streak2 <- data.frame(cbind(TeamName,streak1)) %>%
    mutate(TeamName = as.character(TeamName)) # convert TeamName from factor to character
  # Join streak with join_df, select necessary columns, called this final_df
  final_df <- join_df %>%
    inner_join(streak2, by = c('TeamName')) %>%
    arrange(desc(PPM), desc(Wins),desc(GSM),GAM) %>%
    select(TeamName, Record, HomeRec,AwayRec,MatchesPlayed,Points,PPM,PtPct,GS,GSM,GA,GAM,Last10,Streak = streak1)
  return(final_df)
}

# Please enter the date in mdy format

epl <- EPL_Standings("09/22/2018","2018/19")

epl <- EPL_Standings("09/22/2020","2020/21")

epl <- EPL_Standings("09/22/19","2018/19")
epl
