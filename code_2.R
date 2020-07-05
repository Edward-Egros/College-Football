install.packages('tidyverse')
install.packages("devtools")

#May need to update the rlang package upon restart
#remotes::update_packages("rlang")

devtools::install_github("meysubb/cfbscrapR")
remotes::install_github("rstudio/gt")

#Install and run the Rcpp package if not done

library(tidyverse)
library(cfbscrapR)
library(gt)
library(dplyr)
library(ggplot2)
library(DT)
library(shiny)
library(shinythemes)
library(rsconnect)
library(logger)
library(shinyjs)

######################
#2019 play by play
######################
pbp_2019 <- data.frame()
for(i in 1:15){
  data <- cfb_pbp_data(year = 2019, week = i, epa_wpa = TRUE) %>% mutate(week = i)
  df <- data.frame(data)
  pbp_2019 <- bind_rows(pbp_2019, df) %>% mutate(garbage = ifelse(period == 1 & abs(score_diff) > 43, 1, 
                                                                  ifelse(period == 2 & abs(score_diff) > 37, 1,
                                                                         ifelse(period == 3 & abs(score_diff) > 27, 1,
                                                                                ifelse(period == 4 & abs(score_diff) > 22, 1, 0)))))
}

#To clean up what we want to see that's useable
pbp_2019_select <- pbp_2019 %>% select(offense_play, defense_play, down, distance, play_type, yards_gained, week, EPA, success, rush, pass, game_id, home_wp)

#Creating game numbers
game.numbers.2019 <- pbp_2019 %>% group_by(offense_play, game_id) %>% mutate(num=1) %>% summarise(game.n = mean(num)) %>% ungroup() %>% group_by(offense_play) %>% mutate(game.number = cumsum(game.n)) %>% select(-game.n)

#Creating a plays variable
plays.2019 <- pbp_2019_select %>% filter(rush == 1 | pass == 1) %>% left_join(game.numbers.2019, by=c("game_id","offense_play"))

#Creating EPA variable
offense.2019 <- plays.2019 %>% group_by(offense_play) %>% summarise(epa.pass.off = mean(EPA[pass==1]), epa.rush.off = mean(EPA[rush==1]), num.plays.off = n(), off.success.rate = mean(success))
defense.2019 <- plays.2019 %>% group_by(defense_play) %>% summarise(epa.pass.def = mean(EPA[pass==1]), epa.rush.def = mean(EPA[rush==1]), num.plays.def = n(), def.success.rate = mean(success))
team.epa.2019 <- left_join(offense.2019, defense.2019, by = c("offense_play" = "defense_play")) 


###Computing Drives
# merge games and drives data and convert starting
#yard line to consistent scale - opposite of
#"adj_yd_line" in pbp data, because we want to 
#data to be 0-100 for everyone. 
#can compute successes and drives, other variables
drives_2019 <- data.frame()
for(i in 1:15){
  data <- cfb_pbp_data(year = 2019, week = i, drive = TRUE) %>% mutate(week = i)
  df <- data.frame(data)
  drives_2019 <- bind_rows(drives_2019, df)
}

# Gather game data for home/away
games.2019 <- cfb_game_info(2019) %>% rename("game_id" = id)

#Joining games and drives
drives_week_2019 <- left_join(drives_2019, games.2019, by=c("game_id", "week")) %>% left_join(game.numbers.2019, by=c("game_id", "offense"="offense_play"))

#Tells us the number of drives and average starting
#field position for each team
drives.off.2019 <- drives_week_2019 %>%
  mutate(
    adj_start_yardline = ifelse(offense == away_team, 100-start_yardline, start_yardline), 
    success = ifelse(drive_result %in% c("TD", "FG"), 1, 0),
    drive.pts = ifelse(drive_result == "TD", 6, ifelse(drive_result == "FG", 3, 0))) %>%
  group_by(offense) %>% 
  summarise(
    fp = mean(adj_start_yardline[adj_start_yardline > 10 & adj_start_yardline <40]), 
    srate = mean(success),
    drives = n(),
    drives.pts = sum(drive.pts))

#Merge drive data for app
team.epa.2019 <- left_join(team.epa.2019, drives.off.2019, by=c("offense_play"="offense"))
team.epa.2019$pts.per.drive <- team.epa.2019$drives.pts / team.epa.2019$drives


######################
#2018 play by play
######################
pbp_2018 <- data.frame()
for(i in 1:15){
  data <- cfb_pbp_data(year = 2018, week = i, epa_wpa = TRUE) %>% mutate(week = i)
  df <- data.frame(data)
  pbp_2018 <- bind_rows(pbp_2018, df) %>% mutate(garbage = ifelse(period == 1 & abs(score_diff) > 43, 1, 
                                                                  ifelse(period == 2 & abs(score_diff) > 37, 1,
                                                                         ifelse(period == 3 & abs(score_diff) > 27, 1,
                                                                                ifelse(period == 4 & abs(score_diff) > 22, 1, 0)))))
}

#To clean up what we want to see that's useable
pbp_2018_select <- pbp_2018 %>% select(offense_play, defense_play, down, distance, play_type, yards_gained, week, EPA, success, rush, pass, game_id, home_wp)

#Creating game numbers
game.numbers.2018 <- pbp_2018 %>% group_by(offense_play, game_id) %>% mutate(num=1) %>% summarise(game.n = mean(num)) %>% ungroup() %>% group_by(offense_play) %>% mutate(game.number = cumsum(game.n)) %>% select(-game.n)

#Creating a plays variable
plays.2018 <- pbp_2018_select %>% filter(rush == 1 | pass == 1) %>% left_join(game.numbers.2018, by=c("game_id","offense_play"))

#Creating EPA variable
offense.2018 <- plays.2018 %>% group_by(offense_play) %>% summarise(epa.pass.off = mean(EPA[pass==1]), epa.rush.off = mean(EPA[rush==1]), num.plays.off = n(), off.success.rate = mean(success))
defense.2018 <- plays.2018 %>% group_by(defense_play) %>% summarise(epa.pass.def = mean(EPA[pass==1]), epa.rush.def = mean(EPA[rush==1]), num.plays.def = n(), def.success.rate = mean(success))
team.epa.2018 <- left_join(offense.2018, defense.2018, by = c("offense_play" = "defense_play")) 


###Computing Drives
drives_2018 <- data.frame()
for(i in 1:15){
  data <- cfb_pbp_data(year = 2018, week = i, epa_wpa = TRUE, drive = TRUE ) %>% mutate(week = i)
  df <- data.frame(data)
  drives_2018 <- bind_rows(drives_2018, df)
}

# Gather game data for home/away
games.2018 <- cfb_game_info(2018) %>% rename("game_id" = id)

#Joining games and drives
drives_week_2018 <- left_join(drives_2018, games.2018, by=c("game_id", "week")) %>% left_join(game.numbers.2018, by=c("game_id", "offense"="offense_play"))

#Tells us the number of drives and average starting
#field position for each team
drives.off.2018 <- drives_week_2018 %>%
  mutate(
    adj_start_yardline = ifelse(offense == away_team, 100-start_yardline, start_yardline), 
    success = ifelse(drive_result %in% c("TD", "FG"), 1, 0),
    drive.pts = ifelse(drive_result == "TD", 6, ifelse(drive_result == "FG", 3, 0))) %>%
  group_by(offense) %>% 
  summarise(
    fp = mean(adj_start_yardline[adj_start_yardline > 10 & adj_start_yardline <40]), 
    srate = mean(success),
    drives = n(),
    drives.pts = sum(drive.pts))

#Merge drive data for app
team.epa.2018 <- left_join(team.epa.2018, drives.off.2018, by=c("offense_play"="offense"))
team.epa.2018$pts.per.drive <- team.epa.2018$drives.pts / team.epa.2018$drives

######################
#2017 play by play
######################
pbp_2017 <- data.frame()
for(i in 1:15){
  data <- cfb_pbp_data(year = 2017, week = i, epa_wpa = TRUE) %>% mutate(week = i)
  df <- data.frame(data)
  pbp_2017 <- bind_rows(pbp_2017, df) %>% mutate(garbage = ifelse(period == 1 & abs(score_diff) > 43, 1, 
                                                                  ifelse(period == 2 & abs(score_diff) > 37, 1,
                                                                         ifelse(period == 3 & abs(score_diff) > 27, 1,
                                                                                ifelse(period == 4 & abs(score_diff) > 22, 1, 0)))))
}

#To clean up what we want to see that's useable
pbp_2017_select <- pbp_2017 %>% select(offense_play, defense_play, down, distance, play_type, yards_gained, week, EPA, success, rush, pass, game_id, home_wp)

#Creating game numbers
game.numbers.2017 <- pbp_2017 %>% group_by(offense_play, game_id) %>% mutate(num=1) %>% summarise(game.n = mean(num)) %>% ungroup() %>% group_by(offense_play) %>% mutate(game.number = cumsum(game.n)) %>% select(-game.n)

#Creating a plays variable
plays.2017 <- pbp_2017_select %>% filter(rush == 1 | pass == 1) %>% left_join(game.numbers.2017, by=c("game_id","offense_play"))

#Creating EPA variable
offense.2017 <- plays.2017 %>% group_by(offense_play) %>% summarise(epa.pass.off = mean(EPA[pass==1]), epa.rush.off = mean(EPA[rush==1]), num.plays.off = n(), off.success.rate = mean(success))
defense.2017 <- plays.2017 %>% group_by(defense_play) %>% summarise(epa.pass.def = mean(EPA[pass==1]), epa.rush.def = mean(EPA[rush==1]), num.plays.def = n(), def.success.rate = mean(success))
team.epa.2017 <- left_join(offense.2017, defense.2017, by = c("offense_play" = "defense_play")) 


###Computing Drives
drives_2017 <- data.frame()
for(i in 1:15){
  data <- cfb_pbp_data(year = 2017, week = i, epa_wpa = TRUE, drive = TRUE ) %>% mutate(week = i)
  df <- data.frame(data)
  drives_2017 <- bind_rows(drives_2017, df)
}

# Gather game data for home/away
games.2017 <- cfb_game_info(2017) %>% rename("game_id" = id)

#Joining games and drives
drives_week_2017 <- left_join(drives_2017, games.2017, by=c("game_id", "week")) %>% left_join(game.numbers.2017, by=c("game_id", "offense"="offense_play"))

#Tells us the number of drives and average starting
#field position for each team
drives.off.2017 <- drives_week_2017 %>%
  mutate(
    adj_start_yardline = ifelse(offense == away_team, 100-start_yardline, start_yardline), 
    success = ifelse(drive_result %in% c("TD", "FG"), 1, 0),
    drive.pts = ifelse(drive_result == "TD", 6, ifelse(drive_result == "FG", 3, 0))) %>%
  group_by(offense) %>% 
  summarise(
    fp = mean(adj_start_yardline[adj_start_yardline > 10 & adj_start_yardline <40]), 
    srate = mean(success),
    drives = n(),
    drives.pts = sum(drive.pts))

#Merge drive data for app
team.epa.2017 <- left_join(team.epa.2017, drives.off.2017, by=c("offense_play"="offense"))
team.epa.2017$pts.per.drive <- team.epa.2017$drives.pts / team.epa.2017$drives

######################
#2016 play by play
######################
pbp_2016 <- data.frame()
for(i in 1:15){
  data <- cfb_pbp_data(year = 2016, week = i, epa_wpa = TRUE) %>% mutate(week = i)
  df <- data.frame(data)
  pbp_2016 <- bind_rows(pbp_2016, df) %>% mutate(garbage = ifelse(period == 1 & abs(score_diff) > 43, 1, 
                                                                  ifelse(period == 2 & abs(score_diff) > 37, 1,
                                                                         ifelse(period == 3 & abs(score_diff) > 27, 1,
                                                                                ifelse(period == 4 & abs(score_diff) > 22, 1, 0)))))
}

#To clean up what we want to see that's useable
pbp_2016_select <- pbp_2016 %>% select(offense_play, defense_play, down, distance, play_type, yards_gained, week, EPA, success, rush, pass, game_id, home_wp)

#Creating game numbers
game.numbers.2016 <- pbp_2016 %>% group_by(offense_play, game_id) %>% mutate(num=1) %>% summarise(game.n = mean(num)) %>% ungroup() %>% group_by(offense_play) %>% mutate(game.number = cumsum(game.n)) %>% select(-game.n)

#Creating a plays variable
plays.2016 <- pbp_2016_select %>% filter(rush == 1 | pass == 1) %>% left_join(game.numbers.2016, by=c("game_id","offense_play"))

#Creating EPA variable
offense.2016 <- plays.2016 %>% group_by(offense_play) %>% summarise(epa.pass.off = mean(EPA[pass==1]), epa.rush.off = mean(EPA[rush==1]), num.plays.off = n(), off.success.rate = mean(success))
defense.2016 <- plays.2016 %>% group_by(defense_play) %>% summarise(epa.pass.def = mean(EPA[pass==1]), epa.rush.def = mean(EPA[rush==1]), num.plays.def = n(), def.success.rate = mean(success))
team.epa.2016 <- left_join(offense.2016, defense.2016, by = c("offense_play" = "defense_play")) 


###Computing Drives
drives_2016 <- data.frame()
for(i in 1:15){
  data <- cfb_pbp_data(year = 2016, week = i, epa_wpa = TRUE, drive = TRUE ) %>% mutate(week = i)
  df <- data.frame(data)
  drives_2016 <- bind_rows(drives_2016, df)
}

# Gather game data for home/away
games.2016 <- cfb_game_info(2016) %>% rename("game_id" = id)

#Joining games and drives
drives_week_2016 <- left_join(drives_2016, games.2016, by=c("game_id", "week")) %>% left_join(game.numbers.2016, by=c("game_id", "offense"="offense_play"))

#Tells us the number of drives and average starting
#field position for each team
drives.off.2016 <- drives_week_2016 %>%
  mutate(
    adj_start_yardline = ifelse(offense == away_team, 100-start_yardline, start_yardline), 
    success = ifelse(drive_result %in% c("TD", "FG"), 1, 0),
    drive.pts = ifelse(drive_result == "TD", 6, ifelse(drive_result == "FG", 3, 0))) %>%
  group_by(offense) %>% 
  summarise(
    fp = mean(adj_start_yardline[adj_start_yardline > 10 & adj_start_yardline <40]), 
    srate = mean(success),
    drives = n(),
    drives.pts = sum(drive.pts))

#Merge drive data for app
team.epa.2016 <- left_join(team.epa.2016, drives.off.2016, by=c("offense_play"="offense"))
team.epa.2016$pts.per.drive <- team.epa.2016$drives.pts / team.epa.2016$drives


######################
#2015 play by play
######################
pbp_2015 <- data.frame()
for(i in 1:15){
  data <- cfb_pbp_data(year = 2015, week = i, epa_wpa = TRUE) %>% mutate(week = i)
  df <- data.frame(data)
  pbp_2015 <- bind_rows(pbp_2015, df) %>% mutate(garbage = ifelse(period == 1 & abs(score_diff) > 43, 1, 
                                                                  ifelse(period == 2 & abs(score_diff) > 37, 1,
                                                                         ifelse(period == 3 & abs(score_diff) > 27, 1,
                                                                                ifelse(period == 4 & abs(score_diff) > 22, 1, 0)))))
}

#To clean up what we want to see that's useable
pbp_2015_select <- pbp_2015 %>% select(offense_play, defense_play, down, distance, play_type, yards_gained, week, EPA, success, rush, pass, game_id, home_wp)

#Creating game numbers
game.numbers.2015 <- pbp_2015 %>% group_by(offense_play, game_id) %>% mutate(num=1) %>% summarise(game.n = mean(num)) %>% ungroup() %>% group_by(offense_play) %>% mutate(game.number = cumsum(game.n)) %>% select(-game.n)

#Creating a plays variable
plays.2015 <- pbp_2015_select %>% filter(rush == 1 | pass == 1) %>% left_join(game.numbers.2015, by=c("game_id","offense_play"))

#Creating EPA variable
offense.2015 <- plays.2015 %>% group_by(offense_play) %>% summarise(epa.pass.off = mean(EPA[pass==1]), epa.rush.off = mean(EPA[rush==1]), num.plays.off = n(), off.success.rate = mean(success))
defense.2015 <- plays.2015 %>% group_by(defense_play) %>% summarise(epa.pass.def = mean(EPA[pass==1]), epa.rush.def = mean(EPA[rush==1]), num.plays.def = n(), def.success.rate = mean(success))
team.epa.2015 <- left_join(offense.2015, defense.2015, by = c("offense_play" = "defense_play")) 


###Computing Drives
drives_2015 <- data.frame()
for(i in 1:15){
  data <- cfb_pbp_data(year = 2015, week = i, epa_wpa = TRUE, drive = TRUE ) %>% mutate(week = i)
  df <- data.frame(data)
  drives_2015 <- bind_rows(drives_2015, df)
}

# Gather game data for home/away
games.2015 <- cfb_game_info(2015) %>% rename("game_id" = id)

#Joining games and drives
drives_week_2015 <- left_join(drives_2015, games.2015, by=c("game_id", "week")) %>% left_join(game.numbers.2015, by=c("game_id", "offense"="offense_play"))

#Tells us the number of drives and average starting
#field position for each team
drives.off.2015 <- drives_week_2015 %>%
  mutate(
    adj_start_yardline = ifelse(offense == away_team, 100-start_yardline, start_yardline), 
    success = ifelse(drive_result %in% c("TD", "FG"), 1, 0),
    drive.pts = ifelse(drive_result == "TD", 6, ifelse(drive_result == "FG", 3, 0))) %>%
  group_by(offense, offense_conference) %>% 
  summarise(
    fp = mean(adj_start_yardline[adj_start_yardline > 10 & adj_start_yardline <40]), 
    srate = mean(success),
    drives = n(),
    drives.pts = sum(drive.pts))

#Merge drive data for app
team.epa.2015 <- left_join(team.epa.2015, drives.off.2015, by=c("offense_play"="offense"))
team.epa.2015$pts.per.drive <- team.epa.2015$drives.pts / team.epa.2015$drives


###############################################
#Create Shiny app
###############################################
#Define the user interface
ui <- fluidPage(
  tags$head(tags$style(".checkbox-inline {margin: 0 !important;}")),
  titlePanel(h1("College Football Analytics")),
  sidebarPanel(
    radioButtons("conferencegame", label = h4(
      "Choose All or Only Conference Games"),
      choices = list("All" = "ALL", "Conference" = "CONF")
    ),
    fluidRow(column(12,checkboxGroupInput("selections", label = h4(
      "Choose Weeks"),
      choices = list("1" = 1, "2" = 2,
                     "3" = 3, "4" = 4,
                     "5" = 5, "6" = 6,
                     "7" = 7, "8" = 8,
                     "9" = 9, "10" = 10,
                     "11" = 11, "12" = 12,
                     "13" = 13, "14" = 14,
                     "15" = 15),
      selected = c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11",
                   "12", "13", "14", "15"), inline = TRUE))),
    checkboxGroupInput("downs", label = h4(
      "Choose Downs"),
      choices = list("1st" = 1,
                     "2nd" = 2,
                     "3rd" = 3,
                     "4th" = 4),
      selected = c("1", "2", "3", "4")
    ),
    radioButtons("garbagetime", label = h4(
      "Choose to Filter Garbage Time"),
      choices = list("All" = 1,
                     "Remove Garbage Time" = 0),
    ),
    actionButton("submit", "Update"),
    width = 4),
  mainPanel(
    selectInput("year", "Choose Year, then Click Update",
                c("2015", "2016", "2017", "2018", "2019"),
                selected = "2019"),
    tabsetPanel(type = "pills",
                tabPanel("All", DT::dataTableOutput("table")),
                tabPanel("Power 5", DT::dataTableOutput("power5")),
                tabPanel("Group of 5", DT::dataTableOutput("group5")),
                tabPanel("ACC", DT::dataTableOutput("acc")),
                tabPanel("American", DT::dataTableOutput("american")),
                tabPanel("Big 12", DT::dataTableOutput("big12")),
                tabPanel("Big Ten", DT::dataTableOutput("bigten")),
                tabPanel("Conference USA", DT::dataTableOutput("cusa")),
                tabPanel("Independents", DT::dataTableOutput("indy")),
                tabPanel("Mid-American", DT::dataTableOutput("midamerican")),
                tabPanel("Mountain West", DT::dataTableOutput("mtnwest")),
                tabPanel("Pac-12", DT::dataTableOutput("pac12")),
                tabPanel("SEC", DT::dataTableOutput("sec")),
                tabPanel("Sun Belt", DT::dataTableOutput("sunbelt"))
    ),
    theme = shinytheme("cerulean")
  )
)

#Define server logic
server <- function(input, output, session) {
  cfb.table2 <- reactive({
    input$submit
    isolate({
      if(input$year=="2015"){
    subset(pbp_2015, week %in% input$selections)
    } else if(input$year=="2016"){
    subset(pbp_2016, week %in% input$selections)  
    } else if(input$year=="2017"){
      subset(pbp_2017, week %in% input$selections)
    } else if(input$year=="2018"){
      subset(pbp_2018, week %in% input$selections)
    } else {
      subset(pbp_2019, week %in% input$selections)
    }
    })})
  
  game_numbers <- reactive({cfb.table2() %>% group_by(offense_play, game_id) %>% mutate(num=1) %>% summarise(game.n = mean(num)) %>% ungroup() %>% group_by(offense_play) %>% mutate(game.number = cumsum(game.n)) %>% select(-game.n)})

  plays <- reactive({cfb.table2() %>% filter(rush == 1 | pass == 1) %>% left_join(game_numbers(), by=c("game_id","offense_play"))})
  
  offense <- reactive({plays() %>% group_by(offense_play) %>% summarise(ypa = mean(yards_gained[pass==1]), ypr = mean(yards_gained[rush==1]), num.plays = n()) %>% filter(num.plays > 300)})
  offense <- reactive({plays() %>% group_by(offense_play) %>% summarise(epa.pass.off = mean(EPA[pass==1]), success.rate = mean(success), epa.rush.off = mean(EPA[rush==1]), num.plays = n()) %>% filter(num.plays > 300)})
  defense <- reactive({plays() %>% group_by(defense_play) %>% summarise(epa.pass.def = mean(EPA[pass==1]), epa.rush.def = mean(EPA[rush==1]), num.plays = n()) %>% filter(num.plays > 300)})
  update.epa <- reactive({left_join(offense(), defense(), by = c("offense_play" = "defense_play"))})
  plays.garbage <- reactive({plays() %>% mutate(drive_id=as.character(drive_id)) %>% group_by(game_id, drive_id) %>% summarise(garbage = max(garbage))})
  
  drives.table2 <- reactive({
    input$submit
    isolate({
      if(input$year=="2015"){
        subset(drives_2015, week %in% input$selections)
      } else if(input$year=="2016"){
        subset(drives_2016, week %in% input$selections)    
      } else if(input$year=="2017"){
        subset(drives_2017, week %in% input$selections)
      } else if(input$year=="2018"){
        subset(drives_2018, week %in% input$selections)
      } else{
        subset(drives_2019, week %in% input$selections)
      }
        })
  })  
  
  games <- reactive({
    input$submit
    isolate({
      if(input$year=="2015" & input$conferencegame=="CONF"){
        mutate(cfb_game_info(2015) %>% rename("game_id" = id) %>% filter(conference_game=="TRUE"))
      } else if(input$year=="2015" & input$conferencegame=="ALL"){ 
        mutate(cfb_game_info(2015) %>% rename("game_id" = id))
      } else if(input$year=="2016" & input$conferencegame=="CONF"){
        mutate(cfb_game_info(2016) %>% rename("game_id" = id) %>% filter(conference_game=="TRUE"))
      } else if(input$year=="2016" & input$conferencegame=="ALL"){
        mutate(cfb_game_info(2016) %>% rename("game_id" = id))
      } else if(input$year=="2017" & input$conferencegame=="CONF"){
        mutate(cfb_game_info(2017) %>% rename("game_id" = id) %>% filter(conference_game=="TRUE"))
      } else if(input$year=="2017" & input$conferencegame=="ALL"){
        mutate(cfb_game_info(2017) %>% rename("game_id" = id))
      } else if(input$year=="2018" & input$conferencegame=="CONF"){
        mutate(cfb_game_info(2018) %>% rename("game_id" = id) %>% filter(conference_game=="TRUE"))
      } else if(input$year=="2018" & input$conferencegame=="ALL"){
        mutate(cfb_game_info(2018) %>% rename("game_id" = id))
      } else if(input$year=="2019" & input$conferencegame=="CONF"){
        mutate(cfb_game_info(2019) %>% rename("game_id" = id) %>% filter(conference_game=="TRUE"))
      } else {mutate(cfb_game_info(2019) %>% rename("game_id" = id))
      }
    })
  })
  
  drives.off.tmp <- reactive({
    input$submit
    isolate({
      if (input$garbagetime==0) {
        drives.table2() %>% left_join(games(), by = c("game_id")) %>% 
          left_join(plays.garbage(), by = c("game_id", "id"="drive_id")) %>% filter(garbage==0)
      } else {
        drives.table2() %>% left_join(games(), by = c("game_id"))
      }
    })
  })
    
drives.off <- reactive({drives.off.tmp() %>%
      mutate(
        adj_start_yardline = ifelse(offense == away_team, 100-start_yardline, start_yardline), 
        success = ifelse(drive_result %in% c("TD", "FG"), 1, 0),
        drive.pts = ifelse(drive_result == "TD", 6, ifelse(drive_result == "FG", 3, 0))) %>%
      group_by(offense, offense_conference) %>% 
      summarise(
        fp = mean(adj_start_yardline[adj_start_yardline > 10 & adj_start_yardline <40]), 
        srate = mean(success),
        drives = n(),
        drives.pts = sum(drive.pts))
  })
  
  drive.update.epa <- reactive({left_join(update.epa(), drives.off(), by=c("offense_play"="offense")) %>%
      mutate(pts.per.drive = drives.pts / drives)})
  cfb.table3 <- reactive({data.frame(drive.update.epa() %>% 
                                       select(offense_play, success.rate, epa.pass.off, epa.rush.off, epa.pass.def, epa.rush.def, fp, drives, pts.per.drive))})
  output$table = DT::renderDataTable({
    datatable(cfb.table3(),
              rownames = FALSE, 
              class = 'cell-border stripe',
              colnames = c('Team', 'Success Rate',
                           'Pass EPA', 'Run EPA',
                           'Pass EPA Def.',
                           'Run EPA Def.',
                           'Avg. Field Position',
                           'Drives',
                           'Points Per Drive'),
              list(pageLength = 25)) %>%
      formatPercentage(c('success.rate'),1) %>%
      formatRound(c('epa.pass.off'),3) %>%
      formatRound(c('epa.rush.off'),3) %>%
      formatRound(c('epa.pass.def'),3) %>%
      formatRound(c('epa.rush.def'),3) %>%
      formatRound(c('fp'),1) %>%
      formatRound(c('pts.per.drive'),3) %>%
      formatStyle(c('success.rate'),
                background = styleColorBar(range(cfb.table3()$success.rate),'lightblue'),
                backgroundSize = '95% 50%',
                backgroundRepeat = 'no-repeat',
                backgroundPosition = 'right') %>%
      formatStyle(c('epa.pass.off'),
                  background = styleColorBar(range(cfb.table3()$epa.pass.off),'lightblue'),
                  backgroundSize = '95% 50%',
                  backgroundRepeat = 'no-repeat',
                  backgroundPosition = 'right') %>%
      formatStyle(c('epa.rush.off'),
                  background = styleColorBar(range(cfb.table3()$epa.rush.off),'lightblue'),
                  backgroundSize = '95% 50%',
                  backgroundRepeat = 'no-repeat',
                  backgroundPosition = 'right') %>%
      formatStyle(c('epa.pass.def'),
                  background = styleColorBar(range(cfb.table3()$epa.pass.def),'lightblue'),
                  backgroundSize = '95% 50%',
                  backgroundRepeat = 'no-repeat',
                  backgroundPosition = 'right') %>%
      formatStyle(c('epa.rush.def'),
                  background = styleColorBar(range(cfb.table3()$epa.rush.def),'lightblue'),
                  backgroundSize = '95% 50%',
                  backgroundRepeat = 'no-repeat',
                  backgroundPosition = 'right') %>%
      formatStyle(c('fp'),
                  background = styleColorBar(range(cfb.table3()$fp),'palegoldenrod'),
                  backgroundSize = '95% 50%',
                  backgroundRepeat = 'no-repeat',
                  backgroundPosition = 'right') %>%
      formatStyle(c('drives'),
                  background = styleColorBar(range(cfb.table3()$drives),'palegoldenrod'),
                  backgroundSize = '95% 50%',
                  backgroundRepeat = 'no-repeat',
                  backgroundPosition = 'right') %>%
      formatStyle(c('pts.per.drive'),
                  background = styleColorBar(range(cfb.table3()$pts.per.drive),'palegoldenrod'),
                  backgroundSize = '95% 50%',
                  backgroundRepeat = 'no-repeat',
                  backgroundPosition = 'right')
  })
  
  cfb.table4 <- reactive({
    data.frame(drive.update.epa() %>% filter(offense_conference %in% c("ACC", "Big 12", "Big Ten", "Pac-12", "SEC")) %>%
                 select(offense_play, success.rate, epa.pass.off, epa.rush.off, epa.pass.def, epa.rush.def, fp, drives, pts.per.drive))
  })
  output$power5 = DT::renderDataTable({
    datatable(cfb.table4(),
              rownames = FALSE, 
              class = 'cell-border stripe',
              colnames = c('Team', 'Success Rate',
                           'Pass EPA', 'Run EPA',
                           'Pass EPA Def.',
                           'Run EPA Def.',
                           'Avg. Field Position',
                           'Drives',
                           'Points Per Drive'),
              list(pageLength = 25)) %>%
      formatPercentage(c('success.rate'),1) %>%
      formatRound(c('epa.pass.off'),3) %>%
      formatRound(c('epa.rush.off'),3) %>%
      formatRound(c('epa.pass.def'),3) %>%
      formatRound(c('epa.rush.def'),3) %>%
      formatRound(c('fp'),1) %>%
      formatRound(c('pts.per.drive'),3) %>%
      formatStyle(c('success.rate'),
                  background = styleColorBar(range(cfb.table3()$success.rate),'lightblue'),
                  backgroundSize = '95% 50%',
                  backgroundRepeat = 'no-repeat',
                  backgroundPosition = 'right') %>%
      formatStyle(c('epa.pass.off'),
                  background = styleColorBar(range(cfb.table3()$epa.pass.off),'lightblue'),
                  backgroundSize = '95% 50%',
                  backgroundRepeat = 'no-repeat',
                  backgroundPosition = 'right') %>%
      formatStyle(c('epa.rush.off'),
                  background = styleColorBar(range(cfb.table3()$epa.rush.off),'lightblue'),
                  backgroundSize = '95% 50%',
                  backgroundRepeat = 'no-repeat',
                  backgroundPosition = 'right') %>%
      formatStyle(c('epa.pass.def'),
                  background = styleColorBar(range(cfb.table3()$epa.pass.def),'lightblue'),
                  backgroundSize = '95% 50%',
                  backgroundRepeat = 'no-repeat',
                  backgroundPosition = 'right') %>%
      formatStyle(c('epa.rush.def'),
                  background = styleColorBar(range(cfb.table3()$epa.rush.def),'lightblue'),
                  backgroundSize = '95% 50%',
                  backgroundRepeat = 'no-repeat',
                  backgroundPosition = 'right')  %>%
      formatStyle(c('fp'),
                  background = styleColorBar(range(cfb.table3()$fp),'palegoldenrod'),
                  backgroundSize = '95% 50%',
                  backgroundRepeat = 'no-repeat',
                  backgroundPosition = 'right') %>%
      formatStyle(c('drives'),
                  background = styleColorBar(range(cfb.table3()$drives),'palegoldenrod'),
                  backgroundSize = '95% 50%',
                  backgroundRepeat = 'no-repeat',
                  backgroundPosition = 'right') %>%
      formatStyle(c('pts.per.drive'),
                  background = styleColorBar(range(cfb.table3()$pts.per.drive),'palegoldenrod'),
                  backgroundSize = '95% 50%',
                  backgroundRepeat = 'no-repeat',
                  backgroundPosition = 'right')
  })
  
  cfb.table5 <- reactive({
    data.frame(drive.update.epa() %>% filter(offense_conference %in% c("American Athletic", "Conference USA", "Mid-American", "Mountain West", "Sun Belt")) %>%
                 select(offense_play, success.rate, epa.pass.off, epa.rush.off, epa.pass.def, epa.rush.def, fp, drives, pts.per.drive))
  })
  output$group5 = DT::renderDataTable({
    datatable(cfb.table5(),
              rownames = FALSE, 
              class = 'cell-border stripe',
              colnames = c('Team', 'Success Rate',
                           'Pass EPA', 'Run EPA',
                           'Pass EPA Def.',
                           'Run EPA Def.',
                           'Avg. Field Position',
                           'Drives',
                           'Points Per Drive'),
              list(pageLength = 25)) %>%
      formatPercentage(c('success.rate'),1) %>%
      formatRound(c('epa.pass.off'),3) %>%
      formatRound(c('epa.rush.off'),3) %>%
      formatRound(c('epa.pass.def'),3) %>%
      formatRound(c('epa.rush.def'),3) %>%
      formatRound(c('fp'),1) %>%
      formatRound(c('pts.per.drive'),3) %>%
      formatStyle(c('success.rate'),
                  background = styleColorBar(range(cfb.table3()$success.rate),'lightblue'),
                  backgroundSize = '95% 50%',
                  backgroundRepeat = 'no-repeat',
                  backgroundPosition = 'right') %>%
      formatStyle(c('epa.pass.off'),
                  background = styleColorBar(range(cfb.table3()$epa.pass.off),'lightblue'),
                  backgroundSize = '95% 50%',
                  backgroundRepeat = 'no-repeat',
                  backgroundPosition = 'right') %>%
      formatStyle(c('epa.rush.off'),
                  background = styleColorBar(range(cfb.table3()$epa.rush.off),'lightblue'),
                  backgroundSize = '95% 50%',
                  backgroundRepeat = 'no-repeat',
                  backgroundPosition = 'right') %>%
      formatStyle(c('epa.pass.def'),
                  background = styleColorBar(range(cfb.table3()$epa.pass.def),'lightblue'),
                  backgroundSize = '95% 50%',
                  backgroundRepeat = 'no-repeat',
                  backgroundPosition = 'right') %>%
      formatStyle(c('epa.rush.def'),
                  background = styleColorBar(range(cfb.table3()$epa.rush.def),'lightblue'),
                  backgroundSize = '95% 50%',
                  backgroundRepeat = 'no-repeat',
                  backgroundPosition = 'right')  %>%
      formatStyle(c('fp'),
                  background = styleColorBar(range(cfb.table3()$fp),'palegoldenrod'),
                  backgroundSize = '95% 50%',
                  backgroundRepeat = 'no-repeat',
                  backgroundPosition = 'right') %>%
      formatStyle(c('drives'),
                  background = styleColorBar(range(cfb.table3()$drives),'palegoldenrod'),
                  backgroundSize = '95% 50%',
                  backgroundRepeat = 'no-repeat',
                  backgroundPosition = 'right') %>%
      formatStyle(c('pts.per.drive'),
                  background = styleColorBar(range(cfb.table3()$pts.per.drive),'palegoldenrod'),
                  backgroundSize = '95% 50%',
                  backgroundRepeat = 'no-repeat',
                  backgroundPosition = 'right')
  })
  cfb.table6 <- reactive({
    data.frame(drive.update.epa() %>% filter(offense_conference=="ACC") %>%
                           select(offense_play, success.rate, epa.pass.off, epa.rush.off, epa.pass.def, epa.rush.def, fp, drives, pts.per.drive))
  })
  output$acc = DT::renderDataTable({
    datatable(cfb.table6(),
              rownames = FALSE, 
              class = 'cell-border stripe',
              colnames = c('Team', 'Success Rate',
                           'Pass EPA', 'Run EPA',
                           'Pass EPA Def.',
                           'Run EPA Def.',
                           'Avg. Field Position',
                           'Drives',
                           'Points Per Drive'),
              list(pageLength = 25)) %>%
      formatPercentage(c('success.rate'),1) %>%
      formatRound(c('epa.pass.off'),3) %>%
      formatRound(c('epa.rush.off'),3) %>%
      formatRound(c('epa.pass.def'),3) %>%
      formatRound(c('epa.rush.def'),3) %>%
      formatRound(c('fp'),1) %>%
      formatRound(c('pts.per.drive'),3) %>%
      formatStyle(c('success.rate'),
                  background = styleColorBar(range(cfb.table3()$success.rate),'lightblue'),
                  backgroundSize = '95% 50%',
                  backgroundRepeat = 'no-repeat',
                  backgroundPosition = 'right') %>%
      formatStyle(c('epa.pass.off'),
                  background = styleColorBar(range(cfb.table3()$epa.pass.off),'lightblue'),
                  backgroundSize = '95% 50%',
                  backgroundRepeat = 'no-repeat',
                  backgroundPosition = 'right') %>%
      formatStyle(c('epa.rush.off'),
                  background = styleColorBar(range(cfb.table3()$epa.rush.off),'lightblue'),
                  backgroundSize = '95% 50%',
                  backgroundRepeat = 'no-repeat',
                  backgroundPosition = 'right') %>%
      formatStyle(c('epa.pass.def'),
                  background = styleColorBar(range(cfb.table3()$epa.pass.def),'lightblue'),
                  backgroundSize = '95% 50%',
                  backgroundRepeat = 'no-repeat',
                  backgroundPosition = 'right') %>%
      formatStyle(c('epa.rush.def'),
                  background = styleColorBar(range(cfb.table3()$epa.rush.def),'lightblue'),
                  backgroundSize = '95% 50%',
                  backgroundRepeat = 'no-repeat',
                  backgroundPosition = 'right')  %>%
      formatStyle(c('fp'),
                  background = styleColorBar(range(cfb.table3()$fp),'palegoldenrod'),
                  backgroundSize = '95% 50%',
                  backgroundRepeat = 'no-repeat',
                  backgroundPosition = 'right') %>%
      formatStyle(c('drives'),
                  background = styleColorBar(range(cfb.table3()$drives),'palegoldenrod'),
                  backgroundSize = '95% 50%',
                  backgroundRepeat = 'no-repeat',
                  backgroundPosition = 'right') %>%
      formatStyle(c('pts.per.drive'),
                  background = styleColorBar(range(cfb.table3()$pts.per.drive),'palegoldenrod'),
                  backgroundSize = '95% 50%',
                  backgroundRepeat = 'no-repeat',
                  backgroundPosition = 'right')
  })
  
  cfb.table7 <- reactive({
    data.frame(drive.update.epa() %>% filter(offense_conference=="American Athletic") %>%
                 select(offense_play, success.rate, epa.pass.off, epa.rush.off, epa.pass.def, epa.rush.def, fp, drives, pts.per.drive))
  })
  output$american = DT::renderDataTable({
    datatable(cfb.table7(),
              rownames = FALSE, 
              class = 'cell-border stripe',
              colnames = c('Team', 'Success Rate',
                           'Pass EPA', 'Run EPA',
                           'Pass EPA Def.',
                           'Run EPA Def.',
                           'Avg. Field Position',
                           'Drives',
                           'Points Per Drive'),
              list(pageLength = 25)) %>%
      formatPercentage(c('success.rate'),1) %>%
      formatRound(c('epa.pass.off'),3) %>%
      formatRound(c('epa.rush.off'),3) %>%
      formatRound(c('epa.pass.def'),3) %>%
      formatRound(c('epa.rush.def'),3) %>%
      formatRound(c('fp'),1) %>%
      formatRound(c('pts.per.drive'),3) %>%
      formatStyle(c('success.rate'),
                  background = styleColorBar(range(cfb.table3()$success.rate),'lightblue'),
                  backgroundSize = '95% 50%',
                  backgroundRepeat = 'no-repeat',
                  backgroundPosition = 'right') %>%
      formatStyle(c('epa.pass.off'),
                  background = styleColorBar(range(cfb.table3()$epa.pass.off),'lightblue'),
                  backgroundSize = '95% 50%',
                  backgroundRepeat = 'no-repeat',
                  backgroundPosition = 'right') %>%
      formatStyle(c('epa.rush.off'),
                  background = styleColorBar(range(cfb.table3()$epa.rush.off),'lightblue'),
                  backgroundSize = '95% 50%',
                  backgroundRepeat = 'no-repeat',
                  backgroundPosition = 'right') %>%
      formatStyle(c('epa.pass.def'),
                  background = styleColorBar(range(cfb.table3()$epa.pass.def),'lightblue'),
                  backgroundSize = '95% 50%',
                  backgroundRepeat = 'no-repeat',
                  backgroundPosition = 'right') %>%
      formatStyle(c('epa.rush.def'),
                  background = styleColorBar(range(cfb.table3()$epa.rush.def),'lightblue'),
                  backgroundSize = '95% 50%',
                  backgroundRepeat = 'no-repeat',
                  backgroundPosition = 'right')  %>%
      formatStyle(c('fp'),
                  background = styleColorBar(range(cfb.table3()$fp),'palegoldenrod'),
                  backgroundSize = '95% 50%',
                  backgroundRepeat = 'no-repeat',
                  backgroundPosition = 'right') %>%
      formatStyle(c('drives'),
                  background = styleColorBar(range(cfb.table3()$drives),'palegoldenrod'),
                  backgroundSize = '95% 50%',
                  backgroundRepeat = 'no-repeat',
                  backgroundPosition = 'right') %>%
      formatStyle(c('pts.per.drive'),
                  background = styleColorBar(range(cfb.table3()$pts.per.drive),'palegoldenrod'),
                  backgroundSize = '95% 50%',
                  backgroundRepeat = 'no-repeat',
                  backgroundPosition = 'right')
  })
  
  cfb.table8 <- reactive({
    data.frame(drive.update.epa() %>% filter(offense_conference=="Big 12") %>%
                 select(offense_play, success.rate, epa.pass.off, epa.rush.off, epa.pass.def, epa.rush.def, fp, drives, pts.per.drive))
  })
  output$big12 = DT::renderDataTable({
    datatable(cfb.table8(),
              rownames = FALSE, 
              class = 'cell-border stripe',
              colnames = c('Team', 'Success Rate',
                           'Pass EPA', 'Run EPA',
                           'Pass EPA Def.',
                           'Run EPA Def.',
                           'Avg. Field Position',
                           'Drives',
                           'Points Per Drive'),
              list(pageLength = 25)) %>%
      formatPercentage(c('success.rate'),1) %>%
      formatRound(c('epa.pass.off'),3) %>%
      formatRound(c('epa.rush.off'),3) %>%
      formatRound(c('epa.pass.def'),3) %>%
      formatRound(c('epa.rush.def'),3) %>%
      formatRound(c('fp'),1) %>%
      formatRound(c('pts.per.drive'),3) %>%
      formatStyle(c('success.rate'),
                  background = styleColorBar(range(cfb.table3()$success.rate),'lightblue'),
                  backgroundSize = '95% 50%',
                  backgroundRepeat = 'no-repeat',
                  backgroundPosition = 'right') %>%
      formatStyle(c('epa.pass.off'),
                  background = styleColorBar(range(cfb.table3()$epa.pass.off),'lightblue'),
                  backgroundSize = '95% 50%',
                  backgroundRepeat = 'no-repeat',
                  backgroundPosition = 'right') %>%
      formatStyle(c('epa.rush.off'),
                  background = styleColorBar(range(cfb.table3()$epa.rush.off),'lightblue'),
                  backgroundSize = '95% 50%',
                  backgroundRepeat = 'no-repeat',
                  backgroundPosition = 'right') %>%
      formatStyle(c('epa.pass.def'),
                  background = styleColorBar(range(cfb.table3()$epa.pass.def),'lightblue'),
                  backgroundSize = '95% 50%',
                  backgroundRepeat = 'no-repeat',
                  backgroundPosition = 'right') %>%
      formatStyle(c('epa.rush.def'),
                  background = styleColorBar(range(cfb.table3()$epa.rush.def),'lightblue'),
                  backgroundSize = '95% 50%',
                  backgroundRepeat = 'no-repeat',
                  backgroundPosition = 'right')  %>%
      formatStyle(c('fp'),
                  background = styleColorBar(range(cfb.table3()$fp),'palegoldenrod'),
                  backgroundSize = '95% 50%',
                  backgroundRepeat = 'no-repeat',
                  backgroundPosition = 'right') %>%
      formatStyle(c('drives'),
                  background = styleColorBar(range(cfb.table3()$drives),'palegoldenrod'),
                  backgroundSize = '95% 50%',
                  backgroundRepeat = 'no-repeat',
                  backgroundPosition = 'right') %>%
      formatStyle(c('pts.per.drive'),
                  background = styleColorBar(range(cfb.table3()$pts.per.drive),'palegoldenrod'),
                  backgroundSize = '95% 50%',
                  backgroundRepeat = 'no-repeat',
                  backgroundPosition = 'right')
  })
  
  cfb.table9 <- reactive({
    data.frame(drive.update.epa() %>% filter(offense_conference=="Big Ten") %>%
                 select(offense_play, success.rate, epa.pass.off, epa.rush.off, epa.pass.def, epa.rush.def, fp, drives, pts.per.drive))
  })
  output$bigten = DT::renderDataTable({
    datatable(cfb.table9(),
              rownames = FALSE, 
              class = 'cell-border stripe',
              colnames = c('Team', 'Success Rate',
                           'Pass EPA', 'Run EPA',
                           'Pass EPA Def.',
                           'Run EPA Def.',
                           'Avg. Field Position',
                           'Drives',
                           'Points Per Drive'),
              list(pageLength = 25)) %>%
      formatPercentage(c('success.rate'),1) %>%
      formatRound(c('epa.pass.off'),3) %>%
      formatRound(c('epa.rush.off'),3) %>%
      formatRound(c('epa.pass.def'),3) %>%
      formatRound(c('epa.rush.def'),3) %>%
      formatRound(c('fp'),1) %>%
      formatRound(c('pts.per.drive'),3) %>%
      formatStyle(c('success.rate'),
                  background = styleColorBar(range(cfb.table3()$success.rate),'lightblue'),
                  backgroundSize = '95% 50%',
                  backgroundRepeat = 'no-repeat',
                  backgroundPosition = 'right') %>%
      formatStyle(c('epa.pass.off'),
                  background = styleColorBar(range(cfb.table3()$epa.pass.off),'lightblue'),
                  backgroundSize = '95% 50%',
                  backgroundRepeat = 'no-repeat',
                  backgroundPosition = 'right') %>%
      formatStyle(c('epa.rush.off'),
                  background = styleColorBar(range(cfb.table3()$epa.rush.off),'lightblue'),
                  backgroundSize = '95% 50%',
                  backgroundRepeat = 'no-repeat',
                  backgroundPosition = 'right') %>%
      formatStyle(c('epa.pass.def'),
                  background = styleColorBar(range(cfb.table3()$epa.pass.def),'lightblue'),
                  backgroundSize = '95% 50%',
                  backgroundRepeat = 'no-repeat',
                  backgroundPosition = 'right') %>%
      formatStyle(c('epa.rush.def'),
                  background = styleColorBar(range(cfb.table3()$epa.rush.def),'lightblue'),
                  backgroundSize = '95% 50%',
                  backgroundRepeat = 'no-repeat',
                  backgroundPosition = 'right')  %>%
      formatStyle(c('fp'),
                  background = styleColorBar(range(cfb.table3()$fp),'palegoldenrod'),
                  backgroundSize = '95% 50%',
                  backgroundRepeat = 'no-repeat',
                  backgroundPosition = 'right') %>%
      formatStyle(c('drives'),
                  background = styleColorBar(range(cfb.table3()$drives),'palegoldenrod'),
                  backgroundSize = '95% 50%',
                  backgroundRepeat = 'no-repeat',
                  backgroundPosition = 'right') %>%
      formatStyle(c('pts.per.drive'),
                  background = styleColorBar(range(cfb.table3()$pts.per.drive),'palegoldenrod'),
                  backgroundSize = '95% 50%',
                  backgroundRepeat = 'no-repeat',
                  backgroundPosition = 'right')
  })
  
  cfb.table10 <- reactive({
    data.frame(drive.update.epa() %>% filter(offense_conference=="Conference USA") %>%
                 select(offense_play, success.rate, epa.pass.off, epa.rush.off, epa.pass.def, epa.rush.def, fp, drives, pts.per.drive))
  })
  output$cusa = DT::renderDataTable({
    datatable(cfb.table10(),
              rownames = FALSE, 
              class = 'cell-border stripe',
              colnames = c('Team', 'Success Rate',
                           'Pass EPA', 'Run EPA',
                           'Pass EPA Def.',
                           'Run EPA Def.',
                           'Avg. Field Position',
                           'Drives',
                           'Points Per Drive'),
              list(pageLength = 25)) %>%
      formatPercentage(c('success.rate'),1) %>%
      formatRound(c('epa.pass.off'),3) %>%
      formatRound(c('epa.rush.off'),3) %>%
      formatRound(c('epa.pass.def'),3) %>%
      formatRound(c('epa.rush.def'),3) %>%
      formatRound(c('fp'),1) %>%
      formatRound(c('pts.per.drive'),3) %>%
      formatStyle(c('success.rate'),
                  background = styleColorBar(range(cfb.table3()$success.rate),'lightblue'),
                  backgroundSize = '95% 50%',
                  backgroundRepeat = 'no-repeat',
                  backgroundPosition = 'right') %>%
      formatStyle(c('epa.pass.off'),
                  background = styleColorBar(range(cfb.table3()$epa.pass.off),'lightblue'),
                  backgroundSize = '95% 50%',
                  backgroundRepeat = 'no-repeat',
                  backgroundPosition = 'right') %>%
      formatStyle(c('epa.rush.off'),
                  background = styleColorBar(range(cfb.table3()$epa.rush.off),'lightblue'),
                  backgroundSize = '95% 50%',
                  backgroundRepeat = 'no-repeat',
                  backgroundPosition = 'right') %>%
      formatStyle(c('epa.pass.def'),
                  background = styleColorBar(range(cfb.table3()$epa.pass.def),'lightblue'),
                  backgroundSize = '95% 50%',
                  backgroundRepeat = 'no-repeat',
                  backgroundPosition = 'right') %>%
      formatStyle(c('epa.rush.def'),
                  background = styleColorBar(range(cfb.table3()$epa.rush.def),'lightblue'),
                  backgroundSize = '95% 50%',
                  backgroundRepeat = 'no-repeat',
                  backgroundPosition = 'right')  %>%
      formatStyle(c('fp'),
                  background = styleColorBar(range(cfb.table3()$fp),'palegoldenrod'),
                  backgroundSize = '95% 50%',
                  backgroundRepeat = 'no-repeat',
                  backgroundPosition = 'right') %>%
      formatStyle(c('drives'),
                  background = styleColorBar(range(cfb.table3()$drives),'palegoldenrod'),
                  backgroundSize = '95% 50%',
                  backgroundRepeat = 'no-repeat',
                  backgroundPosition = 'right') %>%
      formatStyle(c('pts.per.drive'),
                  background = styleColorBar(range(cfb.table3()$pts.per.drive),'palegoldenrod'),
                  backgroundSize = '95% 50%',
                  backgroundRepeat = 'no-repeat',
                  backgroundPosition = 'right')
  })
  
  cfb.table11 <- reactive({
    data.frame(drive.update.epa() %>% filter(offense_conference=="FBS Independents") %>%
                 select(offense_play, success.rate, epa.pass.off, epa.rush.off, epa.pass.def, epa.rush.def, fp, drives, pts.per.drive))
  })
  output$indy = DT::renderDataTable({
    datatable(cfb.table11(),
              rownames = FALSE, 
              class = 'cell-border stripe',
              colnames = c('Team', 'Success Rate',
                           'Pass EPA', 'Run EPA',
                           'Pass EPA Def.',
                           'Run EPA Def.',
                           'Avg. Field Position',
                           'Drives',
                           'Points Per Drive'),
              list(pageLength = 25)) %>%
      formatPercentage(c('success.rate'),1) %>%
      formatRound(c('epa.pass.off'),3) %>%
      formatRound(c('epa.rush.off'),3) %>%
      formatRound(c('epa.pass.def'),3) %>%
      formatRound(c('epa.rush.def'),3) %>%
      formatRound(c('fp'),1) %>%
      formatRound(c('pts.per.drive'),3) %>%
      formatStyle(c('success.rate'),
                  background = styleColorBar(range(cfb.table3()$success.rate),'lightblue'),
                  backgroundSize = '95% 50%',
                  backgroundRepeat = 'no-repeat',
                  backgroundPosition = 'right') %>%
      formatStyle(c('epa.pass.off'),
                  background = styleColorBar(range(cfb.table3()$epa.pass.off),'lightblue'),
                  backgroundSize = '95% 50%',
                  backgroundRepeat = 'no-repeat',
                  backgroundPosition = 'right') %>%
      formatStyle(c('epa.rush.off'),
                  background = styleColorBar(range(cfb.table3()$epa.rush.off),'lightblue'),
                  backgroundSize = '95% 50%',
                  backgroundRepeat = 'no-repeat',
                  backgroundPosition = 'right') %>%
      formatStyle(c('epa.pass.def'),
                  background = styleColorBar(range(cfb.table3()$epa.pass.def),'lightblue'),
                  backgroundSize = '95% 50%',
                  backgroundRepeat = 'no-repeat',
                  backgroundPosition = 'right') %>%
      formatStyle(c('epa.rush.def'),
                  background = styleColorBar(range(cfb.table3()$epa.rush.def),'lightblue'),
                  backgroundSize = '95% 50%',
                  backgroundRepeat = 'no-repeat',
                  backgroundPosition = 'right')  %>%
      formatStyle(c('fp'),
                  background = styleColorBar(range(cfb.table3()$fp),'palegoldenrod'),
                  backgroundSize = '95% 50%',
                  backgroundRepeat = 'no-repeat',
                  backgroundPosition = 'right') %>%
      formatStyle(c('drives'),
                  background = styleColorBar(range(cfb.table3()$drives),'palegoldenrod'),
                  backgroundSize = '95% 50%',
                  backgroundRepeat = 'no-repeat',
                  backgroundPosition = 'right') %>%
      formatStyle(c('pts.per.drive'),
                  background = styleColorBar(range(cfb.table3()$pts.per.drive),'palegoldenrod'),
                  backgroundSize = '95% 50%',
                  backgroundRepeat = 'no-repeat',
                  backgroundPosition = 'right')
  })
  
  cfb.table12 <- reactive({
    data.frame(drive.update.epa() %>% filter(offense_conference=="Mid-American") %>%
                 select(offense_play, success.rate, epa.pass.off, epa.rush.off, epa.pass.def, epa.rush.def, fp, drives, pts.per.drive))
  })
  output$midamerican = DT::renderDataTable({
    datatable(cfb.table12(),
              rownames = FALSE, 
              class = 'cell-border stripe',
              colnames = c('Team', 'Success Rate',
                           'Pass EPA', 'Run EPA',
                           'Pass EPA Def.',
                           'Run EPA Def.',
                           'Avg. Field Position',
                           'Drives',
                           'Points Per Drive'),
              list(pageLength = 25)) %>%
      formatPercentage(c('success.rate'),1) %>%
      formatRound(c('epa.pass.off'),3) %>%
      formatRound(c('epa.rush.off'),3) %>%
      formatRound(c('epa.pass.def'),3) %>%
      formatRound(c('epa.rush.def'),3) %>%
      formatRound(c('fp'),1) %>%
      formatRound(c('pts.per.drive'),3) %>%
      formatStyle(c('success.rate'),
                  background = styleColorBar(range(cfb.table3()$success.rate),'lightblue'),
                  backgroundSize = '95% 50%',
                  backgroundRepeat = 'no-repeat',
                  backgroundPosition = 'right') %>%
      formatStyle(c('epa.pass.off'),
                  background = styleColorBar(range(cfb.table3()$epa.pass.off),'lightblue'),
                  backgroundSize = '95% 50%',
                  backgroundRepeat = 'no-repeat',
                  backgroundPosition = 'right') %>%
      formatStyle(c('epa.rush.off'),
                  background = styleColorBar(range(cfb.table3()$epa.rush.off),'lightblue'),
                  backgroundSize = '95% 50%',
                  backgroundRepeat = 'no-repeat',
                  backgroundPosition = 'right') %>%
      formatStyle(c('epa.pass.def'),
                  background = styleColorBar(range(cfb.table3()$epa.pass.def),'lightblue'),
                  backgroundSize = '95% 50%',
                  backgroundRepeat = 'no-repeat',
                  backgroundPosition = 'right') %>%
      formatStyle(c('epa.rush.def'),
                  background = styleColorBar(range(cfb.table3()$epa.rush.def),'lightblue'),
                  backgroundSize = '95% 50%',
                  backgroundRepeat = 'no-repeat',
                  backgroundPosition = 'right')  %>%
      formatStyle(c('fp'),
                  background = styleColorBar(range(cfb.table3()$fp),'palegoldenrod'),
                  backgroundSize = '95% 50%',
                  backgroundRepeat = 'no-repeat',
                  backgroundPosition = 'right') %>%
      formatStyle(c('drives'),
                  background = styleColorBar(range(cfb.table3()$drives),'palegoldenrod'),
                  backgroundSize = '95% 50%',
                  backgroundRepeat = 'no-repeat',
                  backgroundPosition = 'right') %>%
      formatStyle(c('pts.per.drive'),
                  background = styleColorBar(range(cfb.table3()$pts.per.drive),'palegoldenrod'),
                  backgroundSize = '95% 50%',
                  backgroundRepeat = 'no-repeat',
                  backgroundPosition = 'right')
  })
  
  cfb.table13 <- reactive({
    data.frame(drive.update.epa() %>% filter(offense_conference=="Mountain West") %>%
                 select(offense_play, success.rate, epa.pass.off, epa.rush.off, epa.pass.def, epa.rush.def, fp, drives, pts.per.drive))
  })
  output$mtnwest = DT::renderDataTable({
    datatable(cfb.table13(),
              rownames = FALSE, 
              class = 'cell-border stripe',
              colnames = c('Team', 'Success Rate',
                           'Pass EPA', 'Run EPA',
                           'Pass EPA Def.',
                           'Run EPA Def.',
                           'Avg. Field Position',
                           'Drives',
                           'Points Per Drive'),
              list(pageLength = 25)) %>%
      formatPercentage(c('success.rate'),1) %>%
      formatRound(c('epa.pass.off'),3) %>%
      formatRound(c('epa.rush.off'),3) %>%
      formatRound(c('epa.pass.def'),3) %>%
      formatRound(c('epa.rush.def'),3) %>%
      formatRound(c('fp'),1) %>%
      formatRound(c('pts.per.drive'),3) %>%
      formatStyle(c('success.rate'),
                  background = styleColorBar(range(cfb.table3()$success.rate),'lightblue'),
                  backgroundSize = '95% 50%',
                  backgroundRepeat = 'no-repeat',
                  backgroundPosition = 'right') %>%
      formatStyle(c('epa.pass.off'),
                  background = styleColorBar(range(cfb.table3()$epa.pass.off),'lightblue'),
                  backgroundSize = '95% 50%',
                  backgroundRepeat = 'no-repeat',
                  backgroundPosition = 'right') %>%
      formatStyle(c('epa.rush.off'),
                  background = styleColorBar(range(cfb.table3()$epa.rush.off),'lightblue'),
                  backgroundSize = '95% 50%',
                  backgroundRepeat = 'no-repeat',
                  backgroundPosition = 'right') %>%
      formatStyle(c('epa.pass.def'),
                  background = styleColorBar(range(cfb.table3()$epa.pass.def),'lightblue'),
                  backgroundSize = '95% 50%',
                  backgroundRepeat = 'no-repeat',
                  backgroundPosition = 'right') %>%
      formatStyle(c('epa.rush.def'),
                  background = styleColorBar(range(cfb.table3()$epa.rush.def),'lightblue'),
                  backgroundSize = '95% 50%',
                  backgroundRepeat = 'no-repeat',
                  backgroundPosition = 'right')  %>%
      formatStyle(c('fp'),
                  background = styleColorBar(range(cfb.table3()$fp),'palegoldenrod'),
                  backgroundSize = '95% 50%',
                  backgroundRepeat = 'no-repeat',
                  backgroundPosition = 'right') %>%
      formatStyle(c('drives'),
                  background = styleColorBar(range(cfb.table3()$drives),'palegoldenrod'),
                  backgroundSize = '95% 50%',
                  backgroundRepeat = 'no-repeat',
                  backgroundPosition = 'right') %>%
      formatStyle(c('pts.per.drive'),
                  background = styleColorBar(range(cfb.table3()$pts.per.drive),'palegoldenrod'),
                  backgroundSize = '95% 50%',
                  backgroundRepeat = 'no-repeat',
                  backgroundPosition = 'right')
  })
  
  cfb.table14 <- reactive({
    data.frame(drive.update.epa() %>% filter(offense_conference=="Pac-12") %>%
                 select(offense_play, success.rate, epa.pass.off, epa.rush.off, epa.pass.def, epa.rush.def, fp, drives, pts.per.drive))
  })
  output$pac12 = DT::renderDataTable({
    datatable(cfb.table14(),
              rownames = FALSE, 
              class = 'cell-border stripe',
              colnames = c('Team', 'Success Rate',
                           'Pass EPA', 'Run EPA',
                           'Pass EPA Def.',
                           'Run EPA Def.',
                           'Avg. Field Position',
                           'Drives',
                           'Points Per Drive'),
              list(pageLength = 25)) %>%
      formatPercentage(c('success.rate'),1) %>%
      formatRound(c('epa.pass.off'),3) %>%
      formatRound(c('epa.rush.off'),3) %>%
      formatRound(c('epa.pass.def'),3) %>%
      formatRound(c('epa.rush.def'),3) %>%
      formatRound(c('fp'),1) %>%
      formatRound(c('pts.per.drive'),3) %>%
      formatStyle(c('success.rate'),
                  background = styleColorBar(range(cfb.table3()$success.rate),'lightblue'),
                  backgroundSize = '95% 50%',
                  backgroundRepeat = 'no-repeat',
                  backgroundPosition = 'right') %>%
      formatStyle(c('epa.pass.off'),
                  background = styleColorBar(range(cfb.table3()$epa.pass.off),'lightblue'),
                  backgroundSize = '95% 50%',
                  backgroundRepeat = 'no-repeat',
                  backgroundPosition = 'right') %>%
      formatStyle(c('epa.rush.off'),
                  background = styleColorBar(range(cfb.table3()$epa.rush.off),'lightblue'),
                  backgroundSize = '95% 50%',
                  backgroundRepeat = 'no-repeat',
                  backgroundPosition = 'right') %>%
      formatStyle(c('epa.pass.def'),
                  background = styleColorBar(range(cfb.table3()$epa.pass.def),'lightblue'),
                  backgroundSize = '95% 50%',
                  backgroundRepeat = 'no-repeat',
                  backgroundPosition = 'right') %>%
      formatStyle(c('epa.rush.def'),
                  background = styleColorBar(range(cfb.table3()$epa.rush.def),'lightblue'),
                  backgroundSize = '95% 50%',
                  backgroundRepeat = 'no-repeat',
                  backgroundPosition = 'right')  %>%
      formatStyle(c('fp'),
                  background = styleColorBar(range(cfb.table3()$fp),'palegoldenrod'),
                  backgroundSize = '95% 50%',
                  backgroundRepeat = 'no-repeat',
                  backgroundPosition = 'right') %>%
      formatStyle(c('drives'),
                  background = styleColorBar(range(cfb.table3()$drives),'palegoldenrod'),
                  backgroundSize = '95% 50%',
                  backgroundRepeat = 'no-repeat',
                  backgroundPosition = 'right') %>%
      formatStyle(c('pts.per.drive'),
                  background = styleColorBar(range(cfb.table3()$pts.per.drive),'palegoldenrod'),
                  backgroundSize = '95% 50%',
                  backgroundRepeat = 'no-repeat',
                  backgroundPosition = 'right')
  })
  
  cfb.table15 <- reactive({
    data.frame(drive.update.epa() %>% filter(offense_conference=="SEC") %>%
                 select(offense_play, success.rate, epa.pass.off, epa.rush.off, epa.pass.def, epa.rush.def, fp, drives, pts.per.drive))
  })
  output$sec = DT::renderDataTable({
    datatable(cfb.table15(),
              rownames = FALSE, 
              class = 'cell-border stripe',
              colnames = c('Team', 'Success Rate',
                           'Pass EPA', 'Run EPA',
                           'Pass EPA Def.',
                           'Run EPA Def.',
                           'Avg. Field Position',
                           'Drives',
                           'Points Per Drive'),
              list(pageLength = 25)) %>%
      formatPercentage(c('success.rate'),1) %>%
      formatRound(c('epa.pass.off'),3) %>%
      formatRound(c('epa.rush.off'),3) %>%
      formatRound(c('epa.pass.def'),3) %>%
      formatRound(c('epa.rush.def'),3) %>%
      formatRound(c('fp'),1) %>%
      formatRound(c('pts.per.drive'),3) %>%
      formatStyle(c('success.rate'),
                  background = styleColorBar(range(cfb.table3()$success.rate),'lightblue'),
                  backgroundSize = '95% 50%',
                  backgroundRepeat = 'no-repeat',
                  backgroundPosition = 'right') %>%
      formatStyle(c('epa.pass.off'),
                  background = styleColorBar(range(cfb.table3()$epa.pass.off),'lightblue'),
                  backgroundSize = '95% 50%',
                  backgroundRepeat = 'no-repeat',
                  backgroundPosition = 'right') %>%
      formatStyle(c('epa.rush.off'),
                  background = styleColorBar(range(cfb.table3()$epa.rush.off),'lightblue'),
                  backgroundSize = '95% 50%',
                  backgroundRepeat = 'no-repeat',
                  backgroundPosition = 'right') %>%
      formatStyle(c('epa.pass.def'),
                  background = styleColorBar(range(cfb.table3()$epa.pass.def),'lightblue'),
                  backgroundSize = '95% 50%',
                  backgroundRepeat = 'no-repeat',
                  backgroundPosition = 'right') %>%
      formatStyle(c('epa.rush.def'),
                  background = styleColorBar(range(cfb.table3()$epa.rush.def),'lightblue'),
                  backgroundSize = '95% 50%',
                  backgroundRepeat = 'no-repeat',
                  backgroundPosition = 'right')  %>%
      formatStyle(c('fp'),
                  background = styleColorBar(range(cfb.table3()$fp),'palegoldenrod'),
                  backgroundSize = '95% 50%',
                  backgroundRepeat = 'no-repeat',
                  backgroundPosition = 'right') %>%
      formatStyle(c('drives'),
                  background = styleColorBar(range(cfb.table3()$drives),'palegoldenrod'),
                  backgroundSize = '95% 50%',
                  backgroundRepeat = 'no-repeat',
                  backgroundPosition = 'right') %>%
      formatStyle(c('pts.per.drive'),
                  background = styleColorBar(range(cfb.table3()$pts.per.drive),'palegoldenrod'),
                  backgroundSize = '95% 50%',
                  backgroundRepeat = 'no-repeat',
                  backgroundPosition = 'right')
  })
  
  cfb.table16 <- reactive({
    data.frame(drive.update.epa() %>% filter(offense_conference=="Sun Belt") %>%
                 select(offense_play, success.rate, epa.pass.off, epa.rush.off, epa.pass.def, epa.rush.def, fp, drives, pts.per.drive))
  })
  output$sunbelt = DT::renderDataTable({
    datatable(cfb.table16(),
              rownames = FALSE, 
              class = 'cell-border stripe',
              colnames = c('Team', 'Success Rate',
                           'Pass EPA', 'Run EPA',
                           'Pass EPA Def.',
                           'Run EPA Def.',
                           'Avg. Field Position',
                           'Drives',
                           'Points Per Drive'),
              list(pageLength = 25)) %>%
      formatPercentage(c('success.rate'),1) %>%
      formatRound(c('epa.pass.off'),3) %>%
      formatRound(c('epa.rush.off'),3) %>%
      formatRound(c('epa.pass.def'),3) %>%
      formatRound(c('epa.rush.def'),3) %>%
      formatRound(c('fp'),1) %>%
      formatRound(c('pts.per.drive'),3) %>%
      formatStyle(c('success.rate'),
                  background = styleColorBar(range(cfb.table3()$success.rate),'lightblue'),
                  backgroundSize = '95% 50%',
                  backgroundRepeat = 'no-repeat',
                  backgroundPosition = 'right') %>%
      formatStyle(c('epa.pass.off'),
                  background = styleColorBar(range(cfb.table3()$epa.pass.off),'lightblue'),
                  backgroundSize = '95% 50%',
                  backgroundRepeat = 'no-repeat',
                  backgroundPosition = 'right') %>%
      formatStyle(c('epa.rush.off'),
                  background = styleColorBar(range(cfb.table3()$epa.rush.off),'lightblue'),
                  backgroundSize = '95% 50%',
                  backgroundRepeat = 'no-repeat',
                  backgroundPosition = 'right') %>%
      formatStyle(c('epa.pass.def'),
                  background = styleColorBar(range(cfb.table3()$epa.pass.def),'lightblue'),
                  backgroundSize = '95% 50%',
                  backgroundRepeat = 'no-repeat',
                  backgroundPosition = 'right') %>%
      formatStyle(c('epa.rush.def'),
                  background = styleColorBar(range(cfb.table3()$epa.rush.def),'lightblue'),
                  backgroundSize = '95% 50%',
                  backgroundRepeat = 'no-repeat',
                  backgroundPosition = 'right')  %>%
      formatStyle(c('fp'),
                  background = styleColorBar(range(cfb.table3()$fp),'palegoldenrod'),
                  backgroundSize = '95% 50%',
                  backgroundRepeat = 'no-repeat',
                  backgroundPosition = 'right') %>%
      formatStyle(c('drives'),
                  background = styleColorBar(range(cfb.table3()$drives),'palegoldenrod'),
                  backgroundSize = '95% 50%',
                  backgroundRepeat = 'no-repeat',
                  backgroundPosition = 'right') %>%
      formatStyle(c('pts.per.drive'),
                  background = styleColorBar(range(cfb.table3()$pts.per.drive),'palegoldenrod'),
                  backgroundSize = '95% 50%',
                  backgroundRepeat = 'no-repeat',
                  backgroundPosition = 'right')
  })
}

#Run the application
shinyApp(ui = ui, server = server)
