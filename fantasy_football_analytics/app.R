library(shiny)
library(rjson)
library(tidyverse)
library(forcats)
library(lubridate)
library(chron)
library(scales)
library(stringr)
library(ggthemes)
library(gridExtra)
library(DT)
library(plotly)
library(formattable)
library(shinythemes)

# importing transaction data; after transforming to data frame, first 30 rows of data is simply information about the fantasy league which we can safety remove
fantasy_data <- fromJSON(file = "./nfl-fantasy-football-master/transactions/Transaction_new.json") %>%
  unlist() %>% as.data.frame() %>% tail(-30)

# rownames of new data frame are the variables of data that we are interested in; creating a new column for these variables
fantasy_data$variable <- rownames(fantasy_data)
rownames(fantasy_data) <- NULL
names(fantasy_data) <- c("value", "variable")
fantasy_data$value <- as.character(fantasy_data$value)

# cleaning up messy transaction data by splitting new "variable" column into more tangible columns 
fantasy_data <- fantasy_data %>%
  separate(col = variable, into = c("fantasy_content", "league", "transactions", "transaction_number", "transaction", "players", "players_number", "variable"), sep = "\\.", extra = "merge") %>%
  mutate_at(.vars = "transaction_number", .funs = as.numeric) %>%
  unite("fantasy_content_league_transactions_transaction", c("fantasy_content", "league", "transactions", "transaction")) %>%
  unite("player_number", c("players", "players_number"))

# transactions (trades, adding/dropping players) are indicated by NA within the "variable" column; creating a new data frame for transactions data 
transaction_data <- fantasy_data %>%
  filter(is.na(variable)) %>%
  spread(key = player_number, value = value) %>%
  filter(!is.na(transaction_number)) %>%
  select(-c(fantasy_content_league_transactions_transaction,variable, NA_NA)) %>%
  mutate_at(.vars = "timestamp_NA", .funs = as.character) %>%
  mutate_at(.vars = "timestamp_NA", .funs = as.numeric)

# transaction timestamp is coded numerically; we need to convert this into a readable datatime format
transaction_data$timestamp_NA <- as.POSIXct(transaction_data$timestamp_NA, origin = "1970-01-01")

# all player data in the form of trades, waiver pickups, freeagent adds, etc. as well as which team(s) added, dropped, traded for them; 
player_data <- fantasy_data %>%
  filter(!is.na(variable)) %>%
  spread(key = variable, value = value) %>%
  select(-fantasy_content_league_transactions_transaction) %>%
  left_join(transaction_data, by = "transaction_number") %>%
  mutate(timestamp_copy = timestamp_NA) %>%
  separate(col = timestamp_copy, into = c("date", "time"), sep = " ") %>%
  mutate_at(.vars = "date", .funs = ymd) %>%
  mutate_at(.vars = "time", .funs = times)

# have one column dedicated to the fantasy team that the player is on
player_data$player_team <- ifelse(is.na(player_data$player.transaction_data.destination_team_name) == TRUE, player_data$player.transaction_data.source_team_name, player_data$player.transaction_data.destination_team_name)

# rosters data for all 16 weeks are contained in csv files; reading them in and cleaning up so we have data on which players were on which teams throughout the weeks of the fantasy season
setwd("./nfl-fantasy-football-master/data_analysis/weekly_rosters")
roster_file_names <- rep(paste("wk", seq(1,16),"roster.csv", sep = "_"))
roster_info <- lapply(roster_file_names, read.csv)
names(roster_info) <- rep(paste0("Week", seq(1,16)))
roster_info <- do.call(rbind, roster_info)
roster_info$week <- rownames(roster_info)
rownames(roster_info) <- NULL
roster_info <- roster_info %>% gather(key = "league_manager", value = "player", -c(X, week)) %>% separate(col = week, into = c("week", "number"), sep = "\\.") %>% 
  select(-number)

# csv file with player and respective positions
player_data_subset <- read_csv("player_positions.csv")

# reading in and cleaning up scores data (how many points the player/position scored in terms of fantasy points) for teams' players for all 16 weeks 
setwd("../weekly_scores")
score_file_names <- rep(paste("wk", seq(1,16),"scores.csv", sep = "_"))
scores_info <- lapply(score_file_names, read.csv)
names(scores_info) <- rep(paste0("Week", seq(1,16)))
scores_info <- do.call(rbind, scores_info)
scores_info$week <- rownames(scores_info)
rownames(scores_info) <- NULL
scores_info <- scores_info %>% gather(key = "league_manager", value = "score", -c(X, week)) %>% separate(col = week, into = c("week", "number"), sep = "\\.") %>% 
  select(-number)

# weekly rosters for all teams through 13 weeks ("regular season")
weekly_roster_scores <- left_join(roster_info, scores_info, by = c("X", "week", "league_manager"))
names(weekly_roster_scores) <- c("bench_position", "week", "league_manager", "player", "score")
# each fantasy player has an assigned name, we map them accordingly here
weekly_roster_scores$league_manager <- recode(weekly_roster_scores$league_manager, hector = "Ware’s Hunt?", Michael = "Leonard Fournecke", Nelson.L. = "Digg’s Squad", 
                                              Emil = "Peterman To Denver", Jay = "Thomas-sive Dong", Jonathan.C = "Jonathan C's Team", Kelvin.Chan = "Old Breesy F Baby", Gordon = "Faze JuJu", 
                                              Tyler = "LightsKamaraAction", Matthew = "Kicking & Screaming")

# position of some players from roster_info are listed as BN1, BN2, etc. (indicating that they were on a team's bench for the week); using player_data_subset, which has official position of players, to assign accurate player positions
weekly_roster_scores <- weekly_roster_scores %>% left_join(player_data_subset, by = "player") %>% filter(!is.na(score))
weekly_roster_scores$week <- factor(weekly_roster_scores$week, levels = rep(paste0("Week", seq(1,16))))
lineup_scores <- weekly_roster_scores %>% filter(!grepl("BN", bench_position)) %>% filter(week %in% c(rep(paste0("Week", seq(1,13)))))
# ranking players by their positions and how many fantasy points they scored for the week
weekly_roster_scores <- weekly_roster_scores %>% group_by(week, league_manager, position) %>% mutate(rank = rank(-score, ties.method = "first")) %>%
  filter(week %in% c(rep(paste0("Week", seq(1,13)))))

# creating an empty data frame for the best lineup for each player for each week
best_lineup <- data.frame()

# importing weekly scoreboard data (json format)
setwd("../../weekly_scoreboard")
files <- paste0("week_", seq(1,16), "scoreboard.json")
weekly_performance_scores <- lapply(files, function(x) fromJSON(file = x))

names(weekly_performance_scores) <- rep(paste("Week", seq(1,16)))
weekly_performance_scores <- weekly_performance_scores %>% unlist() %>% as.list() %>% 
  as.data.frame() %>% gather(key = "variable", value = "value") %>% filter(grepl("league.scoreboard", variable)) %>%
  separate(col = variable, into = c("Week", "Number", "Variable"), sep = "\\.", extra = "merge") %>% unite(col = "Week", c("Week", "Number"), sep = " ") %>%
  filter(!Variable == "fantasy_content.league.scoreboard.week" & grepl("team", Variable))

# cleaning up weekly performance data so that we have for each week which teams played each other (indicated with Matchup and Team) as well as their Projected and Actual fantasy points totals
weekly_performance_scores$Variable <- str_replace(weekly_performance_scores$Variable, "fantasy_content.league.scoreboard.0.","")
weekly_performance_scores <- weekly_performance_scores %>% 
  separate(col = Variable, into = c("Matchup", "Matchup Number", "2nd Matchup", "2nd Matchup Number", "Team", "Team Number", "Variable"), sep = "\\.", extra = "merge") %>%
  filter(!is.na(Variable) & !`2nd Matchup Number` != 0) %>% select(-c(`2nd Matchup`, `2nd Matchup Number`)) %>%
  unite(col = "Matchup", c("Matchup", "Matchup Number"), sep = " ") %>% unite(col = "Team", c("Team", "Team Number"), sep = " ") %>%
  spread(key = Variable, value = value) %>% select(c("Week", "Matchup", "Team", "team.name", "team.team_points.total", "team.team_projected_points.total")) %>%
  mutate_at(.vars = c("team.team_points.total", "team.team_projected_points.total"), .funs = as.numeric) %>%
  rename(Projected = team.team_projected_points.total, Actual = team.team_points.total) %>% 
  gather(key = "variable", value = "points", -c(Week, Matchup, Team, team.name)) %>%
  rename(Teams = team.name, Variable = variable, Points = points)

# factoring week so it has a natural order
weekly_performance_scores$Week <- factor(weekly_performance_scores$Week, levels = c(paste("Week", seq(1:16))))

# the criteria for best lineup is as followed: we want to take players with the highest points totals across all positions regardless of whether they were on the bench or lineup; each lineup consists
# of 1 QB, 2 RB, 2 WR, 1 TE, 1 K, and 1 TE
for (i in c("QB", "WR", "RB", "TE", "K", "DEF")) {
  if (i == "QB") {
    best_lineup <- rbind(weekly_roster_scores %>% filter(position == "QB" & rank <= 1) %>% arrange(rank) %>% as.data.frame(), best_lineup)
  } else if (i == "WR") {
    best_lineup <- rbind(weekly_roster_scores %>% filter(position == "WR" & rank <= 2) %>% arrange(rank) %>% as.data.frame(), best_lineup)
  } else if (i == "RB") {
    best_lineup <- rbind(weekly_roster_scores %>% filter(position == "RB" & rank <= 2) %>% arrange(rank) %>% as.data.frame(), best_lineup)
  } else if (i == "TE") {
    best_lineup <- rbind(weekly_roster_scores %>% filter(position == "TE" & rank <= 1) %>% arrange(rank) %>% as.data.frame(), best_lineup)
  } else if (i == "K") {
    best_lineup <- rbind(weekly_roster_scores %>% filter(position == "K" & rank <= 1) %>% arrange(rank) %>% as.data.frame(), best_lineup)
  } else if (i == "DEF") {
    best_lineup <- rbind(weekly_roster_scores %>% filter(position == "DEF" & rank <= 1) %>% arrange(rank) %>% as.data.frame(), best_lineup)
  }
}
# flex positions allow a player to put a RB, WR, or TE player in its place; take all applicable "flex" players and rank them based on points scores by week, position, and team 
flex_positions <- weekly_roster_scores %>% 
  anti_join(best_lineup, by = c("bench_position", "week", "league_manager", "player", "score", "position", "rank")) %>% 
  filter(position == "WR" | position == "RB" | position == "TE") %>%
  group_by(week, league_manager) %>% mutate(new_rank = rank(-score, ties.method = "first")) %>%
  filter(new_rank == 1) %>% select(-new_rank) %>% as.data.frame()

best_lineup <- rbind(flex_positions, best_lineup)

# weekly lineup scores which have actual vs. best lineup scores for each player and week
weekly_lineup_scores <- left_join(lineup_scores %>% group_by(week, league_manager) %>% summarise(lineup = sum(score)),
                                  best_lineup %>% group_by(week, league_manager) %>% summarise(best_lineup = sum(score)), by = c("week", "league_manager")) %>% 
  mutate(difference = best_lineup - lineup) %>% group_by(league_manager) %>% mutate(lineup_z_score = scale(lineup), difference_z_score = scale(difference)) %>%
  filter(week %in% c(rep(paste0("Week", seq(1,13)))))

# cleaning out weekly performance scores which contains projected vs. actual scores for each player and week 
weekly_performance_scores$Week <- str_replace(weekly_performance_scores$Week, " ", "")
weekly_performance_scores$Week <- factor(weekly_performance_scores$Week, levels = c(rep(paste0("Week", seq(1,16)))))
weekly_performance_scores <- weekly_performance_scores %>% filter(Week %in% c(rep(paste0("Week", seq(1,13)))))

# table that shows the winner of each matchup based on actual points scored by each team
manager_weekly_outcomes <- left_join(weekly_performance_scores,weekly_performance_scores %>% filter(Variable == "Actual") %>% group_by(Week, Matchup) %>% top_n(1, Points) %>% 
                                       mutate(outcome = "WIN")) %>% mutate(outcome = ifelse(is.na(outcome), "LOSE", outcome)) %>% filter(Variable == "Actual") %>%
  rename(week = Week) %>% rename(league_manager = Teams)

# data frames used for miscellaneous stats
# counting up the number of wins and loses by each team using above manager_weekly_outcomes table
win_lose_record <- manager_weekly_outcomes %>% filter(week %in% c(rep(paste0("Week", seq(1,13))))) %>% group_by(league_manager, outcome) %>%
  summarise(count = n()) %>% spread(key = outcome, value = count) %>% select(league_manager, WIN, LOSE) %>%
  arrange(factor(league_manager, levels = c("Jonathan C's Team", "Digg’s Squad", "Thomas-sive Dong", "Ware’s Hunt?",
                                            "Kicking & Screaming", "Leonard Fournecke", "Peterman To Denver", "Old Breesy F Baby",
                                            "Faze JuJu", "LightsKamaraAction")))

# using weekly_performance_scores to calculate each team's average difference in points when they outscore (actual > projected) vs. underscore (actual < projected)
avg_out_underscore <- weekly_performance_scores %>% filter(Week %in% c(rep(paste0("Week", seq(1,13))))) %>% group_by(Week, Teams, Variable) %>%
  summarise(points = sum(Points)) %>% spread(key = Variable, value = points) %>% mutate(Difference = Actual - Projected, outcome = ifelse(Actual > Projected, "outscore", "underscore")) %>%
  group_by(Teams, outcome) %>% summarise(avg_difference = mean(Difference)) %>% spread(key = outcome, value = avg_difference) %>%
  rename(league_manager = Teams)

# using weekly_roster_scores to calculate the average number of points put up by each position on the weekly starting roster (no bench players)
avg_score_by_position <- weekly_roster_scores %>% filter(week %in% c(rep(paste0("Week", seq(1,13)))) & !grepl("BN", bench_position)) %>% group_by(league_manager, position) %>%
  summarise(avg_points = mean(score)) %>% spread(key = position, value = avg_points)

# using weekly_lineup_scores to calculate the average number of points for both actual and projected best lineups by team and the difference between those averages
avg_score_by_manager <- weekly_lineup_scores %>% filter(week %in% c(rep(paste0("Week", seq(1,13))))) %>% group_by(league_manager) %>% 
  summarise(avg_points = mean(lineup), avg_best_lineup_points = mean(best_lineup), difference = avg_points - avg_best_lineup_points)

# UI - front-end
ui <- fluidPage(
  theme = shinytheme("lumen"),
  titlePanel("Fantasy Football 2018-19 Season Stats"),
  
  # TRANSACTIONS - data as it relates to transactions (adds, drops, trades) of players and between each manager
  tabsetPanel(
    tabPanel(title = "Transactions",
             sidebarLayout(
               sidebarPanel(width = 3,
                            helpText("Graph 1: Transaction Trends Among Position - A look at which players were the most added/dropped/traded throughout the season"),
                            selectInput(inputId = "position",
                                        label = "Select a Position",
                                        choices = unique(player_data$player.display_position)),
                            sliderInput(inputId = "number_players",
                                        label = "Select a Range of Players",
                                        min = 1, max = 20, value = 10),
                            radioButtons(inputId = "transaction_type",
                                         label = "Select a Transaction Type",
                                         choices = unique(player_data$player.transaction_data.type)),
                            
                            br(), 
                            
                            helpText("Graph 2: Number of Transactions Conducted By League Managers Throughout the Season - A look at the transactions trends taken by managers throughout the season"),
                            selectInput(inputId = "manager_name",
                                        label = "Select a League Manager",
                                        choices = unique(player_data$player_team)),
                            
                            br(), 
                            
                            helpText("Graph(s) 3: Transaction Trends Among Players - A look at how league managers went about managing their rosters throughout the season"),
                            selectInput(inputId = "add_drop",
                                        label = "Select a Transaction Type",
                                        choices = c("add", "drop")),
                            dateRangeInput(inputId = "transaction_date",
                                           label = "Select a Range of Dates",
                                           start = min(player_data$date), end = max(player_data$date),
                                           min = min(player_data$date), max = max(player_data$date)),
                            
                            br(), 
                            
                            helpText("Table 1: Transactions Data Table - A record of all transactions made during the season")),
               
               mainPanel(
                 plotOutput("transactions_plot"),
                 plotOutput("player_transactions_plot"),
                 dataTableOutput("player_details")
               )
             )),
    
    # WEEKLY SCORES - data as it relates to weekly scores put up by each manager and weekly projected best lineup
    tabPanel(title = "Weekly Scores",
             sidebarLayout(
               sidebarPanel(width = 3,
                            helpText("Graph(s) 1 : Weekly Performance and Difference from Average Score by League Managers - A look at how each league manager performance on a weekly basis as well as compared to the rest of the league"),
                            
                            br(),
                            
                            helpText("Graph(s) 2: Weekly Set vs. Best Lineup Scores and Difference between the Two -  A look at how league managers set up their lineups vs. their best possible lineup on a given week"),
                            
                            selectInput(inputId = "week",
                                        label = "Select a Week",
                                        choices = unique(lineup_scores$week)),
                            
                            br(),
                            
                            helpText("Graph 3: League Manager and Lineup Performance - A look at how well a league manager sets up their lineup vs. how well their lineup actually performs"),
                            
                            selectInput(inputId = "league_manager",
                                        label = "Select a Manager", 
                                        choices = unique(weekly_lineup_scores$league_manager))
               ),
               
               mainPanel(
                 plotOutput("weekly_scores_plots"),
                 plotOutput("best_weekly_scores_plots"),
                 plotlyOutput("weekly_lineup_scores_plots")
               )
             )),
    
    # WEEKLY PERFORMANCE - similar to WEEKLY SCORES except data as it relates to weekly actual vs. projected points scores as well as points by position by week for each manager
    tabPanel(title = "Weekly Performance",
             sidebarLayout(
               sidebarPanel(width = 3,
                            helpText("Graph 1: Week over Week League Manager Performance - A look at a league manager's projected and actual weekly scores (dashed - projected vs. solid - actual"),
                            
                            br(),
                            
                            sliderInput(inputId = "week_number",
                                        label = "Select a number of weeks",
                                        min = 2, max = 13, value = 13),
                            
                            checkboxGroupInput(inputId = "team_name",
                                               label = "Select a team(s)",
                                               choices = unique(weekly_performance_scores$Teams),
                                               selected = "Ware’s Hunt?"),
                            
                            helpText("Graph 2: Week over Week Position Performance - A look at a position's performance as a total score or a percentage of the team's overall score in a given week and whether
                                     or not the league manager won or lost"),
                            
                            selectInput(inputId = "manager",
                                        label = "Select a Manager",
                                        choices = unique(weekly_roster_scores$league_manager)),
                            
                            selectInput(inputId = "player_position",
                                        label = "Select a position",
                                        choices = c("QB", "WR", "RB", "TE", "K", "DEF")),
                            
                            radioButtons(inputId = "type",
                                         label = "Select a type",
                                         choices = c("total_score", "percent"))
                            
                            ),
               
               mainPanel(
                 plotlyOutput("weekly_manager_performance"),
                 plotOutput("weekly_position_performance")
               )
             )),
    
    # MISCELLANEOUS STATS - graphs that show positional strength, distribution of points scored by week, as well as key stats for each manager
    tabPanel(title = "Miscellaneous Stats",
             mainPanel(
               plotOutput("position_plots"),
               plotOutput("scores_boxplot"),
               dataTableOutput("manager_key_metrics")
             ))
)
)

# SERVER - back-end
server <- function(input, output) {
  # each manager's transaction count based on position (input$position) and transaction type (input$transaction_type)
  player_transactions <- reactive(player_data %>% 
                                    group_by(player.name.full, player.display_position) %>% filter(player.display_position == input$position & player.transaction_data.type == input$transaction_type) %>% 
                                    summarise(count = n()) %>% arrange(desc(count)) %>% head(input$number_players))
  
  # each manager's breakdown of transactions count/percent by player
  transactions_by_player <- reactive(player_data %>%
                                       filter(player.transaction_data.type == input$add_drop & (date >= input$transaction_date[1] & date <= input$transaction_date[2])) %>% 
                                       group_by(player_team, player.display_position) %>% summarise(count = n()) %>% mutate(percent = count / sum(count)))
  
  # each manager's transactions count over the season (disregarding trades)
  player_transactions_over_time <- reactive(player_data %>% filter(player.transaction_data.type != "trade" & player_team == input$manager_name) %>% 
                                              group_by(date, player_team) %>% summarise(count = n()))
  
  # each manager's transaction details - which players where added/dropped/traded by manager(s) when 
  transaction_details <- player_data %>% group_by(timestamp_NA, player_team, player.transaction_data.type, player.name.full) %>% 
    select(timestamp_NA, player_team, player.transaction_data.type, player.name.full) 
  
  # breakdown of each manager's fantasy points scored by week along with average points scored by week to determine which manager's scored above/below the mean
  score_by_week <- reactive(lineup_scores %>% filter(week == input$week) %>% group_by(league_manager) %>% summarise(total_score = sum(score)) %>% 
                              mutate(mean = mean(total_score), difference = total_score - mean, over_under = ifelse(difference > 0, "positive", "negative")) %>%
                              arrange(total_score) %>% mutate(league_manager = fct_inorder(league_manager)))
  
  # breakdown of each manager's fantasy points scored by week by position (e.g. what percentage of points did WR make up for Team A's points total for Week 1?); includes data on whether team won and how many points they put up
  weekly_manager_positions_scores <- reactive(left_join(weekly_roster_scores %>% filter(!grepl("BN", bench_position)) %>% group_by(week, league_manager, position) %>%
                                                          summarise(total_score = sum(score)) %>% mutate(percent = total_score / sum(total_score)), manager_weekly_outcomes, by = c("week", "league_manager")) %>%
                                                filter(week %in% c(rep(paste0("Week", seq(1,13)))) & league_manager == input$manager & position == input$player_position))
  
  # title for transaction graph; doesn't make sense, when only 1 player is selected, to have graph read "Top 1 Add Transaction..." --> instead we can just say "Most Added Player..."
  title <- reactive(
    if (input$transaction_type == "add") {
      if (nrow(player_transactions()) == 1) {
        title <- paste0("(1)\nMost Added Player Among the ", input$position, " position")
      } else {
        if (input$number_players > nrow(player_transactions())) {
          title <- paste("(1)\nTop", nrow(player_transactions()), str_to_title(input$transaction_type), "Transactions Among", input$position, "Position")
        } else {
          title <- paste("(1)\nTop", input$number_players, str_to_title(input$transaction_type), "Transactions Among", input$position, "Position")
        }
      }
    } else if (input$transaction_type == "drop") {
      if (nrow(player_transactions()) == 1) {
        title <- paste0("(1)\nMost Dropped Player Among the ", input$position, " position")
      } else {
        if (input$number_players > nrow(player_transactions())) {
          title <- paste("(1)\nTop", nrow(player_transactions()), str_to_title(input$transaction_type), "Transactions Among", input$position, "Position")
        } else {
          title <- paste("(1)\nTop", input$number_players, str_to_title(input$transaction_type), "Transactions Among", input$position, "Position")
        }
      }
    } else {
      title <- paste("Players Traded Among the", input$position, "Position")
    }
  )
  
  # each manager's actual vs. projected best lineup scores by week 
  weekly_lineup_vs_best_score <- reactive(weekly_lineup_scores %>% gather(key = "lineup_best", value = "score", -c(week, league_manager)) %>% 
                                            filter((lineup_best == "lineup" | lineup_best == "best_lineup") & week == input$week))
  
  # weekly_line_up_scores filtered by league manager; used to graph difference between actual vs. projected best lineup scores
  manager_lineup_vs_best_score <- reactive(weekly_lineup_scores %>% filter(league_manager == input$league_manager))
  
  
  output$transactions_plot <- renderPlot({
    # validation in case transaction trend cannot be found (e.g. no players with WR,RB designated positions were traded --> pops up message)
    validate(
      need(nrow(player_transactions()) > 0, paste("No", input$transaction_type, "transaction found for", input$position, "postion; Please select another position/transaction."))
    )
    
    # plots for Transactions tab - (1) and (2)
    position_transactions <- ggplot(data = player_transactions(), aes(x = reorder(player.name.full, count), y = count)) +
      geom_bar(stat = "identity", fill = "#428bca") + xlab("Player Name (full)") + scale_y_continuous(name = "Transaction(s)", labels = function(x) round(as.numeric(x), digits = 0), breaks = seq(0,max(player_transactions()$count),1)) + 
      coord_flip() + ggtitle(title())
    
    manager_transactions <- ggplot(data = player_transactions_over_time(), aes(x = date, y = count)) + geom_line() +
      xlab("Date") + scale_y_continuous(name = "Transaction(s)") + ggtitle(paste("(2)\nTransaction Trends by League Manager"))
    
    grid.arrange(position_transactions, manager_transactions, ncol = 2)
  })
  
  # plots for Transactions tab - (3A) and (3B)
  output$player_transactions_plot <- renderPlot({
    # validation in case an invalid date range is selected (e.g. 2018-10-30 to 2018-08-30)
    validate(
      need(nrow(transactions_by_player()) > 0, "Selected END DATE < START DATE; Please set a valid range of dates.")
    )
    
    transaction_count <- ggplot(data = transactions_by_player(), aes(x = player_team, y = count, fill = player.display_position)) +
      geom_bar(stat = "identity") + coord_flip() + xlab("League Manager") + scale_y_continuous(name = "Transactions", labels = scales::comma_format(accuracy = 1)) +
      labs(fill = "Position") + ggtitle("(3A)",paste(str_to_title(input$add_drop), "Transactions By League Managers"))
    
    transaction_percent <- ggplot(data = transactions_by_player(), aes(x = player_team, y = percent, fill = player.display_position)) +
      geom_bar(stat = "identity") + coord_flip() + xlab("League Manager") + scale_y_continuous(name = "Percentage of Total Transactions", labels = scales::percent_format(accuracy = 1)) +
      labs(fill = "Position") + ggtitle("(3B)",paste(str_to_title(input$add_drop), "Transactions As a Percentage of Total Transactions By League Managers"))
    
    grid.arrange(transaction_count, transaction_percent, ncol = 2)
  })
  
  # table for Transactions tab 
  output$player_details <- renderDataTable({
    datatable(transaction_details, colnames = c("Timestamp", "Player Team Name", "Transaction Type", "Full Player Name"), 
              filter = "top") %>% formatDate(1, "toLocaleString")
  })
  
  # plots for Weekly Scores - (1A) and (1B)
  output$weekly_scores_plots <- renderPlot({
    weekly_scores_by_manager <- ggplot(data = score_by_week(), aes(x = total_score, y = league_manager)) + 
      geom_segment(aes(xend = 0, yend = league_manager), size = 1, color = "grey65") + geom_point(size = 4, color = "navyblue") +
      geom_vline(xintercept = score_by_week()$mean, linetype = "dashed", size = 1.5, color = "grey25") + xlab("Total Score") + ylab("League Manager") +
      ggtitle("(1A)",paste(input$week, "Performance by League Manager"))
    
    weekly_difference_by_manager <- ggplot(data = score_by_week(), aes(x = league_manager, y = difference, fill = over_under)) +
      geom_bar(stat = "identity") + coord_flip() + xlab("League Manager") + ylab("Difference") + labs(fill = "Positive/Negative") + ggtitle("(1B)",paste(input$week, "Difference from Average Score by League Manager"))
    
    grid.arrange(weekly_scores_by_manager, weekly_difference_by_manager, ncol = 2)
  })
  
  # plots for Weekly Scores - (2A) and (2B)
  output$best_weekly_scores_plots <- renderPlot({
    lineup_vs_best_graph <- ggplot(data = weekly_lineup_vs_best_score(), aes(x = league_manager, y = score, fill = lineup_best)) + geom_bar(stat = "identity",position = "dodge") +
      coord_flip() + xlab("League Manager") + ylab("Total Score") + labs(fill = "Lineup") +
      ggtitle("(2A)",paste(input$week, "Set Lineup vs. Best Lineup Score"))
    
    difference_best_vs_regular <- ggplot(data = weekly_lineup_scores %>% filter(week == input$week), aes(x = reorder(league_manager,difference), y = difference)) + geom_bar(stat = "identity", fill = "#00BFC4") + coord_flip() +
      xlab("League Manager") + ylab("Difference") + ggtitle("(2B)", paste(input$week, "Difference Between Best Lineup and Set Lineup Score"))
    
    grid.arrange(lineup_vs_best_graph, difference_best_vs_regular, ncol = 2)
  })
  
  # plot for Weekly Scores - (3)
  output$weekly_lineup_scores_plots <- renderPlotly({
    p1 <- ggplot(data = manager_lineup_vs_best_score(), aes(x = lineup, y = difference, color = week)) + geom_point() + xlab("Lineup") + ylab("Difference") + labs(color = "Week") +
      ggtitle("(3) How Well Do League Managers Set Up Their Lineup vs. How Well Do Their Lineup Perform?")
    
    ggplotly(p1)
  })
  
  # plot for Weekly Performance (1)
  output$weekly_manager_performance <- renderPlotly({
    # validation in case a team (input$team_name) is not selected; defaulted to first option on list 
    validate(
      need(length(input$team_name) > 0, "Please select at least one team.")
    )
    p <- ggplot(data = weekly_performance_scores %>% filter(Week %in% rep(paste0("Week", seq(1:input$week_number))) & Teams %in% input$team_name), 
                aes(x = Week, y = Points, group = interaction(Variable, Teams), color = Teams, linetype = Variable)) + geom_line(size = 1.5) + geom_point(size = 2.5) +
      theme(legend.position = 'none') + ggtitle("(1) Week over Week League Manager Projected vs. Actual Points Scored")
    
    ggplotly(p, tooltip = c("Variable", "Teams", "Points")) 
  })
  
  # plot for Weekly Performance (2)
  output$weekly_position_performance <- renderPlot({
    if (input$type == "percent") {
      ggplot(data = weekly_manager_positions_scores(), aes_string(x = "week", y = input$type, group = "league_manager")) + 
        geom_line() + geom_point(aes(color = outcome), size = 7) + scale_y_continuous(labels = scales::percent_format(accuracy = 1), name = "Percent") + xlab("Week") +
        ggtitle(paste("(2) Week over Week", input$player_position, "Performance (As a Percentage of Overall Lineup Score)"))
    } else {
      ggplot(data = weekly_manager_positions_scores(), aes_string(x = "week", y = input$type, group = "league_manager")) +
        geom_line() + geom_point(aes(color = outcome), size = 7) + xlab("Week") + ylab("Total Score") +
        ggtitle(paste("(2) Week over Week", input$player_position, "Performance (Total Points Scored by Player(s) of Position)"))
    }
  })
  
  # plot for Miscellaneous Stats
  output$position_plots <- renderPlot({
    # heatmap for positional performance by team
    position_heatmap <- ggplot(data = weekly_roster_scores %>% filter(!grepl("BN", bench_position) & week %in% c(rep(paste0("Week", seq(1,13))))) %>%
                                 group_by(league_manager, position) %>% summarise(total_score = sum(score)) %>% group_by(position) %>% mutate(rescale = rescale(total_score)), 
                               aes(x = position, y = league_manager, fill = rescale)) + geom_tile(color = "black") + xlab("Position") + ylab("League Manager") +
      labs(fill = "Scale") + ggtitle("Position Performance Among League Managers") + scale_fill_gradient(low = "yellow", high = "red")
    
    # score density by postion
    position_density_plot <- ggplot(data = weekly_roster_scores %>% filter(!grepl("BN", bench_position) & week %in% c(rep(paste0("Week", seq(1,13))))),
                                    aes(x = score, color = position, fill = position)) + geom_density(alpha = 0.2, size = 1.1) + xlab("Score") + ylab("Density") +
      ggtitle("Score Density Plot By Position")
    
    grid.arrange(position_heatmap, position_density_plot, ncol = 2)
  })
  
  output$scores_boxplot <- renderPlot({
    # distribution of points by league managers for regular season
    league_manager_boxplot <- ggplot(data = manager_weekly_outcomes %>% filter(week %in% c(rep(paste0("Week", seq(1,13))))), 
                                     aes(x = reorder(league_manager, Points, FUN = median), y = Points)) + geom_boxplot(fill = "khaki2") + 
      coord_flip() + geom_jitter(aes(color = outcome)) + xlab("League Manager") + ggtitle("Distribution of Points Scored on 13 Week Basis by League Managers")
    
    # distribution of points based on outcome (W vs. L)
    win_lose_boxplot <- ggplot(data = manager_weekly_outcomes %>% filter(week %in% c(rep(paste0("Week", seq(1,13))))),
                               aes(x = outcome, y = Points)) + geom_boxplot() + geom_jitter(aes(color = league_manager)) + xlab("Outcome") + 
      labs(color = "League Manager") + ggtitle("Distribution of Points Scored on 13 Week Basis by Outcome")
    
    grid.arrange(league_manager_boxplot, win_lose_boxplot, ncol = 2)
  })
  
  # table of key metric across each league manager (e.g. # of wins/loses, avg. # of points scored per week, avg. # of points scored by position)
  output$manager_key_metrics <- renderDataTable({
    datatable(data = win_lose_record %>% left_join(avg_score_by_manager, by = "league_manager") %>% left_join(avg_out_underscore, by = "league_manager") %>%
                left_join(avg_score_by_position, by = "league_manager"), 
              colnames = c("League Manager", "Wins", "Losses", "Avg. Points/Week", "Avg. Points/Week (Best Lineup)", "Avg. Difference (Lineup - Best Lineup)", "Avg. Margin (Actual > Projected)", "Avg. Margin (Projected > Actual)",
                           "Avg. DEF Points/Week", "Avg. K Points/Week", "Avg. QB Points/Week", "Avg. RB Points/Week", "Avg. TE Points/Week", "Avg. WR Points/Week")) %>%
      formatRound(columns = 4:8, digits = 2) %>% formatRound(columns = 9:14, digits = 1)
  })
  
}

shinyApp(ui = ui, server = server)
