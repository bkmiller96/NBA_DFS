library(tidyverse)
library(rvest)
# library(httr)
library(dplyr)
library(stringr)
# library(readxl)
library(progress)
library(RSelenium)
#library(mail)
library(jsonlite)
library(janitor)
library(httr)
library(janitor)
library(tidyr)
library(tm)
library(SnowballC)
library(stringdist)
library(data.table)
library(RPostgreSQL)
library(DBI)
library(RPostgres)
library(rstudioapi)
library(keyring)
library(XML)
library(lpSolve)


rD <- rsDriver(browser="firefox", port=4545L, verbose=F)
remDr <- rD[["client"]]

remDr$open()
remDr$navigate("https://www.numberfire.com/")
remDr$screenshot(TRUE)

webElem15 <- remDr$findElement(using = 'xpath', '/html/body/nav[1]/div/ul[2]/li[3]/a')
webElem15$clickElement()
webElem15$sendKeysToElement(list(key = "enter"))

Sys.sleep(2)

webElem16 <- remDr$findElement(using = 'xpath', '/html/body/div[5]/div/span/a')
webElem16$clickElement()

Sys.sleep(2)

webElem16.5 <- remDr$findElement(using = "xpath", "/html/body/div[4]/div/ul/li[4]/a")
webElem16.5$clickElement()

Sys.sleep(5)

webElem17 <- remDr$findElement(using = 'xpath', '//*[@id="identifierId"]')
webElem17$sendKeysToElement(list("YOUR EMAIL"))

Sys.sleep(3)

webElem18 <- remDr$findElement(using = 'xpath', '//*[@id="identifierNext"]')
webElem18$clickElement()

Sys.sleep(3)

webElem19 <- remDr$findElement(using = 'xpath', '/html/body/div[1]/div[1]/div[2]/div/div[2]/div/div/div[2]/div/div[1]/div/form/span/section/div/div/div[1]/div[1]/div/div/div/div/div[1]/div/div[1]/input')
webElem19$sendKeysToElement(list("YOUR PASSWORD"))

Sys.sleep(3)

webElem20 <- remDr$findElement(using = 'xpath', '//*[@id="passwordNext"]')
webElem20$clickElement()

Sys.sleep(3)

remDr$navigate("https://www.numberfire.com/nba/daily-fantasy/daily-basketball-projections")

Sys.sleep(3)

webElem21 <- remDr$findElement(using = 'xpath', "/html/body/main/div[2]/div[2]/div/div[2]/div[1]/div/i[1]")
webElem21$clickElement()

Sys.sleep(3)

webElem22 <- remDr$findElement(using = "xpath", "/html/body/main/div[2]/div[2]/div/div[2]/div[1]/div/ul/li[2]")
webElem22$clickElement()

Sys.sleep(2)



#### Create 20 ####
nfHTML <- remDr$getPageSource("outerHTML")[[1]] %>%
  read_html()

nfPlayer <- nfHTML %>%
  html_nodes(".full") %>%
  html_text() %>%
  str_remove("\n                                    ") %>%
  str_remove("                                ")

nfPlayerTeam <- nfHTML %>%
  html_nodes(".team-player__team.active") %>%
  html_text() %>%
  str_remove("\n                                        ") %>%
  str_remove("                                    ")

webElem23 <- remDr$findElement(using = "xpath", "/html/body/main/div[2]/div[2]/section/div[4]/div[2]/table")
nfTable <- webElem23$getElementAttribute("outerHTML")[[1]]
nfTable <- readHTMLTable(nfTable, header = TRUE, as.data.frame = TRUE)[[1]]

nfTable$FP <- nfTable$`
                        FP
                    `

nfScrape <- nfTable %>%
  mutate(Player = nfPlayer,
         Team = nfPlayerTeam,
         Player = str_remove(Player, "'"),
         Player = str_remove_all(Player, "\\."),
         Player = str_remove_all(Player, " Jr"),
         Player = str_remove_all(Player, " IV"),
         Player = str_remove_all(Player, " III"),
         Player = ifelse(Player == "Ishmael Smith", "Ish Smith", Player),
         Player = ifelse(Player == "Patrick Mills", "Patty Mills", Player),
         Player = ifelse(Player == "Jose Juan Barea", "JJ Barea", Player),
         Player = tolower(Player),
         Player = ifelse(Player == "sviatoslav mykhailiuk", "svi mykhailiuk", Player),
         Team = as.character(Team),
         FP = as.character(FP),
         FP = as.numeric(FP)) %>%
  mutate(Team = ifelse(Team == "WSH", "WAS",
                       ifelse(Team == "PHX", "PHO",
                              ifelse(Team == "UTAH", "UTA", Team)))) %>%
  select(Player, Team, FP) %>%
  group_by(Player, Team) %>%
  summarise(FP = max(FP))

### Scrape Daily Fantasy Fuel ####
dailyFantasyFuel <- read_csv("DFF_NBA_cheatsheet_2021-12-17.csv")

dffScrape <- dailyFantasyFuel %>%
  mutate(Player = paste0(first_name, " ", last_name),
         Player = str_remove(Player, "'"),
         Player = str_remove(Player, " Sr."),
         Player = str_remove_all(Player, "\\."),
         Player = str_remove_all(Player, " Jr"),
         Player = str_remove_all(Player, " IV"),
         Player = str_remove_all(Player, " III"),
         Player = ifelse(Player == "Mo Bamba", "Mohamed Bamba", Player),
         Player = ifelse(Player == "Jakarr Sampson", "JaKarr Sampson", Player),
         Player = tolower(Player),
         Player = ifelse(Player == "kj martin", "kenyon martin", Player),
         Player = ifelse(Player == "guillermo hernangomez", "willy hernangomez", Player)) %>%
  rename(Team = team,
         Projection = ppg_projection,
         Sal = salary) %>%
  select(Player, Team, Sal, Projection)

injury_df <- dailyFantasyFuel %>%
  mutate(Player = paste0(first_name, " ", last_name),
         Player = str_remove(Player, "'"),
         Player = str_remove(Player, " Sr."),
         Player = str_remove_all(Player, "\\."),
         Player = str_remove_all(Player, " Jr"),
         Player = str_remove_all(Player, " IV"),
         Player = str_remove_all(Player, " III"),
         Player = ifelse(Player == "Mo Bamba", "Mohamed Bamba", Player),
         Player = ifelse(Player == "Jakarr Sampson", "JaKarr Sampson", Player),
         Player = tolower(Player),
         Player = ifelse(Player == "kj martin", "kenyon martin", Player),
         Player = ifelse(Player == "guillermo hernangomez", "willy hernangomez", Player)) %>%
  mutate(not_injured = ifelse(is.na(injury_status), 1, 0)) %>%
  rename(Team = team) %>%
  select(Player, Team, not_injured)

dff_lines <- dailyFantasyFuel %>%
  group_by(team) %>%
  summarise(total = max(over_under),
            line = max(abs(spread)),
            implied_team_score = max(implied_team_score)) %>%
  ungroup() %>%
  mutate(ou_type = case_when(
    total >=q2 ~ "High",
    total <=q1 ~ "Low",
    TRUE ~ "Middle"
  )) %>%
  mutate(line_type = case_when(
    line >=q2_line ~ "High",
    line <=q1_line ~ "Low",
    TRUE ~ "Middle"
  )) %>%
  na.omit() %>%
  select(team, ou_type, line_type)



### SportsLine Scraper ####
slProjection <- fromJSON("https://www.sportsline.com/sportsline-web/service/v1/fantasy/projections/nba/simulation")
slProjection <- slProjection[["projections"]] %>%
  select(player, team, dk, pos) %>%
  rename(dk_exp = dk)
slProjection <- slProjection %>%
  select(player, team, dk_exp) %>%
  mutate(player = as.character(player),
         dk_exp = as.character(dk_exp),
         dk_exp = as.numeric(dk_exp),
         team = as.character(team),
         Name = sapply(strsplit(player, " \\("), `[`, 1),
         Name = str_remove(Name, "'"),
         Name = str_remove(Name, "`"),
         Name = str_remove_all(Name, "\\."),
         Name = str_remove_all(Name, " Jr"),
         Name = ifelse(Name == "Maximilian Kleber", "Maxi Kleber", Name),
         Name = str_remove_all(Name, " IV"),
         Name = str_remove_all(Name, " III"),
         Name = tolower(Name)) %>%
  rename(slProjection = dk_exp,
         Player = Name,
         Team = team) %>%
  select(Player, Team, slProjection) %>%
  na.omit()

sportslineScrape <- slProjection

### Rotowire ####
rotowire_projections <- fromJSON("https://www.rotowire.com/daily/tables/optimizer-nba.php?siteID=1&slateID=2984&projSource=RotoWire") %>%
  # fromJSON("https://www.rotowire.com/daily/tables/optimizer-nba.php?sport=NBA&site=DraftKings&projections=&type=main&slate=Early") %>%
  select(player, team, proj_points) %>%
  rename(Name = player) %>%
  mutate(Name = str_remove(Name, "'"),
         Name = str_remove(Name, "`"),
         Name = str_remove_all(Name, "\\."),
         Name = str_remove_all(Name, " Jr"),
         Name = ifelse(Name == "Maximilian Kleber", "Maxi Kleber", Name),
         Name = str_remove_all(Name, " IV"),
         Name = str_remove_all(Name, " III"),
         Name = tolower(Name),
         Name = ifelse(Name == "mo bamba", "mohamed bamba", Name)) %>%
  rename(rw_proj = proj_points) %>%
  mutate(Name = as.character(Name),
         team = as.character(team),
         rw_proj = as.numeric(rw_proj))



#### Fantasy Pros ####
remDr$navigate("https://www.fantasypros.com")
remDr$screenshot(TRUE)
remDr$navigate("https://www.fantasypros.com/nba/projections/daily-overall.php")

fantasy_pros_html <- remDr$getPageSource("outerHTML")[[1]] %>%
  read_html()

fantasy_pros_scrape_base <- fantasy_pros_html %>%
  html_nodes(xpath = '//*[@id="data"]') %>%
  html_table() %>%
  .[[1]]

fantasy_pros_scrape <- fantasy_pros_scrape_base %>%
  mutate(team = sapply(strsplit(Player, "\\("), `[`, 2),
         team = substr(team,1,3),
         player = sapply(strsplit(Player, " \\("), `[`, 1),
         player = str_remove(player, "'"),
         player = str_remove_all(player, "\\."),
         player = str_remove_all(player, " Jr"),
         player = str_remove_all(player, " IV"),
         player = str_remove_all(player, " III"),
         player = str_remove_all(player, " II"),
         player = tolower(player),
         pts_10 = ifelse(PTS >= 10, 1, 0),
         ast_10 = ifelse(AST >= 10, 1, 0),
         reb_10 = ifelse(REB >= 10, 1, 0),
         dd = ifelse(pts_10+ast_10+reb_10 >= 2,1,0),
         td = ifelse(pts_10+ast_10+reb_10 >= 3,1,0),
         fp_pred = PTS*1+`3PM`*.5+REB*1.25+AST*1.5+STL*2+BLK*2+TO*-.5+dd*1.5+td*3) %>%
  select(player, team, fp_pred) %>%
  rename(Player = player,
         Team = team)

#### Starters ####
rg_lineups_players <- read_html("https://rotogrinders.com/lineups/nba") %>%
  html_nodes(".info:nth-child(2) .player-popup") %>%
  html_text()
rg_starters <- data.frame("Name" = rg_lineups_players) %>%
  mutate(Name = str_remove(Name, "'"),
         Name = str_remove(Name, "`"),
         Name = str_remove_all(Name, "\\."),
         Name = str_remove_all(Name, " Jr"),
         Name = ifelse(Name == "Maximilian Kleber", "Maxi Kleber", Name),
         Name = str_remove_all(Name, " IV"),
         Name = str_remove_all(Name, " III"),
         Name = tolower(Name),
         Name = case_when(
           Name == "mo bamba" ~ "mohamed bamba",
           TRUE ~ Name
         ),
         GS = 1)


#### Create Projections ####
finalScrape <- rotowire_projections %>%
  mutate(team = case_when(
    team == 'CHR' ~ 'CHA',
    team == 'BRO' ~ 'BKN',
    team == "NOR" ~ "NO",
    team == "SAN" ~ "SA",
    TRUE ~team
  )) %>%
  rename(Player = Name,
         Team = team) %>%
  left_join(dffScrape %>%
              # mutate(Projection = ifelse(Team == "MIL", NA, Projection)) %>%
              mutate(Team = case_when(
                Team == "GSW" ~ "GS",
                TRUE ~ Team
              )) %>%
              mutate(Player = case_when(
                Player == "moe harkless" ~ "maurice harkless",
                TRUE ~ Player
              )), 
            by = c("Player", "Team")) %>%
  left_join(nfScrape %>%
              mutate(FP = ifelse(Player == "anthony davis", NA, FP))
              # mutate(Team = case_when(
              #   # Team == "GS" ~ "GSW",
              #   TRUE ~ Team
              # ))
              , by = c("Player", "Team")) %>%
  left_join(sportslineScrape %>%
              mutate(Team = case_when(
                # Team == "GS" ~ "GSW",
                TRUE ~ Team
              ))
            , by = c("Player", "Team")) %>%
  left_join(fantasy_pros_scrape %>%
              mutate(Player = case_when(
                Player == "mo bamba" ~ "mohamed bamba",
                Player == "marcus morris sr" ~ "marcus morris",
                TRUE ~ Player
              )) %>%
              mutate(Team = case_when(
                Team == "GSW" ~ "GS",
                Team == "NOR" ~ "NO",
                Team == "SAS" ~ "SA",
                Team == "NYK" ~ "NY",
                Team == "UTH" ~ "UTA",
                Team == "NOR" ~ "NO",
                TRUE ~ Team
              )), 
            by = c("Player", "Team")) %>%
  rowwise() %>%
  mutate(avg_pred = mean(c(Projection, FP, rw_proj), na.rm = TRUE),
         avg_pred = case_when(
           abs(slProjection-avg_pred)>=4 ~ mean(c(Projection, FP, rw_proj), na.rm = TRUE),
           TRUE ~ mean(c(Projection, FP, slProjection, rw_proj), na.rm = TRUE)
         ),
         max_pred = max(c(Projection, FP, slProjection, rw_proj, fp_pred), na.rm = TRUE),
         min_pred = min(c(Projection, FP, slProjection, rw_proj, fp_pred), na.rm = TRUE),
         st_dev = sd(c(Projection, FP, slProjection, rw_proj, fp_pred), na.rm = TRUE)) %>%
  # mutate(avg_pred = max(c(Projection, FP, rw_proj), na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(prediction = avg_pred) %>%
  mutate(value = (prediction*1000)/Sal) %>%
  mutate(date = Sys.Date()) %>%
  left_join(rg_starters, by = c("Player" = "Name")) %>%
  mutate(GS = ifelse(is.na(GS), 0, 1)) %>%
  left_join(dff_lines, by = c("Team" = "team")) #%>%
  # filter(!Team %in% c("BKN", "CLE", "CHA", "PHI", "SA", "TOR")) %>%
  # filter(!Player %in% c("giannis antetokounmpo"))

test <- finalScrape %>%
  select(Player, Team, Sal, Projection, FP, rw_proj, slProjection, fp_pred, avg_pred, value) %>%
  mutate(sl_to_avg = slProjection-avg_pred,
         fp_to_avg = fp_pred-avg_pred) %>%
  mutate(avg_value = (avg_pred*1000)/Sal)



### Creating Value Charts ####
playerValueCharts <- nbaDFS %>%
  filter(Date >= as.Date("2020-04-13"),
         Date < as.Date("2021-12-17")) %>%
  rename(Team = Tm,
         Name = Player) %>%
  mutate(Player = Name,
         Player = str_remove(Player, "'"),
         Player = str_remove(Player, " Sr."),
         Player = str_remove_all(Player, "\\."),
         Player = str_remove_all(Player, " IV"),
         Player = str_remove_all(Player, " III"),
         Player = str_remove_all(Player, " Jr"),
         Player = tolower(Player),
         Team = ifelse(Team == "NYK", "NY",
                       ifelse(Team == "GS", "GSW",
                              ifelse(Team == "BRK", "BKN",
                                     ifelse(Team == "CHA", "CHO",
                                            ifelse(Team == "SAS", "SA",
                                                   ifelse(Team == "NOP", "NO",
                                                          ifelse(Team == "GSW", "GS",
                                                                 ifelse(Team == "CHO", "CHA", Team))))))))) %>%
  select(Player, Team, fantasyPoints) %>%
  left_join(nbaDFS %>%
              filter(Date >= as.Date("2020-04-13"),
                     Date < as.Date("2021-12-17")) %>%
              rename(Team = Tm,
                     Name = Player) %>%
              mutate(Player = Name,
                     Player = str_remove(Player, "'"),
                     Player = str_remove(Player, " Sr."),
                     Player = str_remove_all(Player, "\\."),
                     Player = str_remove_all(Player, " IV"),
                     Player = str_remove_all(Player, " III"),
                     Player = str_remove_all(Player, " Jr"),
                     Player = tolower(Player),
                     Team = ifelse(Team == "NYK", "NY",
                                   ifelse(Team == "GS", "GSW",
                                          ifelse(Team == "BRK", "BKN",
                                                 ifelse(Team == "CHA", "CHO",
                                                        ifelse(Team == "SAS", "SA",
                                                               ifelse(Team == "NOP", "NO",
                                                                      ifelse(Team == "GSW", "GS",
                                                                             ifelse(Team == "CHO", "CHA", Team))))))))) %>%
              group_by(Player, Team) %>%
              summarise(avg = median(fantasyPoints),
                        t20 = quantile(fantasyPoints, probs = c(.6)),
                        b20 = quantile(fantasyPoints, probs = c(.4))), by = c("Player", "Team")) %>%
  mutate(below25pct = avg*.8,
         above25pct = avg*1.2,
         bust = ifelse(fantasyPoints <= below25pct, 1, 0),
         boom = ifelse(fantasyPoints >= above25pct, 1, 0),
         withinThreshold = ifelse(bust == 0 & boom == 0, 1, 0),
         count = 1) %>%
  group_by(Player, Team, avg, t20, b20) %>%
  summarise(totalGames = sum(count),
            totalBust = sum(bust),
            totalBoom = sum(boom),
            totalWithinT = sum(withinThreshold)) %>%
  mutate(bustPct = totalBust/totalGames,
         boomPct = totalBoom/totalGames,
         thresholdPct = totalWithinT/totalGames) %>%
  filter(totalGames >= 10) %>%
  mutate(t20_pct = (t20-avg)/avg,
         b20_pct = (b20-avg)/avg) %>%
  filter(t20_pct <= .4,
         b20_pct >= -.4) %>%
  select(Player, Team, avg, t20_pct, b20_pct, bustPct, boomPct, thresholdPct) %>%
  ungroup() %>%
  mutate(Player = iconv(Player, "UTF-8", "latin1")) %>%
  mutate(t20_pct_overall = median(t20_pct),
         b20_pct_overall = median(b20_pct)) %>%
  ungroup() #%>%
  # filter(Team = case_when(
  #   Team == "NYK" ~ "NY",
  #   TRUE ~ Team
  # ))

length(unique(finalScrape$Team))

dkSalaries <- read_csv("DKSalaries_12_17_21.csv")
dkSalaries <- dkSalaries %>%
  mutate(Player = Name,
         Player = str_remove(Player, "'"),
         Player = str_remove(Player, " Sr."),
         Player = str_remove_all(Player, "\\."),
         Player = str_remove_all(Player, " IV"),
         Player = str_remove_all(Player, " III"),
         Player = str_remove_all(Player, " Jr"),
         Player = ifelse(Player == "Mo Bamba", "Mohamed Bamba", Player),
         Player = tolower(Player),
         Player = ifelse(Player == "guillermo hernangomez", "willy hernangomez", Player),
         Player = ifelse(Player == "kj martin", "kenyon martin", Player),
         Player = case_when(
           Player == "moe harkless" ~ "maurice harkless",
           TRUE ~ Player
         ),
         TeamAbbrev = ifelse(TeamAbbrev == 'GSW', 'GS', TeamAbbrev),
         TeamAbbrev = ifelse(TeamAbbrev == 'NOR', 'NO', TeamAbbrev),
         TeamAbbrev = ifelse(TeamAbbrev == 'NOP', 'NO', TeamAbbrev),
         TeamAbbrev = ifelse(TeamAbbrev == 'NYK', 'NY', TeamAbbrev),
         TeamAbbrev = ifelse(TeamAbbrev == 'SAS', 'SA', TeamAbbrev),
         TeamAbbrev = ifelse(TeamAbbrev == 'PHX', 'PHO', TeamAbbrev))%>%
  select(Player, TeamAbbrev, `Roster Position`, ID, Salary) %>%
  # rename(Sal = Salary) %>%
  rename(position = `Roster Position`,
         Team = TeamAbbrev) %>%
  mutate(count = 1,
         pg = ifelse(grepl("PG", position), 1, 0),
         sg = ifelse(grepl("SG", position), 1, 0),
         sf = ifelse(grepl("SF", position), 1, 0),
         pf = ifelse(grepl("PF", position), 1, 0),
         c = ifelse(grepl("C", position), 1, 0),
         g = ifelse(grepl("G", position), 1, 0),
         f = ifelse(grepl("F", position), 1, 0),
         onlyC = ifelse(position == "C/UTIL", 1, 0),
         util = ifelse(grepl("UTIL", position), 1, 0),
         position = as.character(position))
length(unique(dkSalaries$Team))




#### Create Optimal Lineup ####
optimization <- finalScrape %>%
  filter(!is.na(prediction)) %>%
  select(Player, Team, Sal, min_pred, max_pred, avg_pred, st_dev, prediction, value) %>%
  left_join(playerValueCharts, by = c("Player", "Team")) %>%
  mutate(t20_pct_overall = max(t20_pct_overall, na.rm = TRUE),
         b20_pct_overall = max(b20_pct_overall, na.rm = TRUE)) %>%
  mutate(t20_pct = ifelse(is.na(t20_pct), t20_pct_overall, t20_pct),
         b20_pct = ifelse(is.na(b20_pct), b20_pct_overall, b20_pct)) %>%
  select(-c(t20, avg, bustPct, boomPct, thresholdPct, t20_pct_overall, b20_pct_overall)) %>%
  inner_join(dkSalaries, by = c("Player", "Team")) %>%
  mutate(t20 = prediction+(prediction*t20_pct),
         b20 = prediction+(prediction*b20_pct),
         value = (prediction*1000)/Salary) %>%
  rowwise() %>%
  # na.omit() %>%
  mutate(newPoints = list(c(seq(from = b20, to = prediction, length.out = 100),
                            seq(from = prediction, to = t20, length.out = 100))),
         Sal = ifelse(is.na(Sal), Salary, Sal),
         selectedPoints = sample(newPoints, 1),
         selectedValue = selectedPoints*1000/Salary) %>%
  select(-newPoints, -min_pred, -max_pred) %>%
  filter(case_when(
    Salary < 4000 ~ value >= 5,
    TRUE ~ TRUE
    # TRUE ~ value >= 4.8
  )) #%>%
  # filter(!Player %in% c("pj tucker"))

num.player <- length(optimization$Player)
var.types <- rep("B", num.player)
# obj <- optimization$selectedPoints
obj <- optimization$prediction

mat <- rbind(optimization$pg,
             optimization$sg,
             optimization$sf,
             optimization$pf,
             optimization$c,
             optimization$c,
             optimization$g,
             # optimization$g,
             # optimization$f,
             optimization$f,
             optimization$onlyC,
             optimization$util,
             optimization$Salary)

dir <- c(">=",
         ">=",
         ">=",
         ">=",
         ">=",
         "<=",
         ">=",
         # "<=",
         # "<=",
         ">=",
         "<=",
         "==",
         "<=")

rhs <- c(1,
         1,
         1,
         1,
         1,
         3,
         3,
         # 5,
         # 5,
         # 3,
         4,
         2,
         8,
         50000)

result <- lp("max", obj, mat, dir, rhs, all.bin = TRUE)   

scrape_optimal_lineup <- optimization[which(result$solution == 1),] %>%
  arrange(position)

header.true <- function(df) {
  names(df) <- as.character(unlist(df[1,]))
  df[-1,]
}

#### Create Multiple Lineups ####
df <- data.frame(
  position = c("C", "F", "G", "PF", "PG", "SF", "SG", "UTIL")
)
df <- t(df) %>% as.data.frame() %>%
  header.true()

for(i in 1:1000){
  print(i)
  
  optimization <- finalScrape %>%
    filter(!is.na(prediction)) %>%
    select(Player, Team, Sal, min_pred, max_pred, avg_pred, st_dev, prediction, value) %>%
    left_join(playerValueCharts, by = c("Player", "Team")) %>%
    mutate(t20_pct_overall = max(t20_pct_overall, na.rm = TRUE),
           b20_pct_overall = max(b20_pct_overall, na.rm = TRUE)) %>%
    mutate(t20_pct = ifelse(is.na(t20_pct), t20_pct_overall, t20_pct),
           b20_pct = ifelse(is.na(b20_pct), b20_pct_overall, b20_pct)) %>%
    select(-c(t20, avg, bustPct, boomPct, thresholdPct, t20_pct_overall, b20_pct_overall)) %>%
    inner_join(dkSalaries, by = c("Player", "Team")) %>%
    mutate(t20 = prediction+(prediction*t20_pct),
           b20 = prediction+(prediction*b20_pct)) %>%
    rowwise() %>%
    # na.omit() %>%
    mutate(newPoints = list(c(seq(from = b20, to = prediction, length.out = 100),
                              seq(from = prediction, to = t20, length.out = 100))),
           value = (prediction*1000)/Salary,
           Sal = ifelse(is.na(Sal), Salary, Sal),
           selectedPoints = sample(newPoints, 1),
           selectedValue = selectedPoints*1000/Salary) %>%
    select(-newPoints, -min_pred, -max_pred) %>%
  left_join(injury_df, by = c("Player", "Team")) %>%
    filter(case_when(
      Salary < 4000 ~ value >= 5,
      TRUE ~ TRUE
      # TRUE ~ value >= 4.8
    )) #%>%
    # filter(Team %in% c('POR', 'GS', 'ORL', 'SAC', 'BOS', 'LAC'))
    # filter(!Player %in% c("pj tucker"))
  
  num.player <- length(optimization$Player)
  var.types <- rep("B", num.player)
  obj <- optimization$selectedPoints
  # obj <- optimization$wgtProj
  
  mat <- rbind(optimization$pg,
               optimization$sg,
               optimization$sf,
               optimization$pf,
               optimization$c,
               optimization$c,
               optimization$g,
               # optimization$g,
               # optimization$f,
               # optimization$pg,
               optimization$f,
               optimization$onlyC,
               optimization$util,
               optimization$Salary)
  
  dir <- c(">=",
           ">=",
           ">=",
           ">=",
           ">=",
           "<=",
           ">=",
           # "<=",
           # "<=",
           # "<=",
           ">=",
           "<=",
           "==",
           "<=")
  
  rhs <- c(1,
           1,
           1,
           1,
           1,
           3,
           3,
           # 5,
           # 5,
           # 3,
           3,
           2,
           8,
           50000)
  
  
  result <- lp("max", obj, mat, dir, rhs, all.bin = TRUE)   
  
  optimalLineupAVG <- optimization[which(result$solution == 1),] %>%
    arrange(position, desc(not_injured))

  a <- optimalLineupAVG %>%
    rowwise() %>%
    mutate(potentialPos = case_when(
      position == "C/UTIL" ~ list(c("C", "UTIL")),
      position == "PF/C/F/UTIL" ~ list(c("C", "PF", "F", "UTIL")),
      position == "PF/F/UTIL" ~ list(c("PF", "F", "UTIL")),
      position == "PG/G/UTIL" ~ list(c("PG", "G", "UTIL")),
      position == "PG/SF/F/G/UTIL" ~ list(c("SF", "PG", "F", "G", "UTIL")),
      position == "PG/SG/G/UTIL" ~ list(c("PG", "SG", "G", "UTIL")),
      position == "SF/F/UTIL" ~ list(c("SF", "F", "UTIL")),
      position == "SF/PF/F/UTIL" ~ list(c("PF", "SF", "F", "UTIL")),
      position == "SG/G/UTIL" ~ list(c("SG", "G", "UTIL")),
      position == "SG/SF/F/G/UTIL" ~ list(c("SF", "SG", "F", "G", "UTIL"))
    )) %>%
    select(ID, potentialPos)

  df1 <- a[1,] %>%
    mutate(position = potentialPos[1])
  df2 <- df1 %>%
    rbind(
      a[2,] %>%
        mutate(position = potentialPos[1],
               position = ifelse(position %in% df1$position, potentialPos[2], position))
    )
  df3 <- df2 %>%
    rbind(
      a[3,] %>%
        mutate(position = potentialPos[1],
               position = ifelse(position %in% df2$position, potentialPos[2], position),
               position = ifelse(position %in% df2$position, potentialPos[3], position))
    )
  df4 <- df3 %>%
    rbind(
      a[4,] %>%
        mutate(position = potentialPos[1],
               position = ifelse(position %in% df3$position, potentialPos[2], position),
               position = ifelse(position %in% df3$position, potentialPos[3], position),
               position = ifelse(position %in% df3$position, potentialPos[4], position))
    )
  df5 <- df4 %>%
    rbind(
      a[5,] %>%
        mutate(position = potentialPos[1],
               position = ifelse(position %in% df4$position, potentialPos[2], position),
               position = ifelse(position %in% df4$position, potentialPos[3], position),
               position = ifelse(position %in% df4$position, potentialPos[4], position),
               position = ifelse(position %in% df4$position, potentialPos[5], position))
    )
  df6 <- df5 %>%
    rbind(
      a[6,] %>%
        mutate(position = potentialPos[1],
               position = ifelse(position %in% df5$position, potentialPos[2], position),
               position = ifelse(position %in% df5$position, potentialPos[3], position),
               position = ifelse(position %in% df5$position, potentialPos[4], position),
               position = ifelse(position %in% df5$position, potentialPos[5], position))
    )
  df7 <- df6 %>%
    rbind(
      a[7,] %>%
        mutate(position = potentialPos[1],
               position = ifelse(position %in% df6$position, potentialPos[2], position),
               position = ifelse(position %in% df6$position, potentialPos[3], position),
               position = ifelse(position %in% df6$position, potentialPos[4], position),
               position = ifelse(position %in% df6$position, potentialPos[5], position))
    )
  df8 <- df7 %>%
    rbind(
      a[8,] %>%
        mutate(position = potentialPos[1],
               position = ifelse(position %in% df7$position, potentialPos[2], position),
               position = ifelse(position %in% df7$position, potentialPos[3], position),
               position = ifelse(position %in% df7$position, potentialPos[4], position),
               position = ifelse(position %in% df7$position, potentialPos[5], position))
    )

  lineup <- df8 %>%
    select(position, ID) %>%
    arrange(position) %>%
    t() %>%
    as.data.frame() %>%
    header.true()
  
  try({
    df <- rbind(df, lineup)
  }, silent = TRUE)
}

write_csv(df %>% select(PG, SG, SF, PF, C, G, `F`, UTIL), paste0("lineups-", Sys.Date(), ".csv"))

#### Testing ####

i <-80

num = 1:i


df_analysis <- df[1:i,]


df_analysis$lineup <- num

colnames(df_analysis) <- c("C", "F", "G", "PF", "PG", "SF", "SG", "UTIL", "lineup")

df_analysis_final <- df_analysis %>%
  gather("position", "player", -lineup) %>%
  arrange(lineup) %>% 
  mutate(player = as.numeric(str_extract(player, "[0-9]+"))) %>%
  mutate(player = as.character(player)) %>%
  left_join(dkSalaries %>%
              mutate(ID = as.character(ID)) %>%
              select(ID, Player), by = c("player" = "ID")) %>%
  left_join(nbaDFS %>%
              filter(Date == as.Date("2021-11-10")) %>%
              select(Player, fantasyPoints) %>%
              mutate(Player = str_remove(Player, "'"),
                     Player = str_remove(Player, " Sr."),
                     Player = str_remove_all(Player, "\\."),
                     Player = str_remove_all(Player, " IV"),
                     Player = str_remove_all(Player, " III"),
                     Player = str_remove_all(Player, " Jr")) %>%
              mutate(Player = iconv(Player, "UTF-8", "latin1"),
                     Player = to.plain(Player),
                     Player = tolower(Player),
                     Player = ifelse(Player == "juan hernangomez", "juancho hernangomez", Player)), by = "Player") %>%
  group_by(lineup) %>%
  summarise(score = sum(fantasyPoints, na.rm = TRUE))

 player_summary <- df_analysis %>%
  gather("position", "player", -lineup) %>%
  arrange(lineup) %>% 
   mutate(player = as.numeric(str_extract(player, "[0-9]+"))) %>%
  mutate(player = as.character(player)) %>%
  left_join(dkSalaries %>%
              mutate(ID = as.character(ID)) %>%
              select(ID, Player, Salary), by = c("player" = "ID")) %>%
  group_by(Player, Salary) %>%
  summarize(total = n()) %>%
  ungroup() %>%
  mutate(pct_played = total/i) %>%
  left_join(finalScrape %>%
              select(Player, Team, avg_pred, value),
            by = "Player"
            ) %>%
  left_join(nbaDFS %>%
              filter(Date == as.Date("2021-11-10")) %>%
              select(Player, fantasyPoints) %>%
              mutate(Player = str_remove(Player, "'"),
                     Player = str_remove(Player, " Sr."),
                     Player = str_remove_all(Player, "\\."),
                     Player = str_remove_all(Player, " IV"),
                     Player = str_remove_all(Player, " III"),
                     Player = str_remove_all(Player, " Jr")) %>%
              mutate(Player = iconv(Player, "UTF-8", "latin1"),
                     Player = to.plain(Player),
                     Player = tolower(Player),
                     Player = ifelse(Player == "juan hernangomez", "juancho hernangomez", Player)), by = "Player") %>%
  mutate(expected_points = (Salary/1000)*5,
         hot = ifelse(fantasyPoints >= expected_points*1.2, 1, 0),
         cold = ifelse(fantasyPoints <= expected_points*.8, 1, 0))

max(df_analysis_final$score)
median(df_analysis_final$score)
mean(df_analysis_final$score)
nrow(df_analysis_final %>%
       filter(score >= 270.5))



