library(rvest)
library(tidyverse)
library(runner)
library(zoo)

rD <- rsDriver(browser="firefox", port=4545L, verbose=F)
remDr <- rD[["client"]]

remDr$open()
remDr$navigate("https://stathead.com/basketball")
remDr$screenshot(TRUE)

webElem10 <- remDr$findElement(using = "xpath", "/html/body/div[2]/div[1]/ul/li[13]/a")
webElem10$clickElement()

Sys.sleep(2)

webElem11 <- remDr$findElement(using = 'xpath', '//*[@id="username"]')
webElem11$sendKeysToElement(list("ENTER YOUR USERNAME"))

Sys.sleep(2)

webElem12 <- remDr$findElement(using = 'xpath', '//*[@id="password"]')
webElem12$sendKeysToElement(list("ENTER YOUR PASSWORD"))

webElem13 <- remDr$findElement(using = 'xpath', '/html/body/div[2]/div[2]/form/div[4]/div/input')
webElem13 <- webElem13$clickElement()

remDr$navigate("https://stathead.com/basketball/pgl_finder.cgi?request=1&match=game&order_by_asc=0&order_by=date_game&year_min=2020&lg_id=NBA&is_playoffs=N&age_min=0&age_max=99&season_start=1&season_end=-1&positions%5B%5D=G&positions%5B%5D=GF&positions%5B%5D=F&positions%5B%5D=FG&positions%5B%5D=FC&positions%5B%5D=C&positions%5B%5D=CF&game_month=0&offset=000")

link <- remDr$getPageSource("outerHTML")[[1]] %>%
  read_html()

table <- link%>%
  html_nodes(xpath = '//*[@id="stats"]') %>%
  html_table()

table <- table[[1]]
table <- table[-c(21, 42, 63, 84),-8]

for(i in 1:10) {
  print(i)
  
  remDr$navigate(paste0("https://stathead.com/basketball/pgl_finder.cgi?request=1&match=game&order_by_asc=0&order_by=date_game&year_min=2020&lg_id=NBA&is_playoffs=N&age_min=0&age_max=99&season_start=1&season_end=-1&positions%5B%5D=G&positions%5B%5D=GF&positions%5B%5D=F&positions%5B%5D=FG&positions%5B%5D=FC&positions%5B%5D=C&positions%5B%5D=CF&game_month=0&offset=", i, "00"))
  
  # link <- read_html(paste0("https://www.basketball-reference.com/play-index/pgl_finder.cgi?request=1&player_id=&match=game&year_min=2020&year_max=2020&age_min=0&age_max=99&team_id=&opp_id=&season_start=1&season_end=-1&is_playoffs=N&draft_year=&round_id=&game_num_type=&game_num_min=&game_num_max=&game_month=&game_day=&game_location=&game_result=&is_starter=&is_active=&is_hof=&pos_is_g=Y&pos_is_gf=Y&pos_is_f=Y&pos_is_fg=Y&pos_is_fc=Y&pos_is_c=Y&pos_is_cf=Y&c1stat=&c1comp=&c1val=&c1val_orig=&c2stat=&c2comp=&c2val=&c2val_orig=&c3stat=&c3comp=&c3val=&c3val_orig=&c4stat=&c4comp=&c4val=&c4val_orig=&is_dbl_dbl=&is_trp_dbl=&order_by=date_game&order_by_asc=&offset=", i, "00"))

  link <- remDr$getPageSource("outerHTML")[[1]] %>%
    read_html()
  
  tableTemp <- link%>%
    html_nodes(xpath = '//*[@id="stats"]') %>%
    html_table()

  tableTemp <- tableTemp[[1]]
  tableTemp <- tableTemp[-c(21, 42, 63, 84),-8]

  table <- rbind(table, tableTemp)

  Sys.sleep(1)
}

colnames(table)[6] <- "Home_Away"

nbaData <- read_csv("nbaData2014-2020.csv") %>%
  arrange(Date) #%>%
  # filter(Date <= as.Date("2020-08-16"))

table <- table %>%
  mutate(Date = as.Date(Date),
         home = ifelse(is.na(Home_Away), 1, 0)) %>%
  filter(Date > max(nbaData$Date)) %>%
  select(colnames(nbaData)) %>%
  arrange(Date)

nbaData <- nbaData %>%
  mutate(Date = as.Date(Date))

nbaData <- rbind(nbaData, table) #%>%
  # mutate(home = ifelse(is.na(Home_Away), 1, 0))

write_csv(nbaData, "nbaData2014-2020.csv")

nbaDFS <- nbaData %>%
  mutate(PTS = as.numeric(PTS),
         TOV = as.numeric(TOV),
         BLK = as.numeric(BLK),
         STL = as.numeric(STL),
         AST = as.numeric(AST),
         TRB = as.numeric(TRB),
         `3P` = as.numeric(`3P`),
         FG = as.numeric(FG),
         FT = as.numeric(FT),
         FGA = as.numeric(FGA),
         ORB = as.numeric(ORB),
         FTA = as.numeric(FTA),
         DRB = as.numeric(DRB),
         MP = as.numeric(MP),
         PF = as.numeric(PF),
         PTS10 = ifelse(PTS >= 10, 1, 0),
         BLK10 = ifelse(BLK >= 10, 1, 0),
         STL10 = ifelse(STL >= 10, 1, 0),
         AST10 = ifelse(AST >= 10, 1, 0),
         TRB10 = ifelse(TRB >= 10, 1, 0),
         total = PTS10 + BLK10 + STL10 + AST10 + TRB10,
         doubleDouble = ifelse(total >= 2, 1, 0),
         tripleDouble = ifelse(total >= 3, 1, 0),
         Player = str_remove(Player, "'"),
         Player = str_remove(Player, " Sr."),
         Player = str_remove_all(Player, "\\."),
         Player = str_remove_all(Player, " IV"),
         Player = str_remove_all(Player, " III"),
         Player = str_remove_all(Player, " Jr")) %>%
  mutate(fantasyPoints = (PTS*point+`3P`*made3+TRB*rebound+AST*assist+STL*steal+BLK*block+TOV*turnover+doubleDouble*double+tripleDouble*triple)) %>%
  arrange(Date) %>%
  filter(Date > as.Date("2009-01-11"))


### PER Stats ####
league_assists <- mean(nbaDFS$AST)
league_fg <- mean(nbaDFS$FG)
league_ft <- mean(nbaDFS$FT)
league_pts <- mean(nbaDFS$PTS)
league_fga <- mean(nbaDFS$FGA)
league_orb <- mean(nbaDFS$ORB)
league_to <- mean(nbaDFS$TOV)
league_fta <- mean(nbaDFS$FTA)
league_trb <- mean(nbaDFS$TRB)
league_pf <- mean(nbaDFS$PF)

per_factor <- (2/3)-((.5*(league_assists/league_fg))/(2*(league_fg/league_ft)))
per_vop <- league_pts/(league_fga-league_orb+league_to+(.44*league_fta))
per_drdp <- (league_trb-league_orb)/league_trb

pace_df <- nbaDFS %>%
  group_by(Tm, Opp, Date) %>%
  summarise(team_fg = sum(FG),
            team_fta = sum(FTA),
            team_orb = sum(ORB),
            team_fga = sum(FGA),
            team_to = sum(TOV),
            team_drb = sum(DRB),
            team_mp = sum(MP)) %>%
  ungroup() %>%
  left_join(nbaDFS %>%
              group_by(Opp, Date) %>%
              summarise(opp_fg = sum(FG),
                        opp_fta = sum(FTA),
                        opp_orb = sum(ORB),
                        opp_fga = sum(FGA),
                        opp_to = sum(TOV),
                        opp_drb = sum(DRB)) %>%
              ungroup() %>%
              rename(Tm = Opp), by = c("Tm", "Date")) %>%
  mutate(team_pos = ((team_fga+.4*team_fta-1.07*(team_orb/(team_orb+opp_orb))*(team_fga-team_fg)+team_to)+(opp_fga+.4*opp_fta-1.07*(opp_orb/(opp_orb+team_drb))*(opp_fga-opp_fg)+opp_to))) %>%
  select(Tm, Opp, Date, team_mp, team_pos) %>%
  left_join(nbaDFS %>%
              group_by(Tm, Opp, Date) %>%
              summarise(team_fg = sum(FG),
                        team_fta = sum(FTA),
                        team_orb = sum(ORB),
                        team_fga = sum(FGA),
                        team_to = sum(TOV),
                        team_drb = sum(DRB),
                        team_mp = sum(MP)) %>%
              ungroup() %>%
              left_join(nbaDFS %>%
                          group_by(Opp, Date) %>%
                          summarise(opp_fg = sum(FG),
                                    opp_fta = sum(FTA),
                                    opp_orb = sum(ORB),
                                    opp_fga = sum(FGA),
                                    opp_to = sum(TOV),
                                    opp_drb = sum(DRB)) %>%
                          ungroup() %>%
                          rename(Tm = Opp), by = c("Tm", "Date")) %>%
              mutate(opp_pos = ((team_fga+.4*team_fta-1.07*(team_orb/(team_orb+opp_orb))*(team_fga-team_fg)+team_to)+(opp_fga+.4*opp_fta-1.07*(opp_orb/(opp_orb+team_drb))*(opp_fga-opp_fg)+opp_to))) %>%
              select(Opp, Date, opp_pos), by = c("Tm" = "Opp", "Date")) %>%
  mutate(team_pace = 48*((team_pos+opp_pos)/(2*(team_mp/5)))) %>%
  select(Date, Tm, Opp, team_pace)

league_pace <- mean(pace_df$team_pace)

nbaDFS <- nbaDFS %>%
  group_by(Tm, Date) %>%
  mutate(team_ast = sum(AST),
         team_fg = sum(FG)) %>%
  ungroup() %>%
  left_join(pace_df, by = c("Tm", "Opp", "Date")) %>%
  mutate(uPER = (1/MP)*(`3P`-((PF*league_ft)/league_pf)
                        +((FT/2)*(2-(team_ast/(3*team_fg))))
                        +(FG*(2-((per_factor*team_ast)/team_fg)))
                        +(2*AST)/3+per_vop
                        *(per_drdp*(2*ORB+BLK-.2464*(FTA-FT)-(FGA-FG)-TRB)
                          +((.44*league_fga*PF)/league_pf)-(TOV+ORB)+STL+TRB
                          -.1936*(FTA-FT)))) %>%
  filter(!uPER == Inf,
         !uPER == -Inf)

league_per <- mean(nbaDFS$uPER)

nbaDFS <- nbaDFS %>%
  mutate(PER = (uPER*(league_pace/team_pace))*(15/league_per)) %>%
  filter(PER <= 70,
         PER >= -20)

to.plain <- function(s) {

  # 1 character substitutions
  old1 <- "šžþàáâãäåçèéêëìíîïðñòóôõöùúûüý"
  new1 <- "szyaaaaaaceeeeiiiidnooooouuuuy"
  s1 <- chartr(old1, new1, s)

  # 2 character substitutions
  old2 <- c("œ", "ß", "æ", "ø")
  new2 <- c("oe", "ss", "ae", "oe")
  s2 <- s1
  for(i in seq_along(old2)) s2 <- gsub(old2[i], new2[i], s2, fixed = TRUE)

  s2
}

Encoding("šžþàáâãäåçèéêëìíîïðñòóôõöùúûüý")
Encoding("szyaaaaaaceeeeiiiidnooooouuuuy")

nbaDFS <- nbaDFS %>%
    mutate(Player = iconv(Player, "UTF-8", "latin1"),
           Player = to.plain(Player))

