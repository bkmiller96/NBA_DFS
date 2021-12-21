# NBA_DFS
In the attached scripts, I scrape NBA DFS projections and then use an optimization model to create lineups to play on draftkings. To run through everything, you take the following steps:

**Run the historical data scrape**
For this piece of it, run the historical data scrape as far back as you want to go. To start, I pulled in every player game log since 2010 to leverage for analysis and some modeling attempts. I also scraped seperately historical betting lines to use for modeling. As a side note, I've constantly used this data set to build models to predict player fantasy performance. Some variables I found to be effective were usage rate, player PER trend, players injured on team, team efficiency/pace, opponent efficiency/pace, opponent defensive ratings, betting lines/spreads, etc. Some models included (in order of effectiveness from worst to best):
- Basic Linear Regression: This always underpredicted the top players and overpredicted the bottom players
- Poisson model: This was better at differentiating predictions and an improvement over the linear regression but still not as accurate as my average of sources model
- h2o ML model: Several of these had the tendancy to overfit but I was able to leverage pca variables to help. This was very close in accuracy to my average model but did not surpath it
- 2 parts (Minutes Played and Points per minute) variable: In this model, I projected the fantasy points per minute for players each night then did a seperate projection for minutes played. By seperating these 2, I was able to create a model that nearly bested by average of sources.

Recently, I haven't had as much time to work through these but the goal is to eventually be able to use my own model entirely in DFS
