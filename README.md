# NBA_DFS
In the attached scripts, I scrape NBA DFS projections and then use an optimization model to create lineups to play on draftkings. Over the last year, I've profited over $4,000 from this set of code and strategy. The basis behind my modeling and strategy is that the theory of the crowd always is more accurate than any individual. In testing, I've found that the average of a good set of sources is always more accurrate than any one source. The key word on that is good because bad sources can also drag down the good ones. An analogy to this is that if you are in college and want to learn more about math, you would likely go to a math professor and not an arts instructor. The same goes vice versa and to ensure accuracy of my projections, I always test new sources for at least a month before adding them into my model. To run through everything, you take the following steps:

**Run the historical data scrape**
For this piece of it, run the historical data scrape as far back as you want to go. To start, I pulled in every player game log since 2010 to leverage for analysis and some modeling attempts. I also scraped seperately historical betting lines to use for modeling. As a side note, I've constantly used this data set to build models to predict player fantasy performance. Some variables I found to be effective were usage rate, player PER trend, players injured on team, team efficiency/pace, opponent efficiency/pace, opponent defensive ratings, betting lines/spreads, etc. Some models included (in order of effectiveness from worst to best):
- Basic Linear Regression: This always underpredicted the top players and overpredicted the bottom players
- Poisson model: This was better at differentiating predictions and an improvement over the linear regression but still not as accurate as my average of sources model
- h2o ML model: Several of these had the tendancy to overfit but I was able to leverage pca variables to help. This was very close in accuracy to my average model but did not surpath it
- 2 parts (Minutes Played and Points per minute) variable: In this model, I projected the fantasy points per minute for players each night then did a seperate projection for minutes played. By seperating these 2, I was able to create a model that nearly bested by average of sources.

Recently, I haven't had as much time to work through these but the goal is to eventually be able to use my own model entirely in DFS

**Scrape Projections**
- Scrape NumberFire using selenium
- Scrape sportsline using fromJSON
- Scrape rotowire using from JSON
- Get daily fantasy fuel by downloading a csv from their site
- Scrape fantasypros using selenium
- Scrape starters from rotogrinders using read_html

**Combine Projections, Create Value Charts, and Load Draftkings Salaries**
In it's current state, my final projection is the average of rotowire, numberfire, and daily fantasy fuel, then using sportsline where it does not significantly differ from the other sources. Sportsline tends not to update with injuries as well as others but including their projections when they are not drastically different has consistently improved my model. FantasyPros is a site that I added for testing this year but has not been good enough to be included. 

The value charts are a look at the last season and a half of games and the range of outcomes for each player. Those range of outcomes are then applied to my simulation where I mass generate lineups. After testing this for 2 years, I've found that using the 40th-60th percentile of results for each player has generated the best lineups. While it is important to have known noise in the dataset, you also have to have a level of trust in your base projections and too much noise will make the model worse. Once that is accounted for, I download and clean the draftkings salaries

**Create 1 optimal lineup for single entry tournaments and multiple lineups for mass entry tournaments**
The single entry is a straight forward optimizaiton that gives you the optimal lineup for each slate. The mass lineup generator using the range of outcomes for each player discussed above to create a set of lineups that fit in the most likely outcomes for the night. Each loop through then normalizes the lineup to match the entry format to load lineups as a csv directly to draftkings. Doing so enables me to load 1000+ unique lineups to draftkings in only a minute or 2.

**Testing varialbes**
Each season, I do a test run the day after a tournament to look for weak spots in my model and how different adjustments would fair. I also keep all of my projections for each day stored in a database so that I can do a historical test on any adjustment I want to make.


