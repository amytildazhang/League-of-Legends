#What is this?

These are side projects I undertook for fun after noticing some things in Riot's League of Legends broadcast that could be improved. League of Legends is the most popular Multiplayer Online Battleground Arena (MOBA) game in the world, and supports (what I believe is) one of the largest and most financially stable Esports ecosystems. It has been compared to MMA, which in its beginnings was a hugely lucrative competitive sport that only its fans seemed to know about. The big-name pro players in League of Legends often earn millions of dollars.

It's a new scene that's only just beginning to develop its own statistics, so it's an exciting time to think about all the possibilities for statistics in the field. But Riot only makes aggregate stats at the end of a game publicly available, so the code here is limited to a commentary on what Riot is already doing, based on those aggregate stats.

================================

#File genealogy

- Data
  Excel files with match data from [Oracle's Elixir](oracleselixir.com). 
  Edited so the columns match (removed oppcs, etc, columns)

- Damage\_Rating
  League of Legends is a heavily team-centric game, but one measurement of individual player skill that viewers and analysts like to use is damage output. In League, the ability to remove opposing players from the map frees up time and space for your team to take objectives. Central to that is doing damage. However, simply using damage output is misleading, because depending on the champions (characters) that each player takes, the expected damage output differs dramatically. This confounds player skill at outputting damage with the inherent abilities of a champion. 

  To help address this, Riot released a new "damage rating" statistic which calculated percent difference from expected damage for each player in each game. The statistic is damage to champs per minute, adjusted for champion, game result, and time spent around enemies. I compare Riot's damage rating to mine and make the argument for why "game result" is a poor adjustment to make, and why "gold spent" should be accounted for.

    - dmg\_rating.R 
      Main code to implement Riot's model vs mine. Outputs graphs and CSV files.

    - LPRE.R 
      Code to calculate the least product relative estimator, courtesy of Professors Zhanfeng Wang and Kani Chen. Code and description are available at [http://202.38.64.11/~zfw/LPRE.htm](http://202.38.64.11/~zfw/LPRE.htm). 

    - Output
      Contains folders for each model implemented, with diagnostics and output graphs. The graphs are quite nice, recommend checking them out! 

- LeadDeficit\_Numbers\_w\_Context
  League of Legends is a game that often centers around gold. Gold is earned from killing enemies and taking objectives. It is used to equip each champion with stronger items that allow them to kill more enemies and take more objectives--a snowball effect that, when done well, will often earn them the game. One metric that is used to judge a team's skill is the gold difference between teams at given time points of the game--15 minutes or 20 minutes is the most common, as it is late enough into the game that the gold differences have meaning, but still considered to be the "early game," where the game outcome has not been decided yet. 

  Occasionally Riot will compare a team's gold lead at 20 minutes to their average. However, because teams often have deficits, this average is not representative of the average gold lead. I show this with a few graphs and, for fun, also compare how well teams within each league are able to snowball their lead or come back from a deficit.

    - lead\_deficit\_15mins.R
      Code to output graphs in `Output'

    - Output
      Contains histograms of each team's gold lead at 15 minutes, and graphs comparing their relative successes with those leads. 
