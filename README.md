# UCLA MAS Thesis - 2020
## _Beating the Book: A Machine Learning Approach to Identifying an Edge in NBA Betting Markets_

### Overview
Final thesis submitted in June 2020 for the Masters of Applied Statistics program at UCLA. The research covers three topics with the goal of building a model to beat the betting lines in predicting winner of an NBA matchup.

#### 1. Moneyline odds to win probabilities
An analysis of NBA moneylines from 2007-08 to 2019-20 such that we remove the "vig" and are left with the true win probability for each team.

#### 2. Model building to predict matchup winner
Model building using four different methods (logistic regression, random forest, XGBoost, and neural networks) to determine a win probability (based on pace-adjusted box score metrics) for each team in the matchup

#### 3. Various betting strategies 
Implementation of several betting strategies including a fixed wager technique and using Kelly criteria (and several fractional Kellys).  


### Contents

Codebase  - 

Full thesis text -  thesis\_gd\_2020.pdf

Presentation Outline - thesis\_overview.pdf

Executive Summary -  


### Some references
* Data
	* Betting odds: https://sportsbookreviewsonline.com/scoresoddsarchives/mlb/mlboddsarchives.htm
* Betting formula:
	* source: https://www.legitgamblingsites.com/online-betting/calculating-odds/
	* source 2: https://www.oddscoach.com/implied-probability/
	* removing vig: https://www.sportsbettingdime.com/guides/strategy/removing-the-vig/ 
* Kelly Criteria
	* https://www.sportsbookreview.com/forum/handicapper-think-tank/29009-expected-value-vs-expected-growth-kelly-criterion-part-i.html#post250260
	* https://www.sportsbookreview.com/forum/handicapper-think-tank/29841-maximizing-expected-growth-kelly-criterion-part-ii.html
	* https://www.youtube.com/watch?v=k6gqlK6UZhQ
* NBA API
	* http://practicallypredictable.com/2017/12/21/web-scraping-nba-team-matchups-box-scores/


![uncutgems](images/uncut.jpg)
### &copy; Guy Dotan - June 2020
