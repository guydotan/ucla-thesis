##### All functions used throughout the code #####

## function to convert betting moneyline to win probability ##
ml_to_prob <- function ( ml ) {
  if (is.na(ml)){
    prob <- NA
  } else if (ml < 0) {
    prob <- (ml * -1) / ( (-1 * ml) + 100 )
  } else {
    prob <- ( 100 ) / ( ml + 100 )  
  }
  return( prob )
}

## function to convert win probability to betting moneyline ##
prob_to_ml <- function ( prob ) {
  # ensure user enters a number between 0 and 1
  prob <- ifelse(prob > 1 || prob < 0, return("Please enter a probability between 0 and 1"), prob)
  
  # formula to convert from Prob to ML
  if (is.na(ml)){
    prob <- NA
  } else if  (prob > .5) {
    ml <- -1 * ( prob ) / ( 1 - prob ) * 100
  } else {
    ml <- ( 1 - prob ) / prob * 100
  }
  return( ml )
}

## function to convert moneyline to betting odds ##
ml_to_odds <- function ( ml ) {
  if (is.na(ml)){
    odds <- NA
  } else if (ml < 0) {
    odds <- ( 100 ) / ( ml * -1 )
  } else {
    odds <- ( ml ) / ( 100 )  
  }
  return( odds )
}


## function to calculate actual probability ##
act_prob <- function ( team , opp ) {
  
  # actual probability formula
  imp_team_odds <- ml_to_prob( team )
  imp_opp_odds <- ml_to_prob( opp )
  team_odds <- imp_team_odds / (imp_team_odds + imp_opp_odds) 
  opp_odds <- imp_opp_odds / (imp_team_odds + imp_opp_odds)
  
  result_list <- list( "team" = team_odds , "opp" = opp_odds )
  
  return(result_list)
}


##### Functions for Kelly Criteria #####

## function to convert probabilties into odds ##
prob_to_odds <- function ( prob ) {
  if ( prob > 1 | prob < 0  ){
    odds <- NA
  } else
    odds <- (1 - prob) / prob
  return( odds )
}


## function to calculate Kelly Criteria (percentage of bankroll to bet) ##
kelly_crit <- function ( p , b ) {
  q = 1 - p
  if ( p > 1 | p < 0 | b < 0 ){
    kc <- NA
  } else
    kc <- (p * b - q)/b
  return( kc )
}


## function to calculate bet amount based on Kelly Criteria ##
bet_amt <- function ( bank , kc ){
  if( kc < 0 ){
    amt <- 0
  } else{
    amt <- kc*bank
  }
  return( amt )
}


## function to calculate present value of your bankroll based on the Kelly Criteria bet amount ##
bankroll <- function ( bank , ml , amt , win ){
  if( win == TRUE ){
    tot <- bank + ml_pay(ml, amt)
  } else  {
    tot <- bank - amt
  }
  return( tot )
}


## function to calculate moneyline payout ##
ml_pay <- function ( ml , amt ){
  if (ml > 0){
    pay <- ml / 100 * amt
  } else {
    pay <- 100 / (ml * -1) * amt 
  }
  return( pay )
}
