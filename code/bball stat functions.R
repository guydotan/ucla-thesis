##### All functions used throughout the code to calculate team statistics #####

## function to calculate game possessions ##
calc_poss <- function ( fga, to, fta, oreb ) {
  if (is.na(fga) | is.na(to) | is.na(fta) | is.na(oreb)){
    poss <- NA
  } else {
    poss <- 0.96*((fga)+(to)+0.44*(fta)-(oreb))
  }
  return( poss )
}

## advanced function to calculate game possessions ##
calc_poss <- function ( TmFGA, TmFTA, TmORB, TmDRB, TmFG, TmTOV, 
                        OppFGA, OppFTA, OppORB, OppDRB, OppFG, OppTOV ) {
  if (is.na(TmFGA) | is.na(TmFTA) | is.na(TmORB) | is.na(TmDRB) | is.na(TmFG) | is.na(TmTOV) |
      is.na(OppFGA) | is.na(OppFTA) | is.na(OppDRB) | is.na(OppORB) | is.na(OppFG) | is.na(OppTOV)){
    poss <- NA
  } else {
    poss <- 0.5 * ((TmFGA + 0.4 * TmFTA - 1.07 * (TmORB / (TmORB + OppDRB)) * 
                      (TmFGA - TmFG) + TmTOV) + (OppFGA + 0.4 * OppFTA - 1.07 * 
                      (OppORB / (OppORB + TmDRB)) * (OppFGA - OppFG) + OppTOV))
  }
  return( poss )
}

## function to calculate game pace (possessions per 48) ##
calc_pace <- function ( own_poss, opp_poss, min ) {
  if (is.na(own_poss) | is.na(opp_poss) | is.na(min)){
    pace <- NA
  } else {
    pace <- 48 * ((own_poss + opp_poss) / (2 * (min / 5)))
  }
  return( pace )
}

# function to calculate offensive rating (total points per 100 possessions) ##
calc_ortg <- function ( pts, poss ) {
  if (is.na(pts) | is.na(poss)){
    ortg <- NA
  } else {
    ortg <- pts / poss * 100
  }
  return( ortg )
}

# function to calculate defensive rating (total points allowed per 100 possesions) ##
calc_drtg <- function ( opp_pts, opp_poss ) {
  if (is.na(opp_pts) | is.na(opp_poss)){
    drtg <- NA
  } else {
    drtg <- opp_pts / opp_poss * 100
  }
  return( drtg )
}

# function to stat per 100 possessions) ##
calc_per100 <- function ( stat, poss ) {
  if (is.na(stat) | is.na(poss)){
    per100 <- NA
  } else {
    per100 <- stat / poss * 100
  }
  return( per100 )
}
