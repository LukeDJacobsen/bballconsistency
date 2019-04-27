#'
#'NBA Player consistency estimates for a player during a season.
#'
#'Calculated consistency metrics for NBA player for a season
#'
#' @param player url link for player. For example, 't/townska01' for Karl Anthony Towns. Link can be found for desired player at 'https://www.basketball-reference.com/players/'
#' @param season A numeric year
#' @param metrics One of 'basic' or 'advanced'.
#' @param which_metrics_basic Any subset of ('MP', 'GS', 'FG', 'FGA', 'FG%', "3P","3PA", "3P%", "FT", "FTA", "FT%","ORB", "DRB", "TRB", "AST", "STL","BLK", "TOV", "PF", "PTS", "GmSc", "+/-").
#'          Chosen statistics will be computed if metrics = 'basic'.
#' @param which_metrics_advanced Any subset of ('MP',"GS","TS%", "eFG%", 'ORB%', 'DRB%','TRB%', 'AST%',"STL%", "BLK%", "TOV%", "USG%", "ORtg","DRtg", 'GmSc').
#'          Chosen statistics will be computed if metrics = 'advanced'
#'
#'@return A list with three element. The first element is a vector with the percent of games played and percent of games started out of total games played.
#'Second element is vector with average statistic per game. The third element is the variance of each statistic between games. Note if 'GS' is not included in which_metrics argument, the first element of list will not be computed.
#'
#'
#'
#'@example
#'player_consistency('t/townska01', season = 2016, metrics = 'basic')
#'
#'@importFrom magrittr %>%
#'@importFrom stats sd
#'
#' @export


player_consistency <- function(player, season, metrics = 'basic',
                               which_metrics_basic = c('MP', 'GS', 'FG', 'FGA', 'FG%', "3P",
                                                "3PA", "3P%", "FT", "FTA", "FT%",
                                                "ORB", "DRB", "TRB", "AST", "STL",
                                                "BLK", "TOV", "PF", "PTS", "GmSc", "+/-"),
                               which_metrics_advanced = c('MP',"GS","TS%", "eFG%", 'ORB%', 'DRB%','TRB%', 'AST%',
                                                         "STL%", "BLK%", "TOV%", "USG%", "ORtg",
                                                         "DRtg", 'GmSc')){
  #get player stats
  get_stats <- player_stats(player, season, metrics)
  #initiate data.frame
  clean_stat <- data.frame(Game = get_stats$G)
  #determine which metric and includes those included in which metrics
  if (metrics == 'advanced'){
    if ('MP' %in% which_metrics_advanced){clean_stat$MP <- get_stats$MP}
    if ('GS' %in% which_metrics_advanced){clean_stat$GS <- get_stats$GS}
    if ('TS%' %in% which_metrics_advanced){clean_stat$'TS%' <- get_stats$'TS%'}
    if ('eFG%' %in% which_metrics_advanced){clean_stat$'eFG%' <- get_stats$'eFG%'}
    if ('ORB%' %in% which_metrics_advanced){clean_stat$'ORB%' <- get_stats$'ORB%'}
    if ('DRB%' %in% which_metrics_advanced){clean_stat$'DRB%' <- get_stats$'DRB%'}
    if ('TRB%' %in% which_metrics_advanced){clean_stat$'TRB%' <- get_stats$'TRB%'}
    if ('AST%' %in% which_metrics_advanced){clean_stat$'AST%' <- get_stats$'AST%'}
    if ('STL%' %in% which_metrics_advanced){clean_stat$'STL%' <- get_stats$'STL%'}
    if ('BLK%' %in% which_metrics_advanced){clean_stat$'BLK%' <- get_stats$'BLK%'}
    if ('TOV%' %in% which_metrics_advanced){clean_stat$'TOV%' <- get_stats$'TOV%'}
    if ('USG%' %in% which_metrics_advanced){clean_stat$'USG%' <- get_stats$'USG%'}
    if ('ORtg' %in% which_metrics_advanced){clean_stat$ORtg <- get_stats$ORtg}
    if ('DRtg' %in% which_metrics_advanced){clean_stat$DRtg <- get_stats$DRtg}
    if ('GmSc' %in% which_metrics_advanced){clean_stat$GmSc <- get_stats$GmSc}
  }
  if (metrics == 'basic'){
    if ('MP' %in% which_metrics_basic){clean_stat$MP <- get_stats$MP}
    if ('GS' %in% which_metrics_basic){clean_stat$GS <- get_stats$GS}
    if ('FG' %in% which_metrics_basic){clean_stat$FG <- get_stats$FG}
    if ('FGA' %in% which_metrics_basic){clean_stat$FGA <- get_stats$FGA}
    if ('FG%' %in% which_metrics_basic){clean_stat$'FG%' <- get_stats$'FG%'}
    if ('3P' %in% which_metrics_basic){clean_stat$'3P' <- get_stats$'3P'}
    if ('3PA' %in% which_metrics_basic){clean_stat$'3PA' <- get_stats$'3PA'}
    if ('3P%' %in% which_metrics_basic){clean_stat$'3P%' <- get_stats$'3P%'}
    if ('FT' %in% which_metrics_basic){clean_stat$'FT' <- get_stats$FT}
    if ('FTA' %in% which_metrics_basic){clean_stat$FTA <- get_stats$FTA}
    if ('FT%' %in% which_metrics_basic){clean_stat$'FT%' <- get_stats$'FT%'}
    if ('ORB' %in% which_metrics_basic){clean_stat$ORB <- get_stats$ORB}
    if ('DRB' %in% which_metrics_basic){clean_stat$DRB <- get_stats$DRB}
    if ('TRB' %in% which_metrics_basic){clean_stat$TRB <- get_stats$TRB}
    if ('AST' %in% which_metrics_basic){clean_stat$AST <- get_stats$AST}
    if ('STL' %in% which_metrics_basic){clean_stat$STL <- get_stats$STL}
    if ('BLK' %in% which_metrics_basic){clean_stat$BLK <- get_stats$BLK}
    if ('TOV' %in% which_metrics_basic){clean_stat$TOV <- get_stats$TOV}
    if ('PF' %in% which_metrics_basic){clean_stat$PF <- get_stats$PF}
    if ('PTS' %in% which_metrics_basic){clean_stat$PTS <- get_stats$PTS}
    if ('GmSc' %in% which_metrics_basic){clean_stat$GmSc <- get_stats$GmSc}
    if ('+/-' %in% which_metrics_basic){clean_stat$'+/-' <- get_stats$'+/-'}
  }
  #get into correct type for each variable
  if ((metrics == 'basic' & 'MP' %in% which_metrics_basic) | (metrics == 'advanced' & 'MP' %in% which_metrics_advanced)){
    num_stat <- as.data.frame(lubridate::ms(clean_stat$MP))
    num_stat[1] <- as.numeric(num_stat[,1])
    for (i in 3:ncol(clean_stat)){
      num_stat[,i-1] <- as.numeric(clean_stat[,i])
    }
  }
  if (!(metrics == 'basic' & 'MP' %in% which_metrics_basic) & !(metrics == 'advanced' & 'MP' %in% which_metrics_advanced)){
    num_stat <- as.data.frame(as.numeric(clean_stat[,2]))
    for (i in 3:ncol(clean_stat)){
      num_stat[,i-1] <- as.numeric(clean_stat[,i])
    }
  }
  #give column titles
  if (metrics == 'advanced'){
    names(num_stat)[1:ncol(num_stat)] <- which_metrics_advanced
  }
  if (metrics == 'basic'){
    names(num_stat)[1:ncol(num_stat)] <- which_metrics_basic
  }
  consistency_stats <- list()
  consistency_stats[[1]] <- suppressWarnings(c((1 - mean(is.na(num_stat$GS))) * 100, mean(num_stat$GS, na.rm = T) *100))
  consistency_stats[[2]] <- apply(num_stat, 2, mean, na.rm = T)
  consistency_stats[[3]] <- apply(num_stat, 2, sd, na.rm = T)
  if ((metrics == 'basic' & 'MP' %in% which_metrics_basic) | (metrics == 'advanced' & 'MP' %in% which_metrics_advanced)){
    consistency_stats[[2]][1] <-consistency_stats[[2]][1]/60
    consistency_stats[[3]][1] <-consistency_stats[[3]][1]/60
  }
  names(consistency_stats) <- c('Percent games played & percent games started', 'Average per game', 'Variance between game')
  consistency_stats
}

#still left to do to make function correct:

#documentation


