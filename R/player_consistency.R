#'
#'
#'
#'
#'
#'
#'

player_consistency <- function(player, season, metrics = 'basic',
                               whichmetrics = c('MP', 'GS', 'FG', 'FGA', 'FG%', "3P",
                                                "3PA", "3P%", "FT", "FTA", "FT%",
                                                "ORB", "DRB", "TRB", "AST", "STL",
                                                "BLK", "TOV", "PF", "PTS", "GmSc", "+/-")){
  get_stats <- player_stats(player, season, metrics)
  clean_stat <- data.frame(Game = get_stats$G)
  if (metrics == 'basic'){
    if ('MP' %in% whichmetrics){clean_stat$MP <- get_stats$MP}
    if ('GS' %in% whichmetrics){clean_stat$GS <- get_stats$GS}
    if ('FG' %in% whichmetrics){clean_stat$FG <- get_stats$FG}
    if ('FGA' %in% whichmetrics){clean_stat$FGA <- get_stats$FGA}
    if ('FG%' %in% whichmetrics){clean_stat$'FG%' <- get_stats$'FG%'}
    if ('3P' %in% whichmetrics){clean_stat$'3P' <- get_stats$'3P'}
    if ('3PA' %in% whichmetrics){clean_stat$'3PA' <- get_stats$'3PA'}
    if ('3P%' %in% whichmetrics){clean_stat$'3P%' <- get_stats$'3P%'}
    if ('FT' %in% whichmetrics){clean_stat$'FT' <- get_stats$FT}
    if ('FTA' %in% whichmetrics){clean_stat$FTA <- get_stats$FTA}
    if ('FT%' %in% whichmetrics){clean_stat$'FT%' <- get_stats$'FT%'}
    if ('ORB' %in% whichmetrics){clean_stat$ORB <- get_stats$ORB}
    if ('DRB' %in% whichmetrics){clean_stat$DRB <- get_stats$DRB}
    if ('TRB' %in% whichmetrics){clean_stat$TRB <- get_stats$TRB}
    if ('AST' %in% whichmetrics){clean_stat$AST <- get_stats$AST}
    if ('STL' %in% whichmetrics){clean_stat$STL <- get_stats$STL}
    if ('BLK' %in% whichmetrics){clean_stat$BLK <- get_stats$BLK}
    if ('TOV' %in% whichmetrics){clean_stat$TOV <- get_stats$TOV}
    if ('PF' %in% whichmetrics){clean_stat$PF <- get_stats$PF}
    if ('PTS' %in% whichmetrics){clean_stat$PTS <- get_stats$PTS}
    if ('GmSc' %in% whichmetrics){clean_stat$GmSc <- get_stats$GmSc}
    if ('+/-' %in% whichmetrics){clean_stat$'+/-' <- get_stats$'+/-'}
  }
  num_stat <- data.frame(1:nrow(clean_stat))
  if ('MP' %in% whichmetrics){
    MP <- data.frame(MP = lubridate::ms(clean_stat$MP))
    for (i in 3:ncol(clean_stat)){
      num_stat[,i - 1] <- as.numeric(clean_stat[,i])
    }
  }
  if (!('MP' %in% whichmetrics)){
    for (i in 2:ncol(clean_stat)){
      num_stat[,i - 1] <- as.numeric(clean_stat[,i])
    }
  }
  names(num_stat)[1:ncol(num_stat)] <- whichmetrics
  consistency_stats <- list()
  consistency_stats[[1]] <- (1 - mean(is.na(num_stat$GS))) * 100
  consistency_stats[[2]] <- apply(num_stat, 2, mean, na.rm = T)
  consistency_stats[[3]] <- apply(num_stat, 2, sd, na.rm = T)
  consistency_stats
}

#still left to do to make function correct:
#make work for advanced
#figure out why averages are wrong in some stats
#documentation
#probably a better idea to make get_stats spit out correct format.


