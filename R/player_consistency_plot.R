#'
#'
#'
#'

player_consistency_plot <- function(player, season, metrics = 'basic',
                 which_metrics_basic = c('MP', 'FG', 'FGA', 'FG%', "3P",
                                         "3PA", "3P%", "FT", "FTA", "FT%",
                                         "ORB", "DRB", "TRB", "AST", "STL",
                                         "BLK", "TOV", "PF", "PTS", "GmSc", "+/-"),
                 which_metrics_advanced = c('MP', "TS%", "eFG%", 'ORB%', 'DRB%','TRB%', 'AST%',
                                            "STL%", "BLK%", "TOV%", "USG%", "ORtg",
                                            "DRtg", 'GmSc')){
  #get player stats
  get_stats <- player_stats(player, season, metrics)
  #initiate data.frame
  small_stat <- data.frame(Game = get_stats$G)
  mid_stat <- data.frame(Game = get_stats$G)
  big_stat <- data.frame(Game = get_stats$G)
  pm_stat <- data.frame(Game = get_stats$G)
  #determine which metric and includes those included in which metrics
  if (metrics == 'advanced'){
    if ('MP' %in% which_metrics_advanced){mid_stat$MP <- as.numeric(get_stats$MP)}
    if ('TS%' %in% which_metrics_advanced){small_stat$'TS%' <- get_stats$'TS%'}
    if ('eFG%' %in% which_metrics_advanced){small_stat$'eFG%' <- get_stats$'eFG%'}
    if ('ORB%' %in% which_metrics_advanced){small_stat$'ORB%' <- get_stats$'ORB%'}
    if ('DRB%' %in% which_metrics_advanced){small_stat$'DRB%' <- get_stats$'DRB%'}
    if ('TRB%' %in% which_metrics_advanced){small_stat$'TRB%' <- get_stats$'TRB%'}
    if ('AST%' %in% which_metrics_advanced){small_stat$'AST%' <- get_stats$'AST%'}
    if ('STL%' %in% which_metrics_advanced){small_stat$'STL%' <- get_stats$'STL%'}
    if ('BLK%' %in% which_metrics_advanced){small_stat$'BLK%' <- get_stats$'BLK%'}
    if ('TOV%' %in% which_metrics_advanced){small_stat$'TOV%' <- get_stats$'TOV%'}
    if ('USG%' %in% which_metrics_advanced){small_stat$'USG%' <- get_stats$'USG%'}
    if ('ORtg' %in% which_metrics_advanced){big_stat$ORtg <- get_stats$ORtg}
    if ('DRtg' %in% which_metrics_advanced){big_stat$DRtg <- get_stats$DRtg}
    if ('GmSc' %in% which_metrics_advanced){mid_stat$GmSc <- get_stats$GmSc}
  }
  if (metrics == 'basic'){
    if ('MP' %in% which_metrics_basic){big_stat$MP <- as.numeric(get_stats$MP)}
    if ('FG' %in% which_metrics_basic){mid_stat$FG <- get_stats$FG}
    if ('FGA' %in% which_metrics_basic){mid_stat$FGA <- get_stats$FGA}
    if ('FG%' %in% which_metrics_basic){small_stat$'FG%' <- get_stats$'FG%'}
    if ('3P' %in% which_metrics_basic){mid_stat$'3P' <- get_stats$'3P'}
    if ('3PA' %in% which_metrics_basic){mid_stat$'3PA' <- get_stats$'3PA'}
    if ('3P%' %in% which_metrics_basic){small_stat$'3P%' <- get_stats$'3P%'}
    if ('FT' %in% which_metrics_basic){mid_stat$'FT' <- get_stats$FT}
    if ('FTA' %in% which_metrics_basic){mid_stat$FTA <- get_stats$FTA}
    if ('FT%' %in% which_metrics_basic){small_stat$'FT%' <- get_stats$'FT%'}
    if ('ORB' %in% which_metrics_basic){mid_stat$ORB <- get_stats$ORB}
    if ('DRB' %in% which_metrics_basic){mid_stat$DRB <- get_stats$DRB}
    if ('TRB' %in% which_metrics_basic){mid_stat$TRB <- get_stats$TRB}
    if ('AST' %in% which_metrics_basic){mid_stat$AST <- get_stats$AST}
    if ('STL' %in% which_metrics_basic){mid_stat$STL <- get_stats$STL}
    if ('BLK' %in% which_metrics_basic){mid_stat$BLK <- get_stats$BLK}
    if ('TOV' %in% which_metrics_basic){mid_stat$TOV <- get_stats$TOV}
    if ('PF' %in% which_metrics_basic){mid_stat$PF <- get_stats$PF}
    if ('PTS' %in% which_metrics_basic){big_stat$PTS <- get_stats$PTS}
    if ('GmSc' %in% which_metrics_basic){big_stat$GmSc <- get_stats$GmSc}
    if ('+/-' %in% which_metrics_basic){pm_stat$'+/-' <- get_stats$'+/-'}
  }
  #get data into format that ggplot can accept
  #first with small data
  sdata <- matrix(small_stat[,2])
  sstat <- matrix(rep(names(small_stat)[2]))
  for (i in 3:ncol(small_stat)){
    sdata <- rbind(sdata, matrix(small_stat[,i]))
    sstat <- rbind(sstat, matrix(rep(names(small_stat)[i])))
  }
  ggsmall_stat <- data.frame('data' = sdata, 'stat' = sstat)
  #now mid stat
  mdata <- matrix(mid_stat[,2])
  mstat <- matrix(rep(names(mid_stat)[2]))
  for (i in 3:ncol(mid_stat)){
    mdata <- rbind(mdata, matrix(mid_stat[,i]))
    mstat <- rbind(mstat, matrix(rep(names(mid_stat)[i])))
  }
  ggmid_stat <- data.frame('data' = mdata, 'stat' = mstat)
  #now large stat
  bdata <- matrix(big_stat[,2])
  bstat <- matrix(rep(names(big_stat)[2]))
  for (i in 3:ncol(big_stat)){
    bdata <- rbind(bdata, matrix(big_stat[,i]))
    bstat <- rbind(bstat, matrix(rep(names(big_stat)[i])))
  }
  ggbig_stat <- data.frame('data' = bdata, 'stat' = bstat)
  dfvector <- as.vector(rbind(matrix(rep('small', nrow(ggsmall_stat))), matrix(rep('big', nrow(ggbig_stat))), matrix(rep('mid', nrow(ggmid_stat)))))
  ggstat <- rbind(ggsmall_stat,ggbig_stat, ggmid_stat)
  ggstat$df <- dfvector
  ggstat
}

#dont forget to incorporat plus minus
#IN PROGRESS
