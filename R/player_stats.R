#'
#' NBA Player per Game Stats for Season
#'
#' @param player url link for player. For example, 'j/jamesle01'. Link can be found for desired player at 'https://www.basketball-reference.com/players/'
#' @param season A numeric year
#' @param metrics One of 'basic' or 'advanced'. 'basic' will return basic statistics and 'advanced' will return advanced statistics
#'
#'@return An data frame containing player season game by game statistics from basketball-reference.com.
#'
#'@examples
#'player_stats('c/curryst01', season = 2019, metrics = "advanced")
#'#returns Steph Curry's 2019 advanced statistics

player_stats <- function(player, season, metrics = 'basic'){
    if (metrics == 'basic') {
      url <- paste('https://www.basketball-reference.com/players/', player,
        '/gamelog/', season,'/', sep = '')
      page <- xml2::read_html(url)
      stat_list <- rvest::html_table(page, fill = T)[[8]]
    }
    if (metrics == 'advanced'){
      url <- paste('https://www.basketball-reference.com/players/', player,
                   '/gamelog-advanced/', season,'/', sep = '')
      page <- xml2::read_html(url)
      stat_list <- rvest::html_table(page, fill = T)[[1]]
    }
    stat_df <- as.data.frame(stat_list)
    names(stat_df)[6] <- "Home?"
    names(stat_df)[8] <- "Result"
    #handles extra column of NA's being saved
    stat_df <- stat_df[,colSums(is.na(stat_df))<nrow(stat_df)/2]
    #get rid of rows that repeat column titles
    stat_df <- stat_df %>% dplyr::filter(.data$Date != "Date") %>% dplyr::select(-.data$Rk)
    stat_df <- suppressWarnings(apply(stat_df, 2, as.numeric))
    stat_df$MP <- suppressWarnings(lubridate::ms(stat_df$MP))
    return(stat_df)
}
