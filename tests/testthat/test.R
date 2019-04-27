library(WNBAballr)

test_that('player_stats works', {
  expect_equal(player_stats('c/curryst01', season = 2018)[15,15], .222)
})

test_that('player_consistency defaults work',{
  expect_equal(player_consistency('t/townska01', season = 2018)[[2]][2], 1)
})

test_that('player_consistency advanced work',{
  expect_equal(player_consistency('a/antetgi01', season = 2017, metrics = 'advanced')[[3]][2], 0)
})

test_that('player_consistency works with subset of variables',{
  expect_equal(length(player_consistency('g/georgpa01', season = 2017, which_metrics_basic = c('GS', 'FG'))[[2]]), 2)
})
