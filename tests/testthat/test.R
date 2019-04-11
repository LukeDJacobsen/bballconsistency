library(WNBAballr)

test_that('player_stats works', {
  expect_equal(player_stats('c/curryst01', season = 2018)[15,15], ".222")
})
