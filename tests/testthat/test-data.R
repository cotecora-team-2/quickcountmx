data("conteo_2018")

test_that("test data loading", {

  expect_equal(nrow(conteo_2018), 156432)
  expect_equal(names(table((is.na(conteo_2018$TOTAL_VOTOS_CALCULADOS)))), "FALSE")
})
