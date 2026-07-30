test_that("simple gourmex", {
  vars = c('age', 'gender', 'happy', 'symptoms')
  types = c('numerical', 'categorical', 'logical', 'categorical')
  gourmex = mount(vars, 'id') %>%
    ingest(df_test)
  expect_setequal(names(gourmex$storage), c('raw', 'chewed', 'stats'))
  expect_identical(gourmex$storage$raw, df_test)
  expect_equal(gourmex$types$var_type,  types)
})

# check categorical, logical, numerical
