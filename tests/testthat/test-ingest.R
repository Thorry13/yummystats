vars = c('age', 'gender', 'happy', 'symptoms', 'occupation')
types = c('numerical', 'categorical', 'logical', 'categorical', 'categorical')

test_that("simple gourmex", {
  gourmex = mount(vars, 'id') %>%
    ingest(df_test)
  expect_setequal(names(gourmex$storage), c('raw', 'chewed', 'stats'))
  expect_identical(gourmex$storage$raw, df_test)
  expect_equal(gourmex$types$var_type,  types)
})


test_that("gourmex with groups", {
  gourmex = mount(vars, 'id', 'group') %>%
    ingest(df_test)
  expect_setequal(names(gourmex$storage), c('raw', 'chewed', 'stats'))
  expect_identical(gourmex$storage$raw, df_test)
  expect_equal(gourmex$types$var_type,  types)

  gourmex = mount(vars, 'id', c('group', 'location')) %>%
    ingest(df_test)
  expect_setequal(names(gourmex$storage), c('raw', 'chewed', 'stats'))
  expect_identical(gourmex$storage$raw, df_test)
  expect_equal(gourmex$types$var_type,  types)
})


test_that("gourmex with stratas", {
  gourmex = mount(vars, 'id', 'group', 'location') %>%
    ingest(df_test)
  expect_setequal(names(gourmex$storage), c('raw', 'chewed', 'stats'))
  expect_identical(gourmex$storage$raw, df_test)
  expect_equal(gourmex$types$var_type,  types)

  gourmex = mount(vars, 'id', strata_cols=c('group','location')) %>%
    ingest(df_test)
  expect_setequal(names(gourmex$storage), c('raw', 'chewed', 'stats'))
  expect_identical(gourmex$storage$raw, df_test)
  expect_equal(gourmex$types$var_type,  types)
})


