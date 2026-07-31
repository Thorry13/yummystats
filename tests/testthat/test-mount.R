vars = c('age', 'gender', 'happy', 'symptoms')

test_that("basic mount", {
  gourmex = mount(vars, 'id')
  expect_equal(class(gourmex), 'gourmex')
  expect_equal(names(gourmex), c('storage', 'params'))
  expect_equal(gourmex$params$id_cols, 'id')
  expect_equal(gourmex$params$vars, vars)
})


# # check reduced params
# test_that("mount with params",{
#   vars = c('age', 'gender', 'happy', 'symptoms')
#   gourmex = mount(vars, 'id', params=myparams)
# })

test_that("mount with groups", {
  gourmex = mount(vars, 'id', 'group')
  expect_equal(gourmex$params$group_cols, 'group')
})


test_that("mount with stratas", {
  gourmex = mount(vars, 'id', strata_cols='location')
  expect_equal(gourmex$params$strata_cols, 'location')
})
