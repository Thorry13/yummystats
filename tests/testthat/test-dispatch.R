vars = c('gender', 'happy', 'occupation', 'symptoms', 'age')
params0 = default_params()
params_pv = default_params(grouped=TRUE)
params = params_pv
params$stats = params$stats %>% filter(stat_name != 'p_value')
params$shapes = params$shapes %>% filter(stat_name != 'p_value')
params$layout$template$`p-value` = NULL
df_test = generate_test_data(200)

test_that("default dispatch works", {
  gourmex = mount(vars, 'id', group_cols = 'group', params = params) %>%
    ingest(df_test) %>%
    digest() %>%
    shape() %>%
    dispatch()
  expect_false(is.null(gourmex$storage$layout))
})


test_that("dispatch with multiple groups works", {
  expect_warning(# for location
    gourmex <- mount(vars, 'id', group_cols = c('group', 'location'), params = params) %>%
      ingest(df_test) %>%
      digest() %>%
      shape() %>%
      dispatch(),
    'Detected'
  )
  expect_false(is.null(gourmex$storage$layout))
})


test_that("dispatch with strata works", {
  stratas = unique(df_test$location)
  gourmex = mount(vars, 'id', group_cols='group', strata_cols='location', params = params) %>%
    ingest(df_test) %>%
    digest() %>%
    shape() %>%
    dispatch()

  expect_true(all(na.omit(stratas) %in% names(gourmex$storage$layout)))
})



test_that("dispatch with multiple stratas works", {
  gourmex = mount(vars, 'id', strata_cols=c('group', 'location'), params = params0) %>%
    ingest(df_test) %>%
    digest() %>%
    shape()
  expect_no_error(gourmex <- dispatch(gourmex))
})


