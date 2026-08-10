vars = c('gender', 'happy', 'occupation', 'symptoms', 'age')
myparams0 = default_params()
myparams_pv = default_params(grouped=TRUE)
myparams = myparams_pv
myparams$stats = myparams$stats %>% filter(stat_name != 'p_value')
myparams$shapes = myparams$shapes %>% filter(stat_name != 'p_value')
myparams$layout$template$`p-value` = NULL
df_test = generate_test_data(200)

test_that("default dispatch works", {
  gourmex = mount(vars, 'id', group_cols = 'group', params = myparams) %>%
    ingest(df_test) %>%
    digest() %>%
    shape() %>%
    dispatch()
  expect_false(is.null(gourmex$storage$layout))
})


test_that("dispatch with multiple groups works", {
  expect_warning(# for location
    gourmex <- mount(vars, 'id', group_cols = c('group', 'location'), params = myparams) %>%
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
  gourmex = mount(vars, 'id', group_cols='group', strata_cols='location', params = myparams) %>%
    ingest(df_test) %>%
    digest() %>%
    shape() %>%
    dispatch()

  expect_true(all(na.omit(stratas) %in% names(gourmex$storage$layout)))
})



test_that("dispatch with multiple stratas works", {
  gourmex = mount(vars, 'id', strata_cols=c('group', 'location'), params = myparams0) %>%
    ingest(df_test) %>%
    digest() %>%
    shape()
  expect_no_error(gourmex <- dispatch(gourmex))
})


