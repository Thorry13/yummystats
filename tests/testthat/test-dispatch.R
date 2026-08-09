vars = c('gender', 'happy', 'occupation', 'symptoms', 'age')
df_test = generate_test_data(100)
myparams = default_params(grouped=TRUE)
gourmex = mount(vars, 'id', group_cols = 'group', params = myparams) %>%
  ingest(df_test) %>%
  digest() %>%
  shape()

test_that("default dispatch works", {
  gourmex2 = gourmex %>% dispatch()
  expect_false(is.null(gourmex2$storage$layout))
})
