myparams = default_params(grouped=TRUE)
test_that("params reduction works", {
  params = rbind(myparams$shapes, myparams$shapes) # Introduce double entries
  expect_false(check_unicity(params %>% mutate(i=row_number()), var = 'i',  id_cols = c('var_type', 'stat_name')))
  new_params = reduce_params(params)
  expect_true(check_unicity(new_params %>% mutate(i=row_number()), var = 'i', id_cols = c('var_type', 'stat_name')))
})

vars = c('gender', 'happy', 'occupation', 'symptoms', 'age')
df_test = generate_test_data(500)
gourmex = mount(vars, 'id', 'group', params=myparams) %>%
  ingest(df_test) %>%
  digest()

test_that("shaping works", {
  gourmex = shape(gourmex)

  expect_true(!is.null(gourmex$storage$shaped))
  expect_equal(names(gourmex$storage$shaped), vars)


  num_stats = c('mean', 'std', 'SE', 'CI95', 'min', 'q1', 'median', 'q3', 'max')
  num_stats_group = c('mean', 'std', 'SE', 'CI95', 'min', 'q1', 'median', 'q3', 'max') %>% paste0('_group')

  for(var in vars){
    var_type = gourmex$types %>% filter(var == .env$var) %>% pull(var_type)
    stat_names = gourmex$params$shapes %>% filter(var_type == .env$var_type) %>% pull(stat_name)
    if('num_stats' %in% stat_names)
      stat_names = stat_names %>% setdiff('num_stats') %>% c(num_stats)
    if('num_stats_group' %in% stat_names)
      stat_names = stat_names %>% setdiff('num_stats_group') %>% c(num_stats_group)
    expect_all_true(stat_names %in% names(gourmex$storage$stats[[var]]))
  }
})


test_that("reshaping works", {
  gourmex = shape(gourmex)

  labels = c('Gender', 'Is happy', 'Occupation', 'Symptoms', 'Age')
  df_dict = data.frame(var=vars, label=labels)
  f = list(\(x) format_categorical(x, df_dict, from='var', to='label'))
  new_shape = tibble(var_type=c('categorical', 'logical', 'numerical'), stat_name='variable', shape_func=f)
  gourmex$params$shapes = bind_rows(gourmex$params$shapes, new_shape)

  gourmex2 = shape(gourmex)
  expect_equal(dim(gourmex$storage$shaped), dim(gourmex2$storage$shaped))
})

test_that("warning throws if no associated stat", {
  myparams$stats = myparams$stats %>% filter(stat_name != 'p_value')
  gourmex = mount(vars, 'id', 'group', params=myparams) %>%
    ingest(df_test) %>%
    digest()

  warning = unique(capture_warnings(shape(gourmex)))
  expect_true(str_detect(warning, "Couldn't shape"))
})
