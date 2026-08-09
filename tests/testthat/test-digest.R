vars = c('gender', 'happy', 'occupation', 'symptoms', 'age')
myparams0 = default_params()
myparams = default_params(grouped=TRUE)

test_that("params reduction works", {
  params = rbind(myparams$stats, myparams$stats) # Introduce double entries
  expect_false(check_unicity(params %>% mutate(i=row_number()), var = 'i',  id_cols = c('var_type', 'stat_name')))
  new_params = reduce_params(params)
  expect_true(check_unicity(new_params %>% mutate(i=row_number()), var = 'i', id_cols = c('var_type', 'stat_name')))
})


test_that("check digestion", {
  df_test = generate_test_data(100)
  gourmex = mount(vars, 'id', params=myparams0) %>%
    ingest(df_test) %>%
    digest()

  expect_setequal(names(gourmex$storage$stats), gourmex$params$vars)

  num_stats = c('mean', 'std', 'SE', 'CI95', 'min', 'q1', 'median', 'q3', 'max')
  num_stats_group = c('mean', 'std', 'SE', 'CI95', 'min', 'q1', 'median', 'q3', 'max') %>% paste0('_group')

  for(var in vars){
    var_type = gourmex$types %>% filter(var == .env$var) %>% pull(var_type)
    stat_names = gourmex$params$stats %>% filter(var_type == .env$var_type) %>% pull(stat_name)
    if('num_stats' %in% stat_names)
      stat_names = stat_names %>% setdiff('num_stats') %>% c(num_stats)
    if('num_stats_group' %in% stat_names)
      stat_names = stat_names %>% setdiff('num_stats_group') %>% c(num_stats_group)
    expect_all_true(stat_names %in% names(gourmex$storage$stats[[var]]))
  }
})


test_that("check digestion with groups", {
  df_test = generate_test_data(1000)
  gourmex = mount(vars, 'id', group_cols = 'group', params=myparams) %>%
    ingest(df_test) %>%
    digest()

  expect_setequal(names(gourmex$storage$stats), gourmex$params$vars)

  num_stats = c('mean', 'std', 'SE', 'CI95', 'min', 'q1', 'median', 'q3', 'max')
  num_stats_group = c('mean', 'std', 'SE', 'CI95', 'min', 'q1', 'median', 'q3', 'max') %>% paste0('_group')

  for(var in vars){
    var_type = gourmex$types %>% filter(var == .env$var) %>% pull(var_type)
    stat_names = gourmex$params$stats %>% filter(var_type == .env$var_type) %>% pull(stat_name)
    if('num_stats' %in% stat_names)
      stat_names = stat_names %>% setdiff('num_stats') %>% c(num_stats)
    if('num_stats_group' %in% stat_names)
      stat_names = stat_names %>% setdiff('num_stats_group') %>% c(num_stats_group)

    expect_all_true(stat_names %in% names(gourmex$storage$stats[[var]]))
  }
})



test_that("check digestion with multiple group cols", {
  df_test = generate_test_data(5000)
  gourmex = mount(vars, 'id', group_cols = c('group', 'location'), params=myparams) %>%
    ingest(df_test) %>%
    digest()

  expect_setequal(names(gourmex$storage$stats), gourmex$params$vars)

  num_stats = c('mean', 'std', 'SE', 'CI95', 'min', 'q1', 'median', 'q3', 'max')
  num_stats_group = c('mean', 'std', 'SE', 'CI95', 'min', 'q1', 'median', 'q3', 'max') %>% paste0('_group')

  for(var in vars){
    var_type = gourmex$types %>% filter(var == .env$var) %>% pull(var_type)
    stat_names = gourmex$params$stats %>% filter(var_type == .env$var_type) %>% pull(stat_name)
    if('num_stats' %in% stat_names)
      stat_names = stat_names %>% setdiff('num_stats') %>% c(num_stats)
    if('num_stats_group' %in% stat_names)
      stat_names = stat_names %>% setdiff('num_stats_group') %>% c(num_stats_group)

    expect_all_true(stat_names %in% names(gourmex$storage$stats[[var]]))
  }
})



test_that("check digestion with stratas", {
  df_test = generate_test_data(15000)
  gourmex = mount(vars, 'id', group_cols = 'group', strata_cols='location', params=myparams) %>%
    ingest(df_test) %>%
    digest()

  expect_setequal(names(gourmex$storage$stats), gourmex$params$vars)

  num_stats = c('mean', 'std', 'SE', 'CI95', 'min', 'q1', 'median', 'q3', 'max')
  num_stats_group = c('mean', 'std', 'SE', 'CI95', 'min', 'q1', 'median', 'q3', 'max') %>% paste0('_group')

  for(var in vars){
    var_type = gourmex$types %>% filter(var == .env$var) %>% pull(var_type)
    stat_names = gourmex$params$stats %>% filter(var_type == .env$var_type) %>% pull(stat_name)
    if('num_stats' %in% stat_names)
      stat_names = stat_names %>% setdiff('num_stats') %>% c(num_stats)
    if('num_stats_group' %in% stat_names)
      stat_names = stat_names %>% setdiff('num_stats_group') %>% c(num_stats_group)

    expect_all_true(c(stat_names, 'location') %in% names(gourmex$storage$stats[[var]]))
  }
})

test_that("check digestion with stratas", {
  df_test = generate_test_data(15000)
  vars2 = setdiff(vars, 'gender')
  gourmex = mount(vars2, 'id', group_cols = 'group', strata_cols=c('gender', 'location'), params=myparams) %>%
    ingest(df_test) %>%
    digest()

  expect_setequal(names(gourmex$storage$stats), gourmex$params$vars)

  num_stats = c('mean', 'std', 'SE', 'CI95', 'min', 'q1', 'median', 'q3', 'max')
  num_stats_group = c('mean', 'std', 'SE', 'CI95', 'min', 'q1', 'median', 'q3', 'max') %>% paste0('_group')

  for(var in vars2){
    var_type = gourmex$types %>% filter(var == .env$var) %>% pull(var_type)
    stat_names = gourmex$params$stats %>% filter(var_type == .env$var_type) %>% pull(stat_name)
    if('num_stats' %in% stat_names)
      stat_names = stat_names %>% setdiff('num_stats') %>% c(num_stats)
    if('num_stats_group' %in% stat_names)
      stat_names = stat_names %>% setdiff('num_stats_group') %>% c(num_stats_group)

    expect_all_true(c(stat_names, 'gender', 'location') %in% names(gourmex$storage$stats[[var]]))
  }
})
