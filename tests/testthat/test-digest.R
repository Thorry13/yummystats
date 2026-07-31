vars = c('gender', 'happy', 'occupation', 'symptoms', 'age')

test_that("params reduction works", {
  params = rbind(myparams$stats, myparams$stats) # Introduce double entries
  expect_false(check_unicity(params %>% mutate(i=row_number()), var = 'i',  id_cols = c('var_type', 'stat_name')))
  new_params = reduce_params(params)
  expect_true(check_unicity(new_params %>% mutate(i=row_number()), var = 'i', id_cols = c('var_type', 'stat_name')))
})


test_that("check digestion", {
  gourmex = mount(vars, 'id', params=myparams0) %>%
    ingest(df_test) %>%
    digest()

  expect_setequal(names(gourmex$storage$stats), gourmex$params$vars)

  num_stats = c('mean', 'std', 'SE', 'CI95', 'min', 'q1', 'median', 'q3', 'max')
  num_stats_group = c('mean', 'std', 'SE', 'CI95', 'min', 'q1', 'median', 'q3', 'max') %>% paste0('_group')

  for(var in gourmex$types$var){
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
  gourmex = mount(vars, 'id', group_cols = 'group', params=myparams) %>%
    ingest(df_test) %>%
    digest()

  expect_setequal(names(gourmex$storage$stats), gourmex$params$vars)

  num_stats = c('mean', 'std', 'SE', 'CI95', 'min', 'q1', 'median', 'q3', 'max')
  num_stats_group = c('mean', 'std', 'SE', 'CI95', 'min', 'q1', 'median', 'q3', 'max') %>% paste0('_group')

  for(var in gourmex$types$var){
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
  vars = c('age', 'gender', 'happy', 'occupation', 'symptoms')

  gourmex = mount(vars, 'id', group_cols = 'group', params=myparams) %>%
    ingest(df_test) %>%
    digest()

  expect_setequal(names(gourmex$storage$stats), gourmex$params$vars)

  num_stats = c('mean', 'std', 'SE', 'CI95', 'min', 'q1', 'median', 'q3', 'max')
  num_stats_group = c('mean', 'std', 'SE', 'CI95', 'min', 'q1', 'median', 'q3', 'max') %>% paste0('_group')

  for(var in gourmex$types$var){
    var_type = gourmex$types %>% filter(var == .env$var) %>% pull(var_type)
    stat_names = gourmex$params$stats %>% filter(var_type == .env$var_type) %>% pull(stat_name)
    if('num_stats' %in% stat_names)
      stat_names = stat_names %>% setdiff('num_stats') %>% c(num_stats)
    if('num_stats_group' %in% stat_names)
      stat_names = stat_names %>% setdiff('num_stats_group') %>% c(num_stats_group)

    expect_all_true(stat_names %in% names(gourmex$storage$stats[[var]]))
  }
})



