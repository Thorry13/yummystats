vars = c('gender', 'happy', 'occupation', 'symptoms', 'age')
myparams_pv = default_params(grouped=TRUE)
myparams = myparams_pv
myparams$stats = myparams$stats %>% filter(stat_name != 'p_value')
df_test = generate_test_data(200)

gourmex = mount(vars, id_cols='id', group_cols='group', params = myparams) %>%
  ingest(df_test) %>%
  digest()

test_that("stat_total works", {
  for(var in vars){
    df_stats = gourmex$storage$stats[[var]]

    expect_equal(unique(df_stats$N), n_distinct(df_test$id))

    df_N_group = df_test %>% summarize(n = n_distinct(id), .by='group') %>%
      full_join(df_stats, by='group')
    expect_equal(df_N_group$n, df_N_group$N_group)
  }

  for(var in setdiff(vars, 'age')){
    df_stats = gourmex$storage$stats[[var]]

    df_n_level = df_test %>% unnest(!!var, keep_empty = TRUE) %>%
      summarize(n = n_distinct(id), .by=all_of(var)) %>%
      mutate(!!var := as.character(.data[[var]])) %>% full_join(df_stats, by=var)
    expect_equal(df_n_level$n, df_n_level$n_level)

    df_n_level_group = df_test %>% unnest(!!var, keep_empty = TRUE) %>%
      summarize(n = n_distinct(id), .by=all_of(c(var, 'group'))) %>%
      mutate(!!var := as.character(.data[[var]])) %>% full_join(df_stats, by=c(var, 'group'))
    expect_equal(df_n_level_group$n, df_n_level_group$n_level_group)
  }
})


test_that("stat_perc works", {
  for(var in setdiff(vars, 'age')){
    df_stats = gourmex$storage$stats[[var]] %>%
      mutate(!!var := as.character(.data[[var]]))

    df_p_level = df_test %>% unnest(!!var) %>% filter(!is.na(.data[[var]])) %>%
      mutate(n = n_distinct(id)) %>%
      summarize(p = n_distinct(id)/unique(n), .by=all_of(var)) %>%
      mutate(!!var := as.character(.data[[var]])) %>% full_join(df_stats, by=var)
    expect_equal(df_p_level$p, df_p_level$p_level)

    df_p_level_group = df_test %>% unnest(!!var) %>% filter(!is.na(.data[[var]])) %>%
      mutate(n_group = n_distinct(id), .by='group') %>%
      summarize(pg = n_distinct(id)/unique(n_group), .by=all_of(c(var, 'group'))) %>%
      mutate(!!var := as.character(.data[[var]])) %>% full_join(df_stats, by=c(var, 'group'))
    expect_equal(df_p_level_group$pg, df_p_level_group$p_level_group)
  }
})


test_that("stat_n_avail and stat_n_miss work", {
  for(var in setdiff(vars, 'age')){
    df_stats = gourmex$storage$stats[[var]] %>%
      mutate(!!var := as.character(.data[[var]]))

    expect_equal(
      unique(df_stats$n_avail),
      df_test %>% unnest(all_of(var)) %>% filter(!is.na(.data[[var]])) %>% pull(id) %>% n_distinct()
    )
    expect_equal(
      unique(df_stats$n_missing),
      df_test %>% unnest(all_of(var), keep_empty = TRUE) %>% filter(is.na(.data[[var]])) %>% pull(id) %>% n_distinct()
    )

    for(g in unique(df_test$group)){
      expect_equal(
        df_stats %>% filter(group==g) %>% pull(n_avail_group) %>% unique(),
        df_test %>% unnest(all_of(var)) %>% filter(group==g, !is.na(.data[[var]])) %>% pull(id) %>% n_distinct()
      )
    }
  }
})


test_that("pvalues work", {
  df_test = generate_test_data(200, 'group', c('gender', 'happy', 'occupation'), 5)
  gourmex = mount(vars, id_cols='id', group_cols='group', params = myparams_pv) %>%
    ingest(df_test) %>%
    digest()

  for(var in c('gender', 'happy', 'occupation')){
    A = df_test %>% unnest(all_of(var)) %>% filter(!is.na(group))
    pv = chisq.test(A[[var]], A$group)$p.value
    expect_equal(unique(gourmex$storage$stats[[var]]$p_value), pv)
  }

  # multi-level
  df_stat = gourmex$storage$stats$symptoms %>% filter(!is.na(symptoms))
  for(level in unique(df_stat$symptoms)){
    A = df_test %>% unnest(symptoms) %>% filter(!is.na(group)) %>%
      mutate(V = any(symptoms==level, na.rm=TRUE), .by='id') %>%
      distinct(id, V, group)

    expect_equal(
      df_stat %>% filter(symptoms==level) %>% pull(p_value) %>% unique(),
      chisq.test(A$V, A$group)$p.value
    )
  }

  # numerical: >2 groups
  A = df_test %>% filter(!is.na(age), !is.na(group))
  pv = anova_test(A, age ~ group)$p
  expect_equal(unique(gourmex$storage$stats$age$p_value), pv)
})


test_that("pvalues num 2 groups work", {
  gourmex2 = mount('age', id_cols='id', group_cols='gender', params = myparams_pv) %>%
    ingest(df_test %>% filter(!is.na(gender))) %>%
    digest()

  A = df_test %>% filter(!is.na(age), !is.na(gender))
  pv = t.test(age ~ gender, A)$p.value
  expect_equal(unique(gourmex2$storage$stats$age$p_value), pv)
})


test_that("good separation for each strata", {
  df_test = generate_test_data(200, c('group', 'location'), c('gender', 'happy', 'occupation', 'symptoms'), 1)
  gourmex = mount(vars, id_cols='id', group_cols='group', strata_cols='location', params = myparams) %>%
    ingest(df_test) %>%
    digest()

  stratas = df_test %>% filter(!is.na(location)) %>% pull(location) %>% unique()
  for(strata in stratas){
    gourmex_strata = mount(vars, id_cols='id', group_cols='group', params = myparams) %>%
      ingest(df_test %>% filter(location == strata)) %>%
      digest()

    for(var in vars){
      A = gourmex$storage$stats[[var]] %>% filter(location == strata) %>% select(-location) %>% distinct() %>% as.data.frame()
      B = gourmex_strata$storage$stats[[var]]
      expect_identical(A,B)
    }
  }
})


test_that("multiple groups work",{
  df_test = generate_test_data(1100, c('group', 'location'), c('gender', 'happy', 'occupation', 'symptoms'), 5)
  expect_warning( # for location
    gourmexA <- mount(vars, id_cols='id', group_cols=c('group', 'location'), params = myparams_pv) %>%
      ingest(df_test) %>%
      digest(),
    'Detected')
  gourmexB = mount(vars, id_cols='id', group_cols='group_location', params = myparams_pv) %>%
    ingest(df_test %>% filter(!is.na(group), !is.na(location)) %>% mutate(group_location = paste0(group, '_', location), .keep='unused')) %>%
    digest()

  for(var in vars){
    A = gourmexA$storage$stats[[var]] %>% filter(!is.na(group), !is.na(location)) %>% mutate(c = paste0(group, '_', location), .keep='unused') %>% arrange(c) %>% select(ends_with('_group'), p_value)
    B = gourmexB$storage$stats[[var]] %>% mutate(group_location = as.character(group_location)) %>% arrange(group_location) %>% select(ends_with('_group'), p_value)
    A$p_value = round_numbers(A$p_value, precision = 0.0000001)
    B$p_value = round_numbers(B$p_value, precision = 0.0000001)
    expect_identical(A,B)
  }
})


test_that("multiple stratas work", {
  df_test = generate_test_data(500)
  vars2 = setdiff(vars, 'gender')
  gourmexA = mount(vars2, 'id', group_cols='group', strata_cols = c('gender', 'location'), params=myparams) %>%
    ingest(df_test) %>%
    digest()
  gourmexB = mount(vars2, id_cols='id', group_cols='group', strata_cols='gender_location', params = myparams) %>%
    ingest(df_test %>% filter(!is.na(gender), !is.na(location))  %>%  mutate(gender_location = paste0(gender, '_', location), .keep='unused')) %>%
    digest()

  for(var in vars2){
    A = gourmexA$storage$stats[[var]] %>% filter(!is.na(gender), !is.na(location)) %>% mutate(c = paste0(gender, '_', location), .keep='unused') %>% arrange(c) %>% select(ends_with('_group'))
    B = gourmexB$storage$stats[[var]] %>% mutate(gender_location = as.character(gender_location)) %>% arrange(gender_location) %>% select(ends_with('_group'))
    expect_identical(A,B)
  }
})






