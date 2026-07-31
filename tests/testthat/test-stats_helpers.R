test_that("stat_total works", {
  vars = c('gender', 'happy', 'occupation', 'symptoms') # , 'age')
  # vars = 'gender'
  gourmex = mount(vars, id_cols='id', group_cols='group', params = myparams2) %>%
    ingest(df_test) %>%
    digest()

  for(var in vars){
    df_stats = gourmex$storage$stats[[var]] %>%
      mutate(!!var := as.character(.data[[var]]))

    expect_equal(unique(df_stats$N), n_distinct(df_test$id))

    df_n_level = df_test %>% unnest(!!var, keep_empty = TRUE) %>%
      summarize(n = n_distinct(id), .by=all_of(var)) %>%
      mutate(!!var := as.character(.data[[var]])) %>% full_join(df_stats, by=var)
    expect_equal(df_n_level$n, df_n_level$n_level)

    df_N_group = df_test %>% summarize(n = n_distinct(id), .by='group') %>%
      full_join(df_stats, by='group')
    expect_equal(df_N_group$n, df_N_group$N_group)

    df_n_level_group = df_test %>% unnest(!!var, keep_empty = TRUE) %>%
      summarize(n = n_distinct(id), .by=all_of(c(var, 'group'))) %>%
      mutate(!!var := as.character(.data[[var]])) %>% full_join(df_stats, by=c(var, 'group'))
    expect_equal(df_n_level_group$n, df_n_level_group$n_level_group)
  }
})


test_that("stat_perc works", {
  vars = c('gender', 'happy', 'occupation', 'symptoms') # , 'age')
  gourmex = mount(vars, id_cols='id', group_cols='group', params = myparams2) %>%
    ingest(df_test) %>%
    digest()

  for(var in vars){
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
  vars = c('gender', 'happy', 'occupation', 'symptoms') # , 'age')
  # vars = 'gender'
  gourmex = mount(vars, id_cols='id', group_cols='group', params = myparams2) %>%
    ingest(df_test) %>%
    digest()

  for(var in vars){
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
