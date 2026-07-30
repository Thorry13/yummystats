reduce_params = function(params){
  new_params = params %>%
    group_by(var_type, stat_name) %>%
    slice(n()) %>%
    ungroup() %>%
    mutate(stat_id = row_number())

  return(new_params)
}

# W2
digest = function(gourmex){
  # stats_defs = reduce_params(gourmex$params$stats, verbose=TRUE) # W2.1
  params_stats = reduce_params(gourmex$params$stats) # W2.1

  # W2.2
  for(var in gourmex$types$var){
    var_type = gourmex$types %>% filter(var == .env$var) %>% pull(var_type)
    stat_ids = params_stats %>% filter(var_type == .env$var_type) %>% pull(stat_id)

    for(stat_id in stat_ids){
      current_stat = params_stats %>% filter(stat_id == .env$stat_id)
      stat_name = current_stat$stat_name
      stat_func = eval(current_stat$stat_func[[1]])
      df_stats = stat_func(gourmex, var, stat_name=stat_name) # W2.2.1
      gourmex = store_stats(gourmex, var, df_stats) # W2.2.2
    }

    # , keep_na_rows=T # Comment faire si on relance ?
  }

  return(gourmex)
}


# W2.2.2
store_stats = function(gourmex, var, new_stats){
  df_stats = gourmex$storage$stats[[var]]
  join_cols = intersect(names(new_stats), c(var, gourmex$params$group_cols))
  stat_names = names(new_stats) %>% setdiff(join_cols)

  # Remove former stats (show warning if any ?)
  df_stats = df_stats %>% select(-any_of(stat_names))

  # Add stats
  if(is.null(join_cols))
    df_stats = cbind(df_stats, new_stats)
  else
    df_stats = df_stats %>% left_join(new_stats, by=join_cols)

  gourmex$storage$stats[[var]] = df_stats
  return(gourmex)
}
