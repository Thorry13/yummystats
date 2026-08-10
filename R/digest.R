#' Run Gourmex digestion
#'
#' @description
#' At this state, the Gourmex object is sufficiently set up and can process the
#' statistical analysis.
#'
#' It saves the results internally so it can be used later with minimal arguments.
#'
#' @param gourmex A Gourmex object ready for digestion (ingestion done).
#'
#' @returns A Gourmex ready for shaping.
#' @importFrom rlang .data .env
#' @importFrom tibble rownames_to_column
#' @export
#'
#' @examples
#' data = tibble::rownames_to_column(iris, 'fid')
#' gourmex = mount(c('Sepal.Length', 'Species'), id_cols='fid', params=default_params())
#' gourmex = ingest(gourmex, data)
#' gourmex = digest(gourmex)
#'
digest = function(gourmex){
  params_stats = reduce_params(gourmex$params$stats) # W2.1

  # W2.2
  for(var in gourmex$types$var){
    var_type = gourmex$types %>% filter(.data$var == .env$var) %>% pull(.data$var_type)
    stat_ids = params_stats %>% filter(.data$var_type == .env$var_type) %>% pull(.data$stat_id)

    for(stat_id in stat_ids){
      current_stat = params_stats %>% filter(.data$stat_id == .env$stat_id)
      stat_name = current_stat$stat_name
      stat_func = eval(current_stat$stat_func[[1]])
      df_stats = stat_func(gourmex, var, stat_name=stat_name) # W2.2.1
      gourmex = store_stats(gourmex, var, df_stats) # W2.2.2
    }
  }

  return(gourmex)
}


# W2.2.2
store_stats = function(gourmex, var, new_stats){
  df_stats = gourmex$storage$stats[[var]]
  join_cols = intersect(names(new_stats), c(var, gourmex$params$group_cols, gourmex$params$strata_cols))
  stat_names = names(new_stats) %>% setdiff(join_cols)

  # Remove former stats (show warning if any ?)
  df_stats = df_stats %>% select(-any_of(stat_names))

  # Add stats
  if(length(join_cols)==0)
    df_stats = cbind(df_stats, new_stats)
  else
    df_stats = df_stats %>% left_join(new_stats, by=join_cols) # cross_join warning ?

  gourmex$storage$stats[[var]] = df_stats
  return(gourmex)
}
