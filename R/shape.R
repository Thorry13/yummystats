#' Gourmex shaping.
#'
#' @description
#' Once the statistics are computed and stored inside a Gourmex,
#' they usually need to be formatted to be more human-friendly.
#'
#' This can be done through this shaping operation. It will transform each
#' statistical column independently, usually into character vectors.
#'
#' @param gourmex Gourmex ready for shaping.
#'
#' @return A Gourmex with shaped statistical information (ready for dispatch).
#' @importFrom rlang .data .env
#' @importFrom tibble tibble rownames_to_column
#' @export
#'
#' @examples
#' data = tibble::rownames_to_column(iris, 'fid')
#' gourmex = mount(c('Sepal.Length', 'Species'), id_cols='fid', params=default_params())
#' gourmex = ingest(gourmex, data)
#' gourmex = digest(gourmex)
#' gourmex = shape(gourmex)
#'
shape = function(gourmex){
  # W3.1
  params_shapes = reduce_params(gourmex$params$shapes)

  gourmex$storage$shaped = list()
  # W3.2
  # shaped = sapply(gourmex$types$var, function(var){
  for(var in gourmex$types$var){
    df_shaped = gourmex$storage$stats[[var]]
    var_type = gourmex$types %>% filter(.data$var == .env$var) %>% pull(.data$var_type)
    stat_ids = params_shapes %>% filter(.data$var_type == .env$var_type) %>% pull(.data$stat_id)

    for(stat_id in stat_ids){
      current_stat = params_shapes %>% filter(.data$stat_id == .env$stat_id)
      stat_name = current_stat$stat_name
      shape_func = current_stat$shape_func[[1]]

      # W3.2.1
      if(stat_name %in% group_vars(df_shaped))
        df_shaped = df_shaped %>%
          ungroup() %>%
          mutate(across(all_of(stat_name), shape_func)) %>%
          group_by(across(all_of(group_vars(df_shaped))))
      else if(stat_name %in% names(df_shaped))
        df_shaped = df_shaped %>% mutate(across(all_of(stat_name), shape_func))
      else{
        warning(sprintf("Couldn't shape stat `%s` because it wasn't calculated during digestion.", stat_name))
      }
    }
    gourmex$storage$shaped[[var]] = df_shaped
  }

  # W3.3
  return(gourmex)
}
