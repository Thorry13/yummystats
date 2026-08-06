#' @importFrom rlang .data
check_unicity = function(df, var, id_cols=NULL, return_exceptions = FALSE, na.rm = F){
  checks = df %>%
    filter(!na.rm | !is.na(.data[[var]])) %>%
    group_by(across(all_of(id_cols))) %>%
    summarize(n = n_distinct(.data[[var]])) %>%
    ungroup()

  if(return_exceptions)
    return(checks %>% filter(n > 1) %>% select(all_of(id_cols), n)) %>% arrange(desc(n))
  else
    return(!any(checks$n > 1))
}


#' @importFrom rlang .data
reduce_params = function(params){
  new_params = params %>%
    group_by(var_type, stat_name) %>%
    slice(n()) %>%
    ungroup() %>%
    mutate(stat_id = row_number())

  return(new_params)
}

