check_unicity = function(df, var, id_cols=NULL, return_exceptions = FALSE, na.rm = F){
  checks = df %>%
    filter(!na.rm | !is.na(.data[[var]])) %>%
    group_by(across({{id_cols}})) %>%
    summarize(n = n_distinct(.data[[var]])) %>%
    ungroup()

  if(return_exceptions)
    return(checks %>% filter(n > 1) %>% select({{id_cols}}, n)) %>% arrange(desc(n))
  else
    return(!any(checks$n > 1))
}
