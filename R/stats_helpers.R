#' Calculate counts of a level for a given variable.
#'
#' @param gourmex A `gourmex` object.
#' @param var The variable to compute statistic on.
#' @param stat_name Indicates how the new column should be named.
#' @param by Is used as grouping variables.
#'
#' @return A dataframe with the required counts.
stat_total = function(gourmex, var, stat_name='N', by=NULL){
  # check_report_df(df)
  df_chewed = gourmex$storage$chewed[[var]]

  # Calculate total
  df_stat = df_chewed %>%
    group_by(across(all_of(gourmex$params$id_cols))) %>%
    mutate(unity_id = cur_group_id()) %>%
    ungroup() %>%
    group_by(across(all_of(unname(by)))) %>%
    summarize(!!stat_name := n_distinct(unity_id)) %>%
    ungroup()

  return(df_stat)
}


#' Extend the statistics with percentage column named `stat_name`.
#'
#' @param gourmex A `gourmex` object.
#' @param numerator_col The numerator.
#' @param denominator_col The denominator. If NULL, the sum of the `count_col` column is used as `total_col`.
#' @param stat_name indicates how the percentage column should be named.
#' @param by is used as grouping variables.
#'
#' @return A dataframe with the required percentages.
stat_perc = function(gourmex, var, stat_name='p', numerator_col='n', denominator_col=NULL, by=NULL){
  # numerator_col and denominator_col should be numerical...

  # df_chewed = gourmex$storage$chewed[[var]]
  df_stat = gourmex$storage$stats[[var]]
  var_type =gourmex$types %>% filter(var == .env$var) %>% pull(var_type)

  # Use grouping variables if necessary
  df_stat = df_stat %>% group_by(across(all_of(by)))

  # Use default denominator if NULL
  if(is.null(denominator_col)){
    df_stat = df_stat %>% mutate(tmp_denom = sum(!!sym(numerator_col)))
    denominator_col = 'tmp_denom'
  }

  if(var_type == 'numerical')
    df_stat[[var]] = TRUE

  # Calculate percentage
  df_stat = df_stat %>%
    mutate(!!stat_name := case_when(
      !is.na(.data[[var]]) ~ !!sym(numerator_col)/(!!sym(denominator_col) + .Machine$double.eps),
      is.na(.data[[var]]) ~ as.numeric(NA))) %>%
    select(-any_of('tmp_denom')) %>%
    ungroup()

  if(var_type == 'numerical')
    df_stat = df_stat %>% select(-any_of(var))

  return(df_stat)
}


#' Extend the statistics with the number of available data as a column named `stat_name`
#'
#' @param gourmex A `gourmex` object.
#' @param var The column name of `df` where nan values can be detected.
#' @param stat_name indicates how the column reporting the number of available data should be named.
#' @param by is used as grouping variables
#'
#' @return A dataframe with the required available values.
stat_n_avail = function(gourmex, var, stat_name="n_avail", by=NULL){
  df_chewed = gourmex$storage$chewed[[var]]
  df_stat = gourmex$storage$stats[[var]]
  var_type =gourmex$types %>% filter(var == .env$var) %>% pull(var_type)

  # Count available values
  df_stat = df_chewed %>%
    group_by(across(all_of(c(by, gourmex$params$id_cols)))) %>%
    mutate(is_avail = any(!is.na(.data[[var]]))) %>%
    ungroup() %>%
    select(all_of(c(by, gourmex$params$id_cols)), is_avail) %>%
    distinct() %>%
    group_by(across(all_of(by))) %>%
    summarize(!!stat_name := sum(is_avail)) %>%
    ungroup()

  return(df_stat)
}


#' Extend the statistics with the number of missing data as a column named `stat_name`
#'
#' @param gourmex A `gourmex` object.
#' @param var The column name of `df` where nan values can be detected.
#' @param count_col The name of the column to count non-nan values on.
#' @param missing_col indicates how the column reporting the number of missing data should be named.
#' @param by is used as grouping variables
#'
#' @return A dataframe with the required missing values.
stat_n_miss = function(gourmex, var, stat_name='n_miss', by=NULL){
  # check_report_df(df)
  df_chewed = gourmex$storage$chewed[[var]]
  # df_stat = gourmex$storage$stats[[var]]
  var_type =gourmex$types %>% filter(var == .env$var) %>% pull(var_type)

  # Count missing values
  df_stat = df_chewed %>%
    group_by(across(all_of(c(by, gourmex$params$id_cols)))) %>%
    mutate(is_missing = all(is.na(.data[[var]]))) %>%
    ungroup() %>%
    select(all_of(c(by, gourmex$params$id_cols)), is_missing) %>%
    distinct() %>%
    group_by(across(all_of(by))) %>%
    summarize(!!stat_name := sum(is_missing)) %>%
    ungroup()

  return(df_stat)
}

mychisq_test = function(vals, groups){
  if(n_distinct(groups) > 1 & n_distinct(vals) > 1)
    p = chisq.test(as.factor(vals), as.factor(groups), correct = F)$p.value
  else
    p = as.numeric(NA)
  return(p)
}

#' @import rstatix
myanova_test = function(data, f){
  res_anova = anova_test(data, f)
  if(is_grouped_df(data))
    return(as.data.frame(res_anova[,c(group_vars(data), 'p')]))
  else
    return(data.frame(p=res_anova$p))
}

#' Extend the statistics with the significance
#'
#' @param gourmex A `gourmex` object.
#' @param var The variable used to evaluate p-value.
#' @param stat_name Indicates how the p-values columns should be named.
#' @param by Is used as grouping variables.
#' @param strata_var is used to separate multiple comparisons.
#'
#' @return A dataframe with the required p-values.
stat_pvalue = function(gourmex, var, stat_name='p_value', by=NULL, strata_var=NULL){
  df_chewed = gourmex$storage$chewed[[var]]
  # df_stat = gourmex$storage$stats[[var]]
  var_type = gourmex$types %>% filter(var == .env$var) %>% pull(var_type)
  id_cols = gourmex$params$id_cols
  # strata_var = gourmex$params$strata_var

  # Filter nans
  inputs_filtered = df_chewed %>% filter(!is.na(.data[[var]])) # %>% distinct() # Why distinct ?

  if(!is.null(by)){
    for (col in by)
      inputs_filtered = inputs_filtered %>% filter(!is.na(.data[[col]]))
    df_by = inputs_filtered %>%
      group_by(across(all_of(by))) %>%
      summarize(id_ = as.character(cur_group_id())) %>% ungroup()
    # Display warning ?
    inputs_filtered = inputs_filtered %>% left_join(df_by, by=by)
  }

  # No within groups tests for the moment
  # if(is.null(by) || n_distinct(df$id_) < 2)
  else
    stop("Only \"between groups\" tests for significance are currently supported. Please provide a `by` argument containing at least two levels.")

  if(var_type %in% c('categorical', 'logical')){
    # attr(df_stat, 'p_test') = 'X²-test' # Set testing method as a parameter

    # Check unity
    U = check_unicity(df_chewed, var, id_cols, return_exceptions = T)
    pval_per_level = nrow(U) > 1 # if a multiple choice field is detected, compute p-value per level

    if(!pval_per_level){
      # Calculate p-value
      df_stat = inputs_filtered %>%
        group_by(across(all_of(strata_var))) %>%
        summarize(!!stat_name := mychisq_test(.data[[var]], id_)) %>%
        ungroup()
    }

    # Else each level is considered as an independent variable
    else{
      # Test already checked above ?
      if(n_distinct(inputs_filtered$id_) > 1 & n_distinct(inputs_filtered[[var]]) > 1){
        # Calculate p-value
        df_stat = inputs_filtered %>%
          mutate(V = TRUE) %>%
          pivot_wider(names_from = all_of(var), values_from=V, names_sort = T) %>% # Pivoting to convert level as a logical variable
          mutate(across(-all_of(c(by, id_cols, 'id_', strata_var)), ~replace_na(.x, FALSE))) %>%
          group_by(across(all_of(strata_var))) %>%
          summarize(across(-all_of(c(id_cols, by, 'id_')),
                           function(X) {mychisq_test(X, .data[['id_']])})) %>%
          ungroup() %>%

          # Reshape back
          pivot_longer(-all_of(strata_var), names_to=var, values_to=stat_name)

        # Fill NAs
        df_stat = df_stat %>%
          mutate(!!var := factor(!!sym(var), levels = levels(inputs_filtered[[var]]))) %>%
          complete(!!sym(var), fill = list(value=NA))
      }
      else
        df_stat = tibble(as.numeric(NA)) %>% setNames(stat_name)
    }
  }

  # Compare means for numerical stats
  else if(var_type == 'numerical'){
    f = sprintf('%s ~ id_', var)
    if(n_distinct(inputs_filtered$id_) == 2 & min(table(inputs_filtered$id_)) > 1){
      df_stat = inputs_filtered %>%
        group_by(across(all_of(strata_var))) %>%
        summarize(!!stat_name := t.test(.data[[var]]~ as.numeric(id_))$p.value) %>%
        ungroup() %>%
        select(all_of(c(strata_var, stat_name))) %>%
        distinct()
    }
    else if(n_distinct(inputs_filtered$id_) > 2){
      df_stat = inputs_filtered %>%
        group_by(across(all_of(strata_var))) %>%
        myanova_test(as.formula(f)) %>%
        rename(!!stat_name := p) %>%
        select(all_of(c(strata_var, stat_name))) %>%
        distinct()
      # p = do.call('anova_test', list(inputs_filtered, as.formula(f)))$p
      # attr(df_stat, 'p_test') = 'ANOVA'
    }
    else{
      df_stat = inputs_filtered %>%
        select(all_of(strata_var)) %>%
        mutate(!!stat_name := as.numeric(NA))
    }

  }
  return(df_stat)
}


#' Extend statistics for numerical variables, with the following values :
#' mean, std, min, Q1, median, Q3, max
#'
#' @param gourmex A `gourmex` object.
#' @param var A numerical variable name.
#' @param stat_name Name used to store the results.
#' @param suffix Used to append a suffix on numerical statistics names.
#' @param by Used to group results.
#'
#' @return A dataframe with the numerical stats.
stat_numeric = function(gourmex, var, stat_name, suffix=NULL, by=NULL){
  df_chewed = gourmex$storage$chewed[[var]]
  id_cols = gourmex$params$id_cols

  base_names = c('mean', 'std', 'SE', 'CI95', 'min', 'q1', 'median', 'q3', 'max')
  new_names = paste0(base_names, suffix) # Raise warning if already existing columns

  # Summary

  df_stats = df_chewed %>%
    group_by(across(all_of(by))) %>%
    summarise(
      n = sum(!is.na(.data[[var]])),
      mean = mean(.data[[var]], na.rm=T),
      std = sd(.data[[var]], na.rm=T),
      SE = std/sqrt(n),
      CI95 = qnorm(0.975)*SE,
      min = min(.data[[var]], na.rm=T),
      q1 = quantile(.data[[var]], 0.25, na.rm=T),
      median = median(.data[[var]], na.rm=T),
      q3= quantile(.data[[var]], 0.75, na.rm=T),
      max = max(.data[[var]], na.rm=T)
    ) %>%
    select(-n) %>%
    ungroup() %>%
    rename(base_names %>% setNames(new_names))

  return(df_stats)
}
