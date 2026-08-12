check_unicity = function(df, var, id_cols=NULL, return_exceptions = FALSE, na.rm = F){
  checks = df %>%
    filter(!na.rm | !is.na(.data[[var]])) %>%
    group_by(across(all_of(id_cols))) %>%
    summarize(n = n_distinct(.data[[var]])) %>%
    ungroup()

  if(return_exceptions)
    return(checks %>% filter(n > 1) %>% select(all_of(id_cols), n) %>% arrange(desc(n)))
  else
    return(!any(checks$n > 1))
}


reduce_params = function(params){
  new_params = params %>%
    group_by(var_type, stat_name) %>%
    slice(n()) %>%
    ungroup() %>%
    mutate(stat_id = row_number())

  return(new_params)
}


#' Load default parameters for Gourmex
#'
#' @param grouped If TRUE, returns additional parameters for comparative analysis.
#'
#' @returns A full set of parameters to make running the Gourmex process.
#' @export
#'
#' @examples
#' params = default_params()
#' gourmex = mount(c('Sepal.Length', 'Petal.Width'), 'Species', params = params)
default_params = function(grouped=FALSE){
  stat_funcs_cat = list(
    N = expr(\(...) stat_total(..., by=gourmex$params$strata_cols)),
    n_avail = expr(\(...) stat_n_avail(..., by=gourmex$params$strata_cols)),
    n_level = expr(\(...) stat_total(..., by= c(var, gourmex$params$strata_cols))),
    n_missing = expr(\(...) stat_n_miss(..., by=gourmex$params$strata_cols)),
    p_level = expr(\(...) stat_perc(..., numerator_col = 'n_level', denominator_col = 'n_avail', by=gourmex$params$strata_cols)))

  stat_funcs_logic = stat_funcs_cat
  stat_funcs_num = list(
    num_stats = expr(\(...) stat_numeric(..., by=gourmex$params$strata_cols)),
    N = expr(\(...) stat_total(..., by=gourmex$params$strata_cols)),
    n_avail = expr(\(...) stat_n_avail(..., by=gourmex$params$strata_cols)),
    n_missing = expr(\(...) stat_n_miss(..., by=gourmex$params$strata_cols)))

  # for comparative analysis
  stat_funcs_cat_gp = list(
    N_group = expr(\(...) stat_total(..., by=c(gourmex$params$group_cols, gourmex$params$strata_cols))),
    n_avail_group = expr(\(...) stat_n_avail(..., by=c(gourmex$params$group_cols, gourmex$params$strata_cols))),
    n_level_group = expr(\(...) stat_total(..., by=c(var, gourmex$params$group_cols, gourmex$params$strata_cols))),
    p_level_group = expr(\(...) stat_perc(..., numerator_col = 'n_level_group', denominator_col = 'n_avail_group', by=gourmex$params$strata_cols)),
    p_value = expr(\(...) stat_pvalue(..., by=gourmex$params$group_cols, strata_cols=gourmex$params$strata_cols))
  )


  stat_funcs_logic_gp = stat_funcs_cat_gp


  stat_funcs_num_gp = list(
    num_stats_group = expr(\(...) stat_numeric(..., by=c(gourmex$params$group_cols, gourmex$params$strata_cols), suffix = '_group')),
    N_group = expr(\(...) stat_total(..., by=c(gourmex$params$group_cols, gourmex$params$strata_cols))),
    n_avail_group = expr(\(...) stat_n_avail(..., by=c(gourmex$params$group_cols, gourmex$params$strata_cols))),
    p_value = expr(\(...) stat_pvalue(..., by=gourmex$params$group_cols, strata=gourmex$params$strata_cols))
  )

  A = tibble(var_type = 'categorical', stat_name = names(stat_funcs_cat), stat_func = unname(stat_funcs_cat))
  B = tibble(var_type = 'logical', stat_name = names(stat_funcs_logic), stat_func = unname(stat_funcs_logic))
  C = tibble(var_type = 'numerical', stat_name = names(stat_funcs_num), stat_func = unname(stat_funcs_num))
  A_gp = tibble(var_type = 'categorical', stat_name = names(stat_funcs_cat_gp), stat_func = unname(stat_funcs_cat_gp))
  B_gp = tibble(var_type = 'logical', stat_name = names(stat_funcs_logic_gp), stat_func = unname(stat_funcs_logic_gp))
  C_gp = tibble(var_type = 'numerical', stat_name = names(stat_funcs_num_gp), stat_func = unname(stat_funcs_num_gp))

  stat_funcs = bind_rows(A,B,C)
  stat_funcs_gp = bind_rows(A_gp,B_gp,C_gp)
  params0 = list('stats'=stat_funcs, 'shapes'=NULL, 'layout'=NULL)
  params = list('stats'=bind_rows(stat_funcs, stat_funcs_gp), 'shapes'=NULL, 'layout'=NULL)


  #---
  # SHAPES

  shapes_cat = list(
    N = format_numbers,
    n_avail = format_numbers,
    n_level = \(x) round_numbers(x, precision=1) %>% format_numbers(),
    n_missing = format_numbers,
    p_level = format_perc
  )
  shapes_logic = shapes_cat
  shapes_num = list(
    N = format_numbers,
    n_avail = format_numbers,
    mean = \(x) style_number(x, 1),
    CI95 = \(x) style_number(x, 1),
    median = \(x) style_number(x, 1),
    min = \(x) style_number(x, 1),
    max = \(x) style_number(x, 1),
    q1 = \(x) style_number(x, 1),
    q3 = \(x) style_number(x, 1)
  )

  shapes_cat_gp = list(
    N_group = format_numbers,
    n_avail_group = format_numbers,
    n_level_group = \(x) round_numbers(x, precision=1) %>% format_numbers(),
    p_level_group = format_numbers,
    p_value = style_pvalue
  )
  shapes_logic_gp = shapes_cat_gp
  shapes_num_gp = list(
    N_group = format_numbers,
    n_avail_group = format_numbers,
    mean_group = \(x) style_number(x, 1),
    CI95_group = \(x) style_number(x, 1),
    median_group = \(x) style_number(x, 1),
    min_group = \(x) style_number(x, 1),
    max_group = \(x) style_number(x, 1),
    q1_group = \(x) style_number(x, 1),
    q3_group = \(x) style_number(x, 1),
    p_value = style_pvalue
  )

  A = tibble(var_type = 'categorical', stat_name = names(shapes_cat), shape_func = unname(shapes_cat))
  B = tibble(var_type = 'logical', stat_name = names(shapes_logic), shape_func = unname(shapes_logic))
  C = tibble(var_type = 'numerical', stat_name = names(shapes_num), shape_func = unname(shapes_num))
  A_gp = tibble(var_type = 'categorical', stat_name = names(shapes_cat_gp), shape_func = unname(shapes_cat_gp))
  B_gp = tibble(var_type = 'logical', stat_name = names(shapes_logic_gp), shape_func = unname(shapes_logic_gp))
  C_gp = tibble(var_type = 'numerical', stat_name = names(shapes_num_gp), shape_func = unname(shapes_num_gp))

  shapes = bind_rows(A,B,C)
  shapes_gp = bind_rows(A_gp,B_gp,C_gp)
  params0$shapes = shapes
  params$shapes = bind_rows(shapes, shapes_gp)

  # ----
  # LAYOUT
  # Change default names with language dictionaries and ENVIRONMENT VARIABLES
  template_cat = tibble(
    var_type = 'categorical',
    "Variable" = "{variable}",
    "Label" = "{level}, N(%)",
    "Donn\u00e9es disponibles" = "{n_avail}",
    "Total, N={N}" = "{n_level} ({p_level})",
  )
  template_cat_group = tibble(
    var_type = 'categorical',
    "Variable" = "{variable}",
    "Label" = "{level}, N(%)",
    "Donn\u00e9es disponibles" = "{n_avail}",
    "{.pivot}" = "{str_replace(sprintf('%s (%s)', n_level_group, p_level_group), fixed('NA (0)'), '-')}",
    "Total, N={N}" = "{n_level} ({p_level})",
    "p-value" = "{p_value}"
  )

  template_logic = tibble(
    var_type = 'logical',
    "Variable" = "{variable}",
    "Label" = "N(%)",
    "Donn\u00e9es disponibles" = "{n_avail}",
    "Total, N={N}" = "{n_level} ({p_level})",
  )
  template_logic_group = tibble(
    var_type = 'logical',
    "Variable" = "{variable}",
    "Label" = "N(%)",
    "Donn\u00e9es disponibles" = "{n_avail}",
    "{.pivot}" = "{str_replace(sprintf('%s (%s)', n_level_group, p_level_group), fixed('NA (0)'), '-')}",
    "Total, N={N}" = "{n_level} ({p_level})",
    "p-value" = "{p_value}"
  )

  template_num = tibble(
    var_type = 'numerical',
    "Variable" = "{variable}",
    "Label" = c("Med.[Q1;Q3](Min;Max)","Moy. \u00b1 CI95%"),
    "Donn\u00e9es disponibles" = "{n_avail}",
    "Total, N={N}" = c("{median} [{q1};{q3}] ({min},{max})", "{mean} \u00b1 {CI95}")
  )
  template_num_group = tibble(
    var_type = 'numerical',
    "Variable" = "{variable}",
    "Label" = c("Med.[Q1;Q3](Min;Max)","Moy. \u00b1 CI95%"),
    "Donn\u00e9es disponibles" = "{n_avail}",
    "{.pivot}" = c("{median_group} [{q1_group};{q3_group}] ({min_group},{max_group})", "{mean_group} \u00b1 {CI95_group}"),
    "Total, N={N}" = c("{median} [{q1};{q3}] ({min},{max})", "{mean} \u00b1 {CI95}"),
    "p-value" = c("", "{p_value}")
  )

  full_template = bind_rows(template_cat, template_logic, template_num)
  full_template_group = bind_rows(template_cat_group, template_logic_group, template_num_group)

  params0$layout = list()
  params0$layout$template = full_template

  params$layout = list()
  params$layout$template = full_template_group
  params$layout$pivot_sep = '_'
  params$layout$pivot_glue = sprintf('{.pivot}, N={N_group}')

  if(grouped)
    return(params)
  else
    return(params0)
}
