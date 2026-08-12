#' Ingest data: Prepare for analysis
#'
#' @description
#' During ingestion the algorithm will process each variable to analyse by
#' starting evaluating its main type (categorical, logical, numerical), then
#' preparing homogeneous dataframes for stats.
#'
#' The `logical` type usually refers to the same type as `categorical`, however
#' can be displayed in a different way (single line).
#'
#' @param gourmex Mounted Gourmex object ready for analysis.
#' @param data The data to ingest.
#'
#' @returns Gourmex object ready for digestion.
#' @importFrom tibble rownames_to_column
#' @export
#'
#' @examples
#' data = tibble::rownames_to_column(iris, 'fid')
#' gourmex = mount(c('Sepal.Length', 'Species'), id_cols='fid')
#' gourmex = ingest(gourmex, data)
ingest = function(gourmex, data){
  gourmex = gourmex %>%
    introduce(data) %>%
    swallow()

  return(gourmex)
}


introduce = function(gourmex, data){
  # Manage NAs for groups
  if(!is.null(gourmex$params$group_cols)){
    for(col in gourmex$params$group_cols){
      if(any(is.na(data[[col]]))){
        warning(sprintf('Detected and removed observations with NA values for group %s. ', col))
        data = data %>% filter(!is.na(.data[[col]]))
      }
    }
  }

  gourmex$storage$raw = data

  gourmex$types = tibble(var=gourmex$params$vars) %>%
    rowwise() %>%
    mutate(
      var_type = case_when(
        is.logical(data[[var]]) ~ 'logical',
        is.numeric(data[[var]]) ~ 'numerical',
        TRUE ~ 'categorical')
    ) %>%
    ungroup()

  return(gourmex)
}


swallow = function(gourmex){
  gourmex$storage$chewed = list()
  gourmex$storage$stats = list()

  # Process each variable independently
  for(var in gourmex$params$vars){
    chewed = chew(gourmex, var) # W1.1
    gourmex$storage$chewed[[var]] = chewed$chewed # W2.1
    gourmex$storage$stats[[var]] = chewed$stats %>% mutate(variable = var) # W2.1
  }

  return(gourmex)
}


#' @import dplyr tidyr
#' @importFrom rlang .data .env
#' @importFrom data.table :=
chew = function(gourmex, var){
  # Rewrite unnest
  unnest = function(data, ...){
    new_data = tidyr::unnest(data, ...) %>% dplyr_reconstruct(data)
    return(new_data)
  }

  id_cols = gourmex$params$id_cols
  group_cols = c(gourmex$params$group_cols, gourmex$params$strata_cols)

  # Simplify dataframe
  chewed = gourmex$storage$raw %>%
    select(all_of(c(id_cols, group_cols, var))) %>%
    distinct()

  if(!is.null(group_cols)){
    chewed = chewed %>% mutate(across(all_of(group_cols), factor))
  }

  # W1.1.1: Get variable type (numerical, categorical or logical)
  var_type = gourmex$types %>% filter(.data$var == .env$var) %>% pull(.data$var_type)

  # W1.1.2: For categorial and logical
  if(var_type != 'numerical'){
    # Process lists
    if(is.list(chewed[[var]])){
      chewed = chewed %>%
        unnest(!!sym(var), keep_empty = T)  %>%

        # Remove nans unless an individual has NAs only
        group_by(across(all_of(id_cols))) %>%
        mutate(all_na = all(is.na(.data[[var]]))) %>%
        ungroup() %>%
        filter(!is.na(.data[[var]]) | .data$all_na) %>%
        select(-all_of('all_na')) %>%
        distinct()

      # Simplify dataframe
      chewed = chewed %>%
        select(all_of(c(id_cols, var, group_cols))) %>%
        distinct()
    }

    # Reorder levels for factors
    if(!is.factor(chewed[[var]])){
      levels = sort(unique(chewed[[var]]), na.last=T)
      if(var_type == 'logical')
        levels = c(TRUE, FALSE)
      # Factor -> Character
      chewed = chewed %>%
        mutate(!!var := factor(.data[[var]], levels = as.character(levels), exclude=NULL)) %>%
        arrange(!!sym(var))
    }

    # W1.1.3
    stats = chewed %>%
      select(all_of(c(group_cols, var))) %>%
      distinct() %>%
      # rename(level=.data[[var]]) %>%
      complete(!!sym(var), !!!syms(group_cols)) %>%
      mutate(!!var := factor(.data[[var]], levels = na.omit(levels(.data[[var]]))))

    # W1.1.4
    chewed = chewed %>%
      mutate(!!var := factor(.data[[var]], levels = na.omit(levels(.data[[var]]))))
  }
  else{
    # W1.1.3
    stats = chewed %>% select(all_of(group_cols)) %>% distinct() %>%
      complete(!!!syms(group_cols))
  }

  return(list(
    'chewed' = chewed,
    'stats' = stats
  ))
}

