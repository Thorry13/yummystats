#' This is an advanced function to round numbers.
#' It can apply a "smart rounding" taking all values into account instead of
#' considering them independently to make it consistent with the total (e.g. percentages).
#' There are optional parameters to keep raw values for low numbers, which might be useful
#' in this environment of functions.
#'
#' @param V A numeric vector whose values need to be rounded
#' @param precision If NULL, no rounding is applied.
#' @param total_consistency If TRUE, find the best adjustments to make the sum of rounded values
#' equal to the rounded sum.
#' @param keep_low_numbers If TRUE, avoid rounding low numbers, according to `low_limit` argument.
#' @param low_limit Values below this threshold are considered as low. If NULL, `low_limit` is set as
#' `precision`.
#'
#' @return A numeric vector with rounded values
#' @export
#'
#' @examples
#' V = c(3.3,33.3,13.4)
#' round_numbers(V)
#' round_numbers(V, precision=10)
#' round_numbers(V, total_consistency=T)
#' round_numbers(V, precision=5, keep_low_numbers=T)
#' round_numbers(V, precision=5, keep_low_numbers=T, low_limit=3)
round_numbers = function(V, precision = 1, total_consistency = FALSE, keep_low_numbers = FALSE, low_limit=NULL){
  if(is.null(precision))
    return(as.numeric(V))
  else{
    # if(is.null(total_consistency))
    #   total_consistency=T

    # Consider values as independent
    if(!total_consistency)
      new_V = as.numeric(round(V/precision)*precision)

    # Otherwise find the best rounding so that sum of rounded values is equal to the rounded sum.
    # Add reference for the method
    else{
      rounded_sum = round_numbers(sum(V), precision = precision)
      floor_V = as.numeric(floor(V/precision)*precision)
      missing = rounded_sum - sum(floor_V) # Space between rounded sum and the sum of floored values

      # Order residuals
      residuals = V - floor_V
      ranks = length(residuals) + 1 - rank(residuals, ties.method='first')

      # Find the best adjustments
      indexes = which(ranks <= missing/precision)
      new_V = floor_V
      new_V[indexes] = new_V[indexes] + precision
    }

    # It might be necessary to keep low numbers to avoid rounding to 0 and hide it instead
    if(keep_low_numbers){
      if(is.null(low_limit))
        low_limit = precision
      new_V = case_when(
        V < low_limit & V > 0 ~ as.numeric(V),
        V < precision & new_V < V ~ as.numeric(V),
        TRUE ~ new_V)
    }
    return(new_V)
  }
}


#' Basic tool to format numbers
#'
#' @param V A numeric vector to format.
#' @param big.mark Imported parameter.
#' @param digits Imported parameter.
#' @param lower_bound If not NULL, used as a threshold to hide values below.
#'
#' @return A character vector with formatted numbers
#' @import gtsummary
#' @export
#'
#' @examples
#' V = c(13,52.1,2300,4)
#' format_numbers(V, big.mark=',', digits=2, lower_bound=10)
format_numbers = function(V, big.mark = '', digits = 0, lower_bound = NULL){
  # dec.mark ?

  formatted_V = formatC(V, format = 'f', big.mark = big.mark, digits = digits)

  if(is.null(lower_bound))
    return(formatted_V)
  else{
    formatted_V = case_when(
      V < lower_bound & V > 0 ~ sprintf('<%s', style_sigfig(lower_bound, digits=1)),
      TRUE ~ formatted_V)
  }
  return(formatted_V)
}


#' Tool to display percentages
#'
#' @param V A numeric vector with ratios as values.
#' @param precision Imported parameter. Applied on values multiplied by 100.
#' @param digits Imported parameter.
#' @param sumTo100 If TRUE, apply the best rounding adjustments to assure a sum equal to 100%.
#' @param add_sign If TRUE, basically paste a "%" sign.
#'
#' @return A character vector with formatted percentages
#' @export
#'
#' @examples
#' V = c(0.12,0.346,0.534)
#' format_perc(V, digits=1, add_sign=TRUE)
format_perc = function(V, precision=1, digits=0, sumTo100=F, add_sign=F){
  V = (100*V) %>%
    round_numbers(precision=precision, total_consistency=sumTo100, keep_low_numbers=T, low_limit = .5) %>%
    format_numbers(digits=digits, lower_bound=1)

  # big.mark ? dec.mark ?

  if(add_sign)
    V = paste(V, '%')

  return(V)
}


#' Translate terms following a given dictionary.
#'
#' @param V A character vector containing terms to translate.
#' @param df_dict A data frame used as a dictionary (columns=var, rname, label, group).
#' @param from Reference for origin column.
#' @param to Reference for destination column.
#' @param .fn is applied finally on the translated terms.
#' @param default Default value if no correspondance was found in *df_dict*. If NULL,
#' keep the previous values.
#'
#' @return A character vector with the translated terms.
#' @importFrom data.table :=
#' @export
#'
#' @examples
#' df_dict = tibble(low=letters, up=LETTERS)
#' V = sample(letters, 5, replace=TRUE)
#' format_categorical(V, df_dict, from='low', to='up')
format_categorical = function(V, df_dict, .fn = NULL, from='id', to='label', default=NULL, unmatched=NULL){
  na_positions = which(is.na(V))
  labels = data.frame(col=as.character(V)) %>%
    left_join(df_dict %>% mutate(!!from:=as.character(.data[[from]])), by=c(col=from)) %>%
    pull(!!sym(to))

  unmatched_positions = setdiff(which(is.na(labels)), na_positions)

  labels = case_when(is.na(labels) ~ as.character(V), !is.na(labels) ~ as.character(labels))

  if(!is.null(default))
    labels = labels %>% replace_na(default)
  if(!is.null(unmatched))
    labels[unmatched_positions] = unmatched

  if(!is.null(.fn))
    labels = .fn(labels)

  if(is.null(default))
    labels[na_positions] = as.character(NA)

  if(is.factor(V)){
    new_levels = format_categorical(levels(V), df_dict, .fn, from, to, default)
    labels = factor(labels, levels=new_levels)
  }

  return(labels)
}


#' Apply a format function on a list-column, as if it was a unnested.
#'
#' @param L The list whose values must be formatted.
#' @param format_func The format function to apply on the unnested data.
#' @param .fn A post-process function to apply on row elements. Ignored if NULL.
#'
#' @return A formatted list
#' @export
#'
#' @examples
#' L = list(A=c(12,3.1), B=NULL, C=c(1202.435), D=c(NA, 9, 11.7))
#' format_list(L, \(x) format_numbers(x, big.mark=',', lower_bound=10))
format_list = function(L, format_func, .fn=NULL){
  empty_positions = which(sapply(L, is.null))

  # Process list
  new_L = tibble(i=1:length(L), L=L) %>%
    unnest(L, keep_empty = T) %>% # unnest to apply formatting on each value
    mutate(L = format_func(L)) %>% # apply formatting
    chop(L) %>% # convert back to list
    pull(L)

  new_L[empty_positions] = list(NULL)
  # Apply post-process function if provided
  if(!is.null(.fn))
    new_L = lapply(new_L, .fn)


  return(new_L)
}


#' @import stringr
string_to_multiline = function(s0, l=25, linesep='\n', patterns=c(' ', '-')){
  if(length(s0) > 1)
    return(sapply(s0, string_to_multiline, l, linesep))

  if(str_length(s0) <= l)
    return(s0)

  if(!length(patterns))
    patterns=''

  if(str_starts(s0, patterns[1]))
    string_to_multiline(str_remove(s0, patterns[1]), l, linesep, patterns)

  tokens = str_split_1(as.character(s0), patterns[1])

  if(str_length(tokens[1]) > l){
    new_patterns = patterns[-1]
    parts = string_to_multiline(tokens[1], l=l, linesep=linesep, patterns=new_patterns)
    part1 = str_split_1(parts, linesep)[1]
    remaining_s = str_remove(s0, part1)
    return(paste0(part1, linesep, string_to_multiline(remaining_s, l, linesep, patterns)))
  }
  else{
    lines = data.frame(token = tokens) %>%
      mutate(
        len = sapply(token, str_length) + (row_number() > 1)*str_length(patterns[1]),
        cum_len = cumsum(len),
        group = cum_len > l
      ) %>%
      group_by(group) %>%
      summarise(lines = paste(token, collapse = patterns[1])) %>%
      pull(lines)

    return(paste0(lines[1], patterns[1], linesep, string_to_multiline(lines[2], l, linesep, patterns)))
  }
}


#' @import stringr
string_to_thead = function(s0, l=25, force_tag=F){
  if(length(s0) > 1)
    return(sapply(s0, string_to_thead, l, force_tag))

  if(force_tag | str_length(s0) > l){
    new_s = string_to_multiline(s0, l, linesep='\\\\')
    new_s = sprintf('\\thead{%s}', new_s)}
  else
    new_s = s0
  return(new_s)
}


#' @import stringr
string_to_makecell = function(s0, l=45, force_tag=F){
  if(length(s0) > 1)
    return(sapply(s0, string_to_makecell, l, force_tag))

  if(force_tag | str_length(s0) > l){
    new_s = string_to_multiline(s0, l, linesep='\\\\')
    new_s = sprintf('\\makecell[l]{%s}', new_s)}
  else
    new_s = s0
  return(new_s)
}


# string_to_makecell_old = function(s0, l=45){
#   if(str_detect(s0, '\n')){
#     S = str_split_1(s0, '\n')
#     tmp_s = paste0(S, collapse='\\\\')
#     new_s = sprintf('\\makecell[l]{%s}', tmp_s)}
#   else if(!is.null(l) && str_length(s0) > l){
#     starts = seq(1, str_length(s0), l)
#     ends = starts + l - 1
#     S = str_sub(s0, starts, ends)
#     tmp_s = paste0(S, collapse='\\\\')
#     new_s = sprintf('\\makecell[l]{%s}', tmp_s)}
#   else
#     new_s = s0
#   return(new_s)
# }
