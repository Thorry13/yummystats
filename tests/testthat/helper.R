#### Definition of a standard dataframe generator for tests ####
recycle_df = function(df, N){
  if(nrow(df)){
    m = N %/% nrow(df) + 1
    df = rbind(df, do.call("rbind", replicate(m-1, df, simplify = FALSE)))
    return(slice(df, seq_len(N)))
  }
  else
    return(df)
}

all_values = list(
  gender = factor(c('Female', 'Male', NA), levels=c('Male', 'Female')), # factor
  occupation = c('Doctor', 'Teacher', 'Manager', 'Firefighter', 'Police officer', NA), # categorical
  location = c('Europe', 'US/Canada', 'South America', 'Africa', 'Asia', 'Oceania', NA), # for stratas
  symptoms = expand.grid(list('s1', NULL), list('s2', NULL), list('s3', NULL)) %>% rowwise() %>% mutate(symptoms = list(c(unlist(Var1), unlist(Var2), unlist(Var3)))) %>% pull(symptoms),
  group = c('A', 'B', 'AB+'),
  happy = c(T,F,NA)
)

generate_minimum_test_data = function(group_vars, study_vars, nmin){
  all_groups = rlang::inject(expand.grid(!!!all_values[group_vars]))

  all_df_mins = lapply(study_vars, \(var) rlang::inject(expand.grid(!!!all_values[c(group_vars, var)])) %>% recycle_df(nmin*nrow(.)) %>% select(all_of(var)))
  nmax = max(sapply(all_df_mins, nrow))
  all_groups = recycle_df(all_groups, nmax)
  all_df_mins = lapply(all_df_mins, \(X) recycle_df(X, nmax))

  df_min = rlang::inject(bind_cols(all_groups, !!!all_df_mins))
  return(df_min)
}

generate_test_data = function(N, group_vars=NULL, study_vars=NULL, nmin=0){
  # set.seed(3)

  if(!is.null(group_vars) && !is.null(study_vars))
    df_test_min = generate_minimum_test_data(group_vars, study_vars, nmin)
  else
    df_test_min = tibble()

  if(N < nrow(df_test_min))
    warning(sprintf('Argument `N` is lower than the minimum size (=%d) ensuring all combinations will be satisfied.', nrow(df_test_min)))

  # numerical values
  ages = runif(N, 0, 100)

  # # insert missing values
  ages[1:(0.03*N)] = NA
  ages = sample(ages, N)

  genders = sample(all_values$gender, N, prob=c(0.45, 0.35, 0.2), replace=TRUE)
  occupations = sample(all_values$occupation, N, replace=TRUE) %>% sample(N)
  locations = sample(all_values$location, N, replace=TRUE) %>% sample(N)
  happy = sample(all_values$happy, N, prob=c(0.6, 0.3, 0.1), replace=TRUE) # boolean

  symptoms = sample(all_values$symptoms, N, replace=TRUE)

  groups = sample(all_values$group, N, prob=c(0.5,0.3,0.2), replace=TRUE)

  # Build data frame
  df_test = tibble(
      group=groups,
      age=ages,
      gender=genders,
      happy=happy,
      occupation=occupations,
      symptoms=symptoms,
      location=locations)

  if(nrow(df_test_min))
    df_test[1:nrow(df_test_min), c(group_vars, study_vars)] = df_test_min

  # Shuffle
  df_test = df_test[sample(1:nrow(df_test)),] %>% mutate(id = row_number(), .before=everything())

  return(df_test)
}

