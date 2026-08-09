#### Definition of a standard dataframe for test ####
set.seed(1)

generate_test_data = function(N){
  # numerical values
  ages = runif(N, 0, 100)

  # # insert missing values
  ages[1:(0.03*N)] = NA
  ages = sample(ages, N)

  # factor
  genders = factor(c(rep('Female', 0.45*N), rep('Male', 0.35*N), rep(NA, 0.2*N))) %>% sample(N)

  # categorical values
  some_occupations = c('Doctor', 'Teacher', 'Manager', 'Firefighter', 'Police officer', NA)
  occupations = sample(some_occupations, N, replace=TRUE) %>% sample(N)

  # for stratas
  some_locations = c('Europe', 'US/Canada', 'South America', 'Africa', 'Asia', 'Oceania', NA)
  locations = sample(some_locations, N, replace=TRUE) %>% sample(N)

  # boolean values
  happy = c(rep(T, 0.78*N), rep(F, 0.18*N), rep(NA, 0.4*N)) %>% sample(N)

  # lists
  symptoms_list = c('s1', 's2', 's3')
  n_s = floor(runif(N, 0, 4))
  symptoms = sapply(n_s, \(n) sample(symptoms_list, n)) %>% sample(N)

  groups = c(rep('A', 0.5*N), rep('B', 0.3*N), rep('AB+', 0.2*N)) %>% sample(N)

  # Build data frame
  df_test = tibble(
    group=groups,
    age=ages,
    gender=genders,
    happy=happy,
    occupation=occupations,
    symptoms=symptoms,
    location=locations) %>%
    mutate(id = row_number())

  return(df_test)
}

