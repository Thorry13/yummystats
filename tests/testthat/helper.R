#### Definition of a standard dataframe for test ####

# numerical values
vals = rnorm(100, 50,5)

# insert missing values
vals[c(13,84,51)] = NA

# categorical values
cats = rep(c(rep('F', 20), rep('M', 20), rep(NA, 10)), 2)

# boolean values
sicks = c(rep(T, 40), rep(F, 10),
          rep(NA, 4), rep(T,38), rep(F, 8))

# lists
signs_list = c('s1', 's2', 's3')
n_signs = floor(runif(100, 0, 4))
S = sapply(n_signs, \(n) sample(signs_list, n))

# Build data frame
df_test = tibble(
  group=c(rep('A', 50), rep('B', 50)),
  # group=rep(c(rep('A', 20), rep('B', 30)), 2),
  val=vals,
  cat=cats,
  id=1:100,
  sick=sicks,
  signs=S
)
