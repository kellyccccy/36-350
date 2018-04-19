generate_data = function (n, p) {
  draws = rnorm(n*p)
  mat = matrix(draws, nrow = n)
  res = rnorm(n)
  return (list(covariates = mat, responses = res))
}

model_select = function (covariates, responses, cutoff) {
  my.lm = lm(responses ~ covariates)
  p_values = summary(my.lm)$coefficients[,4]
  ind = p_values <= cutoff
  ind = ind[2:length(ind)]
  retained = covariates[,ind]
  if (length(retained) == 0) return (vector(length = 0))
  else {
    new.lm = lm(responses ~ retained)
    new.p_values = summary(new.lm)$coefficients[,4]
    return (new.p_values)
  }
}


run_simulation = function (n_trials, n, p, cutoff) {
  ps = vector(mode = "list", length = n_trials)
  for (i in 1:n_trials) {
    dat = generate_data(n, p)
    ps[[i]] = model_select(dat$covariates, dat$responses, cutoff)
  }
  ps_all = Reduce(c, ps)
  if (!length(ps_all) == 0) hist(ps_all)
}

for (n in c(100, 1000, 10000)) {
  for (p in c(10, 20, 50)) {
    run_simulation(50, n, p, 0.05)
  }
}