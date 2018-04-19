generate_data = function (n, p) {
  draws = rnorm(n*p)
  mat = matrix(draws, nrow = n)
  res = rnorm(n)
  return (list(covariates = mat, responses = res))
}

model_select = function (covariates, responses, cutoff) {
  my.lm = lm(responses ~ covariates)
  p-values = summary(my.lm)$coefficients[,4]
  retained = covariates[,p-values <= cutoff]
  if (length(retained) == 0) return (vector(length = 0))
  else {
    new.lm = lm(responses ~ retained)
    new.p-values = summary(new.lm)$coefficients[,4]
    return (new.p-values)
  }
}
