generate_data = function (n, p) {
  draws = rnorm(n*p)
  mat = matrix(draws, nrow = n)
  res = rnorm(n)
  return (list(covariates = mat, responses = res))
}