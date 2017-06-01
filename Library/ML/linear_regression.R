linear_regression <- function(data, features, target) {
  feature_columns <- paste(features, collapse="+")
  columns <- as.formula(paste(target, feature_columns, sep='~'))
  
  states <- as.data.frame(data[, c(target, features)])
  fit <- lm(columns, states)
  return(fit)
}

dump_regression_model <- function(fit, path) {
  sink(file=path, append=F, split=T)
  print(fit)
  sink(file=NULL)
}