
is_factor <- unlist(lapply(d1, is.factor), use.names = FALSE)
sum(is_factor)
factor_names <- names(d1)[is_factor]

for (f in factor_names) {
   for (l in levels(d1[[f]])[-1]) {
     d1[, gsub("/|-|\\s", "", paste(f, l, sep = "_"))] <- ifelse(d1[[f]] %in% l, 1, 0)
   }
}

freqfit$coefficients
freqfit$terms
attributes(freqfit)
names(freqfit$coefficients)
freqfit$model
length()

test_data <- cbind(ifelse(rnorm(length(freqfit$coefficients)) < 0.5, 1, 0),
             ifelse(rnorm(length(freqfit$coefficients)) < 0.5, 1, 0))

predict_function <- function(mod_fit, test_data) {
  mod_cof <- coef(summary(mod_fit))[, 1]
  mod_p <- coef(summary(freqfit))[, 4]
  cond <- mod_p < 0.05
  mod_cof[cond] %*% test_data[cond, ]
  
}
predict_function(freqfit, test_data)
exp(predict_function(freqfit, test_data))

 