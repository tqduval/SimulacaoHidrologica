# Nash-Sutcliff
fun_nse <- function(obs, sim){
  1 - sum((obs - sim)^2)/sum((obs - mean(obs))^2)
}