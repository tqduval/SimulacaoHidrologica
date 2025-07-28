fun_obj_kge <- function(obs, sim){
  1 - ((cor(obs, sim) - 1)^2) + (((sd(sim)/sd(obs)) - 1)^2) + (((mean(sim)/mean(obs)) - 1)^2)
}