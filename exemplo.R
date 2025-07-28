
# Pacotes
if(!require(pacman)) install.packages("pacman")
pacman::p_load(lubridate, tidyverse, ggplot2, SoilHyP, beepr, patchwork)

# Importar funções
source("proj_functions.R")

# fun_setup_files()
path <- "Base de Dados"
catchment_code <- 33
which_et <- "et_ens"
res_setup_files <- fun_setup_files(path = path,
                                   catchment_code = catchment_code,
                                   which_et = which_et)

epq <- res_setup_files[["data"]]  # tbl_df c/ 'date', 'et', 'p', e 'qobs'
area <- res_setup_files[["area"]] # área da bacia (km²)

# fun_periods()
periods <- c(0.05, 0.5, 0.45)
periods <- c(0.5, 0.5)

res_periods <- fun_periods(epq = epq,
                           periods = periods)

epq_calib <- res_periods[["calib"]]
epq_valid <- res_periods[["valid"]]

which_obj <- "nse"
which_obj <- "kge"

# Escolher função objetivo
if(which_obj == "nse") fun_obj <- fun_obj_nse
if(which_obj == "kge") fun_obj <- fun_obj_kge

which_model <- "hymod"

# Escolher modelo
if(which_model == "hymod") fun_model <- HyMod

# Rodar o modelo e calibrar
# Criar uma função que roda o modelo e calcula a função objetivo
run <- function(par, epq, area){
  
  # Vazões simuladas (m³/s)
  qsim <- fun_model(x = par,
                    EPQ = epq[,c(2,3)],
                    area = area)
  
  # Calcular função objetivo
  qobs <- epq[["qobs"]]
  fo <- fun_obj(obs = qobs[!is.na(qobs)], # filtrar vazões observadas NA
                sim = qsim[!is.na(qobs)]) # filtrar vazões observadas NA
  return(-fo)
  
}

par <- c(300, 0.3, 0.6, 0.01, 0.4)
res_model <- fun_model(x = par,
                       EPQ = epq_calib[,c(2,3)],
                       area = area)

res_run <- run(par = par,
               epq = epq_calib,
               area = area)

# Calibração
control <- list(maxit = 20,
                reltol = 0.001,
                ncomplex = 5,
                tolsteps = 6,
                trace = 1)

which_cal <- "sce"

par_limits <- tibble(par_name = c("cmax", "b", "alpha", "f_lento", "f_rap"),
                     min = c(50, 1e-6, 1e-6, 1e-6, 1e-6),
                     max = c(1000, 2, 1, 0.2, 0.9))

res_cal <- fun_cal(which_cal = which_cal, # p/ fins de calculo somente essa importa
                   which_obj = which_obj, # esse argumento
                   periods = periods)     # e esse só servem p/ gerar o resultado final

# Extrair informações
ts <- res_cal[["ts"]]             # série temporal
fo_value <- res_cal[["fo_value"]] # valor da função objetivo

# Visualização
# Extrair datas de validação e calibração
date_calib <- epq_calib[["date"]][[1]]
date_valid <- epq_valid[["date"]][[1]]

# Plotar série histórica
colors <- c("Observada" = "grey50", "Simulada" = "black")
plot_ts <-
  ggplot(data = ts, aes(x = date)) +
    geom_line(aes(y = qobs, color = "Observada")) +
    geom_line(aes(y = qsim, color = "Simulada"), alpha = 0.6) +
    geom_vline(xintercept = date_calib, alpha = 0.4, linewidth = 0.3, linetype = "dashed") +
    geom_vline(xintercept = date_valid, alpha = 0.4, linewidth = 0.3, linetype = "dashed") +
    labs(x = "Tempo [anos]", y = "Vazão [m³/s]", color = "") +
    scale_color_manual(values = colors) +
    theme_bw() +
    theme(legend.position =  "bottom",
          plot.background = element_rect(color = "white"),
          panel.border = element_rect(color = "black", fill = NA),
          text = element_text(family = "JetBrains Mono", size = 10))


# APLICAÇÃO DA FUNÇÃO COMPLETA --------------------------------------------

rm(list = ls()); gc()
set.seed(9)
pacman::p_load(lubridate, tidyverse, ggplot2, SoilHyP, beepr, patchwork)
source("fun_run_hydromod.R")

# Função completa
# Argumentos
path <- "Base de Dados"
catchment_code <- 697
which_et <- "pet_pm"
periods <- c(0.05, 0.6, 0.35)
par_limits <- tibble(par_name = c("cmax", "b", "alpha", "f_lento", "f_rap"),
                     min = c(50, 1e-6, 1e-6, 1e-6, 1e-6),
                     max = c(1000, 2, 1, 0.2, 0.9))
which_obj <- "nse"
which_model <- "hymod"
which_cal <- "sce"
control <- list(maxit = 20,
                reltol = 0.001,
                ncomplex = 5,
                tolsteps = 6,
                trace = 1)

# Aplicação
# NSE e SCE
res_run_hydromod_nse_sce <-
  fun_run_hydromod(path = path,
                   catchment_code = catchment_code,
                   which_et = "pet_pm",
                   periods = periods,
                   which_obj = which_obj,
                   which_model = which_model,
                   which_cal = which_cal,
                   par <- c(300, 0.3, 0.6, 0.01, 0.4),
                   par_limits = par_limits,
                   control = control)

# NSE, SCE, SMAP
res_run_smap_nse_sce <-
  fun_run_hydromod(path = path,
                   catchment_code = catchment_code,
                   which_et = "pet_pm",
                   periods = periods,
                   which_obj = "nse",
                   which_model = "smap",
                   which_cal = "sce",
                   par = params[["smap"]],
                   par_limits = par_limits[["smap"]],
                   control = control)

# Próximo script: quantis.R, erros.R
source("quantis.r")
source("erros.r")

# Visualização
res_run_hydromod_nse_sce$plot
plot_curva_permanencia
plot_bar_res