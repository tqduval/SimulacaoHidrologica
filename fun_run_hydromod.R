
# Função desenvolvida p/ disciplina de Simulação Hidrológica PTARH/UnB p/ rodar
# modelos hidrológicos para bacias hidrográficas utilizando dados de climaticos
# e de vazão da plataforma CABra.

# Parâmetros
## path: 'character' caminho do diretório c/ arquivos: 
### CABra_<catchment_code>_streamflow.txt e CABra_<catchment_code>_climate.txt
## catchment_code: 'int' código da bacia hidrográfica do CABra
## which_et: 'character' qual modelo (escolher 1) de evapotranspiração usar (et_ens, pet_pm, pet_pt, pet_hg)
## periods: 'vector' c/ períodos de aquecimento, calibração e validação (soma = 1)
## par: 'vector' de parâmetros do modelo hidrológico
## which_obj: 'character' c/ qual função objetivo utilizar (nse, kge)
## which_model: 'character' c/ qual modelo hidrológico utilizar (hymod, smap)
## ...: outros parâmetros:
### par_limits: tbl_df c/ colunas 'nome_par', 'min' e 'max' nessa ordem p/ SCEOptim

fun_run_hydromod <- function(path, catchment_code, which_et,
                             periods, par, which_obj = "nse", which_model = "hymod",
                             which_cal = "sce", control, par_limits, ...){ # talvez esse 'control' tenha q passar p/ '...'
  
  # Pacotes
  if(!require(pacman)) install.packages("pacman")
  pacman::p_load(lubridate, tidyverse, ggplot2, SoilHyP, beepr, patchwork)
  
  # Verificações
  if(!(which_obj %in% c("nse", "kge"))) stop("Escolha uma função objetivo: 'nse' ou 'kge'.")
  if(!(which_cal %in% c("nm", "sce"))) stop("Escolha um otimizador: 'nm' ou 'sce'.")
  
  # Importar funções
  source("proj_functions.R")
  

  # ORGANIZAR DADOS DE ENTRADA ----------------------------------------------
  
  # Ler e organizar arquivos
  res_setup_files <- fun_setup_files(path = path,
                                     catchment_code = catchment_code,
                                     which_et = which_et)
  
  epq <- res_setup_files[["data"]]  # tbl_df c/ 'date', 'et', 'p', e 'qobs'
  area <- res_setup_files[["area"]] # área da bacia (km²)
  
  message("Rodando bacia ", catchment_code,
          "\nÁrea igual a ", area, " km²\n")
  
  # Filtrar períodos de aquecimento, calibração e validação
  res_periods <- fun_periods(epq = epq,
                             periods = periods)
  
  epq_calib <- res_periods[["calib"]]
  epq_valid <- res_periods[["valid"]]
  
  # Escolher função objetivo (NSE, KGE) e modelo
  if(which_obj == "nse") fun_obj <- fun_obj_nse
  if(which_obj == "kge") fun_obj <- fun_obj_kge
  if(which_model == "hymod") fun_model <- HyMod
  if(which_model == "smap") fun_model <- fun.smap
  
  # Rodar o modelo e calibrar
  # Criar uma função que roda o modelo e calcula a função objetivo
  run <- function(par, epq, area){
    
    # Vazões simuladas (m³/s)
    qsim <- fun_model(x = par,
                      EPQ = epq[,c(2,3,4)],
                      area = area)
    
    # Calcular função objetivo
    qobs <- epq[["qobs"]]
    fo <- fun_obj(obs = qobs[!is.na(qobs)], # filtrar vazões observadas NA
                  sim = qsim[!is.na(qobs)]) # filtrar vazões observadas NA
    return(-fo)
    
  }
  

  # CALIBRAR ----------------------------------------------------------------
  
  len <- length(periods)
  
  # Escolher calibrador
  if(which_cal == "nm"){
    
    # Optim (Nelder-Mead)
    calib <- optim(par = par,
                   method = "Nelder-Mead",
                   fn = run,
                   epq = epq_calib,
                   area = area)
    
  }
  if(which_cal == "sce"){
    
    # Suffle Complex Evolution (SCE)
    min <- par_limits[[2]]; max <- par_limits[[3]]
    calib <- SoilHyP::SCEoptim(FUN = run, par = par,
                               lower = min, upper = max,
                               epq = epq_calib,
                               area = area,
                               control = control)
    
  }
  
  # Extrair resultados
  par_final <- calib[["par"]]
  fo_valid <- -run(par = par_final,
                   epq = epq_valid,
                   area = area)
  qsim <- fun_model(x = par_final,
                    EPQ = epq[,c(2,3,4)],
                    area = area)
  results <- cbind(epq, qsim)
  
  res_cal <- list("which_cal" = which_cal,
                  "periods" = sprintf("%0.1f%%", periods*100),
                  "par" = par_final,
                  "fo" = which_obj,
                  "fo_value" = fo_valid,
                  "ts" = results,
                  "optimization" = calib)
  
  
  # Extrair informações
  ts <- res_cal[["ts"]]             # série temporal
  fo_value <- res_cal[["fo_value"]] # valor da função objetivo
  

  # VISUALIZAÇÃO ------------------------------------------------------------

  plot_ts <- fun_visual(ts = ts,
                        epq_calib = epq_calib,
                        epq_valid = epq_valid)
  
  beepr::beep(sound = 10)
  
  return(c(res_cal, "plot" = list(plot_ts)))
  
}
