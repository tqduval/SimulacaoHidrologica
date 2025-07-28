
# FUN_SETUP_FILES ---------------------------------------------------------

# Função p/ ler arquivos 'streamflow' e 'climate' da base de dados CABra e organizá-los dentro do R.
# Parâmetros:
## path: 'character' caminho do diretório c/ arquivos
### CABra_<catchment_code>_streamflow.txt e CABra_<catchment_code>_climate.txt
## catchment_code: código da bacia hidrográfica do CABra
## which_et: qual modelo (escolher 1) de evapotranspiração usar (et_ens, pet_pm, pet_pt, pet_hg)

fun_setup_files <- function(path, catchment_code, which_et){
  
  # File paths
  path_streamflow <- paste0(path, "/CABra_", catchment_code, "_streamflow.txt")
  path_climate <- paste0(path, "/CABra_", catchment_code, "_climate.txt")
  
  # Read files
  # Streamflow
  vazao <- read.table(file = path_streamflow, skip = 8, header = TRUE, sep = "\t")
  vazao <- vazao[-1,]                                                         # remove row with units
  vazao$Streamflow <- as.numeric(vazao$Streamflow)                            # streamflow column is as character
  vazao$date <- as.Date(paste(vazao$Year, vazao$Month, vazao$Day, sep = "-")) # create 'dates' column
  vazao <- vazao[, c("date", "Streamflow")]                                   # subset columns
  colnames(vazao) <- c("date", "qobs")                                        # "Streamflow" to "Qobs"
  
  # Climate
  clima <- read.table(file = path_climate, skip = 13, header = TRUE, sep = "\t")
  clima <- clima[-1,]                                                         # remove row with units
  clima$date <- as.Date(paste(clima$Year, clima$Month, clima$Day, sep = "-")) # create 'dates' column
  clima <- clima[, c("date", "p_ens", which_et)]
  colnames(clima) <- c("date", "p", "et")
  clima$p <- as.numeric(clima$p)
  clima$et <- as.numeric(clima$et)
  clima <- clima[, c(1, 3, 2)]
  
  # Merge 'vazao' and 'clima'
  data <- merge(clima, vazao, all.x = TRUE)
  
  # Get area (km²)
  area <- read.delim(path_streamflow, skip = 4, nrow = 1, header = FALSE)[[2]]
  
  # Results
  res <- list("data" = data, "area" = area)
  
  return(res)
  
}


# FUN_PERIODS -------------------------------------------------------------

# Função p/ filtrar períodos de aquecimento, calibração e validação do modelo
# Parâmetros:
## periods: 'vector' c/ 2 ou 3 objetos (soma = 1)

fun_periods <- function(epq, periods){
  
  if(sum(periods) != 1) stop("Soma de 'periods' precisa ser igual a 1.\nAtualmente sum(periods) = ", sum(periods))
  
  len <- length(periods)
  n <- nrow(epq)
  
  # Define periods
  if(len == 2){
    
    calib <- periods[[1]]*n # calibration period
    valid <- periods[[2]]*n # validation period
    
    epq_calib <- head(epq, calib)
    epq_valid <- tail(epq, valid)
    
  } else if(len == 3){
    
    warm <- periods[[1]]*n  # warmup period
    calib <- periods[[2]]*n # calibration period
    valid <- periods[[3]]*n # validation period
    
    epq_calib <- epq[warm:calib,]
    epq_valid <- tail(epq, valid)
    
  } else{
    
    stop("Argumento 'periods' precisa ter tamanho 2 ou 3 contendo períodos de:\n
         1. calibração; 2. validação; ou 1. aquecimento; 2. calibração; 3. validação")
    
  }
  
  # Results
  res <- list("calib" = epq_calib,
              "valid" = epq_valid)
  
  return(res)
  
}


# HYMOD -------------------------------------------------------------------

# Modelo HyMod - Thiago Lappicy
# Modelo HyMod ####
HyMod <- function(x, EPQ, area) {
  
  # Bibliotecas utilizadas ####
  
  
  # Informações gerais --------------------------------------------------------
  
  # Código original em MatLab por Jasper A. Vrugt
  # Los Alamos, Setembro de 2005
  # Re-escrita por Arno de Vreng
  # Amsterdam, Dezembro de 2005
  # Traduzido para o R por Thiago Lappicy (lappicy@gmail.com)
  # Brasília, Maio de 2025
  
  # Implementação em R do modelo conceitual HYMOD 
  # x: vetor do parâmetro (comprimento 3 ou 5)
  # EPQ: matriz ou data.frame com duas colunas:
  # evapotranspiração potencial [mm] e precipitação [mm]
  
  
  # Acessar dados -------------------------------------------------------------
  
  # Recupera parâmetros do vetor x
  if(length(x) == 5){
    Cmax <- x[1]                        # capacidade máxima de armazenamento [mm]
    B <- x[2]                           # grau de variabilidade espacial da capacidade de armazenamento de água do solo [-]
    Alpha <- x[3]                       # fator que distribui o fluxo entre os reservatórios lento e rápido [-]
    FractionLeavesSlowTank <- x[4]      # fração do reservatório lento que esvazia a cada passo de tempo (RL) [-]
    FractionLeavesQuickTank <- x[5]     # fração do reservatório rápido que esvazia a cada passo de tempo (RR) [-]
  } else if(length(x) == 3){
    Cmax <- x[1]                      # capacidade máxima de armazenamento [mm]
    B <- x[2]                         # grau de variabilidade espacial da capacidade de armazenamento de água do solo [-]
    Alpha <- 1                        # fator que distribui o fluxo entre os reservatórios lento e rápido [-]
    FractionLeavesSlowTank <- 0       # fração do reservatório lento que esvazia a cada passo de tempo (RL) [-]
    FractionLeavesQuickTank <- x[3]   # fração do reservatório rápido que esvazia a cada passo de tempo (RR) [-]
    
  } else {
    stop("O vetor x deve ter comprimento 3 ou 5.")
  }
  
  
  # Inicializar o modelo ------------------------------------------------------
  
  # Inicializa armazenamento de água no solo
  # Usei aqui como 0.0 para garantir que o R não vai definir o valor como inteiro
  WaterStorage <- 0.0
  
  # Inicializa estado do reservatório de escoamento lento
  SlowTankState <- 0.0
  
  # Inicializa estados dos reservatório de escoamento rápido
  QuickTankState <- rep(0.0, 3)
  
  # Extrai evaporação potencial da matriz EPQ (coluna 1)
  PotEvapArray <- EPQ[, 1]
  
  # Extrai precipitação da matriz EPQ (coluna 2)
  PrecipArray <- EPQ[, 2]
  
  # Define número de passos de tempo
  nSteps <- nrow(EPQ)
  
  # Prealoca vetor de saída de vazão simulada
  output <- numeric(nSteps)
  
  
  # Rodar o modelo em si ------------------------------------------------------
  
  # Loop de simulação para cada passo de tempo
  for(t in seq_len(nSteps)){
    
    # Obtém precipitação no passo t
    Precip <- PrecipArray[t]
    
    # Obtém evaporação potencial no passo t
    PotEvap <- PotEvapArray[t]
    
    # Calcula capacidade de armazenamento crítica do solo em um momento específico
    StorageCapacity <-
      Cmax * (1 - (1 - ((B + 1) * WaterStorage / Cmax))^(1 / (B + 1)))
    
    # Calcula escoamento superficial inicial (ExcessPrecip1)
    # Parte da precipitação que está acima do disponível
    # O StorageCapacity transborda como escoamento superficial (rsv. rápidos)
    ExcessPrecip1 <- StorageCapacity + Precip - Cmax
    if(ExcessPrecip1 < 0) ExcessPrecip1 <- 0
    
    # Calcula precipitação que infiltra no solo
    PrecipStorage <- Precip - ExcessPrecip1
    
    # Normaliza variável dummy para atualização do armazenamento
    dummy <- (StorageCapacity + PrecipStorage) / Cmax
    if(dummy > 1) dummy <- 1
    
    # Atualiza camada de armazenamento de água no solo
    WaterStorageNew <- (Cmax / (B + 1)) * (1 - (1 - dummy)^(B + 1))
    
    # Calcula variação no armazenamento de água
    DeltaWaterStorage <- WaterStorageNew - WaterStorage
    
    # Calcula escoamento adicional (ExcessPrecip2) p/ os rsv. lentos e rápidos
    ExcessPrecip2 <- PrecipStorage - DeltaWaterStorage
    if(ExcessPrecip2 < 0) ExcessPrecip2 <- 0
    
    # Calcula evaporação real limitada pelo armazenamento disponível
    Evap <- ifelse(WaterStorageNew > PotEvap, PotEvap, WaterStorageNew)
    
    # Calcula entrada de fluxo no reservatórios lento
    SlowTankInflow <- (1 - Alpha) * ExcessPrecip2
    
    # Prepara vetor de entrada para reservatórios rápidos
    QuickTankInflow <- numeric(4)
    
    # Calcula entrada de fluxo no primeiro reservatório rápido
    QuickTankInflow[1] <- ExcessPrecip1 + Alpha * ExcessPrecip2
    
    # Roteia fluxo pelo reservatório lento (único linear)
    SlowTankStateNew <-
      (1 - FractionLeavesSlowTank) * (SlowTankState + SlowTankInflow)
    
    # Calcula saída do reservatórios lento
    SlowTankOutflow <-
      (FractionLeavesSlowTank / (1 - FractionLeavesSlowTank)) * SlowTankStateNew
    
    # Inicializa vetores para reservatórios rápidos em série
    QuickTankStateNew <- numeric(3)
    QuickTankOutflow <- numeric(3)
    
    # Loop de roteamento pelos 3 reservatórios rápidos (lineares)
    for(k in 1:3){
      # Atualiza estado do reservatório rápido "k"
      QuickTankStateNew[k] <-
        (1 - FractionLeavesQuickTank) * (QuickTankState[k] + QuickTankInflow[k])
      
      # Calcula saída do reservatório rápido "k'
      QuickTankOutflow[k] <-
        (FractionLeavesQuickTank / (1 - FractionLeavesQuickTank)) * QuickTankStateNew[k]
      
      # Define entrada para o próximo reservatório rápido
      QuickTankInflow[k + 1] <- QuickTankOutflow[k]
    }
    
    # Atualizar dados para o próximo passo de tempo "t" e calcula a vazão total
    
    # Armazenamento de água no solo após evaporação
    WaterStorage <- WaterStorageNew - Evap
    
    # Estado do reservatórios lento
    SlowTankState <- SlowTankStateNew
    
    # Estados dos reservatórios rápidos
    QuickTankState <- QuickTankStateNew
    
    # Calcula vazão total combinada (mm/dia)
    output[t] <- SlowTankOutflow + QuickTankOutflow[3]
  }
  
  
  # Retorna série de vazão simulada -------------------------------------------
  # O output está na unidade de (mm/dia).
  return(output*area*1e6/(1e3*86400)) # converte em m³/s
}


# SMAP --------------------------------------------------------------------

# Tomás Antonio Quezado Duval
# tqduval@gmail.com

fun.smap <- function(area,  # área da bacia
                     x,     # vetor c/ 6 parâmetros de entrada e 2 condiçoes iniciais
                     EPQ,   # tabela com dados observados
                     NSE = FALSE){  
  
  param <- x; epq <- EPQ
  m <- length(param)
  
  # Pega precipitação, evapotranspiração e vazão observadas
  e <- epq[,1]    # coluna com a evapotranspiração
  p <- epq[,2]    # coluna com a precipitação
  qobs <- epq[,3] # coluna com a vazão observada
  ndias <- length(p)
  ad <- area
  
  # Parâmetros
  str <- param[1]
  k2t <- param[2]
  ai <- param[3]
  crec <- param[4]
  capc <- param[5]
  kkt <- param[6]
  tuin <- param[7] # condição inicial
  ebin <- param[8] # condição inicial
  
  qca <- rep(0, nrow(epq) + 1) # vetor nulo de vazões calculadas
  es <- rep(0, nrow(epq) + 1)  # vetor nulo de escoamento superficial
  er <- rep(0, nrow(epq) + 1)  # vetor nulo de evapotranspiração real 
  rec <- rep(0, nrow(epq) + 1) # vetor nulo de recarga
  ed <- rep(0, nrow(epq) + 1)  # vetor nulo do fluxo no reservatório rápido
  eb <- rep(0, nrow(epq) + 1)  # vetor nulo de escoamento básico
  rsolo <- rep(0, nrow(epq) + 1)
  rss <- rep(0, nrow(epq) + 1)
  res <- rep(0, nrow(epq) + 1)
  tu <- rep(0, nrow(epq) + 1)
  
  # Conversões de unidade
  capc <- capc/100
  crec <- crec/100
  kkt <- 0.5^(1/kkt);	
  k2t <- 0.5^(1/k2t);	
  
  # Inicialização dos reservatórios
  rsolo[1] <- tuin/100*str         # reservatório do solo
  rss[1] <- ebin/(1 - kkt)/ad*86.4 # reservatório subterrâneo
  res[1] <- 0                      # escoamento
  
  # Itera entre os dias e calcula vazão
  for(i in 2:ndias){
    
    tu[i-1] <- rsolo[i-1]/str # teor de umidade
    
    # Calcula o escoamento superficial
    if(p[i] > ai){
      es[i] <- (p[i] - ai)^2/(p[i] - ai + str - rsolo[i-1])
    }
    else{
      es[i] <- 0
    }
    
    # Calcula a evapotranspiração real
    if((p[i] - es[i]) > e[i]){
      er[i] <- e[i]
    }
    else{
      er[i] <- (p[i] - es[i]) + (e[i] - (p[i] - es[i]))*tu[i-1]
    }
    
    # Calcula a recarga
    if(rsolo[i-1] > capc*str){
      rec[i] <- crec*tu[i-1]*(rsolo[i-1] - (capc*str))
    }
    else{
      rec[i] <- 0
    }
    
    # Atualiza o reservatório do solo
    rsolo[i] <- rsolo[i-1] + p[i] - es[i] - er[i] - rec[i]
    
    # Testa a saturação do solo
    if(rsolo[i] > str){
      es[i] <- es[i] + rsolo[i] - str
      rsolo[i] <- str
    }
    
    ed[i] <- (res[i-1] + es[i])*(1 - k2t) # escoamento direto
    res[i] <- res[i-1] + es[i] - ed[i]    # atualiza reservatório da superficie
    eb[i] <- rss[i-1]*(1 - kkt)           # escoamento básico
    rss[i] <- rss[i-1] - eb[i] + rec[i]   # atualiza reservatório subterrâneo
    qca[i] <- (ed[i] + eb[i])*ad/86.4     # vazão calculada
    
  }
  
  # Dataframe resultante
  resultados <- data.frame(
    # data = as.Date(epq[,1]),
    evap = e,
    precip = p,
    tu = tu[-1],
    rsolo = rsolo[-1],
    rss = rss[-1],
    rec = rec[-1],
    es = es[-1],
    er = er[-1],
    ed = ed[-1],
    eb = eb[-1],
    qobs = qobs,
    qcalc = qca[-1])
  
  # Calcula o coeficiente de Nash-Sutcliff ou retorna o dataframe com os dados simulados
  if(NSE == TRUE){
    ns <- -(1 - sum((qca[-1] - qobs)^2)/sum((qobs - mean(qobs))^2))
    return(ns)
  }
  else{
    # ns <- -(1 - sum((qca[-1] - qobs)^2)/sum((qobs - mean(qobs))^2))
    # cat(paste("Coeficiente Nash-Sutcliff igual à", round(-ns, digits = 3)))
    return(resultados[["qcalc"]])
  }
  
}


# NSE ---------------------------------------------------------------------

# Nash-Sutcliff
fun_obj_nse <- function(obs, sim){
  1 - sum((obs - sim)^2)/sum((obs - mean(obs))^2)
}


# KGE ---------------------------------------------------------------------

# Kling-Gupta
fun_obj_kge <- function(obs, sim){
  1 - sqrt(((cor(obs, sim) - 1)^2) + (((sd(sim)/sd(obs)) - 1)^2) + (((mean(sim)/mean(obs)) - 1)^2))
}


# CALIBRAR ----------------------------------------------------------------

# Defazada
fun_cal <- function(fun = run, par, epq_calib, epq_valid, area,
                    which_cal, which_obj, periods, ...){
  
  run <- match.fun(fun)
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
                    EPQ = epq[,c(2,3)],
                    area = area)
  results <- cbind(epq, qsim)
  
  res <- list("which_cal" = which_cal,
              "periods" = sprintf("%0.1f%%", periods*100),
              "par" = par_final,
              "fo" = which_obj,
              "fo_value" = fo_valid,
              "ts" = results,
              "optimization" = calib)
  
  return(res)
  
}


# VISUALIZAÇÃO ------------------------------------------------------------

# Plotar série histórica de vazões observadas e simuladas
fun_visual <- function(ts, epq_calib, epq_valid){
  
  # Extrair datas de validação e calibração
  date_calib <- epq_calib[["date"]][[1]]
  date_valid <- epq_valid[["date"]][[1]]
  
  # Plotar série histórica
  colors <- c("Observada" = "black", "Simulada" = "red")
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
          text = element_text(family = "JetBrains Mono", size = 10)); plot_ts
  
  return(plot_ts)
  
}
