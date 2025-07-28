
# PACKAGES ----------------------------------------------------------------

pacman::p_load(tidyverse, ggplot2, optimx, SoilHyP, beepr, patchwork)


# READ DATA ---------------------------------------------------------------

rm(list = ls()); gc()

# Source hymod function from file
# R function by Thiago Lappicy
source("C:/Users/tomas/OneDrive/1 - Acadêmico/Mestrado/Disciplinas/2025-1/Simulação Hidrológica/2-TRABALHOS/HYMOD/HyMod.R")
source("fun_run_hymod.R")
source("fun_obj_nse.R")
source("fun_obj_kge.R")

# CABra dataset - catchment no.
catchment_code <- 33

# Define paths
path_streamflow <- paste0("Base de Dados/CABra_", catchment_code, "_streamflow.txt")
path_climate <- paste0("Base de Dados/CABra_", catchment_code, "_climate.txt")

# Read streamflow data
vazao <- read.table(file = path_streamflow, skip = 8, header = TRUE, sep = "\t")
vazao <- vazao[-1,]                                                         # remove row with units
vazao$Streamflow <- as.numeric(vazao$Streamflow)                            # streamflow column is as character
vazao$Date <- as.Date(paste(vazao$Year, vazao$Month, vazao$Day, sep = "-")) # create 'dates' column
vazao <- vazao[, c("Date", "Streamflow")]                                   # subset columns
colnames(vazao) <- c("date", "q_obs")                                       # "Streamflow" to "Qobs"

# Catchment area
area <- read.delim(path_streamflow, skip = 4, nrow = 1, header = FALSE)[[2]]

# Read climate data
clima <- read.table(file = path_climate, skip = 13, header = TRUE, sep = "\t")
clima <- clima[-1,]                                                         # remove row with units
clima$Date <- as.Date(paste(clima$Year, clima$Month, clima$Day, sep = "-")) # create 'dates' column
clima <- clima[, c("Date", "p_ens", "pet_pm")]
colnames(clima) <- c("date", "precip", "etp_pm")
# clima$precip <- gsub(" ", "", clima$precip)
# clima$etp_pm <- gsub(" ", "", clima$etp_pm)
clima$precip <- as.numeric(clima$precip)
clima$etp_pm <- as.numeric(clima$etp_pm)

# Join data
df_in <- merge(clima, vazao, all.x = "T")


# QUALITY CHECK -----------------------------------------------------------

# # Empty, NA or duplicate data
# sum(is.na(vazao$q_obs)); sum(is.nan(vazao$q_obs)); sum(duplicated(vazao$date))   # streamflows
# sum(is.na(clima$precip)); sum(is.nan(clima$precip)); sum(duplicated(clima$date)) # clima -> precip
# sum(is.na(clima$etp_pm)); sum(is.nan(clima$etp_pm)); sum(duplicated(clima$date)) # clima -> etp
# vazao$date[which(is.na(vazao$q_obs))]                                            # get where are empty 'q_obs'
valid <- which(!is.na(df_in$q_obs))

# PREPARE MODEL RUN -------------------------------------------------------

# Initial parameters
par_in <- c(100, 0.3, 0.6, 0.01, 0.4)

# Create table with only evapotranspiration and precipitation data
ep <- clima[, c(3,2)]


# RUN MODEL ---------------------------------------------------------------

# Run HyMod function
hymod_q_sim <- HyMod(x = par_in, EPQ = ep) # returns only simulated streamflow [mm/d]
hymod_q_sim <- hymod_q_sim*area*1000000/(1000*86400)
res <- cbind(df_in, hymod_q_sim)


# ASSESS RESULTS ----------------------------------------------------------

teste <- na.omit(res)
fun_nse(obs = teste$q_obs, sim = teste$hymod_q_sim)
nse <- fun_nse(obs = res$q_obs[!is.na(df_in$q_obs)],
               sim = res$hymod_q_sim[!is.na(df_in$q_obs)])

# saveRDS(res, file = "res.rds")


# CALIBRAÇÃO --------------------------------------------------------------

# Função objetivo completa p/ HyMod e NSEe
par_limits <- tibble(par_name = c("cmax", "b", "alpha", "f_lento", "f_rap"),
                     min = c(50, 1e-6, 1e-6, 1e-6, 1e-6),
                     max = c(1000, 2, 1, 0.2, 0.9))

# Set seed for SCE-UA randomic processes
set.seed(9)

# Suffle Complex Evolution (SCE)
max_warm <- 5
ls_results <- list(rep(list(), max_warm))

for(w in 1:max_warm){
  
  message("\n\nInício da calibração com ", w, " anos de aquecimento...\n")
  name <- paste("warmup", w, sep = "_")
  ls_results[[w]] <- fun_run_hymod(par = par_in,
                                   df_in = df_in,
                                   area = area,
                                   par_limits = par_limits,
                                   warmup = w,
                                   valid = 10,
                                   optim = "sce")
  
}; beepr::beep(sound = 10)

# Avaliar qual período de aquecimento teve melhor NSE p/ validação e calibração
# 1 ano de aquecimento -> NSE = 0.58
# Salvar resultados
saveRDS(ls_results[[1]][["results"]],
        file = paste0("Dados Gerados/results_1yr_warmup_", catchment_code, ".rds"))