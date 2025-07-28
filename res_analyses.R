
# LER DADOS ---------------------------------------------------------------

rm(list = ls()); gc()

# CABra dataset - catchment no.
catchment_code <- 33

# Séries de vazão simulada vs. observada
path_res <- paste0("Dados Gerados/results_1yr_warmup_", catchment_code, ".rds")
res_1yr_33 <- readRDS(path_res)

# VERIFICAR RESÍDUOS ------------------------------------------------------

# Manter no tbl_df somente o período de calibração
warmup <- 1
valid <- 10
date_min <- min(res_1yr_33$date)
date_max <- max(res_1yr_33$date)
calib_start <- date_min + years(warmup)
calib_end <- date_max - years(valid)
res_1yr_33 <- res_1yr_33[res_1yr_33$date > calib_start & res_1yr_33$date <= calib_end,] # calibration data

# Calcular resíduos
res_1yr_33 <- res_1yr_33 %>%
  mutate(residuals = (q_obs - qsim)) %>% 
  rowwise() %>% 
  mutate(residuals = ifelse(is.nan(residuals), NA, residuals),
         q_obs = ifelse(is.nan(q_obs), NA, q_obs))

# Média
residuals_mean <- mean(res_1yr_33$residuals, na.rm = TRUE)

# Verificar se os resíduos são homoscedásticos
ggplot(data = res_1yr_33, aes(x = q_obs, y = residuals)) +
  geom_line(color = "grey30", alpha = 0.6) +
  geom_smooth(se = FALSE, color = "black", linetype = "solid", linewidth = 0.6) +
  labs(x = "Qobs [m³/s]", y = "Qobs - Qsim") +
  theme_bw() +
  theme(panel.background = NULL,
        legend.position =  "none",
        plot.background = element_rect(color = "white"),
        panel.border = element_rect(color = "black", fill = NA),
        text = element_text(family = "serif", size = 10))

# Distribuição de frequência dos resíduos
plot_mse_hist <- ggplot(data = res_1yr_33) +
  geom_histogram(aes(x = residuals), color = "grey30", alpha = 0.7, linewidth = 0.3, bins = 40) +
  labs(x = "", y = "Frequência [%]") +
  theme_minimal() +
  theme(legend.position =  "none",
        plot.background = element_rect(color = "white"),
        axis.text.x = element_blank(),
        panel.border = element_rect(color = "black", fill = NA),
        text = element_text(family = "JetBrains Mono", color = "black", size = 8))

plot_mse_boxplot <- ggplot(data = res_1yr_33) +
  stat_boxplot(aes(x = residuals), geom = "errorbar", width = 0.3) +
  geom_boxplot(aes(x = residuals), linewidth = 0.5, outlier.size = 1, show.legend = FALSE) +
  labs(x = "Resíduos") +
  theme_minimal() +
  theme(plot.background = element_rect(color = "white"),
        panel.border = element_rect(color = "black", fill = NA),
        text = element_text(family = "JetBrains Mono", color = "black", size = 8))

plot_mse_hist / plot_mse_boxplot + plot_layout(heights = c(2, 1))

# COMPARAR QUANTIS --------------------------------------------------------

# Novo data.frame
freq_obs <-
  res_1yr_33 %>%
  select(date, q_obs) %>% 
  filter(!is.na(q_obs)) %>% 
  arrange(desc(q_obs)) %>% 
  # mutate(n = length(q_obs), i = row_number(), p = i/(n + 1), tr = 1/p)
  mutate(i = row_number())

percentiles <- c(0.05, 0.1, 0.2, 0.5, 0.8, 0.9, 0.95)
quantiles <- tibble(probs = percentiles,
                    quantiles_sim = quantile(freq_sim$qsim, probs = percentiles, na.rm = TRUE),
                    quantiles_obs = quantile(freq_obs$q_obs, probs = percentiles,  na.rm = TRUE)) %>% 
  pivot_longer(cols = starts_with("quantiles"),
               names_to = "vazao",
               values_to = "quantil")

# Plotar gráfico de barras
ggplot(data = quantiles, aes(x = as.factor(probs), y = quantil, fill = vazao)) +
  geom_bar(stat = "identity", position = position_dodge2(), color = "black", linewidth = 0.3) +
  scale_fill_manual(values = c("quantiles_obs" = "grey70", "quantiles_sim" = "grey40")) +
  labs(x = "Probabilidade", y = "Quantil [m³/s]", fill = "") +
  theme_minimal() +
  theme(plot.background = element_rect(color = "white"),
        legend.position = "bottom",
        panel.border = element_rect(color = "black", fill = NA),
        text = element_text(family = "JetBrains Mono", color = "black", size = 8))
  