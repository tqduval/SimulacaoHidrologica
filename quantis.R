
# QUANTIS -----------------------------------------------------------------

# Extrair série histórica dos resultados (date, qobs, qsim)
ts <- res_run_hydromod_nse_sce[["ts"]][, c("date", "qobs", "qsim")]
ts <- na.omit(ts)
names_ts <- colnames(ts)

# Calcular quantis empíricos e comparar curvas de permanência
# das vazões observadas e dos simuladas
seq_percentiles <- seq(0.05, 0.99, 0.05)
df_quantiles <-
  tibble(probs = seq_percentiles,
         quant_obs = quantile(ts[[names_ts[[2]]]], probs = seq_percentiles),
         quant_sim = quantile(ts[[names_ts[[3]]]], probs = seq_percentiles)) %>%
  pivot_longer(cols = starts_with("quant_"),
               values_to = "quantiles",
               names_to = "which_quant")


# VISUALIZAÇÃO ------------------------------------------------------------

# Plotar curva de permanência: quantis vs. percentis
colors <- c("quant_obs" = "black", "quant_sim" = "red")
perc_breaks <- c(0.05, 0.10, 0.20, 0.50, 0.80, 0.90, 0.95)
plot_curva_permanencia <-
  ggplot(data = df_quantiles, aes(x = probs, y = quantiles, color = which_quant)) +
  geom_line() +
  scale_x_reverse(name = "Porcentagem acima do tempo [%]",
                  breaks = perc_breaks,
                  labels = scales::percent) +
  scale_y_continuous(name = "Vazão [m³/s]", trans = "log10") +
  scale_color_manual(values = colors) +
  labs(color = "") +
  theme_minimal() +
  theme(panel.background = NULL,
        legend.position =  "bottom",
        plot.background = element_rect(color = "white"),
        panel.border = element_rect(color = "black", fill = NA),
        text = element_text(family = "JetBrains Mono", size = 10)); plot_curva_permanencia
