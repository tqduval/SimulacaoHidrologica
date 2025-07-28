
# ERROS -------------------------------------------------------------------

# Extrair série histórica dos resultados (date, qobs, qsim)
ts_res <- res_run_hydromod_nse_sce[["ts"]][, c("date", "qobs", "qsim")]
# ts <- na.omit(ts)
names_ts <- colnames(ts_res)

# Calcular resíduos
ts_res$res <- ts_res[[names_ts[[2]]]] - ts_res[[names_ts[[3]]]]
res_mean <- mean(ts_res[["res"]], na.rm = TRUE)


# CONTINGÊNCIA ------------------------------------------------------------



# CORRELAÇÃO --------------------------------------------------------------

ts_corr <- cor(ts[[names_ts[[2]]]], ts[[names_ts[[3]]]])
ggplot(data = ts_res) +
  geom_abline(linetype = "dashed", alpha = 0.70, color = "grey40", linewidth = 0.3) +
  geom_point(aes(x = qobs, y = qsim), fill = "grey", alpha = 0.6, pch = 21) +
  labs(x = "Qobs", y = "Qsim") +
  coord_fixed() +
  theme_minimal() +
  theme(legend.position =  "bottom",
        aspect.ratio = 1,
        plot.background = element_rect(color = "white"),
        panel.border = element_rect(color = "black", fill = NA),
        text = element_text(family = "JetBrains Mono", color = "black", size = 8))


# VISUALIZAÇÃO ------------------------------------------------------------

# Verificações visuais:
# Verificar se os resíduos são homoscedásticos
ggplot(data = ts_res, aes(x = qobs, y = res)) +
  geom_line(color = "grey30", alpha = 0.6, na.rm = TRUE) +
  geom_smooth(se = FALSE, color = "black", linetype = "dashed", linewidth = 0.6) +
  labs(x = "Qobs [m³/s]", y = "Qobs - Qsim") +
  theme_minimal() +
  theme(panel.background = NULL,
        legend.position =  "none",
        plot.background = element_rect(color = "white"),
        panel.border = element_rect(color = "black", fill = NA),
        text = element_text(family = "JetBrains Mono", size = 10))

# Distribuição de frequência dos resíduos
plot_mse_hist <- ggplot(data = ts_res) +
  geom_histogram(aes(x = res), color = "grey30", alpha = 0.7, linewidth = 0.3, bins = 40) +
  labs(x = "", y = "Frequência [%]") +
  theme_minimal() +
  theme(legend.position =  "none",
        plot.background = element_rect(color = "white"),
        axis.text.x = element_blank(),
        panel.border = element_rect(color = "black", fill = NA),
        text = element_text(family = "JetBrains Mono", color = "black", size = 8))

plot_mse_boxplot <- ggplot(data = ts_res) +
  stat_boxplot(aes(x = res), geom = "errorbar", width = 0.3) +
  geom_boxplot(aes(x = res), linewidth = 0.5, outlier.size = 1, show.legend = FALSE) +
  labs(x = "Resíduos") +
  theme_minimal() +
  theme(plot.background = element_rect(color = "white"),
        panel.border = element_rect(color = "black", fill = NA),
        text = element_text(family = "JetBrains Mono", color = "black", size = 8))

plot_mse_hist / plot_mse_boxplot + plot_layout(heights = c(2, 1))

# Gráfico de barras
res_rank <- sort(ts$res, decreasing = TRUE)
plot_bar_res <- 
  ggplot(data.frame(res_rank)) +
  geom_bar(stat = "identity", aes(x = seq_along(res_rank), y = res_rank),
           fill = "grey50", color = "black", alpha = 0.8, linewidth = 0.2) +
  labs(x ="Número de observações", y = "Resíduos") +
  theme_minimal() +
  theme(legend.position =  "none",
        plot.background = element_rect(color = "white"),
        panel.border = element_rect(color = "black", fill = NA),
        text = element_text(family = "JetBrains Mono", color = "black", size = 8)); plot_bar_res
