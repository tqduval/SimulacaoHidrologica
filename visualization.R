data_sce_4 <- ls_results[[4]]
data_sce_5 <- ls_results[[5]]

par_sce_4 <- data_sce_4[["calibration"]][["par"]] 
par_sce_5 <- data_sce_5[["calibration"]][["par"]] 

res_sce_4 <- data_sce_4[["results"]]
res_sce_5 <- data_sce_5[["results"]]

ggplot() +
  geom_line(data = df_in, aes(x = date, y = q_obs), color = "grey70", linetype = "solid", alpha = 0.3, linewidth = 1) +
  geom_line(data = res_sce_4, aes(x = date, y = qsim), color = "steelblue", linewidth = 0.4) +
  geom_line(data = res_sce_5, aes(x = date, y = qsim), color = "red", linewidth = 0.4) +
  # coord_cartesian(xlim = c(as.Date("2000-01-01"), as.Date("2010-01-01"))) +
  labs(x = "Tempo [anos]", y = "Vazão [m³/s]") +
  theme_bw() +
  theme(panel.background = NULL,
        legend.position =  "none",
        plot.background = element_rect(color = "white"),
        panel.border = element_rect(color = "black", fill = NA),
        text = element_text(family = "serif", size = 10))