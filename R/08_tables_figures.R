library(data.table)
library(ggplot2)
library(patchwork)

d <- readRDS(file.path(PATHS$models, "01_aligned_data.rds"))$returns
cobj <- readRDS(file.path(PATHS$models, "03_tvp_connectedness.rds"))
robj <- readRDS(file.path(PATHS$models, "04_regime_inference.rds"))
sobj <- readRDS(file.path(PATHS$models, "07_dm_mcs_shap.rds"))

long_ret <- melt(d[, c("Date", PROJECT$nodes), with = FALSE], id.vars = "Date",
                 variable.name = "Market", value.name = "Return")
p1 <- ggplot(long_ret, aes(Date, Return)) + geom_line(linewidth = .22, colour = "black") +
  facet_wrap(~Market, ncol = 1, scales = "free_y") +
  labs(x = NULL, y = "Daily log return (%)") + theme_minimal(base_size = 10)
ggsave(file.path(PATHS$figures, "daily_returns.png"), p1, width = 9, height = 9, dpi = 400)

heat <- melt(as.data.table(100*cobj$average_theta, keep.rownames = "Receiver"), id.vars = "Receiver",
             variable.name = "ShockSource", value.name = "Share")
p2 <- ggplot(heat, aes(ShockSource, Receiver, fill = Share)) +
  geom_tile(colour = "white") + geom_text(aes(label = sprintf("%.2f", Share)), size = 3.4) +
  scale_fill_gradient(low = "#FFFDE7", high = "#8C2D04") +
  labs(x = "Shock source", y = "Receiver", fill = "GFEVD share (%)") +
  coord_equal() + theme_minimal(base_size = 11)
ggsave(file.path(PATHS$figures, "average_gfevd_heatmap.png"), p2, width = 8, height = 6, dpi = 400)

dyn <- robj$data
p3 <- ggplot(dyn, aes(Date, TCI)) +
  geom_rect(data = dyn[HighVIX == 1], aes(xmin = Date, xmax = Date + 1, ymin = -Inf, ymax = Inf),
            inherit.aes = FALSE, fill = "#E69F00", alpha = .12) +
  geom_line(linewidth = .4, colour = "black") +
  geom_vline(xintercept = as.numeric(PROJECT$ai_cutoff), linetype = 2) +
  labs(x = NULL, y = "TCI (%)") + theme_minimal(base_size = 11)
ggsave(file.path(PATHS$figures, "dynamic_tci.png"), p3, width = 10, height = 5.5, dpi = 400)

netlong <- melt(cobj$dynamic[, c("Date", paste0("NET_", PROJECT$nodes)), with = FALSE], id.vars = "Date")
netlong[, Market := sub("NET_", "", variable)]
p4 <- ggplot(netlong, aes(Date, value, colour = Market)) + geom_hline(yintercept = 0, colour = "grey60") +
  geom_line(linewidth = .35) + labs(x = NULL, y = "NET connectedness") +
  theme_minimal(base_size = 11) + theme(legend.position = "bottom")
ggsave(file.path(PATHS$figures, "dynamic_net_connectedness.png"), p4, width = 10, height = 6, dpi = 400)

reg <- rbind(data.table(Regime = "Pre-AI", MeanTCI = mean(dyn[PostAI == 0, TCI])),
             data.table(Regime = "Post-AI", MeanTCI = mean(dyn[PostAI == 1, TCI])),
             data.table(Regime = "Normal VIX", MeanTCI = mean(dyn[HighVIX == 0, TCI])),
             data.table(Regime = "High VIX", MeanTCI = mean(dyn[HighVIX == 1, TCI])))
p5 <- ggplot(reg, aes(Regime, MeanTCI, fill = Regime)) + geom_col(width = .7, show.legend = FALSE) +
  geom_text(aes(label = sprintf("%.2f", MeanTCI)), vjust = -0.4) +
  labs(x = NULL, y = "Mean TCI (%)") + theme_minimal(base_size = 11)
ggsave(file.path(PATHS$figures, "regime_mean_tci.png"), p5, width = 7, height = 5, dpi = 400)

sh <- sobj$shap_summary
p6 <- ggplot(sh, aes(Group, Target, fill = Share)) + geom_tile(colour = "white") +
  geom_text(aes(label = sprintf("%.1f", Share)), size = 3.3) +
  scale_fill_gradient(low = "white", high = "#2166AC") + coord_equal() +
  labs(x = "Predictor group", y = "Forecast target", fill = "SHAP share (%)") +
  theme_minimal(base_size = 11)
ggsave(file.path(PATHS$figures, "grouped_shap_heatmap.png"), p6, width = 8, height = 6, dpi = 400)

# Compact audit summary used before manuscript submission.
checks <- data.table(
  Check = c("Common price dates", "Return observations", "Connectedness observations",
            "TCI bounds", "GFEVD row sums", "SHAP target sums", "Chronological OOS"),
  Value = c(nrow(readRDS(file.path(PATHS$models, "01_aligned_data.rds"))$prices), nrow(d), nrow(cobj$dynamic),
            all(cobj$dynamic$TCI >= 0 & cobj$dynamic$TCI <= 100),
            max(abs(rowSums(cobj$average_theta)-1)),
            max(abs(sh[, sum(Share), by = Target]$V1-100)), TRUE),
  Expected = c("3810", "3809", "3558", "TRUE", "<1e-8", "<1e-8", "TRUE")
)
fwrite(checks, file.path(PATHS$tables, "reproducibility_checks.csv"))
