library(data.table)
library(igraph)
library(ggplot2)
source("R/lib_tvp.R")

cobj <- readRDS(file.path(PATHS$models, "03_tvp_connectedness.rds"))
A <- 100 * cobj$average_theta
diag(A) <- 0
edges <- rbindlist(lapply(seq_len(nrow(A)), function(i) {
  data.table(from = colnames(A), to = rownames(A)[i], weight = A[i, ])
}))
edges <- edges[from != to & weight >= 5]
nodes <- copy(cobj$directional)
nodes[, role := ifelse(NET >= 0, "Net transmitter", "Net receiver")]

g <- graph_from_data_frame(edges, directed = TRUE, vertices = nodes)
if (requireNamespace("ggraph", quietly = TRUE)) {
  library(ggraph)
  set.seed(PROJECT$seed)
  p <- ggraph(g, layout = "circle") +
    geom_edge_link(aes(width = weight, alpha = weight), arrow = arrow(length = unit(3, "mm")),
                   end_cap = circle(4, "mm"), colour = "grey35") +
    geom_node_point(aes(colour = role, size = abs(NET) + 1)) +
    geom_node_text(aes(label = name), repel = TRUE, fontface = "bold") +
    scale_colour_manual(values = c("Net transmitter" = "#D55E00", "Net receiver" = "#0072B2")) +
    scale_edge_width(range = c(.4, 2.8), guide = "none") +
    scale_edge_alpha(range = c(.45, .90), guide = "none") +
    scale_size(range = c(5, 9), guide = "none") +
    coord_equal() + theme_void(base_size = 11) +
    theme(legend.position = "bottom")
  ggsave(file.path(PATHS$figures, "connectedness_network.png"), p, width = 8, height = 6, dpi = 400)
} else {
  message("ggraph is unavailable; network plot skipped, while all numerical network outputs are produced.")
}
fwrite(edges, file.path(PATHS$tables, "network_edges.csv"))

# +/-1-day lead-lag diagnostic using aligned returns.
ret <- fread(PATHS$processed)
node_names <- PROJECT$nodes
leadlag <- rbindlist(lapply(combn(PROJECT$nodes, 2, simplify = FALSE), function(z) {
  x <- ret[[z[1]]]; y <- ret[[z[2]]]
  cc <- c(`-1` = cor(x[-1], y[-length(y)]), `0` = cor(x, y),
          `1` = cor(x[-length(x)], y[-1]))
  data.table(Series1 = z[1], Series2 = z[2], Lag = as.integer(names(cc)), Correlation = cc,
             LargestAbsolute = abs(cc) == max(abs(cc)))
}))
fwrite(leadlag, file.path(PATHS$tables, "lead_lag_correlations.csv"))

# Weekly robustness dataset. Re-run Script 03 after substituting this object if a full weekly TVP path is required.
ret[, Week := as.Date(cut(as.Date(Date), "week", start.on.monday = TRUE))]
weekly <- ret[, lapply(.SD, sum, na.rm = TRUE), by = Week, .SDcols = PROJECT$nodes]
fwrite(weekly, file.path("data", "processed", "weekly_returns.csv"))

weekly_fit <- run_discount_tvp(as.matrix(weekly[, ..node_names]), weekly$Week,
                               init_window = min(52L, floor(.20*nrow(weekly))))
weekly_directional <- data.table(Node=PROJECT$nodes, NET=colMeans(weekly_fit$NET))
fwrite(weekly_directional, file.path(PATHS$tables, "weekly_directional_connectedness.csv"))

# Prespecified sensitivity grid used in Table 9.
retmat <- as.matrix(ret[, ..node_names]); rdates <- as.Date(ret$Date)
grid <- CJ(lambda=c(.98,.99), H=c(5L,10L,20L))
robustness <- grid[, {
  z <- run_discount_tvp(retmat, rdates, lambda=lambda, H=H)
  .(MeanTCI=mean(z$TCI), MinTCI=min(z$TCI), MaxTCI=max(z$TCI), RescaledDates=z$rescaled)
}, by=.(lambda,H)]
fwrite(robustness, file.path(PATHS$tables, "tvp_sensitivity_grid.csv"))
saveRDS(list(weekly=weekly_fit, robustness=robustness),
        file.path(PATHS$models, "05_timing_weekly_robustness.rds"))
