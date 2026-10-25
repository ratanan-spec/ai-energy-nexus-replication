library(data.table)
library(sandwich)
library(lmtest)

cobj <- readRDS(file.path(PATHS$models, "03_tvp_connectedness.rds"))
dat <- copy(cobj$dynamic)
aligned <- fread(PATHS$processed)
aligned[, Date := as.Date(Date)]
dat <- merge(dat, aligned[, .(Date, VIX)], by = "Date", all.x = TRUE)
vix_cut <- as.numeric(quantile(dat$VIX, PROJECT$vix_quantile, na.rm = TRUE))
dat[, PostAI := as.integer(Date >= PROJECT$ai_cutoff)]
dat[, HighVIX := as.integer(VIX >= vix_cut)]

stationary_indices <- function(n, mean_block) {
  p <- 1 / mean_block; idx <- integer(n); idx[1] <- sample.int(n, 1)
  for (i in 2:n) idx[i] <- if (runif(1) < p) sample.int(n, 1) else (idx[i-1] %% n) + 1L
  idx
}

contrast <- function(x, g) mean(x[g == 1], na.rm = TRUE) - mean(x[g == 0], na.rm = TRUE)
bootstrap_contrast <- function(x, g, B, block) {
  set.seed(PROJECT$seed)
  vals <- replicate(B, {ii <- stationary_indices(length(x), block); contrast(x[ii], g[ii])})
  c(Estimate = contrast(x, g), L90 = unname(quantile(vals, .05)), U90 = unname(quantile(vals, .95)),
    L95 = unname(quantile(vals, .025)), U95 = unname(quantile(vals, .975)))
}

tests_for <- function(group_name) {
  g <- dat[[group_name]]; x <- dat$TCI
  welch <- t.test(x ~ g, var.equal = FALSE)
  wilcox <- wilcox.test(x ~ g, exact = FALSE)
  boot <- bootstrap_contrast(x, g, PROJECT$bootstrap_B, PROJECT$bootstrap_block)
  data.table(Contrast = paste0(group_name, " 1 minus 0"), Mean0 = mean(x[g == 0]), Mean1 = mean(x[g == 1]),
             Difference = boot["Estimate"], Welch_t = unname(welch$statistic), Welch_p = welch$p.value,
             Wilcoxon_W = unname(wilcox$statistic), Wilcoxon_p = wilcox$p.value,
             Bootstrap_L90 = boot["L90"], Bootstrap_U90 = boot["U90"],
             Bootstrap_L95 = boot["L95"], Bootstrap_U95 = boot["U95"])
}
regime_tests <- rbind(tests_for("PostAI"), tests_for("HighVIX"))

fit <- lm(TCI ~ PostAI + HighVIX, data = dat)
vc <- NeweyWest(fit, lag = PROJECT$hac_lag, prewhite = FALSE, adjust = TRUE)
## `coeftest` carries an S3 class that can recurse in recent data.table versions.
## Convert the numerical coefficient matrix explicitly before tabulation.
hac <- data.table(Term = rownames(coeftest(fit, vcov. = vc)),
                  as.data.frame(unclass(coeftest(fit, vcov. = vc))))
setnames(hac, c("Estimate", "Std. Error", "t value", "Pr(>|t|)"),
         c("Estimate", "HAC_SE", "t", "p"))

fwrite(regime_tests, file.path(PATHS$tables, "regime_tests_bootstrap.csv"))
fwrite(hac, file.path(PATHS$tables, "regime_hac_regression.csv"))
saveRDS(list(data = dat, threshold = vix_cut, regime_tests = regime_tests,
             fit = fit, hac = hac), file.path(PATHS$models, "04_regime_inference.rds"))
