packages <- c("readxl", "data.table", "ggplot2", "patchwork", "tseries", "urca",
              "sandwich", "lmtest", "glmnet", "ranger", "xgboost", "igraph", "ggraph")
missing <- packages[!vapply(packages, requireNamespace, quietly = TRUE, FUN.VALUE = logical(1))]
if (length(missing)) install.packages(missing, repos = "https://cloud.r-project.org")
message("Required packages are installed.")
