pkgs <- c(
  "afex", "BayesFactor", "bayesplot", "bayestestR", "correlation",
  "datawizard", "dplyr", "effectsize", "emmeans", "finalfit", "ggeffects",
  "ggplot2", "haven", "Hmisc", "insight", "marginaleffects", "mediation",
  "mice", "modelbased", "parameters", "performance", "permuco",
  "psych", "psychTools", "pwr", "qqplotr", "ragg", "readxl", "see",
  "summarytools", "tidyr", "tidySEM", "tidyverse"
)

install.packages(pkgs, repos = c("https://easystats.r-universe.dev", getOption("repos")))

