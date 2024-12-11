# SCATTERPLOTS
# 11 sec

# Packages
library(openxlsx)
library(sqldf)
library(ggplot2)

# Clear memory and start stopwatch
rm(list = objects())
t0 = Sys.time()

# Parameters
path.metadata = "../Data/01 Metadata/"
path.database = "../Database/"
path.plots = "../Results/Scatterplots/"
file.metadata = "Metadata Thesis.xlsx"
file.database = "ENI Innovating Firms.xlsx"
alpha = 0.05
z = -qnorm(alpha/2)

# 1. Load metadata and data
df_variables = read.xlsx(paste0(path.metadata, file.metadata), sheet = "Variables")
df_data = read.xlsx(paste0(path.database, file.database))

# 2. Make plots
variable = df_variables[!is.na(df_variables$Include.in.DB) & df_variables$Role.in.Dataset %in% c("Explanatory", "Instrument") & df_variables$Data.Type=="Numeric", "Variable.R"]
for (v in variable){
  print(v)
  df = df_data[, c("Cooperation", v)]
  names(df)[2] = "x"
  n = nrow(df)
  df$Cooperation = df$Cooperation + runif(n, -2, 2) # Create jitter for greater visibility
  df$Cooperation = ifelse(df$Cooperation < 0, 0, df$Cooperation) # Constrain to lower bound
  df$Cooperation = ifelse(df$Cooperation > 100, 100, df$Cooperation) # Constrain to upper bound
  lb = min(df$x, na.rm = T) # Lower bound for x
  ub = max(df$x, na.rm = T) # Upper bound for x
  jitter = 0.02*ub # Create jitter for greater visibility
  df$x = df$x + runif(n, -jitter, jitter) # Add jitter for greater visibility
  df$x = ifelse(df$x < lb, lb, df$x)
  df$x = ifelse(df$x > ub, ub, df$x)
  g = ggplot(data = df, mapping = aes(y=Cooperation, x=x)) + xlab(v) +
    geom_point(size=0.1) +
    geom_smooth(method = "lm")
  ggsave(plot = g, filename = paste0(path.plots, "Cooperation vs ", v, ".png"), units = "px", height = 1000, width = 1200)
}

# Show time taken
print(Sys.time() - t0)
