# FIRST-STEP REGRESSION
# Outputs: Table A3. This is used to choose the appropriate instruments.
# 2 sec

# Packages
library(openxlsx)

# Clear memory and start stopwatch
rm(list = objects())
t0 = Sys.time()

# Parameters
path.metadata = "../Data/01 Metadata/"
path.database = "../Database/"
path.results = "../Results/"
file.metadata = "Metadata Thesis.xlsx"
file.database = "ENI Innovating Firms.xlsx"
file.table1 = "Table 1.xlsx"

# Functions
source("Functions.R")

# Load metadata and data
df_variables = read.xlsx(paste0(path.metadata, file.metadata), sheet = "Variables")
df_data = read.xlsx(paste0(path.database, file.database))

# Get main explanatory variables and instruments for first-stage regression
explanatory = c("Incoming.Spillovers", "Appropriability", "RD")
instruments = df_variables[df_variables$Role.in.Dataset=="Instrument" & !is.na(df_variables$Include.in.DB), "Variable.R"]

# Create Table A3 dataframe
nv = length(instruments)
Variables = rep("", 2*nv)
Variables[2*(1:nv)-1] = instruments
Variables = c(Variables, "(Intercept)", "")
df_tableA3 = data.frame(  Variable = Variables
                        , Cooperation = NA
                        , Incoming.Spillovers = NA
                        , Appropriability = NA
                        , RD = NA
                        )

# Create first-stage predictions dataframe
df_stage1.predictions = df_data[, c("ID_B", explanatory, instruments)]

# Logit regression for Cooperation coefficients and standard errors
formula = paste(instruments, collapse = " + ")
formula = paste("Cooperation.Bin ~", formula)
mod = glm(data = df_data, formula = formula, family = binomial(link = "logit"))
df_coeff = summary(mod)$coefficients
var = row.names(df_coeff)
for (v in var){
  print(v)
  i = which(df_tableA3$Variable==v)
  p = df_coeff[v, 4]
  df_tableA3[i, "Cooperation"] = paste(round(df_coeff[v, "Estimate"], 4), asterisk(p))
  df_tableA3[i+1, "Cooperation"] = paste0("(", round(df_coeff[v, "Std. Error"], 4), ")")
}

# First-stage regressio significance n for Incoming.Spillovers
formula = paste(instruments, collapse = " + ")
formula = paste("Incoming.Spillovers ~", formula)
mod = lm(data = df_stage1.predictions, formula = formula)
df_coeff = summary(mod)$coefficients
var = row.names(df_coeff)
for (v in var){
  print(v)
  i = which(df_tableA3$Variable==v)
  p = df_coeff[v, 4]
  df_tableA3[i, "Incoming.Spillovers"] = paste(round(df_coeff[v, "Estimate"], 4), asterisk(p))
  df_tableA3[i+1, "Incoming.Spillovers"] = paste0("(", round(df_coeff[v, "Std. Error"], 4), ")")
}

# First-stage regression for Appropriability
formula = paste(instruments, collapse = " + ")
formula = paste("Appropriability ~", formula)
mod = lm(data = df_stage1.predictions, formula = formula)
df_coeff = summary(mod)$coefficients
var = row.names(df_coeff)
for (v in var){
  print(v)
  i = which(df_tableA3$Variable==v)
  p = df_coeff[v, 4]
  df_tableA3[i, "Appropriability"] = paste(round(df_coeff[v, "Estimate"], 4), asterisk(p))
  df_tableA3[i+1, "Appropriability"] = paste0("(", round(df_coeff[v, "Std. Error"], 4), ")")
}

# First-stage regression for RD
formula = paste(instruments, collapse = " + ")
formula = paste("RD ~", formula)
mod = lm(data = df_stage1.predictions, formula = formula)
df_coeff = summary(mod)$coefficients
var = row.names(df_coeff)
for (v in var){
  print(v)
  i = which(df_tableA3$Variable==v)
  p = df_coeff[v, 4]
  df_tableA3[i, "RD"] = paste(round(df_coeff[v, "Estimate"], 4), asterisk(p))
  df_tableA3[i+1, "RD"] = paste0("(", round(df_coeff[v, "Std. Error"], 4), ")")
}

# Format and save to results folder
names(df_tableA3) = gsub(".", " ", names(df_tableA3), fixed = T)
hs = createStyle(textDecoration = "bold", fgFill = "#DDEBF7", wrapText = T)
write.xlsx(df_tableA3, file = paste0(path.results, file.table1), firstRow = T, colWidths = 15, headerStyle = hs)

# Show time taken
print(Sys.time() - t0)
