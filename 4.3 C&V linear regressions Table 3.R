# CASSIMAN & VEUGELERS LINEAR REGRESSIONS FOR TABLE 3
# 1 sec

# Packages
library(openxlsx)
library(AER)

# Clear memory and start stopwatch
rm(list = ls())
t0 = Sys.time()

# Parameters
path.metadata = "../Data/01 Metadata/"
path.database = "../Database/"
path.predictions = "../Data/04 First-stage predictions/"
path.results = "../Results/"
file.metadata = "Metadata Thesis.xlsx"
file.database = "ENI Innovating Firms.xlsx"
file.predictions = "First-stage predictions.xlsx"
file.table3 = "Table 3 - Linear.xlsx"

# Functions
source("Functions.R")

# Load metadata and data
df_variables = read.xlsx(paste0(path.metadata, file.metadata), sheet = "Variables")
instruments = df_variables[df_variables$Role.in.Dataset=="Instrument" & !is.na(df_variables$Table2), "Variable.R"]
instruments = paste(instruments, collapse = " + ")
df_data = read.xlsx(paste0(path.database, file.database))

# Create dataframe for regression results (Table 3)
variables = df_variables[!is.na(df_variables$Table2), "Variable.R"]
variables = intersect(variables, names(df_data)) # Only explanatory and instrumental variables that are included in the database.
nv = length(variables)
variable.rows = rep("", 2*nv)
variable.rows[2*(1:nv)-1] = variables
df_table3 = data.frame(  Variable = c(variable.rows, "R2", "Wu-Hausman p-value")
                       , R1 = NA
                       , R2 = NA
                       , R3 = NA
                       , R4 = NA
                       , R5 = NA
                       , R6 = NA
                       , R7 = NA
                       )
row.names = variable.rows
nr = length(row.names)
row.names[seq(2,nr,2)] = row.names[seq(1,nr,2)]
nr = length(row.names)
row.names[seq(1,nr,2)] = paste0(row.names[seq(1,nr,2)], ".Estimate")
row.names[seq(2,nr,2)] = paste0(row.names[seq(2,nr,2)], ".Std.Error")
row.names = c(row.names, "R2", "Wu-Hausman p-value")
row.names(df_table3) = row.names

# Regression (1)
# Cooperation on instruments only
formula = paste0("Cooperation ~ ", instruments)
mod = lm(data = df_data, formula = formula)
df_coeff = summary(mod)$coefficients
var = setdiff(row.names(df_coeff), "(Intercept)")
for (v in var){
  print(v)
  p = df_coeff[v, 4]
  df_table3[paste0(v, ".Estimate"), "R1"] = paste(round(df_coeff[v, "Estimate"], 4), asterisk(p))
  df_table3[paste0(v, ".Std.Error"), "R1"] = paste0("(", round(df_coeff[v, "Std. Error"], 4), ")")
}
df_table3["R2", "R1"] = round(summary(mod)$r.squared, 2)

# Regression (2)
# Cooperation on explanatory variables and instruments
formula = paste0("Cooperation ~ Incoming.Spillovers + Appropriability + RD + ", instruments)
mod = lm(data = df_data, formula = formula)
df_coeff = summary(mod)$coefficients
var = setdiff(row.names(df_coeff), "(Intercept)")
for (v in var){
  print(v)
  p = df_coeff[v, 4]
  df_table3[paste0(v, ".Estimate"), "R2"] = paste(round(df_coeff[v, "Estimate"], 4), asterisk(p))
  df_table3[paste0(v, ".Std.Error"), "R2"] = paste0("(", round(df_coeff[v, "Std. Error"], 4), ")")
}
df_table3["R2", "R2"] = round(summary(mod)$r.squared, 2)

# Regression (3)
# Cooperation on explanatory variables without RD, and instruments
formula = paste0("Cooperation ~ Incoming.Spillovers + Appropriability + ", instruments)
mod = lm(data = df_data, formula = formula)
df_coeff = summary(mod)$coefficients
var = setdiff(row.names(df_coeff), "(Intercept)")
for (v in var){
  print(v)
  p = df_coeff[v, 4]
  df_table3[paste0(v, ".Estimate"), "R3"] = paste(round(df_coeff[v, "Estimate"], 4), asterisk(p))
  df_table3[paste0(v, ".Std.Error"), "R3"] = paste0("(", round(df_coeff[v, "Std. Error"], 4), ")")
}
df_table3["R2", "R3"] = round(summary(mod)$r.squared, 2)

# Regression (4)
# Same as (2) but IV
mod = ivreg(data = df_data, formula = paste("Cooperation ~ Incoming.Spillovers + Appropriability + RD |", instruments))
df_coeff = summary(mod)$coefficients
var = setdiff(row.names(df_coeff), "(Intercept)")
for (v in var){
  print(v)
  p = df_coeff[v, 4]
  df_table3[paste0(v, ".Estimate"), "R4"] = paste(round(df_coeff[v, "Estimate"], 4), asterisk(p))
  df_table3[paste0(v, ".Std.Error"), "R4"] = paste0("(", round(df_coeff[v, "Std. Error"], 4), ")")
}
df_table3["R2", "R4"] = round(summary(mod)$r.squared, 2)
df_table3["Wu-Hausman p-value", "R4"] = format(summary(mod, diagnostics = T)$diagnostics["Wu-Hausman", "p-value"], digits = 2)

# Regression (5)
# Same as (3) but two-stage
mod = ivreg(data = df_data, formula = paste("Cooperation ~ Incoming.Spillovers + Appropriability |", instruments))
df_coeff = summary(mod)$coefficients
var = setdiff(row.names(df_coeff), "(Intercept)")
for (v in var){
  print(v)
  p = df_coeff[v, 4]
  df_table3[paste0(v, ".Estimate"), "R5"] = paste(round(df_coeff[v, "Estimate"], 4), asterisk(p))
  df_table3[paste0(v, ".Std.Error"), "R5"] = paste0("(", round(df_coeff[v, "Std. Error"], 4), ")")
}
df_table3["R2", "R5"] = round(summary(mod)$r.squared, 2)
df_table3["Wu-Hausman p-value", "R5"] = format(summary(mod, diagnostics = T)$diagnostics["Wu-Hausman", "p-value"], digits = 2)

# Regression (6)
# Dependent variable is Cooperation.Firms
mod = ivreg(data = df_data, formula = paste("Cooperation.Firms ~ Incoming.Spillovers + Appropriability + RD |", instruments))
df_coeff = summary(mod)$coefficients
var = setdiff(row.names(df_coeff), "(Intercept)")
for (v in var){
  print(v)
  p = df_coeff[v, 4]
  df_table3[paste0(v, ".Estimate"), "R6"] = paste(round(df_coeff[v, "Estimate"], 4), asterisk(p))
  df_table3[paste0(v, ".Std.Error"), "R6"] = paste0("(", round(df_coeff[v, "Std. Error"], 4), ")")
}
df_table3["R2", "R6"] = round(summary(mod)$r.squared, 2)
df_table3["Wu-Hausman p-value", "R6"] = format(summary(mod, diagnostics = T)$diagnostics["Wu-Hausman", "p-value"], digits = 2)

# Regression (7)
# Dependent variable is Cooperation.Universities
mod = ivreg(data = df_data, formula = paste("Cooperation.Universities ~ Incoming.Spillovers + Appropriability + RD |", instruments))
df_coeff = summary(mod)$coefficients
var = setdiff(row.names(df_coeff), "(Intercept)")
for (v in var){
  print(v)
  p = df_coeff[v, 4]
  df_table3[paste0(v, ".Estimate"), "R7"] = paste(round(df_coeff[v, "Estimate"], 4), asterisk(p))
  df_table3[paste0(v, ".Std.Error"), "R7"] = paste0("(", round(df_coeff[v, "Std. Error"], 4), ")")
}
df_table3["R2", "R7"] = round(summary(mod)$r.squared, 2)
df_table3["Wu-Hausman p-value", "R7"] = format(summary(mod, diagnostics = T)$diagnostics["Wu-Hausman", "p-value"], digits = 2)

# Format and save to results folder
names(df_table3) = c("Variable", "(1)", "(2)", "(3)", "(4) (2-Step)", "(5) (2-Step)", "(6) Cooperation with suppliers and customers (2-Step)", "(7) Cooperation with research institutions (2-Step)")
df_table3$Variable = gsub(".Scaled", "", df_table3$Variable)
df_table3$Variable = gsub(".", " ", df_table3$Variable, fixed = T)
names(df_table3) = gsub(".", " ", names(df_table3), fixed = T)
hs = createStyle(textDecoration = "bold", fgFill = "#DDEBF7", wrapText = T)
write.xlsx(df_table3, file = paste0(path.results, file.table3), firstRow = T, colWidths = 15, headerStyle = hs)

# Show time taken
print(Sys.time() - t0)