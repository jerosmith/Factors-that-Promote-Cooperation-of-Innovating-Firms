# CASSIMAN & VEUGELERS LOGIT REGRESSIONS FOR TABLE 2
# 1 sec

# Packages
library(openxlsx)

# Clear memory and start stopwatch
rm(list = ls())
t0 = Sys.time()

# Parameters
prob.threshold = 0.5
path.metadata = "../Data/01 Metadata/"
path.database = "../Database/"
path.predictions = "../Data/04 First-stage predictions/"
path.results = "../Results/"
file.metadata = "Metadata Thesis.xlsx"
file.database = "ENI Innovating Firms.xlsx"
file.predictions = "First-stage predictions.xlsx"
file.table2 = "Table 2 - Logit.xlsx"

# Functions
source("Functions.R")

# Load metadata and data
df_variables = read.xlsx(paste0(path.metadata, file.metadata), sheet = "Variables")
instruments = df_variables[df_variables$Role.in.Dataset=="Instrument" & !is.na(df_variables$Table2), "Variable.R"]
instruments = paste(instruments, collapse = " + ")
df_data = read.xlsx(paste0(path.database, file.database))

# Create dataframe for regression results (Table 2)
variables = df_variables[!is.na(df_variables$Table2), "Variable.R"]
variables = intersect(variables, names(df_data)) # Only explanatory and instrumental variables that are included in the database.
nv = length(variables)
variable.rows = rep("", 2*nv)
variable.rows[2*(1:nv)-1] = variables
df_table2 = data.frame(  Variable = c(variable.rows, "Accuracy", "Specificity", "Sensitivity")
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
row.names = c(row.names, "Accuracy", "Specificity", "Sensitivity")
row.names(df_table2) = row.names

# Stage 1 for 2SLS regressions 4-7: predict explanatory variables from instruments
mod1 = lm(data = df_data, formula = paste0("Incoming.Spillovers ~ ", instruments))
df_data$Incoming.Spillovers.Hat = predict.lm(object = mod1, newdata = df_data)
mod1 = lm(data = df_data, formula = paste0("Appropriability ~ ", instruments))
df_data$Appropriability.Hat = predict.lm(object = mod1, newdata = df_data)
mod1 = lm(data = df_data, formula = paste0("RD ~ ", instruments))
df_data$RD.Hat = predict.lm(object = mod1, newdata = df_data)

# Regression (1)
# Cooperation.Bin on instruments only
formula = paste0("Cooperation.Bin ~ ", instruments)
mod = glm(data = df_data, formula = formula, family = binomial(link = "logit"))
df_coeff = summary(mod)$coefficients
var = setdiff(row.names(df_coeff), "(Intercept)")
for (v in var){
  print(v)
  p = df_coeff[v, 4]
  df_table2[paste0(v, ".Estimate"), "R1"] = paste(round(df_coeff[v, "Estimate"], 4), asterisk(p))
  df_table2[paste0(v, ".Std.Error"), "R1"] = paste0("(", round(df_coeff[v, "Std. Error"], 4), ")")
}
df_data$Cooperation.Prob = predict.glm(object = mod, newdata = df_data, type = "response")
df_data$Cooperation.Hat = ifelse(df_data$Cooperation.Prob > prob.threshold, 1, 0)
mat_confusion = table(df_data[, c("Cooperation.Bin", "Cooperation.Hat")])
df_table2["Accuracy", "R1"] = round((mat_confusion[1,1]+mat_confusion[2,2]) / sum(mat_confusion), 2)
df_table2["Specificity", "R1"] = round(mat_confusion[1,1]/sum(mat_confusion[1,]), 2)
df_table2["Sensitivity", "R1"] = round(mat_confusion[2,2]/sum(mat_confusion[2,]), 2)

# Regression (2)
# Cooperation.Bin on explanatory variables and instruments
formula = paste0("Cooperation.Bin ~ Incoming.Spillovers + Appropriability + RD + ", instruments)
mod = glm(data = df_data, formula = formula, family = binomial(link = "logit"))
df_coeff = summary(mod)$coefficients
var = setdiff(row.names(df_coeff), "(Intercept)")
for (v in var){
  print(v)
  p = df_coeff[v, 4]
  df_table2[paste0(v, ".Estimate"), "R2"] = paste(round(df_coeff[v, "Estimate"], 4), asterisk(p))
  df_table2[paste0(v, ".Std.Error"), "R2"] = paste0("(", round(df_coeff[v, "Std. Error"], 4), ")")
}
df_data$Cooperation.Prob = predict.glm(object = mod, newdata = df_data, type = "response")
df_data$Cooperation.Hat = ifelse(df_data$Cooperation.Prob > prob.threshold, 1, 0)
mat_confusion = table(df_data[, c("Cooperation.Bin", "Cooperation.Hat")])
df_table2["Accuracy", "R2"] = round((mat_confusion[1,1]+mat_confusion[2,2]) / sum(mat_confusion), 2)
df_table2["Specificity", "R2"] = round(mat_confusion[1,1]/sum(mat_confusion[1,]), 2)
df_table2["Sensitivity", "R2"] = round(mat_confusion[2,2]/sum(mat_confusion[2,]), 2)

# Regression (3)
# Cooperation.Bin on explanatory variables without RD, and instruments
formula = paste0("Cooperation.Bin ~ Incoming.Spillovers + Appropriability + ", instruments)
mod = glm(data = df_data, formula = formula, family = binomial(link = "logit"))
df_coeff = summary(mod)$coefficients
var = setdiff(row.names(df_coeff), "(Intercept)")
for (v in var){
  print(v)
  p = df_coeff[v, 4]
  df_table2[paste0(v, ".Estimate"), "R3"] = paste(round(df_coeff[v, "Estimate"], 4), asterisk(p))
  df_table2[paste0(v, ".Std.Error"), "R3"] = paste0("(", round(df_coeff[v, "Std. Error"], 4), ")")
}
df_data$Cooperation.Prob = predict.glm(object = mod, newdata = df_data, type = "response")
df_data$Cooperation.Hat = ifelse(df_data$Cooperation.Prob > prob.threshold, 1, 0)
mat_confusion = table(df_data[, c("Cooperation.Bin", "Cooperation.Hat")])
df_table2["Accuracy", "R3"] = round((mat_confusion[1,1]+mat_confusion[2,2]) / sum(mat_confusion), 2)
df_table2["Specificity", "R3"] = round(mat_confusion[1,1]/sum(mat_confusion[1,]), 2)
df_table2["Sensitivity", "R3"] = round(mat_confusion[2,2]/sum(mat_confusion[2,]), 2)

# Regression (4)
# Same as (2) but two-stage
mod = glm(data = df_data, formula = Cooperation.Bin ~ Incoming.Spillovers.Hat + Appropriability.Hat + RD.Hat, family = binomial(link = "logit"))
df_coeff = summary(mod)$coefficients
row.names(df_coeff) = gsub(".Hat", "", row.names(df_coeff)) # Remove ".Hat" suffix from row names.
var = setdiff(row.names(df_coeff), "(Intercept)")
for (v in var){
  print(v)
  p = df_coeff[v, 4]
  df_table2[paste0(v, ".Estimate"), "R4"] = paste(round(df_coeff[v, "Estimate"], 4), asterisk(p))
  df_table2[paste0(v, ".Std.Error"), "R4"] = paste0("(", round(df_coeff[v, "Std. Error"], 4), ")")
}
df_data$Cooperation.Prob = predict.glm(object = mod, newdata = df_data, type = "response")
df_data$Cooperation.Hat = ifelse(df_data$Cooperation.Prob > prob.threshold, 1, 0)
mat_confusion = table(df_data[, c("Cooperation.Bin", "Cooperation.Hat")])
df_table2["Accuracy", "R4"] = round((mat_confusion[1,1]+mat_confusion[2,2]) / sum(mat_confusion), 2)
df_table2["Specificity", "R4"] = round(mat_confusion[1,1]/sum(mat_confusion[1,]), 2)
df_table2["Sensitivity", "R4"] = round(mat_confusion[2,2]/sum(mat_confusion[2,]), 2)

# Regression (5)
# Same as (3) but two-stage
mod = glm(data = df_data, formula = Cooperation.Bin ~ Incoming.Spillovers.Hat + Appropriability.Hat, family = binomial(link = "logit"))
df_coeff = summary(mod)$coefficients
row.names(df_coeff) = gsub(".Hat", "", row.names(df_coeff)) # Remove ".Hat" suffix from row names.
var = setdiff(row.names(df_coeff), "(Intercept)")
for (v in var){
  print(v)
  p = df_coeff[v, 4]
  df_table2[paste0(v, ".Estimate"), "R5"] = paste(round(df_coeff[v, "Estimate"], 4), asterisk(p))
  df_table2[paste0(v, ".Std.Error"), "R5"] = paste0("(", round(df_coeff[v, "Std. Error"], 4), ")")
}
df_data$Cooperation.Prob = predict.glm(object = mod, newdata = df_data, type = "response")
df_data$Cooperation.Hat = ifelse(df_data$Cooperation.Prob > prob.threshold, 1, 0)
mat_confusion = table(df_data[, c("Cooperation.Bin", "Cooperation.Hat")])
df_table2["Accuracy", "R5"] = round((mat_confusion[1,1]+mat_confusion[2,2]) / sum(mat_confusion), 2)
df_table2["Specificity", "R5"] = round(mat_confusion[1,1]/sum(mat_confusion[1,]), 2)
df_table2["Sensitivity", "R5"] = round(mat_confusion[2,2]/sum(mat_confusion[2,]), 2)

# Regression (6)
# Dependent variable is Cooperation.Firms.Bin
mod = glm(data = df_data, formula = Cooperation.Firms.Bin ~ Incoming.Spillovers.Hat + Appropriability.Hat + RD.Hat, family = binomial(link = "logit"))
df_coeff = summary(mod)$coefficients
row.names(df_coeff) = gsub(".Hat", "", row.names(df_coeff)) # Remove ".Hat" suffix from row names.
var = setdiff(row.names(df_coeff), "(Intercept)")
for (v in var){
  print(v)
  p = df_coeff[v, 4]
  df_table2[paste0(v, ".Estimate"), "R6"] = paste(round(df_coeff[v, "Estimate"], 4), asterisk(p))
  df_table2[paste0(v, ".Std.Error"), "R6"] = paste0("(", round(df_coeff[v, "Std. Error"], 4), ")")
}
df_data$Cooperation.Prob = predict.glm(object = mod, newdata = df_data, type = "response")
df_data$Cooperation.Hat = ifelse(df_data$Cooperation.Prob > prob.threshold, 1, 0)
mat_confusion = table(df_data[, c("Cooperation.Bin", "Cooperation.Hat")])
df_table2["Accuracy", "R6"] = round((mat_confusion[1,1]+mat_confusion[2,2]) / sum(mat_confusion), 2)
df_table2["Specificity", "R6"] = round(mat_confusion[1,1]/sum(mat_confusion[1,]), 2)
df_table2["Sensitivity", "R6"] = round(mat_confusion[2,2]/sum(mat_confusion[2,]), 2)

# Regression (7)
# Dependent variable is Cooperation.Universities.Bin
mod = glm(data = df_data, formula = Cooperation.Universities.Bin ~ Incoming.Spillovers.Hat + Appropriability.Hat + RD.Hat, family = binomial(link = "logit"))
df_coeff = summary(mod)$coefficients
row.names(df_coeff) = gsub(".Hat", "", row.names(df_coeff)) # Remove ".Hat" suffix from row names.
var = setdiff(row.names(df_coeff), "(Intercept)")
for (v in var){
  print(v)
  p = df_coeff[v, 4]
  df_table2[paste0(v, ".Estimate"), "R7"] = paste(round(df_coeff[v, "Estimate"], 4), asterisk(p))
  df_table2[paste0(v, ".Std.Error"), "R7"] = paste0("(", round(df_coeff[v, "Std. Error"], 4), ")")
}
df_data$Cooperation.Prob = predict.glm(object = mod, newdata = df_data, type = "response")
df_data$Cooperation.Hat = ifelse(df_data$Cooperation.Prob > prob.threshold, 1, 0)
mat_confusion = table(df_data[, c("Cooperation.Bin", "Cooperation.Hat")])
df_table2["Accuracy", "R7"] = round((mat_confusion[1,1]+mat_confusion[2,2]) / sum(mat_confusion), 2)
df_table2["Specificity", "R7"] = round(mat_confusion[1,1]/sum(mat_confusion[1,]), 2)
df_table2["Sensitivity", "R7"] = round(mat_confusion[2,2]/sum(mat_confusion[2,]), 2)

# Format and save to results folder
names(df_table2) = c("Variable", "(1)", "(2)", "(3)", "(4) (2-Step)", "(5) (2-Step)", "(6) Cooperation with suppliers and customers (2-Step)", "(7) Cooperation with research institutions (2-Step)")
df_table2$Variable = gsub(".Scaled", "", df_table2$Variable)
df_table2$Variable = gsub(".", " ", df_table2$Variable, fixed = T)
names(df_table2) = gsub(".", " ", names(df_table2), fixed = T)
hs = createStyle(textDecoration = "bold", fgFill = "#DDEBF7", wrapText = T)
write.xlsx(df_table2, file = paste0(path.results, file.table2), firstRow = T, colWidths = 15, headerStyle = hs)

# Show time taken
print(Sys.time() - t0)