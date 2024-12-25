# EXPLORATORY REGRESSIONS - OLS & IV

# Packages
library(openxlsx)
library(AER)

# Clear memory and start stopwatch
rm(list = objects())
t0 = Sys.time()

# Parameters
path.database = "../Database/"
file.database = "ENI Chile 2020.xlsx"

# Load data
df_data = read.xlsx(paste0(path.database, file.database))

# LINEAR MODELS

# 1. OLS Cooperation ~ Incoming.Spillovers
mod = lm(data = df_data, formula = Cooperation ~ Incoming.Spillovers)
summary(mod)

# 2. OLS Cooperation ~ Appropriability
mod = lm(data = df_data, formula = Cooperation ~ Appropriability)
summary(mod)

# 3. OLS Cooperation ~ Incoming.Spillovers + Appropriability
mod = lm(data = df_data, formula = Cooperation ~ Incoming.Spillovers + Appropriability)
summary(mod)

# 4. IV Cooperation ~ Incoming.Spillovers | Appropriability
mod = ivreg(data = df_data, formula = Cooperation ~ Incoming.Spillovers | Appropriability)
summary(mod, diagnostics = T)

# 5. IV Cooperation ~ Incoming.Spillovers + Control Explanatory Variables | Appropriability
mod = ivreg(data = df_data, formula = Cooperation ~ Incoming.Spillovers + Innovation + RD + Basicness.RD + Personnel.Scaled + Industry.Cooperation + Industry.Incoming.Spillovers | Appropriability + Innovation.Bin + Size.Scaled + Size2.Scaled + Cost + Agriculture + Innovation + RD + Basicness.RD + Personnel.Scaled + Industry.Cooperation + Industry.Incoming.Spillovers)
summary(mod, diagnostics = T)





# 4. OLS Cooperation ~ Incoming.Spillovers + Appropriability + Control Explanatory Variables
mod = lm(data = df_data, formula = Incoming.Spillovers ~ Appropriability + RD + Basicness.RD + Industry.Cooperation + Industry.Incoming.Spillovers)
summary(mod)

# 5. IV with Innovation as instrument, exogenous not specified with instruments
mod = ivreg(data = df_data, formula = Cooperation ~ Incoming.Spillovers + Appropriability + RD | Innovation + Appropriability + RD)
summary(mod, diagnostics = T)

# 6. IV with Innovation as instrument, exogenous specified with instruments
mod = ivreg(data = df_data, formula = Cooperation ~ Incoming.Spillovers + RD | Innovation + RD)
summary(mod, diagnostics = T)

# 5 2SLS
mod1 = lm(data = df_data, formula = Incoming.Spillovers ~ Innovation)
summary(mod1)
df_data$Incoming.Spillovers.Hat = predict.lm(object = mod1)
mod2 = lm(data = df_data, formula = Cooperation ~ Incoming.Spillovers.Hat + RD)
summary(mod2)

# 6B 2SLS
mod1 = lm(data = df_data, formula = Incoming.Spillovers ~ Basicness.RD)
summary(mod1)
df_data$Incoming.Spillovers.Hat = predict.lm(object = mod1)
mod2 = lm(data = df_data, formula = Cooperation ~ Incoming.Spillovers.Hat)
summary(mod2)

# 7B 2SLS
mod1 = lm(data = df_data, formula = Incoming.Spillovers ~ RD + Basicness.RD)
summary(mod1)
mod2 = lm(data = df_data, formula = Appropriability ~ RD + Basicness.RD)
summary(mod2)
df_data$Incoming.Spillovers.Hat = predict.lm(object = mod1)
df_data$Appropriability.Hat = predict.lm(object = mod2)
mod3 = lm(data = df_data, formula = Cooperation ~ Incoming.Spillovers.Hat + Appropriability.Hat)
summary(mod3)

# BINARY MODELS

# 1. OLS Cooperation.Bin ~ Incoming.Spillovers
mod = glm(data = df_data, formula = Cooperation.Bin ~ Incoming.Spillovers, family = binomial(link = "logit"))
summary(mod)

# 2. OLS Cooperation ~ Appropriability
mod = glm(data = df_data, formula = Cooperation.Bin ~ Appropriability, family = binomial(link = "logit"))
summary(mod)

# 3. OLS Cooperation ~ Incoming.Spillovers + Appropriability
mod = glm(data = df_data, formula = Cooperation.Bin ~ Incoming.Spillovers + Appropriability, family = binomial(link = "logit"))
summary(mod)

# 2B 2SLS
mod1 = lm(data = df_data, formula = Incoming.Spillovers ~ RD)
summary(mod1)
df_data$Incoming.Spillovers.Hat = predict.lm(object = mod1)
mod2 = glm(data = df_data, formula = Cooperation.Bin ~ Incoming.Spillovers.Hat, family = binomial(link = "logit"))
summary(mod2)
df_data$Cooperation.Prob = predict.glm(object = mod2, type = "response")
prob.thresh = 0.2
df_data$Cooperation.Hat = ifelse(df_data$Cooperation.Prob > prob.thresh, 1, 0)
conf.mat = table(df_data[, c("Cooperation.Bin", "Cooperation.Hat")])
conf.mat[1,1]/sum(conf.mat[1,])
conf.mat[2,2]/sum(conf.mat[2,])
(conf.mat[1,1]+conf.mat[2,2]) / sum(conf.mat)

# 2C 2SLS
mod1 = lm(data = df_data, formula = Incoming.Spillovers ~ Basicness.RD)
summary(mod1)
df_data$Incoming.Spillovers.Hat = predict.lm(object = mod1)
mod2 = glm(data = df_data, formula = Cooperation.Bin ~ Incoming.Spillovers.Hat, family = binomial(link = "logit"))
summary(mod2)
df_data$Cooperation.Prob = predict.glm(object = mod2, type = "response")
prob.thresh = 0.2
df_data$Cooperation.Hat = ifelse(df_data$Cooperation.Prob > prob.thresh, 1, 0)
conf.mat = table(df_data[, c("Cooperation.Bin", "Cooperation.Hat")])
conf.mat[1,1]/sum(conf.mat[1,])
conf.mat[2,2]/sum(conf.mat[2,])
(conf.mat[1,1]+conf.mat[2,2]) / sum(conf.mat)

