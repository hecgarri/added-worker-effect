rm(list=ls())

setwd("/home/hector/GoogleDrivePersonal/Research/added-worker-effect")

if (!require(vars)) install.packages("vars")
if (!require(urca)) install.packages("urca")
if (!require(data.table)) install.packages("data.table")
if (!require(tsDyn)) install.packages("tsDyn")
if (!require(forecast)) install.packages("forecast")

datos = fread("datos_.csv")

plot(datos)

modelo = VAR(datos, p = 12, type = "both")

plot(modelo)

summary(modelo)

normality.test(modelo)

lapply(8:24, function(x) serial.test(modelo, lags.pt = x, type = "PT.adjusted"))

hist(residuals(modelo)[,1])
hist(residuals(modelo)[,2])

shapiro.test(residuals(modelo)[,1])
shapiro.test(residuals(modelo)[,2])

vecm1 = ca.jo(datos, type = "eigen", ecdet = c("const"), spec = "longrun")



plot.ts(datos)

arch.test(vec2var(vecm1))


vecm = VECM(datos, lag = 12, r = 1, estim = c("ML"),LRinclude = c("const"))

cajorls(vecm1, r=1)

summary(vecm)