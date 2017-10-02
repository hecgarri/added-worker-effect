rm(list=ls())

setwd("/home/hector/GoogleDrivePersonal/Research/added-worker-effect")

if (!require(vars)) install.packages("vars"); require(vars) 
if (!require(urca)) install.packages("urca"); require(urca) 
if (!require(data.table)) install.packages("data.table"); require(data.table)
if (!require(tsDyn)) install.packages("tsDyn"); require(tsDyn)
if (!require(forecast)) install.packages("forecast"); require(forecast)
if (!require(ggplot2)) install.packages("ggplot2"); require(ggplot2)
if (!require(reshape2)) install.packages("reshape2"); require(reshape2)
if (!require(RColorBrewer)) install.packages("RColorBrewer"); require(RColorBrewer)

#############################################################################
# Modelo Hombres
#############################################################################

datos = fread("hombres.csv")

names(datos) = c("mes","Desocupación", "Participación")

datos$Desocupación = ts(datos$Desocupación, frequency = 12, start=c(2010,2))

dummies = seasonaldummy(datos$Desocupación)

datos = datos[,c(2,3)]

VARselect(datos, lag.max = 12, type=c("const"), exogen = dummies)

plot.ts(datos, family="serif")


vecm2 = lapply(2:12, function(x) ca.jo(datos, type=c("eigen"), ecdet=c( "const"), K=x,
                                      spec="longrun", dumvar = dummies))

modelo = lapply(1:11, function(x) vec2var(vecm2[[x]], r=1))

lags = 3:30
resultado = sapply(lags, function(x) lapply(1:length(modelo), 
            function(y) serial.test(modelo[[y]],
          lags.pt = x)$serial$p.value) %>% as.numeric()) %>% t() %>% 
  data.frame() %>% mutate(rezago = lags)


meltresultado = melt(resultado, id = "rezago", na.rm=TRUE) %>% 
  rename(Modelo = variable, `p-value`=value) 

#meltresultado$Modelo = recode(meltresultado$Modelo, 'X1'='1 rezago', 'X2'='2 rezagos', 
#                              'X3'='3 rezagos', 'X4'='4 rezagos', 'X5'='5 rezagos', 
#                              'X6' = '6 rezagos')

ggplot(meltresultado, aes(x=rezago, y=`p-value`, colour=Modelo, group=Modelo)) + 
  geom_line(size=1.2)+geom_hline(yintercept = 0.05, color="red")+
  geom_hline(yintercept = 0.1, col="red2")+
  theme(panel.background = element_rect(),
        plot.background = element_rect(colour = "white",size = 0.5), 
        axis.text.x = element_text(size=10, family="serif"), 
        axis.title.x = element_text(size=15, family="serif"), 
        axis.title.y = element_text(size=15, family = "serif"), 
        legend.background = element_rect(fill="grey95"), 
        legend.text = element_text(size=10, family="serif"), 
        legend.title = element_text(face="bold", family="serif"), 
        legend.title.align = 0.5)+
  scale_colour_manual(values = 1:11)+
  scale_x_continuous(breaks = c(5,10,15,20,25,30))+
  scale_y_continuous(breaks = seq(0.0,1,0.05))


vecm1 = ca.jo(datos, type = "eigen", ecdet = c("const"),
              K=5, spec = "transitory",dumvar = dummies)



normality.test(vec2var(vecm1))

arch.test(vec2var(vecm1))

summary(vecm1)

vecm2 = VECM(datos, lag = 11, r = 1, estim = c("ML"),LRinclude = c("const"), 
             beta = beta)

cajorls(vecm1, r=1)

impulso = irf(vec2var(vecm1), n.ahead = 12)
plot(impulso)

summary(blrtest(vecm1, H=c(1,-0.48,18.9),r=1))

head(vecm1@x)

# para ver todos los objetos de un objeto de tipo S4

slotNames(vecm1)

#############################################################################
# Modelo Mujeres
#############################################################################

rm(list=ls())

setwd("/home/hector/GoogleDrivePersonal/Research/added-worker-effect")

datos = fread("mujeres.csv")

names(datos) = c("mes","Desocupación", "Participación")

datos$Desocupación = ts(datos$Desocupación, frequency = 12, start=c(2010,2))

dummies = seasonaldummy(datos$Desocupación)

datos = datos[,c(2,3)]


datos$Desocupación = ts(datos$Desocupación, frequency = 12, start=c(2010,2))
datos$Participación = ts(datos$Participación, frequency = 12, start=c(2010,2))

datos2 = mutate(datos,Desocupación = c(NA,diff(Desocupación, differences = 1)),
                Participación = c(NA,diff(Participación, differences = 1)))


plot.ts(datos)

VARselect(datos, lag.max = 12, type=c("const"), exogen = dummies)

n = length(datos$Desocupación)

VARselect(datos2[2:n,], lag.max = 30, type=c("none"), exogen = dummies[2:n,])


vecm2 = lapply(2:12, function(x) ca.jo(datos, type=c("eigen"), ecdet=c("const"), K=x,
                                       spec="longrun", dumvar = dummies))

modelo = lapply(1:11, function(x) vec2var(vecm2[[x]], r=1))

lags = 3:30
resultado = sapply(lags, function(x) lapply(1:length(modelo), 
            function(y) serial.test(modelo[[y]],lags.pt = x, type="PT.asymptotic")$serial$p.value) %>%
              as.numeric()) %>% t() %>% 
  data.frame() %>% mutate(rezago = lags)


meltresultado = melt(resultado, id = "rezago", na.rm=TRUE) %>% 
  rename(Modelo = variable, `p-value`=value) 

#meltresultado$Modelo = recode(meltresultado$Modelo, 'X1'='1 rezago', 'X2'='2 rezagos', 
#                              'X3'='3 rezagos', 'X4'='4 rezagos', 'X5'='5 rezagos', 
#                              'X6' = '6 rezagos')

ggplot(meltresultado, aes(x=rezago, y=`p-value`, colour=Modelo, group=Modelo)) + 
  geom_line(size=1.2)+geom_hline(yintercept = 0.05, color="red")+
  geom_hline(yintercept = 0.1, col="red2")+
  theme(panel.background = element_rect(),
        plot.background = element_rect(colour = "white",size = 0.5), 
        axis.text.x = element_text(size=10, family="serif"), 
        axis.title.x = element_text(size=15, family="serif"), 
        axis.title.y = element_text(size=15, family = "serif"), 
        legend.background = element_rect(fill="grey95"), 
        legend.text = element_text(size=10, family="serif"), 
        legend.title = element_text(face="bold", family="serif"), 
        legend.title.align = 0.5)+
  scale_colour_manual(values = 1:11)+
  scale_x_continuous(breaks = c(5,10,15,20,25,30))+
  scale_y_continuous(breaks = seq(0.0,1,0.05))


vecm1 = ca.jo(datos, type = "eigen", ecdet = c("none"),
              K=4, spec = "transitory",dumvar = dummies)

summary(vecm1)

model_var = VAR(datos, lag.max = 12, type = "const", exogen = dummies, ic = "AIC")

summary(model_var)

normality.test(model_var)

arch.test(model_var)

serial = lapply(4:20, function(i) serial.test(model_var, lags.pt = i,
          type = "PT.asymptotic")$serial$p.value) %>% as.numeric()
abline(h=0.05)
ts.plot(serial)

estabil = stability(model_var, type = "Rec-MOSUM", dynamic = TRUE)
plot(estabil)

causality(model_var, cause = "Desocupación", boot = TRUE, boot.runs = 1000)
causality(model_var, cause = "Participación", boot = TRUE, boot.runs = 1000)

plot(irf(model_var, runs = 1000))



#############################################################################
# Modelo Mujeres Jóvenes (15-29)
#############################################################################


rm(list=ls())

setwd("/home/hector/GoogleDrivePersonal/Research/added-worker-effect")

datos = fread("mujeres_jov.csv")

names(datos) = c("mes","Desocupación", "Participación")

datos$Desocupación = ts(datos$Desocupación, frequency = 12, start=c(2010,2))

dummies = seasonaldummy(datos$Desocupación)

datos = datos[,c(2,3)]

datos$Desocupación = ts(datos$Desocupación, frequency = 12, start=c(2010,2))
datos$Participación = ts(datos$Participación, frequency = 12, start=c(2010,2))

ts.plot(datos)
plot.ts(datos)

VARselect(datos, lag.max = 30, type=c("none"), exogen = dummies)

