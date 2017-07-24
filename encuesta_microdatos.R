
rm(list=ls())

dir = "/home/hector/GoogleDriveUBB/OLR Ñuble - Observatorio laboral de Ñuble/Bases de datos/Encuesta de ocupación y desocupación en el gran Santiago/"

setwd(dir)

a = list.files(pattern = "^mar")[18:36]
b = list.files(pattern = "^jun")[41:58]
c = list.files(pattern = "^sept")[1:18]
d = list.files(pattern = "^dic")[1:17]

x = vector(class(a), length(a)+length(b)+length(c)+length(d))

x[c(TRUE, FALSE, FALSE, FALSE)] <- a
x[c(FALSE, TRUE, FALSE, FALSE)] <- b
x[c(FALSE, FALSE, TRUE, FALSE)] <- c
x[c(FALSE, FALSE, FALSE, TRUE)] <- d

muchas = x[1:71]

if (!require(foreign)) install.packages("foreign")
if (!require(data.table)) install.packages("data.table")

varias = lapply(muchas, function(x) read.spss(x, use.value.labels = FALSE))


