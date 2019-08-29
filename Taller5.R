setwd("~/semestre7/Modelacion_lineal/Taller5")
datos <- read.csv2("Taller5.csv")
summary(datos)
attach(datos)
plot(edad,hd,pch=20,cex.main=1.7,main="Base de datos P.patula")
names(datos)
#Vertalanfy
library(FSA)
valpart <- vbStarts(hd~edad,data=datos)#Valores de partida iniciales
unlist(valpart)#opcion sintetica para ver valpart

vpartVB <- list(Linf=28.90689616, K=0.04300411, t0=-2.51631906 )#Valores de partida para modelo vB
vpartVB

vbpppb<- hd~Linf*(1-exp(-K*(edad-t0)))#modelo VB para P. patula Piedras Blancas
vbpppb

ajvbpppb<- nls(vbpppb,data=datos,start=vpartVB)
ajvbpppb
summary(ajvbpppb)
#Grafico
fitPlot(ajvbpppb,xlab="Edad plantacion",ylab="Altura total ",
        main="Funcion de crecimiento VB para P.patula",cex.lab=1.5,cex.axis=1.5,cex.main=1.7)


 #library(tkrplot)

# growthModelSim("vbTypical",hd~edad,data=datos)#permite ajustar los parametros.
library(nlstools)
overview(ajvbpppb)
bootpppb<- nlsBoot(ajvbpppb,niter=200) # estimado bootstrap para los Limites de los parámetros del modelo
confint(bootpppb,plot=TRUE)

confint(bootpppb,plot=TRUE,cex.lab=1.5,cex.axis=1.5,cex.main=1.5,main="Interv parametros")

htest(bootpppb,"K",0.014,"less")
htest(bootpppb,"Linf",57.15769,"less")
htest(bootpppb,"t0",-5.07898,"less")

#Todos son buenos parametros

# library(MASS)
# par(mfrow=c(1,1))
# hist(studres(ajvbpppb), xlab='Residuales', main='Histograma de resid. estudent, verific de calidad', cex.lab=1.5, cex.axis=1.5,cex.main=1.5)

AIC(ajvbpppb)
Error1=((predict(ajvbpppb)-hd)*100/hd)
mean(Error1)
sd(Error1)

#Schumancher

ms <- with(datos,lm(log(hd)~I(1/edad)))
fo <- hd~a*exp(b/edad)
b0 <- exp(coefficients(ms)[1]+anova(ms)$"Mean Sq"[2]/2)
msnl <- with(datos,nls(hd~a*exp(b/edad),start=list(a=b0,b=coefficients(ms)[2])))
msnl
overview(msnl)
summary(msnl)

#Grafico
fitPlot(msnl,xlab="Edad plantacion",ylab="Altura total ",
        main="Funcion de crecimiento VB para P.patula",cex.lab=1.5,cex.axis=1.5,cex.main=1.7)

#msnl1<- nlsBoot(msnl,niter=200) # estimado bootstrap para los Limites de los parámetros del modelo
#confint(bootpppb,plot=TRUE)

#confint(bootpppb,plot=TRUE,cex.lab=1.5,cex.axis=1.5,cex.main=1.5,main="Interv parametros")

# htest(bootpppb,"K",0.014,"less")
# htest(bootpppb,"Linf",57.15769,"less")
# htest(bootpppb,"t0",-5.07898,"less")

AIC(msnl)
Error2=((predict(msnl)-hd)*100/hd)
mean(Error2)
sd(Error2)

#Chapman-Richards 
library(minpack.lm)
fcr <- hd ~ A*(1 - exp(-k*edad))^p
library(nls2)

mcr <- nlsLM((hd ~ A*(1 - exp(-k*edad))^p),data=datos, start=list(A = 83, k = 0.03, p = 4))

summary(mcr)

overview(mcr)


#Grafico
fitPlot(mcr,xlab="Edad plantacion",ylab="Altura total ",
        main="Funcion de crecimiento VB para P.patula",cex.lab=1.5,cex.axis=1.5,cex.main=1.7)

AIC(mcr)
Error3=((predict(mcr)-hd)*100/hd)
mean(Error3)
sd(Error3)

#modelo por LOCALIDAD
names(datos)
LocalidadPB<-droplevels(subset(datos,loc=="PB"))
LocalidadCA<-droplevels(subset(datos,loc=="CA"))
LocalidadPB
LocalidadCA

#MODELO VERTALANFY
library(FSA)
valpart1 <- vbStarts(hd~edad,data=LocalidadCA)#Valores de partida para esta localidad
unlist(valpart1)#opcion sintetica para ver valpart

vpartVBca <- list(Linf=49.52988317, K=0.01959337, t0=-8.95033920 )#Valores de partida para modelo vB
vpartVBca

vbpppb1<- hd~Linf*(1-exp(-K*(edad-t0)))#modelo VB para P. patula caldas
vbpppb1

ajvbpppb1<- nls(vbpppb1,data=LocalidadCA,start=vpartVBca)
ajvbpppb1
summary(ajvbpppb1)
#MODELO PARA PIEDRAS BLANCAS
library(FSA)
valpart2 <- vbStarts(hd~edad,data=LocalidadPB)#Valores de partida para esta localidad
unlist(valpart2)#opcion sintetica para ver valpart

vpartVBpb <- list(Linf=22.51345816, K=0.05264151, t0=-3.03598395 )#Valores de partida para modelo pb
vpartVBpb

vbpppb2<- hd~Linf*(1-exp(-K*(edad-t0)))#modelo VB para P. patula caldas
vbpppb2

ajvbpppb2<- nls(vbpppb2,data=LocalidadPB,start=vpartVBpb)
ajvbpppb2
summary(ajvbpppb2)

#GRAFICAS

fitPlot(ajvbpppb1,xlab="Edad plantacion",ylab="Altura total ",
        main="Funcion de crecimiento VB CA para P.patula",cex.lab=1.5,cex.axis=1.5,cex.main=1.7)


#library(tkrplot)

# growthModelSim("vbTypical",hd~edad,data=Localiddca)#permite ajustar los parametros.
library(nlstools)
overview(ajvbpppb1)
bootpppb1<- nlsBoot(ajvbpppb1,niter=200) # estimado bootstrap para los Limites de los parámetros del modelo
confint(bootpppb1,plot=TRUE)

confint(bootpppb1,plot=TRUE,cex.lab=1.5,cex.axis=1.5,cex.main=1.5,main="Interv parametros")

htest(bootpppb1,"K",0.02616,"less")
htest(bootpppb1,"Linf",42.87786,"less")
htest(bootpppb1,"t0",-3.18775,"less")#Grafico


fitPlot(ajvbpppb2,xlab="Edad plantacion",ylab="Altura total ",
        main="Funcion de crecimiento VB para P.patula",cex.lab=1.5,cex.axis=1.5,cex.main=1.7)


#library(tkrplot)

# growthModelSim("vbTypical",hd~edad,data=datos)#permite ajustar los parametros.
library(nlstools)
overview(ajvbpppb2)
bootpppb2<- nlsBoot(ajvbpppb2,niter=200) # estimado bootstrap para los Limites de los parámetros del modelo
confint(bootpppb2,plot=TRUE)

confint(bootpppb2,plot=TRUE,cex.lab=1.5,cex.axis=1.5,cex.main=1.5,main="Interv parametros")

htest(bootpppb2,"K",0.008218,"less")
htest(bootpppb2,"Linf",85.003708,"less")
htest(bootpppb2,"t0",-5.297874,"less")

#AIC ERRORES
AIC(ajvbpppb1)
Error11=((predict(ajvbpppb1)-hd)*100/hd)
mean(Error11)
sd(Error11)
#PIEDRAS BLANCAS
AIC(ajvbpppb2)
Error12=((predict(ajvbpppb2)-hd)*100/hd)
mean(Error12)
sd(Error12)
#Comparar con el modelo completo
ks.test(predict(ajvbpppb),hd)
ks.test(predict(ajvbpppb1),hd)
ks.test(predict(ajvbpppb2),hd)
#mejor modelo completo que separado por localidad