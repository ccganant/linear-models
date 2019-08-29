setwd("C:/Users/Dell/Desktop/Taller 5")
crecimiento<-read.csv("datos5.csv", sep=";", dec=".")
str(crecimiento)
install.packages("FSA")
install.packages("FSAdata")
install.packages("nlstools")
install.packages("FSAteach")
install.packages("nls")
library(FSA)
library(FSAdata)
library(nlstools)
library(nls)

with(crecimiento, plot(hd,edad, main="CRECIMIENTO DE P. PATULA", pch=20, cex.lab=1.5, cex.axis=1.5,
     cex.main=1.7, xlab="Edad (Años)", ylab="Altura dominante (m)"))
partida<- vbStarts(hd~edad,data=crecimiento)
unlist(partida)
parVB <- list(Linf=28.90689616, K=0.04300411, t0=-2.51631906)
parVB
ModelH<- hd~Linf*(1-exp(-K*(edad-t0)))
ModelH
ModelHajustado<- nls(ModelH,data=crecimiento,start=partidaVB)
ModelHajustado
fitPlot(ModelHajustado,xlab="Edad (Años)",ylab="Altura dominante (m)",main="CRECIMIENTO DE P. PATULA",cex.lab=1.5,cex.axis=1.5,cex.main=1.7 )
overview(ModelHajustado)
sitios<-read.csv("sitios.csv", sep=";", dec=".")
par(mfrow=c(1,1))
with(sitios, plot(hd,edad, main="CRECIMIENTO DE PINO PATULA", pch=20, cex.lab=1.5, cex.axis=1.5,
                       cex.main=1.7, xlab="Edad", ylab="Altura dominante"))
with(sitios, plot(hd,edad, main="CRECIMIENTO DE PINO PATULA", pch=20, cex.lab=1.5, cex.axis=1.5,
                       cex.main=1.7, xlab="Edad", ylab="Altura dominante"))


sitio1<- subset(sitios,sitios==1)
sitio1<- as.data.frame(sitio1)
sitio2<- subset(sitios,sitios==2)
sitio2<- as.data.frame(sitio2)
plot(sitio1$edad, sitio1$hd, pch = 17,col="cadetblue", type="b",main="CRECIMIENTO DE P.PATULA", xlab = 'Edad (Años)',
     ylab="Altura dominante (m)", xlim=range(sitio1$edad), ylim=range(sitio1$hd), cex.lab=1.5, cex.axis=1.5,
     cex.main= 1.5)
lines(sitio2$edad,sitio2$hd,pch=16,type="b",xlab="Edad(taños)",ylab="Altura dominante", xlim=range(sitio2$t),
      ylim=range(sitio2$hd), col="darksalmon")
legend (19, 12, c("Sitio 1 (PB)", "Sitio 2 (CA)"), pch = c(17, 16), col=c("cadetblue", "darksalmon"))
sitiosdummy<-data.frame(rbind(cbind(sitio1, sit1=1, sit2=0), 
                              cbind(sitio2, sit1=0, sit2=1)))
str(sitiosdummy)
head(sitiosdummy)
partidaVB2<-list(Linf1=28.90689616, K1=0.04300411, t01=-2.51631906, 
                 Linif2=28.90689616, K2=0.04300411, Linif2=28.90689616)
a<-partidaVB$Linf
b<-partidaVB$K
c<-partidaVB$t0
Modelositios<-with(sitiosdummy, (nls(hd~sit1*Linf1*(1-exp(-K1*(edad-t01)))
                                     +sit2*Linf2*(1-exp(-K2*(edad-t02))), start=
                                       list(Linf1=a,K1=b,t01=c,Linf2=a,K2=b,t02=c))))
overview((Modelositios))
X<-c(0:28)
hdlin1<-with(sitiosdummy, 85.003757*(1-exp(-0.008218*(X+5.297875))))
lines(sitiosdummy$edad, hdlin1, col="blue")
hdlin2<-with(sitiosdummy, 42.877048*(1-exp(-0.026164*(X+3.187655))))
plot(sitio1$edad, sitio1$hd, pch = 17,col="cadetblue", 
     type="b",main="Crecimiento de P. patula", xlab = 'Edad',
     ylab="Altura dominante", xlim=range(sitio1$edad), ylim=range(sitio1$hd), 
     cex.lab=1.5, cex.axis=1.5,
     cex.main= 1.5)
lines(sitio2$edad,sitio2$hd,pch=16,type="b",xlab="Edad(taños)",
      ylab="Altura dominante", xlim=range(sitio2$t),
      ylim=range(sitio2$hd), col="darksalmon")
legend (19, 12, c("Sitio 1 (PB)", "Sitio 2 (CA)"), pch = c(17, 16), 
        col=c("cadetblue", "darksalmon"))
lines(sitiosdummy$edad, hdlin2, col="red")
lines(0:28, hdlin1, col="blue")      
lines(0:28, hdlin2, col="red", lty=2)
legend (3, 22, c("IS=PB", "IS=CA"), lty = c(1, 4), col=c("cadetblue", "darksalmon"))
legend (19, 12, c("Sitio 1 (PB)", "Sitio 2 (CA)"), pch = c(17, 16), 
        col=c("cadetblue", "darksalmon"))
AIC(ModelHajustado)
AIC(Modelositios)
overvie

b1<2.26832-7 
b2<-26.06115
partidaSCH<-list(b1=72.26832, b2=26.06115)
ModelSCH<-hd ~ b1*exp(-b2/edad)
ModelSCH
ModelSCHajustado<- nls(ModelSCH, data=crecimiento, start=list(3,2))
ModelHajustado
fitPlot(ModelHajustado,xlab="Edad (Años)",ylab="Altura dominante (m)",main="CRECIMIENTO DE P. PATULA",cex.lab=1.5,cex.axis=1.5,cex.main=1.7 )
overview(ModelHajustado)
LM<-with(crecimiento, lm(log(hd)~I(-1/edad)))
anova(LM)
b1<-exp(coefficients(LM)[1]+anova(LM)$"Mean Sq"[2]/2)
ModelSCHajustado<-with(crecimiento, nls(hd~b1*exp(-b2/edad), 
                                        start=list(b1=b0, b2=coefficients(LM)[2])))
overview((ModelSCHajustado))
fitPlot(ModelSCHajustado,xlab="Edad (Años)",ylab="Altura dominante (m)",main="CRECIMIENTO DE P. PATULA",cex.lab=1.5,cex.axis=1.5,cex.main=1.7 )
ModelSCHajustado
X<-3.2:28
SCH<-with(crecimiento, 25.307*exp(-7.750/X))
lines(3.2:28, SCH)

ModelSCHositios<-with(sitiosdummy, nls(hd~sit1*b1*exp(-b2/edad)
                          +sit2*b12*exp(-b22/edad), start=list(b1=b0,
                                          b2=coefficients(LM)[2], b12=b0,
                                                     b22=coefficients(LM)[2])))

X<-c(0:28)
sch1<-with(sitiosdummy, 24.4490*exp(-8.3797/X))
lines(sitiosdummy$edad, hdlin1, col="blue")
sch2<-with(sitiosdummy, 28.9556*exp(-7.8892/X))
plot(sitio1$edad, sitio1$hd, pch = 17,col="cadetblue", 
     type="b",main="Crecimiento de P. patula", xlab = 'Edad',
     ylab="Altura dominante", xlim=range(sitio1$edad), ylim=range(sitio1$hd), 
     cex.lab=1.5, cex.axis=1.5,
     cex.main= 1.5)
lines(sitio2$edad,sitio2$hd,pch=16,type="b",xlab="Edad(taños)",
      ylab="Altura dominante", xlim=range(sitio2$t),
      ylim=range(sitio2$hd), col="darksalmon")
legend (19, 12, c("Sitio 1 (PB)", "Sitio 2 (CA)"), pch = c(17, 16), 
        col=c("cadetblue", "darksalmon"))
lines(3.2:28, sch2, col="red", lty=2)
lines(3.2:28, sch1, col="blue")      
legend (3, 22, c("IS=PB", "IS=CA"), lty = c(1, 4), col=c("cadetblue", "darksalmon"))
legend (19, 12, c("Sitio 1 (PB)", "Sitio 2 (CA)"), pch = c(17, 16), 
        col=c("cadetblue", "darksalmon"))
overview(ModelSCHajustado)
overview(ModelSCHositios)
AIC(ModelSCHajustado)
AIC(ModelSCHositios)

#CHAMPAN

install.packages("minpack.lm")

CHR<- hd~b1*(1-exp(-b2*edad))^b3
CHRajustado<-nlsLM(hd~ b1*(1-exp(-b2*edad))^b3,start = list(b1 = 30,
                                                   b2 = 0.002,
                                                   b3 = 0.64),data = crecimiento)
overview((CHRajustado))
X<-3.2:28
SCHR<-2.437e+02*(1-exp(-1.125e-03*X))^6.290e-01
lines(3.2:28, SCHR)

CHRsitios<-with(sitiosdummy, nlsLM(hd~ sit1*b1*(1-exp(-b2*edad))^b3+
              sit2*b1.2*(1-exp(-b2.2*edad))^b3.2,
            start = list(b1 = 30, b2 = 0.002, b3 = 0.64, b1.2=30,
                         b2.2 = 0.002, b3.2 = 0.64),data = sitiosdummy))
overview(CHRsitios)

X<-c(3.2:28)
CHR1<-2.437e+02*(1-exp(-8.231e-04*X))^6.617e-01
CHR2<-1.128e+02*(1-exp(-3.411e-03*X))^6.461e-01
plot(sitio1$edad, sitio1$hd, pch = 17,col="cadetblue", 
     type="b",main="Crecimiento de P. patula", xlab = 'Edad',
     ylab="Altura dominante", xlim=range(sitio1$edad), ylim=range(sitio1$hd), 
     cex.lab=1.5, cex.axis=1.5,
     cex.main= 1.5)
lines(sitio2$edad,sitio2$hd,pch=16,type="b",xlab="Edad(taños)",
      ylab="Altura dominante", xlim=range(sitio2$t),
      ylim=range(sitio2$hd), col="darksalmon")
legend (19, 12, c("Sitio 1 (PB)", "Sitio 2 (CA)"), pch = c(17, 16), 
        col=c("cadetblue", "darksalmon"))
lines(3.2:28, CHR2, col="red", lty=2)
lines(3.2:28, CHR1, col="blue")      
legend (3, 22, c("IS=PB", "IS=CA"), lty = c(1, 4), col=c("cadetblue", "darksalmon"))
legend (19, 12, c("Sitio 1 (PB)", "Sitio 2 (CA)"), pch = c(17, 16), 
        col=c("cadetblue", "darksalmon"))
overview(CHRsitios)


LM<-with(crecimiento, lm(log(hd)~I(-1/edad)))
anova(LM)
b1<-exp(coefficients(LM)[1]+anova(LM)$"Mean Sq"[2]/2)
ModelCHR<-nlsLM(CHR, start=list(b1=35, b2=0.05, b3=0.5))
View(sitiosdummy)

#weibull
weibull.fun<-function(b1,b2,b3,edad){
  b1*(1-exp(-b2*t^b3))
}

w<-fithd(x2$hd,x2$edad,plot = x2$g, modelName = "weibull")
summary(w)

Weibull<-hd~b1*(1-exp(-b2*edad^b3))
Wajustado<-with(crecimiento, nlsLM(Weibull,start = list(b1 = 26, b2 = 0.001, b3 = 2.47),
                     data = crecimiento, control = list(maxiter = 200),trace = T))

overview((Wajustado))
Y<-(3.2:28)
W<-6.264e+02*(1-exp(-4.188e-03*Y^6.311e-01))
lines(3.2:28, W)

Wsitios<-with(sitiosdummy, nlsLM(hd~ sit1*b1*(1-exp(-b2*edad^b3))+
                                     sit2*b1.2*(1-exp(-b2.2*edad^b3.2)),
                                   start = list(b1 = 26, b2 = 0.002, b3 = 2.47, 
                                                b1.2=26,b2.2 = 0.001, b3.2 = 2.47),
                                                    data = sitiosdummy,
                                        control=list(maxiter = 200), trace=T))
overview(Wsitios)

Y<-c(3.2:28)
W1<-4.623e+02*(1-exp(-4.771e-03 *Y^6.673e-01))
W2<-2.233e+02*(1-exp(-1.286e-02 *Y^6.538e-01))
plot(sitio1$edad, sitio1$hd, pch = 17,col="cadetblue", 
     type="b",main="Crecimiento de P. patula", xlab = 'Edad',
     ylab="Altura dominante", xlim=range(sitio1$edad), ylim=range(sitio1$hd), 
     cex.lab=1.5, cex.axis=1.5,
     cex.main= 1.5)
lines(sitio2$edad,sitio2$hd,pch=16,type="b",xlab="Edad(taños)",
      ylab="Altura dominante", xlim=range(sitio2$t),
      ylim=range(sitio2$hd), col="darksalmon")
legend (19, 12, c("Sitio 1 (PB)", "Sitio 2 (CA)"), pch = c(17, 16), 
        col=c("cadetblue", "darksalmon"))
lines(3.2:28, W2, col="red", lty=2)
lines(3.2:28, W1, col="blue")      
legend (3, 22, c("IS=PB", "IS=CA"), lty = c(1, 4), col=c("cadetblue", "darksalmon"))
legend (19, 12, c("Sitio 1 (PB)", "Sitio 2 (CA)"), pch = c(17, 16), 
        col=c("cadetblue", "darksalmon"))
overview(Wajustado)
overview(Wsitios)


LM<-with(crecimiento, lm(log(hd)~I(-1/edad)))
anova(LM)
b1<-exp(coefficients(LM)[1]+anova(LM)$"Mean Sq"[2]/2)
ModelCHR<-nlsLM(CHR, start=list(b1=35, b2=0.05, b3=0.5))
View(sitiosdummy)