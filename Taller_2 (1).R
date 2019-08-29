datos<-read.csv2("taller2.csv")
summary(datos)



#--------------- 1 ------------------# 
pairs(datos[-27,])
pairs(Vol ~ alt+dap+edad, data = datos[-27,])

#b
library(mgcv)
#family=gaulss,fit = F
modesuav<-with(datos,gam(Vol~s(dap)+s(alt)+s(edad),data=datos[-27,]))
summary(modesuav)
plot(modesuav)
par(mfrow=c(2,2))

#c
library(tree)
modarb<-tree(Vol~.,data=datos[-27,])
plot(modarb)
text(modarb)

names(modarb)
modarb$frame


modelo1<-with(datos[-27,], lm(Vol~dap*alt*edad+I(dap^2)+I(edad^2)+I(alt^2)) )
summary(modelo1)

modbw<- step( modelo1, direction = "backward", trace=T)

modelo2<-with(datos[-27,], lm(Vol ~ dap + alt + edad + I(dap^2) + dap:edad + alt:edad) )
summary(modelo2)

modbw2<- step( modelo2, direction = "backward", trace=T)
modelo3<-with(datos[-27,], lm(Vol ~ dap + alt + edad + I(dap^2) + dap:edad ) )
summary(modelo3)

#d
anova(modelo3)
shapiro.test(residuals(modelo3))
par(mfrow=c(2,2))
plot(modelo3)


#---------------- 2 --------------#

#a



#b
#cor.test(x, y, alternative = c("two.sided", "less", "greater"),method = c("pearson", "kendall", 
#"spearman"),exact = NULL, conf.level = 0.95, continuity = FALSE, ...)

#--------------- 3 ---------------#
inv<-read.csv2("Pinus_patula.csv"  )


#a
r1 <- droplevels(subset(inv, lote=="R1"))
m1 <- lm(VCSCha~dappr + dcpr + altpro + narha, data = r1)
summary(m1)
shapiro.test(residuals(m1))

r3 <- droplevels(subset(inv, lote=="R3"))
m3 <- lm(VCSCha~dappr + dcpr + altpro + narha, data = r3)
summary(m3)
shapiro.test(residuals(m3))

r4 <- droplevels(subset(inv, lote=="R4"))
m4 <- lm(VCSCha~dappr + dcpr + altpro + narha, data = r4)
summary(m4)
shapiro.test(residuals(m4))

#3b- Compare los resultados de cada rodal con el modelo general con todos los datos para todo ese bosque.

modg <- lm(VCSCha~dappr + dcpr + altpro + narha, data = inv)
summary(modg)
shapiro.test(residuals(modg))

#c
modnl <- nls(Vtccha~a*dappr*exp(-b*altpro), data=inv,start = list(a=100, b=0.3))
summary(modnl)





#--------------- 4 ---------------#
datos3<-read.csv2("volu.csv")

with(datos3,plot(d,vol))
with(datos3,plot(alt,vol))

#1
mod1<-lm(vol~d,data=datos3)
summary(mod1)
with(datos3,plot(d,vol))
abline(mod1,col="red")
anova(mod1)
summary(mod1)
AIC(mod1)
shapiro.test(residuals(mod1))

predm1=predict(mod1)
error1=100*(predm1-datos3$vol)/datos3$vol
mean(error1)
sd(error1)



#2
m2<-lm(vol~I(d^2*alt), datos3)
summary(m2)
anova(m2)
shapiro.test(residuals(m2))
AIC(m2)

datos3$predm2=predict(m2)
plot(datos3$predm2,datos3$vol)
abline(0,1, col="red")

predm2=predict(m2)
error2=100*(predm2-datos3$vol)/datos3$vol
mean(error2)
sd(error2)

#prueba de bondad de ajuste 
pred <- predict(mod1)
pred
chi <- (datos3$vol-pred)^2/pred
chi
summary(chi)

#c modelo 
#voy a proponer el modelo lm(log(vs)~log(D)+log(H))
mod3<-lm(log(vol)~log(d)+log(alt),data=datos3)
summary(mod3)
anova(mod3)
AIC(mod3)
shapiro.test(residuals(mod3)) 

MSE <- anova(mod3)$"Mean Sq"[3]
FC<- exp(MSE/2)
FC
bo<-summary(mod3)$coefficients[1]+(MSE/2)
bo



b0<-exp(bo)
b0


datos3$pred.2<-exp(predict(mod3))*exp(MSE/2)
rse.exp<-with(datos3,(vol-pred.2)^2)
rse.exp<-sqrt(sum(rse.exp)/15) 
rse.exp

error3<- with(datos3, 100*(pred.2-vol)/vol)
mean(error3)
sd(error3)
