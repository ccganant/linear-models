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
anova(modelo2)
shapiro.test(residuals(modelo2))



#---------------- 2 --------------#

#a



#b
#cor.test(x, y, alternative = c("two.sided", "less", "greater"),method = c("pearson", "kendall", 
#"spearman"),exact = NULL, conf.level = 0.95, continuity = FALSE, ...)

#--------------- 3 ---------------#
datos2<-read.csv2("Pinus_patula.csv"  )

#--------------- 4 ---------------#
datos3<-read.csv2("volu.csv")

mod1<-lm(vol~d,data=datos3)
summary(mod1)

mod2<-lm(vol~d+alt,data=datos3)
summary(mod2)


