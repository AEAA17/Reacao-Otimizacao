library("rsm")

library(FrF2)

planej= FrF2(nfactors=2,nruns=2^2,factor.names=c("T","C"), replications=2,randomize=FALSE)

y <- c(14761,28425,26210,30047,18064, 25009, 26963, 28319)

planej$y = y

planej=add.response(planej,y)

modelo <- lm(y ~ T*C, data = planej)

summary(modelo)

anov <- aov(modelo)

summary(anov)

shapiro.test(modelo$residuals) 

par(mfrow=c(2,2))

plot(modelo)

par(mfrow=c(1,1))

MEPlot(modelo) # Gráfico dos efeitos principais

legend(-1.1,32,legend=c("T−Temperatura","C−Catalisador"),cex=0.5)

IAPlot(modelo) # Gráfico de interação

legend(0.8,24,legend=c("T−Temperatura","C−Catalisador"),cex=0.5)

T1<-c(200,250,200,250,200,250,200,250)

C1<-c(0.1,0.1,0.3,0.3,0.1,0.1,0.3,0.3)

y <- c(14761,28425,26210,30047,18064, 25009, 26963, 28319)

modelo2 <-lm(y~T1*C1,data=planej) 

persp(modelo2,T1~C1,zlab="MW",col=rainbow(50), contours="colors")

legend("bottomright",legend=c("C1−Catalisador","T1−Temperatura"),cex=0.75, pt.cex=0.75)
