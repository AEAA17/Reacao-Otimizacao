Este código em R implementa um projeto para otimizar a etapa de transesterificação do polímero PBS (polibutileno succinato) utilizando uma abordagem estatística. O objetivo é analisar e modelar os efeitos das variáveis envolvidas, como a temperatura e a concentração de catalisador, para maximizar o rendimento do processo.

1. Carregamento das Bibliotecas
As bibliotecas rsm (Response Surface Methodology) e FrF2 (Fractional Factorial Design) foram carregadas para realizar o planejamento experimental e a análise estatística do processo:

rsm: Usada para análise de superfícies de resposta, que auxilia na otimização de processos.
FrF2: Utilizada para criar experimentos fatoriais, ajudando a explorar a relação entre os fatores e a resposta.

2. Planejamento Fatorial 2²
Foi criado um planejamento fatorial completo 2² com dois fatores (Temperatura T e Catalisador C) e quatro corridas experimentais (incluindo réplicas):

nfactors=2: Define o número de fatores (Temperatura e Catalisador).
nruns=2^2: Especifica o número de corridas experimentais, correspondente a um planejamento fatorial completo.
replications=2: Define o número de réplicas para aumentar a precisão dos resultados.
Os resultados experimentais (valores de y) foram atribuídos ao planejamento experimental.

3. Modelagem Estatística
Um modelo linear foi ajustado aos dados utilizando a função lm, considerando a interação entre os fatores T (Temperatura) e C (Catalisador):

modelo <- lm(y ~ T*C, data = planej): Ajusta o modelo considerando os efeitos principais e a interação entre os fatores.

4. Análise de Variância (ANOVA)
Foi realizada uma análise de variância (ANOVA) para avaliar a significância dos efeitos dos fatores:

summary(anov): Exibe os resultados da ANOVA, indicando a importância estatística de cada fator e da interação entre eles.

5. Teste de Normalidade dos Resíduos
Para verificar a adequação do modelo, foi aplicado o teste de Shapiro-Wilk aos resíduos do modelo:

shapiro.test(modelo$residuals): Verifica se os resíduos seguem uma distribuição normal, condição importante para a validade das inferências estatísticas.

6. Diagnóstico Gráfico do Modelo
Gráficos de diagnóstico foram gerados para avaliar a qualidade do ajuste do modelo:

Gráficos de Resíduos: Auxiliam na identificação de problemas como heterocedasticidade ou valores atípicos.
Gráficos de Efeitos Principais (MEPlot(modelo)) e de Interação (IAPlot(modelo)): Visualizam a influência dos fatores e suas interações na resposta.

7. Superfície de Resposta
Uma superfície de resposta foi gerada para visualizar a interação entre os fatores Temperatura e Catalisador em relação ao rendimento do processo:

persp(modelo2, T1~C1, zlab="MW", col=rainbow(50), contours="colors"): Gera uma superfície tridimensional que mostra como a combinação de temperatura e concentração de catalisador influencia a resposta, auxiliando na identificação das condições ótimas de operação.

Código:

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