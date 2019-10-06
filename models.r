################################################################################################
#
# ANÁLISE DE SÉRIES TEMPORAIS - MBA Business Analytics e Big Data
# Por: RICARDO REIS
#
# CASE - SALES
#
################################################################################################


#limpa variaveis da memoria
rm(list = ls())

#carrega o pacote de projecao
library(forecast)
library(lubridate)

# 1
#carrega o arquivo com a série temporal de vendas bruta
vendas <- read_excel("DepartmentStoreSales_V2.xls")

#--------------------------------------------------------------------------

# 2
#Converte a série de vendas bruta no formato de série temporal
vendas_ts <- ts(vendas$Sales, start=c(2005,1), end=c(2010,4), frequency = 4)

#Plota o gráfico da série temporal
plot(vendas_ts, xlab="Tempo", ylab="Vendas", bty="l")

#--------------------------------------------------------------------------

# 3
#Resumo estatístico da base
summary(vendas)

#--------------------------------------------------------------------------

# 4
#Separa as amostras em treinamento e teste

#Define o tamanho da amostra de teste
tam_amostra_teste <- 4

#define o tamanho da amostra de treinamento
tam_amostra_treinamento <- length(vendas_ts) - tam_amostra_teste

#cria a série temporal de treinamento
treinamento_ts <- window(vendas_ts, start=c(2005, 1), end=c(2005,tam_amostra_treinamento))

#cria a série temporal de teste
validacao_ts <- window(vendas_ts, start=c(2005, tam_amostra_treinamento + 1), end=c(2005,tam_amostra_treinamento+tam_amostra_teste))

#plota o gráfico da série temporal de treinamento e teste
plot(treinamento_ts, xlab="Tempo", ylab="Vendas", xaxt="n", xlim=c(2005, 2011), bty="l")

axis(1, at=seq(2005, 2011, 1), labels=format(seq(2005, 2011,1)))

lines(validacao_ts, bty="l", col="red")

#--------------------------------------------------------------------------

# 5
# Modelo de Tendência Linear

#Estima o modelo de tendência linear
modelo_tendencia_linear <- tslm(treinamento_ts ~ trend)

#resumo do modelo
summary(modelo_tendencia_linear)

#Verificando resíduos

#Plotando os resíduos
plot(modelo_tendencia_linear$residuals, xlab="Tempo", ylab="Resíduos", ylim=c(-500, 500), bty="l")

#calcula a autocorrelação dos resíduos
Acf(modelo_tendencia_linear$residuals)

#verifica os resíduos com teste de Ljung-Box
checkresiduals(modelo_tendencia_linear, test="LB")

#plot modelo com tendência
plot(treinamento_ts, xlab="Tempo", ylab="Passageiros", bty="l")
lines(modelo_tendencia_linear$fitted.values, lwd=2)

#projeta o modelo durante o período de validação
modelo_tendencia_linear_proj <- forecast(modelo_tendencia_linear, h = tam_amostra_teste, level=0.95)

#plota o gráfico da série temporal de treinamento e teste
plot(modelo_tendencia_linear_proj, xlab="Tempo", ylab="Vendas", xaxt="n" , xlim=c(2005, 2011), bty="l", flty=2)

axis(1, at=seq(2005, 2011, 1), labels=format(seq(2005, 2011,1)))

lines(validacao_ts)
lines(modelo_tendencia_linear_proj$fitted, lwd=2, col="blue")

#Verifica a acurácia do modelo
accuracy(modelo_tendencia_linear_proj, validacao_ts)

#--------------------------------------------------------------------------

# 6
# Modelo de Tendência Quadrática

#Estima o modelo de Tendência Quadrática
modelo_tendencia_poli <- tslm(treinamento_ts ~ trend + I(trend^2))

#resumo do modelo
summary(modelo_tendencia_poli)

#Verificando resíduos

#Plotando os resíduos
plot(modelo_tendencia_poli$residuals, xlab="Tempo", ylab="Resíduos", ylim=c(-500, 500), bty="l")

#calcula a autocorrelação dos resíduoss
Acf(modelo_tendencia_poli$residuals)

#verifica os resíduos com teste de Ljung-Box
checkresiduals(modelo_tendencia_poli, test="LB")

#plot modelo com tendência
plot(treinamento_ts, xlab="Tempo", ylab="Vendas", bty="l")
lines(modelo_tendencia_poli$fitted.values, lwd=2)

#projeta o modelo durante o período de validação
modelo_tendencia_poli_proj <- forecast(modelo_tendencia_poli, h = tam_amostra_teste, level=0.95)

#plota o gráfico da série temporal de treinamento e teste
plot(modelo_tendencia_poli_proj, xlab="Tempo", ylab="Vendas", xaxt="n", xlim=c(2005, 2011), bty="l", flty=2,main="Forecast from Polynomial regression model")

axis(1, at=seq(2005, 2011, 1), labels=format(seq(2005, 2011,1)))

lines(validacao_ts)
lines(modelo_tendencia_poli_proj$fitted, lwd=2, col="blue")

#Verifica a acurácia do modelo
accuracy(modelo_tendencia_poli_proj, validacao_ts)

#--------------------------------------------------------------------------

# 7
# Modelo de Tendência Linear Com Sazonalidade

#Estima o modelo de tendência linear com sazonalidade
modelo_sazonalidade_tendencia_linear <- tslm(treinamento_ts ~ season + trend)

#resumo do modelo
summary(modelo_sazonalidade_tendencia_linear)

#Verificando resíduos

#Plotando os resíduos
plot(modelo_sazonalidade_tendencia_linear$residuals, xlab="Tempo", ylab="Resíduos", ylim=c(-500, 500), bty="l")

#calcula a autocorrelação dos resíduos
Acf(modelo_sazonalidade_tendencia_linear$residuals)

#verifica os resíduos com teste de Ljung-Box
checkresiduals(modelo_sazonalidade_tendencia_linear, test="LB")

#plot modelo com sazonalidade
plot(treinamento_ts, xlab="Tempo", ylab="Vendas", bty="l")
lines(modelo_sazonalidade_tendencia_linear$fitted.values, lwd=2, col="blue")

#projeta o modelo durante o período de validação
modelo_sazonalidade_tendencia_linear_proj <- forecast(modelo_sazonalidade_tendencia_linear, h = tam_amostra_teste, level=0.95)

#plota o gráfico da série temporal de treinamento e teste
plot(modelo_sazonalidade_tendencia_linear_proj, xlab="Tempo", ylab="Vendas", xaxt="n", xlim=c(2005, 2011), bty="l", flty=2, main="Forecast from Seasonal and Trend regression model")

axis(1, at=seq(2005, 2011, 1), labels=format(seq(2005, 2011,1)))

lines(validacao_ts)
lines(modelo_sazonalidade_tendencia_linear_proj$fitted, lwd=2, col="blue")

#Verifica a acurácia do modelo
accuracy(modelo_sazonalidade_tendencia_linear_proj, validacao_ts)

#--------------------------------------------------------------------------

# 8
# Modelo de Tendência Quadrática Com Sazonalidade

#Estima o modelo de tendência quadrática
modelo_sazonal_tend_poli <- tslm(treinamento_ts ~ season + trend + I(trend^2))

#resumo do modelo
summary(modelo_sazonal_tend_poli)

#Verificando resíduos

#Plotando os resíduos
plot(modelo_sazonal_tend_poli$residuals, xlab="Tempo", ylab="Resíduos", ylim=c(-500, 500), bty="l")

#calcula a autocorrelação dos resíduos
Acf(modelo_sazonal_tend_poli$residuals)

#verifica os resíduos com teste de Ljung-Box
checkresiduals(modelo_sazonal_tend_poli, test="LB")

#plot modelo com sazonal_tend
plot(treinamento_ts, xlab="Tempo", ylab="Vendas", bty="l")
lines(modelo_sazonal_tend_poli$fitted.values, lwd=2, col="blue")

#projeta o modelo durante o período de validação
modelo_sazonal_tend_poli_proj <- forecast(modelo_sazonal_tend_poli, h = tam_amostra_teste, level=0.95)

#plota o gráfico da série temporal de treinamento e teste
plot(modelo_sazonal_tend_poli_proj, xlab="Tempo", ylab="Vendas", xaxt="n", xlim=c(2005, 2011), bty="l", flty=2, main="Forecast from Seasonal & Tendencia regression model")

axis(1, at=seq(2005, 2011, 1), labels=format(seq(2005, 2011,1)))

lines(validacao_ts)
lines(modelo_sazonal_tend_poli_proj$fitted, lwd=2, col="blue")

#Verifica a acurácia do modelo
accuracy(modelo_sazonal_tend_poli_proj, validacao_ts)

#--------------------------------------------------------------------------

# 9
# Calcular os erros de projeção para cada modelo

#Verifica a acurácia do modelo de tendência linear
accuracy(modelo_tendencia_linear_proj, validacao_ts)

#Verifica a acurácia do modelo de tendência quadrática
accuracy(modelo_tendencia_poli_proj, validacao_ts)

#Verifica a acurácia do modelo de tendência linear com sazonalidade
accuracy(modelo_sazonalidade_tendencia_linear_proj, validacao_ts)

#Verifica a acurácia do modelo de tendência quadrática com sazonalidade
accuracy(modelo_sazonal_tend_poli_proj, validacao_ts)

#--------------------------------------------------------------------------

# 10
# Escolher o melhor modelo de projeção justificando
