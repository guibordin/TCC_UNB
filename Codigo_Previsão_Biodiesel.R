################################################################################
# Trabalho de Conclusão de Curso – Engenharia de Produção (UnB)
# Tema: Modelos Computacionais Aplicados à Decisão:
#       Análise de Modelos Estatísticos na Produção de Biodiesel
#
# Autor: Guilherme Bordin de Meira e Silva
# Orientador: Prof. Dr. André Luiz Marques Serrano
# Versão do R: 4.3.3   |   Versão do RStudio: 2024.04.2+764
# Data do script: 15/06/2025
################################################################################

################################################################################
# 1. Instalação e carregamento dos pacotes necessários
# (Descomente as linhas de install.packages caso esteja rodando pela primeira vez)
################################################################################

# install.packages("tidyverse")
# install.packages("lubridate")
# install.packages("readxl")
# install.packages("openxlsx")
# install.packages("forecast")
# install.packages("tseries")
# install.packages("lmtest")
# install.packages("prophet")
# install.packages("ggplot2")

library(tidyverse)    # Manipulação de dados 
library(lubridate)    # Manipulação de datas
library(readxl)       # Leitura de arquivos Excel (.xlsx)
library(openxlsx)     # Outra opção para leitura e escrita de Excel
library(forecast)     # Modelos de séries temporais (ARIMA, ETS, TBATS, HW)
library(tseries)      # Testes estatísticos para séries temporais (ADF, KPSS)
library(lmtest)       # Testes de diagnóstico (ex: Ljung-Box)
library(prophet)      # Modelo Prophet do Facebook
library(ggplot2)      # Visualização de dados
library(scales)       # Ajuste de escalas em gráficos
library(dplyr)        # Auxilia na manipulação de dados

################################################################################
# 2. Importação, interpolação e preparação da série temporal
################################################################################

# Define o link do arquivo Excel (raw) hospedado no GitHub do projeto
url_excel <- "https://github.com/guibordin/TCC_UNB/raw/refs/heads/main/ANP_DB_Biodiesel_2005-2021.xlsx"

# Faz o download do arquivo Excel para um arquivo temporário local
temp_file <- tempfile(fileext = ".xlsx")
download.file(url_excel, destfile = temp_file, mode = "wb")

# Lê a aba "Consolidada" usando readxl
Dados <- read_excel(temp_file, sheet = "Consolidada")

# Remove a coluna de datas ("ANO MÊS") caso exista — ajuste se necessário
if ("ANO MÊS" %in% names(Dados)) {
  Dados <- Dados[, -which(names(Dados) == "ANO MÊS")]
}

# Função para interpolar valores faltantes/zeros na coluna Total
# Zeros são substituídos pela média dos vizinhos, exceto nos extremos
preencher_zeros <- function(dados) {
  total <- dados$Total
  n <- length(total)
  for (i in 1:n) {
    if (total[i] == 0) {
      if (i == 1) {
        total[i] <- total[i + 1] / 2
      } else if (i == n) {
        total[i] <- total[i - 1] * 2
      } else {
        total[i] <- mean(c(total[i - 1], total[i + 1]), na.rm = TRUE)
      }
    }
  }
  dados$Total <- total
  return(dados)
}

# Aplica a interpolação na base consolidada
Dados_interpolados <- preencher_zeros(Dados)

# Remove os primeiros 36 meses (jan/2005–dez/2007), conforme decisão metodológica
Dados_interpolados <- Dados_interpolados[-(1:36), ]

# Visualiza as primeiras linhas após o recorte
print(head(Dados_interpolados))

# Extrai o vetor principal da produção (Total)
TO <- Dados_interpolados$Total

# Checa se ainda há valores NA ou zero após tratamento
cat("Alguma linha NA? ", any(is.na(TO)), "\n")
cat("Alguma linha igual a zero? ", any(TO == 0), "\n")

# Exibe o vetor e sua classe
print(TO)
print(class(TO))

# Converte o vetor para série temporal mensal iniciando em jan/2008
TO.ts <- ts(TO, start = c(2008, 1), frequency = 12)

# Mostra início e fim da série
print(start(TO.ts))
print(end(TO.ts))

# Plota a série temporal (produção nacional de biodiesel)
par(mar = c(5, 5, 4, 2))
plot(TO.ts,
     main = "Série Temporal - Produção Nacional",
     ylab = expression("Produção de Biodiesel (m"^3*")"),
     xlab = "Ano",
     col = "black",
     yaxt = "n")

# Personaliza escala do eixo y para formato brasileiro (milhares com ponto)
axis(2,
     at = pretty(TO.ts),
     labels = format(pretty(TO.ts), big.mark = ".", decimal.mark = ",", scientific = FALSE))

################################################################################
# 3. Testes estatísticos de estacionariedade e diferenciação
################################################################################

# Realiza os principais testes para verificar se a série temporal é estacionária,
# condição fundamental para modelagem ARIMA e similares.

# --- Teste Dickey-Fuller Aumentado (ADF) ---
# Hipótese nula: a série NÃO é estacionária (presença de raiz unitária).
adf_result <- tseries::adf.test(TO.ts)
print(adf_result)

# --- Teste KPSS ---
# Hipótese nula: a série É estacionária.
kpss_result <- tseries::kpss.test(TO.ts)
print(kpss_result)

# Interpretação:
# Se o p-valor do ADF > 0,05 e o p-valor do KPSS < 0,05, a série NÃO é estacionária,
# indicando a necessidade de diferenciação.

# --- Aplica a primeira diferenciação para obter estacionariedade ---
TO_diff <- diff(TO.ts, differences = 1)

# --- Repete os testes na série diferenciada ---
adf_diff_result <- tseries::adf.test(TO_diff)
print(adf_diff_result)

kpss_diff_result <- tseries::kpss.test(TO_diff)
print(kpss_diff_result)

# --- Visualização da série diferenciada ---
par(mar = c(5, 5, 4, 2))
plot(TO_diff,
     main = "Série Temporal Diferenciada - Produção Nacional",
     ylab = expression("Produção de Biodiesel (Série Diferenciada) (m"^3*")"),
     xlab = "Ano",
     col = "blue",
     yaxt = "n")
axis(2,
     at = pretty(TO_diff),
     labels = format(pretty(TO_diff), big.mark = ".", decimal.mark = ",", scientific = FALSE))

# --- Gráficos de autocorrelação (ACF) e autocorrelação parcial (PACF) ---
acf(TO_diff, main = "ACF - Série Diferenciada")
pacf(TO_diff, main = "PACF - Série Diferenciada")

# Observação: 
# ACF (Autocorrelation Function) e PACF (Partial Autocorrelation Function) são gráficos
# adimensionais que auxiliam na identificação de padrões de dependência temporal, lags relevantes,
# e orientam a escolha dos parâmetros dos modelos ARIMA.


################################################################################
# 4. MODELO ARIMA – Ajuste, avaliação e gráficos
################################################################################

# --- Ajuste automático do modelo ARIMA à série completa ---
modeloTO <- auto.arima(TO.ts)
summary(modeloTO) # Exibe os parâmetros e estatísticas do ajuste

# --- Diagnóstico dos resíduos do modelo ARIMA ---
# Testes estatísticos e gráficos para avaliar normalidade, autocorrelação e aderência do modelo
coeftest(modeloTO)
checkresiduals(forecast(modeloTO))
tsdiag(modeloTO)
Box.test(residuals(modeloTO), type = "Ljung-Box")
residuoTO <- residuals(modeloTO)

# --- Gráfico dos resíduos do ARIMA ---
par(mar = c(5, 5, 4, 2))
ylim <- c(-120000, 120000)  # Limites ajustados para leitura
plot(residuoTO,
     main = "Resíduos do ARIMA",
     ylab = "Erro (m³)",
     xlab = "Ano",
     col = "black",
     yaxt = "n",
     ylim = ylim)
axis(2,
     at = seq(ylim[1], ylim[2], by = 20000),
     labels = format(seq(ylim[1], ylim[2], by = 20000), big.mark = ".", decimal.mark = ",", scientific = FALSE))

# --- Previsão para 10 anos à frente (120 meses) ---
prognosticoTO <- forecast(modeloTO, h = 120)
plot(prognosticoTO,
     main = "Previsão ARIMA para 10 anos",
     xlab = "Ano",
     ylab = expression("Produção de Biodiesel (m"^3*")"))

# --- Segmentação em treino e teste ---
treinamento_end <- c(2017, 12)        # Último mês do treino
teste_start <- c(2018, 1)             # Primeiro mês do teste
historico_arima <- window(TO.ts, end = treinamento_end)
teste_arima <- window(TO.ts, start = teste_start)

# --- Ajuste ARIMA no treino e previsão para o teste ---
modelo_ARIMA <- auto.arima(historico_arima)
previsao_ARIMA <- forecast(modelo_ARIMA, h = length(teste_arima))

# --- Avaliação das previsões no teste ---
erro_ARIMA <- accuracy(previsao_ARIMA, teste_arima)
print(erro_ARIMA)

# --- Guarda métricas para tabela consolidada ---
result_arima <- data.frame(Modelo = "ARIMA",
                           RMSE = erro_ARIMA["Test set", "RMSE"],
                           MAE  = erro_ARIMA["Test set", "MAE"],
                           MAPE = erro_ARIMA["Test set", "MAPE"])

# --- Gráfico comparativo: valores reais vs previsão ARIMA ---
autoplot(TO.ts, series = "Série Real") +
  autolayer(previsao_ARIMA$mean, series = "Previsão ARIMA", PI = FALSE) +
  autolayer(teste_arima, series = "Valores Reais (2018–2021)") +
  ggtitle("Comparação: Série Real vs Previsão ARIMA") +
  xlab("Ano") +
  ylab("Produção de Biodiesel (m³)") +
  scale_color_manual(values = c("red", "black", "blue")) +
  scale_y_continuous(labels = comma) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    legend.title = element_blank(),
    legend.text = element_text(size = 10),
    plot.title = element_text(hjust = 0.5)
  ) +
  guides(colour = guide_legend(nrow = 1))


################################################################################
# 5. MODELO HOLT-WINTERS – Ajuste, avaliação e gráficos
################################################################################

# --- Ajuste do modelo Holt-Winters à série completa ---
modelo_HW <- HoltWinters(TO.ts)
summary(modelo_HW) # Exibe os parâmetros e estatísticas do ajuste

# --- Diagnóstico dos resíduos do Holt-Winters ---
residuoHW <- residuals(modelo_HW)

# Gráfico dos resíduos Holt-Winters
par(mar = c(5, 5, 4, 2))
ylim <- c(-120000, 120000)  # Limites ajustados para leitura (ajuste se necessário)

plot(residuoHW,
     main = "Resíduos Holt-Winters",
     ylab = "Erro (m³)",
     xlab = "Ano",
     col = "black",
     yaxt = "n",
     ylim = ylim)

axis(2,
     at = seq(ylim[1], ylim[2], by = 20000),
     labels = format(seq(ylim[1], ylim[2], by = 20000), big.mark = ".", decimal.mark = ",", scientific = FALSE))

# Teste de ruído branco (indica se os resíduos são aleatórios)
Box.test(residuoHW, type = "Ljung-Box")

# Análise gráfica das autocorrelações dos resíduos
acf(residuoHW, main = "ACF dos Resíduos HW")
pacf(residuoHW, main = "PACF dos Resíduos HW")

# --- Previsão para 10 anos à frente (120 meses) ---
prognosticoHW <- forecast(modelo_HW, h = 120)
plot(prognosticoHW,
     main = "Previsão Holt-Winters para 10 anos",
     xlab = "Ano",
     ylab = expression("Produção de Biodiesel (m"^3*")"))

# --- Segmentação em treino e teste ---
historico_hw <- window(TO.ts, end = treinamento_end)
teste_hw <- window(TO.ts, start = teste_start)

# --- Ajuste Holt-Winters apenas nos dados de treino ---
modelo_HWt <- HoltWinters(historico_hw)
previsao_HW <- forecast(modelo_HWt, h = length(teste_hw))

# --- Avaliação das previsões no teste ---
erro_HW <- accuracy(previsao_HW, teste_hw)
print(erro_HW)

# --- Guarda métricas para tabela consolidada ---
result_hw <- data.frame(Modelo = "Holt-Winters",
                        RMSE = erro_HW["Test set", "RMSE"],
                        MAE  = erro_HW["Test set", "MAE"],
                        MAPE = erro_HW["Test set", "MAPE"])

# --- Gráfico comparativo: valores reais vs previsão Holt-Winters ---
autoplot(TO.ts, series = "Série Real") +
  autolayer(previsao_HW$mean, series = "Previsão Holt-Winters", PI = FALSE) +
  autolayer(teste_hw, series = "Valores Reais (2018–2021)") +
  ggtitle("Comparação: Série Real vs Previsão Holt-Winters") +
  xlab("Ano") +
  ylab("Produção de Biodiesel (m³)") +
  scale_color_manual(values = c("red", "black", "blue")) +
  scale_y_continuous(labels = comma) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    legend.title = element_blank(),
    legend.text = element_text(size = 10),
    plot.title = element_text(hjust = 0.5)
  ) +
  guides(colour = guide_legend(nrow = 1))


################################################################################
# 6. MODELO ETS – Ajuste, avaliação e gráficos
################################################################################

# --- Ajuste do modelo ETS à série completa ---
modelo_ETS <- ets(TO.ts)
summary(modelo_ETS)  # Exibe parâmetros do ajuste (modelo escolhido, suavização etc.)

# --- Diagnóstico dos resíduos do ETS ---
residuoETS <- residuals(modelo_ETS)

# Gráfico dos resíduos ETS
plot(residuoETS, main = "Resíduos ETS", ylab = "Erro (m³)", xlab = "Ano")

# Teste de ruído branco nos resíduos do ETS
Box.test(residuoETS, type = "Ljung-Box")

# Gráficos de autocorrelação dos resíduos
acf(residuoETS, main = "ACF dos Resíduos ETS")
pacf(residuoETS, main = "PACF dos Resíduos ETS")

# --- Previsão para 10 anos (120 meses) ---
prognosticoETS <- forecast(modelo_ETS, h = 120)
plot(prognosticoETS,
     main = "Previsão ETS para 10 anos",
     xlab = "Ano",
     ylab = expression("Produção de Biodiesel (m"^3*")"))

# --- Segmentação em treino e teste ---
historico_ets <- window(TO.ts, end = treinamento_end)
teste_ets <- window(TO.ts, start = teste_start)

# --- Ajuste ETS apenas nos dados de treino ---
modelo_ETSt <- ets(historico_ets)
previsao_ETS <- forecast(modelo_ETSt, h = length(teste_ets))

# --- Avaliação das previsões no teste ---
erro_ETS <- accuracy(previsao_ETS, teste_ets)
print(erro_ETS)

# Guarda métricas para tabela consolidada
result_ets <- data.frame(Modelo = "ETS",
                         RMSE = erro_ETS["Test set", "RMSE"],
                         MAE  = erro_ETS["Test set", "MAE"],
                         MAPE = erro_ETS["Test set", "MAPE"])

# --- Gráfico comparativo: valores reais vs previsão ETS ---
autoplot(TO.ts, series = "Série Real") +
  autolayer(previsao_ETS$mean, series = "Previsão ETS", PI = FALSE) +
  autolayer(teste_ets, series = "Valores Reais (2018–2021)") +
  ggtitle("Comparação: Série Real vs Previsão ETS") +
  xlab("Ano") +
  ylab("Produção de Biodiesel (m³)") +
  scale_color_manual(values = c("red", "black", "blue")) +
  scale_y_continuous(labels = comma) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    legend.title = element_blank(),
    legend.text = element_text(size = 10),
    plot.title = element_text(hjust = 0.5)
  ) +
  guides(colour = guide_legend(nrow = 1))


################################################################################
# 7. MODELO TBATS – Ajuste, avaliação e gráficos
################################################################################

# --- Ajuste do modelo TBATS à série completa ---
modelo_TBATS_completo <- tbats(TO.ts)
summary(modelo_TBATS_completo)  # Exibe parâmetros do ajuste

# --- Diagnóstico dos resíduos (com gráfico padronizado) ---
residuoTBATS <- residuals(modelo_TBATS_completo)

# Gráfico dos resíduos TBATS (padronizado)
par(mar = c(5, 5, 4, 2))
plot(residuoTBATS,
     main = "Resíduos TBATS",
     ylab = expression("Erro (m"^3*")"),
     xlab = "Ano",
     col = "black",
     yaxt = "n")
axis(2,
     at = pretty(residuoTBATS),
     labels = format(pretty(residuoTBATS), big.mark = ".", decimal.mark = ",", scientific = FALSE))

# Diagnósticos estatísticos dos resíduos
Box.test(residuoTBATS, type = "Ljung-Box")  # Teste de ruído branco
acf(residuoTBATS, main = "ACF dos Resíduos TBATS")
pacf(residuoTBATS, main = "PACF dos Resíduos TBATS")

# --- Previsão para 10 anos (120 meses) ---
prognosticoTBATS <- forecast(modelo_TBATS_completo, h = 120)
plot(prognosticoTBATS,
     main = "Previsão TBATS para 10 anos",
     xlab = "Ano",
     ylab = expression("Produção de Biodiesel (m"^3*")"))

# --- Divisão treino e teste ---
historico_tbats <- window(TO.ts, end = treinamento_end)
teste_tbats <- window(TO.ts, start = teste_start)

# --- Ajuste TBATS com parâmetros manuais no treino ---
modelo_TBATSt <- tbats(historico_tbats,
                       use.box.cox = TRUE,
                       use.trend = TRUE,
                       use.damped.trend = FALSE)
previsao_TBATS <- forecast(modelo_TBATSt, h = length(teste_tbats))

# --- Avaliação do desempenho no teste ---
erro_TBATS <- accuracy(previsao_TBATS, teste_tbats)
print(erro_TBATS)

result_tbats <- data.frame(Modelo = "TBATS",
                           RMSE = erro_TBATS["Test set", "RMSE"],
                           MAE  = erro_TBATS["Test set", "MAE"],
                           MAPE = erro_TBATS["Test set", "MAPE"])

# --- Gráfico comparativo: real vs previsão TBATS ---
autoplot(TO.ts, series = "Série Real") +
  autolayer(previsao_TBATS$mean, series = "Previsão TBATS", PI = FALSE) +
  autolayer(teste_tbats, series = "Valores Reais (2018–2021)") +
  ggtitle("Comparação: Série Real vs Previsão TBATS") +
  xlab("Ano") +
  ylab("Produção de Biodiesel (m³)") +
  scale_color_manual(values = c("red", "black", "blue")) +
  scale_y_continuous(labels = comma) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    legend.title = element_blank(),
    legend.text = element_text(size = 10),
    plot.title = element_text(hjust = 0.5)
  ) +
  guides(colour = guide_legend(nrow = 1))



################################################################################
# 8. MODELO PROPHET – Ajuste, avaliação, gráficos e resíduos
################################################################################

# --- Preparação dos dados para Prophet (formato exigido pelo pacote) ---
df_prophet <- data.frame(
  ds = seq.Date(from = as.Date("2008-01-01"), by = "month", length.out = length(TO.ts)),
  y  = as.numeric(TO.ts)
)

# --- Ajuste Prophet na série completa e previsão para 10 anos ---
modelo_Prophet <- prophet(df_prophet)
future_Prophet <- make_future_dataframe(modelo_Prophet, periods = 120, freq = "month")
forecast_Prophet <- predict(modelo_Prophet, future_Prophet)

# Marcação do início do período de previsão futura (linha vermelha)
linha_prev <- as.Date("2022-01-01")
previsao_futuro <- forecast_Prophet[forecast_Prophet$ds >= linha_prev, ]

# Garantindo que colunas de data estão corretas
df_prophet$ds <- as.Date(df_prophet$ds)
forecast_Prophet$ds <- as.Date(forecast_Prophet$ds)
previsao_futuro$ds <- as.Date(previsao_futuro$ds)

# Para legendas padronizadas
df_prophet$grupo <- "Série Real"
previsao_futuro$grupo <- "Previsão Prophet"

# --- Gráfico: Previsão de 10 anos com Prophet ---
ggplot() +
  geom_line(data = df_prophet, aes(x = ds, y = y, color = grupo), size = 1) +
  geom_line(data = previsao_futuro, aes(x = ds, y = yhat, color = grupo), size = 1) +
  geom_ribbon(data = previsao_futuro, aes(x = ds, ymin = yhat_lower, ymax = yhat_upper, fill = "Intervalo de confiança"), alpha = 0.3) +
  geom_vline(xintercept = linha_prev, linetype = "dashed", color = "red", size = 1) +
  xlab("Ano") +
  ylab(expression("Produção (m"^3*")")) +
  ggtitle("Previsão da Produção de Biodiesel com Prophet (2022–2031)") +
  scale_color_manual(values = c("Série Real" = "black", "Previsão Prophet" = "blue")) +
  scale_fill_manual(values = c("Intervalo de confiança" = "grey50")) +
  scale_y_continuous(labels = comma) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 14),
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 11),
    legend.position = "bottom",
    legend.title = element_blank(),
    legend.text = element_text(size = 10)
  )

# --- Divisão treino e teste para Prophet ---
ds_train <- df_prophet[1:120, ]  # 2008–2017 (120 meses)
ds_test  <- df_prophet[121:168, ] # 2018–2021 (48 meses)

modelo_Prophet_treino <- prophet(ds_train)
future_test <- make_future_dataframe(modelo_Prophet_treino, periods = 48, freq = "month")
previsao_test <- predict(modelo_Prophet_treino, future_test)

# --- Métricas de erro (RMSE, MAE, MAPE) para o período de teste ---
prev_teste_prophet <- tail(previsao_test$yhat, 48)
real_teste_prophet <- ds_test$y

prophet_rmse <- sqrt(mean((prev_teste_prophet - real_teste_prophet)^2))
prophet_mae  <- mean(abs(prev_teste_prophet - real_teste_prophet))
prophet_mape <- mean(abs((prev_teste_prophet - real_teste_prophet)/real_teste_prophet)) * 100

cat("Prophet – RMSE:", round(prophet_rmse, 2), "m³\n")
cat("Prophet – MAE:",  round(prophet_mae,  2), "m³\n")
cat("Prophet – MAPE:", round(prophet_mape, 2), "%\n")

result_prophet <- data.frame(Modelo = "Prophet",
                             RMSE = prophet_rmse,
                             MAE  = prophet_mae,
                             MAPE = prophet_mape)

# --- Gráfico treino/teste Prophet ---
df_pred_test <- data.frame(
  ds = ds_test$ds,
  y_real = real_teste_prophet,
  y_prev = prev_teste_prophet
)

ggplot() +
  geom_line(data = df_prophet, aes(x = ds, y = y, color = "Série Real"), size = 1) +
  geom_line(data = df_pred_test, aes(x = ds, y = y_real, color = "Valores Reais (2018–2021)"), size = 1.2) +
  geom_line(data = df_pred_test, aes(x = ds, y = y_prev, color = "Previsão Prophet"), size = 1.2) +
  ggtitle("Comparação: Série Real vs Previsão Prophet") +
  xlab("Ano") +
  ylab("Produção de Biodiesel (m³)") +
  scale_color_manual(values = c("Previsão Prophet" = "red", "Série Real" = "black", "Valores Reais (2018–2021)" = "blue")) +
  scale_y_continuous(labels = comma) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    legend.title = element_blank(),
    legend.text = element_text(size = 10),
    plot.title = element_text(hjust = 0.5)
  ) +
  guides(colour = guide_legend(nrow = 1))

# --- Gráfico dos resíduos Prophet (padronizado) ---
# Calcula resíduos como diferença entre observado e previsto (no período observado)
residuos_prophet <- df_prophet$y - forecast_Prophet$yhat[1:length(df_prophet$y)]
datas_prophet <- df_prophet$ds

par(mar = c(5, 5, 4, 2))
plot(datas_prophet, residuos_prophet,
     type = "l",
     col = "black",
     main = "Resíduos Prophet",
     xlab = "Ano",
     ylab = expression("Erro (m"^3*")"),
     xaxt = "n",
     yaxt = "n")
# Eixo X com anos legíveis
axis(1, at = pretty(datas_prophet), labels = format(pretty(datas_prophet), "%Y"))
# Eixo Y formatado
axis(2, at = pretty(residuos_prophet),
     labels = format(pretty(residuos_prophet), big.mark = ".", decimal.mark = ",", scientific = FALSE))

# --- (Opcional) Diagnóstico extra: histograma dos resíduos ---
# hist(residuos_prophet, breaks = 20, main = "Distribuição dos Resíduos Prophet",
#      xlab = expression("Erro (m"^3*")"), col = "grey", border = "white")



##############################################################
# ERROS CONSOLIDADOS – Tabela comparativa de desempenho
##############################################################

# (Cada resultado já traz RMSE, MAE e MAPE do conjunto de teste)

tabela_resultados <- rbind(
  result_arima,
  result_hw,
  result_ets,
  result_tbats,
  result_prophet
)

# Exibe a tabela com formatação amigável
tabela_formatada <- tabela_resultados
tabela_formatada$RMSE <- format(round(tabela_resultados$RMSE, 2), big.mark = ".", decimal.mark = ",")
tabela_formatada$MAE  <- format(round(tabela_resultados$MAE, 2), big.mark = ".", decimal.mark = ",")
tabela_formatada$MAPE <- paste0(format(round(tabela_resultados$MAPE, 2), decimal.mark = ","), " %")

print(tabela_formatada, row.names = FALSE)

##############################################################
# GRÁFICO CONSOLIDADO – PREVISÃO PARA 10 ANOS (2022–2031)
##############################################################

# --- 1. Criação dos dataframes das previsões futuras (120 meses) ---

datas_futuras <- seq.Date(from = as.Date("2022-01-01"), by = "month", length.out = 120)

df_arima    <- data.frame(Data = datas_futuras, Valor = as.numeric(prognosticoTO$mean),       Modelo = "ARIMA")
df_ets      <- data.frame(Data = datas_futuras, Valor = as.numeric(prognosticoETS$mean),      Modelo = "ETS")
df_hw       <- data.frame(Data = datas_futuras, Valor = as.numeric(prognosticoHW$mean),       Modelo = "Holt-Winters")
df_tbats    <- data.frame(Data = datas_futuras, Valor = as.numeric(prognosticoTBATS$mean),    Modelo = "TBATS")
df_prophet  <- data.frame(Data = datas_futuras, Valor = as.numeric(tail(forecast_Prophet$yhat, 120)), Modelo = "Prophet")

# --- 2. Série histórica real ---
datas_reais <- seq.Date(from = as.Date("2008-01-01"), by = "month", length.out = length(TO.ts))
df_real     <- data.frame(Data = datas_reais, Valor = as.numeric(TO.ts), Modelo = "Série Real")

# --- 3. Consolidação dos dados ---
df_consolidado <- bind_rows(
  df_arima,
  df_ets,
  df_hw,
  df_tbats,
  df_prophet,
  df_real
)

# --- 4. Cores padronizadas para cada modelo ---
cores_modelos <- c(
  "ARIMA"        = "orange",
  "ETS"          = "green",
  "Holt-Winters" = "gray40",
  "Prophet"      = "blue",
  "TBATS"        = "red",
  "Série Real"   = "black"
)

# --- 5. Geração do gráfico comparativo ---
ggplot(df_consolidado, aes(x = Data, y = Valor, color = Modelo)) +
  geom_line(size = 1.2) +
  xlab("Ano") +
  ylab(expression("Produção de Biodiesel (m"^3*")")) +
  ggtitle("Previsão da Produção de Biodiesel: Comparação dos Modelos (2022–2031)") +
  scale_color_manual(values = cores_modelos) +
  scale_y_continuous(labels = comma) +
  theme_minimal(base_size = 15) +
  theme(
    plot.title   = element_text(hjust = 0.5, size = 18),
    legend.position = "bottom",
    legend.title    = element_text(face = "bold"),
    legend.text     = element_text(size = 13)
  ) +
  guides(color = guide_legend(title = "Modelo", nrow = 1))


##############################################################
# GRÁFICO CONSOLIDADO – PREVISÃO NO PERÍODO DE TESTE (2018–2021)
##############################################################

# --- 1. Criação dos data frames para as previsões no período de teste ---

datas_teste <- seq.Date(from = as.Date("2018-01-01"), by = "month", length.out = 48)

df_arima_test   <- data.frame(Data = datas_teste, Valor = as.numeric(previsao_ARIMA$mean),     Modelo = "ARIMA")
df_ets_test     <- data.frame(Data = datas_teste, Valor = as.numeric(previsao_ETS$mean),       Modelo = "ETS")
df_hw_test      <- data.frame(Data = datas_teste, Valor = as.numeric(previsao_HW$mean),        Modelo = "Holt-Winters")
df_tbats_test   <- data.frame(Data = datas_teste, Valor = as.numeric(previsao_TBATS$mean),     Modelo = "TBATS")
df_prophet_test <- data.frame(Data = datas_teste, Valor = as.numeric(prev_teste_prophet),      Modelo = "Prophet")
df_real_test    <- data.frame(Data = datas_teste, Valor = as.numeric(teste_arima),             Modelo = "Valores Reais (2018–2021)")

# --- 2. Série real até 2017 (histórico) ---
datas_reais <- seq.Date(from = as.Date("2008-01-01"), by = "month", length.out = length(TO.ts))
df_real <- data.frame(Data = datas_reais, Valor = as.numeric(TO.ts), Modelo = "Série Real")
df_real_historico <- df_real[df_real$Data < as.Date("2018-01-01"), ]

# --- 3. Consolidação de todos os data frames para o gráfico ---
df_consolidado_teste <- bind_rows(
  df_arima_test,
  df_ets_test,
  df_hw_test,
  df_tbats_test,
  df_prophet_test,
  df_real_historico,
  df_real_test
)

# --- 4. Cores padronizadas para cada modelo ---
cores_modelos <- c(
  "ARIMA" = "orange",
  "ETS" = "green",
  "Holt-Winters" = "gray40",
  "Prophet" = "blue",
  "TBATS" = "red",
  "Série Real" = "black",
  "Valores Reais (2018–2021)" = "black"
)

# --- 5. Geração do gráfico consolidado ---
ggplot(df_consolidado_teste, aes(x = Data, y = Valor, color = Modelo)) +
  geom_line(size = 1.2) +
  xlab("Ano") +
  ylab(expression("Produção de Biodiesel (m"^3*")")) +
  ggtitle("Previsão da Produção de Biodiesel: Modelos no Período de Teste (2018–2021)") +
  scale_color_manual(values = cores_modelos) +
  scale_y_continuous(labels = comma) +
  theme_minimal(base_size = 15) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 18),
    legend.position = "bottom",
    legend.title = element_text(face = "bold"),
    legend.text = element_text(size = 13)
  ) +
  guides(color = guide_legend(title = "Modelo", nrow = 1))


#######################################################################
# FIM DO SCRIPT
#
# Script desenvolvido para o Trabalho de Conclusão de Curso – Engenharia de Produção (UnB)
# Modelos Computacionais Aplicados à Decisão: Análise de Modelos Estatísticos na Produção de Biodiesel
# Autor: Guilherme Bordin
# Orientador: Prof. Dr. André Luiz Marques Serrano
# Data: 15/06/2025
#
# Para dúvidas ou colaboração, entre em contato:
# Email: [gui_bordin@yahoo.com]   |   GitHub: github.com/guibordin
#
# Referências dos principais pacotes utilizados:
# - forecast (Hyndman et al.)
# - prophet (Taylor & Letham)
# - tidyverse (Wickham et al.)
# - tseries (Trapletti & Hornik)
# - ggplot2 (Wickham)
#
# Para garantir a reprodutibilidade dos resultados, consulte:
sessionInfo()
#######################################################################
