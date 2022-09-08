# PROBLEMA DE NEGÓCIO:

#Determinada empresa do ramo da construção civil deseja avaliar se fatores econômicos
#influenciam no saldo de emprego desse setor. Dessa forma, construa um modelo de Machine
#Learning que seja capaz de prever o saldo(positivo/negativo) em determinado mês.

#VARIÁVEIS DISPONÍVEIS NO BANCO DE DADOS
#• saldo_emprego – diferença entre a contratação e a demissão na construção civil em
#determinado mês; (positivo e negativo);
#• PIB – Variação do produto interno bruto, no mês/ano (IBGE);
#• inflacao – inflação no mês/ano (IBGE);
#• taxa_juros – Taxa de Juros(IPCA) no mês/ano;
#• taxa_cambio – taxa de câmbio no mês/ano;
#• patentes – número de patentes no mês/ano;


# Prevendo saldo

set.seed(50)

# ------------------------------------------
# Etapa 1 - Verificando  os dados 
library(readxl)
emprego <- read_excel("D:/Users/1031580/Desktop/Tharick_modelos_estatisticos/Exercicio_2/EXERCICIO1_2_Emprego.xlsx")
View(emprego)

# Adicionando um indicador para saldo positivo
emprego$saldo_emprego <- ifelse(emprego$saldo_emprego == "Positivo", 1, 0)
emprego$saldo_emprego = as.factor(emprego$saldo_emprego)

# ---------------------------------------------------------------------------------------------------------
# Etapa 3 - Calculando a correlação
str(emprego)
# Explorando relacionamento entre as variáveis: Matriz de correlação
correlacao = cor(doenca_hepatica[c("PIB","inflacao","taxa_juros","taxa_cambio","patentes")])
require(corrplot)
corrplot(correlacao,method="color",tl.cex = 0.6)

emprego$taxa_cambio = NULL

# ---------------------------------------------------------------------------------------------------------
# Etapa 3 - Separando em base de treino e teste

#install.packages("caret", dependencies = TRUE)
library(caret)

indice_treino = createDataPartition(y=emprego$saldo_emprego, p=0.6, list=FALSE)
base_treino = emprego[indice_treino, ]
base_teste = emprego[-indice_treino, ]

# ---------------------------------------------------------------------------------------------------------
## Etapa 4 - Balanceando a base de dados

table(base_treino$saldo_emprego)
prop.table(table(base_treino$saldo_emprego))
barplot(prop.table(table(base_treino$saldo_emprego)))

#Como há mais respostas negativas que positivas será feito o balanceamento:

#install.packages("tidymodels")
require(tidymodels)
dados_treino_balanceado <-
  recipe(saldo_emprego ~ ., data = base_treino) %>%
  themis::step_upsample(saldo_emprego) %>%
  prep() %>%
  juice
prop.table(table(dados_treino_balanceado$saldo_emprego))

# ---------------------------------------------------------------------------------------------------------

#Etapa 5 - Construindo o modelo de regressão logística
modelo1 <- glm(saldo_emprego ~ ., 
               data = dados_treino_balanceado, family = "binomial"(link = "logit"))
summary(modelo1)

# -------------------------------
# Retirando a variável inflacao
modelo2 <- glm(saldo_emprego ~ . -inflacao, 
               data = dados_treino_balanceado, family = "binomial"(link = "logit"))
summary(modelo2)

# Comparando modelo menor com o maior
anova(modelo2, modelo1, test="Chisq") 
# se valor p > niv.sig., as variáveis omitidas não são significativas 

#Comparando os dois AICs
AIC(modelo1)
AIC(modelo2)

# Multicolinearidade
library(car)
vif(modelo2) # valores abaixo de 5 - OK

## ---------------------------------------
##Etapa 6 - FAzendo previsões do modelo de treino

#Probabilidade da pessoa desenvolver ou não do saldo ser positivo
previsoes_treino <- predict(modelo2, dados_treino_balanceado,type = "response")

resultados_treino <- data.frame(previsoes_treino, dados_treino_balanceado$saldo_emprego)
resultados_treino$previsao_cat <- as.factor(ifelse(previsoes_treino > 0.6,1,0))

library(dplyr)
resultados_treino <- resultados_treino %>%
  mutate(faixa = 
           case_when(previsoes_treino <= 0.10 ~ "0-FX10",
                     (previsoes_treino > 0.10 & previsoes_treino <= 0.20) ~ "1-FX20",
                     (previsoes_treino > 0.20 & previsoes_treino <= 0.30) ~ "2-FX30",
                     (previsoes_treino > 0.30 & previsoes_treino <= 0.40) ~ "3-FX40",
                     (previsoes_treino > 0.40 & previsoes_treino <= 0.50) ~ "4-FX50",
                     (previsoes_treino > 0.50 & previsoes_treino <= 0.60) ~ "5-FX60",
                     (previsoes_treino > 0.60 & previsoes_treino <= 0.70) ~ "6-FX70",
                     (previsoes_treino > 0.70 & previsoes_treino <= 0.80) ~ "7-FX80",
                     (previsoes_treino > 0.80 & previsoes_treino <= 0.90) ~ "8-FX90",
                     (previsoes_treino > 0.90 & previsoes_treino <= 1) ~ "9-FX100"))

colnames(resultados_treino) <- c('previsto_prob','real_treino','previsto_cat',"previsto_faixa")

## ---------------------------------------
##Etapa 7- Desempenho do modelo de treino


# Matriz de confusão e medidas
library(caret)
matriz_confusao_treino = confusionMatrix(resultados_treino$previsto_cat, dados_treino_balanceado$saldo_emprego, positive = "1")
matriz_confusao_treino

#Curva ROC
library(ROSE)
roc.curve(dados_treino_balanceado$saldo_emprego, previsoes_treino, plotit = T, col = "red")


resultados_treino$Qtde = 1
require(ggplot2)
ggplot(resultados_treino, aes(fill = real_treino,
                              y = Qtde, x = previsto_faixa))+
  geom_bar(position = "fill", stat = "identity")

## ---------------------------------------

##Etapa 8- FAzendo previsões do modelo de teste

previsoes_teste <- predict(modelo2, base_teste,type = "response")

resultados_teste <- data.frame(previsoes_teste, base_teste$saldo_emprego)
resultados_teste$previsao_cat <- as.factor(ifelse(previsoes_teste > 0.6,1,0))

colnames(resultados_teste) <- c('previsto_prob','real_treino','previsto_cat')

matriz_confusao_teste = confusionMatrix(resultados_teste$previsto_cat, base_teste$saldo_emprego, positive = "1")
matriz_confusao_teste

matriz_confusao_treino




