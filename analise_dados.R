
setwd("~/rdata/projeto--1")

# Pacotes necessários
library(dplyr)
if(!require(rstatix)) install.packages("rstatix") 
library(rstatix)

df = read.csv("covid_sp_tratado.csv", sep=",", encoding="utf-8")
View(df)

# Arrumando a coluna criada e a data
glimpse(df)

df["idoso%"] = df$idoso.
df$`idoso%` = as.integer(gsub("%", "", df$`idoso%`))
df$idoso. = NULL
df = rename(df, porcentagem_idoso = `idoso%`)

df$data = as.Date(df$data, format = "%d/%m/%Y")

# Criando função para criação de DF para cidades
cidade_df = function (nome_cidade) {
  df_cidade = df %>% filter(municipio==nome_cidade)
  df_cidade$densidade_demo = floor(df_cidade$pop/(df_cidade$area/100))
  return(df_cidade)
}

### FILTRAGEM

# Fazendo filtro por cidade
df_campinas = cidade_df("Campinas")
df_garulhos = cidade_df("Guarulhos")

### ANÁLISES ESTATÍSTICAS
summary(df_campinas$obitos)

# Plot(eixo_x, eixo_y, ) -> De crescimento ou decrescimento, linha simples
plot(df_campinas$data, df_campinas$obitos, title("MÉDIA ÓBITOS"), col = "red", type = "line")

# Histograma -> frequências de óbitos
df_campinas_outubro = df_campinas %>% filter(df_campinas$mes == 7)
hist(df_campinas_outubro$obitos, col="purple")


### MEDIDAS DE POSIÇÃO
# Mínimo e máximo
min(df_campinas$obitos_novos)
min(df_campinas$casos_novos)
max(df_campinas$obitos_novos)

# Amplitude
range(df_campinas$obitos_novos)
max(df_campinas$obitos_novos) - min(df_campinas$obitos_novos)

# Quartis
quantile(df_campinas$obitos_novos)

# Amplitude interquartil IQR (Q3-Q1)
amp_iqr <- IQR(df_campinas$obitos_novos)

# Limite superior e inferior
# Outliners > limite superior; Outliners < limite inferior
quartis <- quantile(df_campinas$obitos_novos)
limite_superior <- quartis[3] + (1.5 * amp_iqr)
limite_inferior <- quartis[1] - (1.5 * amp_iqr)
# Chegando a -12 e 15, sabendo que números dentro desses são normais.

# Verificando tudo com o plot do box plot para casos novos
summary(df_campinas$obitos_novos)
boxplot(df_campinas$obitos_novos)

### Tratando outliners
# Indentificando outliners
if(!require(outliers)) install.packages("outliers") 
library(outliers)

# Removendo o maior outlier
outlier_in_campinas <- outlier(df_campinas$obitos_novos)
df_campinas <- df_campinas %>% filter(df_campinas$obitos_novos != outlier_in_campinas)

# Removendo todos outliers
df_campinas <- df_campinas %>% filter(df_campinas$obitos_novos >= limite_inferior &
                                        df_campinas$obitos_novos <= limite_superior)
boxplot(df_campinas$obitos_novos)

### Testes de normalidade
# Utilizamos para verificar se a varíavel é uma distribuição normal simétrica
# Análise com gráficos
hist(df_campinas$obitos_novos, probability = T, col="blue")
lines(density(df_campinas$obitos_novos), col="red")

# Outro tipo
qqnorm(df_campinas$obitos_novos)
qqline(df_campinas$obitos_novos)

### Correlação e regressão linear
# Lembrando, dados paramétricos: normais, normalidade
# Coeficiente de Pearson -> homogeneos e paramétricos
# Correlação de Spearman -> volume grande de dados não paramétricos
# Correlação de Kendall -> Volume pequeno de dados não paramétricos
plot(df_campinas$casos, df_campinas$obitos)

# Verificando que nessa variável não há normalidade
qqnorm(df_campinas$obitos)
qqline(df_campinas$obitos)

# Como não há normalidade, precisamos usar Spearman ou Kendall, como é grande vamos de spearman
cor(df_campinas$casos, df_campinas$obitos, method="spearman")

# Buscando a equação da reta de regressão
# lm -> linear model
# Formula -> y em x
regressao <- lm(formula= obitos ~ casos, data=df_campinas)
# [1] é o coeficiente linear e [2] é o angular
regressao$coefficients
# Chegaríamos a uma equação como: 0.03384923x+47.20146719
# Montando uma função matemática para chegar ao resultado de óbitos com 3000 casos.
regressao_funcao_matematica <- function(x) {
  return (0.03384923 * x) + 47.20146719
}
# 1015, podendo verificar no plot a veracidade da informação
regressao_funcao_matematica(30000) 

# Para verificarmos a precisão dessa função, podemos rodar o summary da nossa regresão
summary(regressao) # 0.9834 ou 98%

### GRÁFICO DE LINHA COM AJUSTE DE RETA
if(!require(ggplot2)) install.packages("ggplot2")
library(ggplot2)

ggplot(data = df_campinas, mapping = aes(x = casos, y = obitos)) +
  geom_point() +
  geom_smooth(method = "lm", color = "red") +
  theme_gray()
  
# Verificando a correlação da melhor forma
if(!require(corrplot)) install.packages("corrplot")
library(corrplot)

matriz_cor <- cor(
  x = df_campinas$casos, 
  y = df_campinas[, c("obitos", "obitos_novos")], 
  method = "spearman"
)

corrplot(matriz_cor, method="color")


### Gráfico final de exploração com ggplot
covid_cidades <- df %>% filter(municipio %in% c("Campinas", "Guarulhos", "Sorocaba"))

ggplot(covid_cidades, aes(x = casos, y = obitos, color = municipio)) +
  geom_line() +
  labs(title="Evolução dos óbitos em função dos casos de COVID em 3 cidades",
       x = "Casos",
       y = "Óbitos", 
       color = "Cidades") +
  theme_classic()

### Utilizando regressão para chegar ao numero de óbitos em 20000 casos nas 3 cidades
funcao_regressao_obitos_cidade <- function(cidade, casos) {
  df_regressao <- df %>% filter(municipio == cidade)
  regressao <- lm(formula = obitos ~ casos, data = df_regressao)
  coeficientes <- regressao$coefficients
  
  calculo <- coeficientes[2] * casos + coeficientes[1]
  r_squared <- summary(regressao)$r.squared
  
  return(sprintf("%.2f óbitos com a precisão de %.4f", calculo, (r_squared * 100)))
}
funcao_regressao_obitos_cidade("Campinas", 60000)
