
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
