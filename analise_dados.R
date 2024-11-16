
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

# Fazendo filtro por cidade
df_campinas = cidade_df("Campinas")
df_garulhos = cidade_df("Guarulhos")

# Análises estatísticas
summary(df_campinas$obitos)

# Plot(eixo_x, eixo_y, ) -> De crescimento ou decrescimento, linha simples
plot(df_campinas$data, df_campinas$obitos, title("MÉDIA ÓBITOS"), col = "red", type = "line")

# Histograma -> frequências de óbitos
df_campinas_outubro = df_campinas %>% filter(df_campinas$mes == 7)
hist(df_campinas_outubro$obitos, col="purple")
