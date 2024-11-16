if(!require(dplyr)) install.packages("dplyr")
library(dplyr)


# Importação e visualização inicial

covid_sp <- read.csv("dados_covid_sp.csv", sep=";")
View(covid_sp)

covid_sp <- read.csv("dados_covid_sp.csv", sep=";", encoding = "utf-8")
View(head(covid_sp))

# Renomear colunas (variáveis)

covid_sp_alterado <- rename(covid_sp, municipio = nome_munic)
View(head(covid_sp_alterado))

covid_sp_alterado <- rename(covid_sp_alterado, data = datahora, rotulo_mapa = map_leg, codigo_mapa = map_leg_s)
View(head(covid_sp_alterado))

# Remover colunas
# Recebe null -> exclui
covid_sp_alterado$cod_ra <- NULL
covid_sp_alterado$rotulo_mapa <- NULL

# Excluir múltiplas colunas
# -c é concatenando TODAS as colunas, menos aquelas selecionadas.
# Subset cria um novo dataset apartir de um com determinadas condições
covid_sp_alterado <- subset(covid_sp_alterado, select = -c(codigo_ibge, cod_drs))

# Excluir uma linha
covid_sp_alterado <- slice(covid_sp_alterado, -c(239660))

# Excluir várias linhas que tenham determinado valor

covid_sp_alterado <- covid_sp_alterado %>% filter(municipio!="Ignorado")


# Mudança e tipos
glimpse(covid_sp_alterado)

# Facilitando varíavel
df <- covid_sp_alterado

# Modificando tipos
df$semana_epidem <- as.integer(df$semana_epidem)

df$data <- as.Date(df$data, format = "%Y-%m-%d")
df$data <- format(df$data, "%d/%m/%Y")

# Criando colunas

df["idoso%"] = 100*(df$pop_60/df$pop)
df$`idoso%` = floor(df$`idoso%`)
df$`idoso%` = paste0(df$`idoso%`, "%")

# Exportando arquivo
write.table(df, file = "covid_sp_tratado.csv", sep = ",")
