# Referências
# https://smolski.github.io/livroavancado/analise-de-clusters.html
# http://www.sthda.com/english/articles/31-principal-component-methods-in-r-practical-guide/118-principal-component-analysis-in-r-prcomp-vs-princomp/


# Importando as bibliotecas
library(readxl)
library(readr)
library(dplyr)
library(cluster) # for computing clustering algorithms
library(fpc)
library(reshape2)
library(ggplot2)
library(factoextra) # Para criar alguns gráficos #for ggplot2-based elegant visualization of clustering results
library(sf)
library(geobr)
library(stats) # Para PCA
library(writexl)
library(tidyverse) # Para manipular os dados
require(vegan)
require(psych)
require(lattice)
require(ggplot2)
library(clustrd)
library(formattable)
library(data.table)

#################################### IMPORTANDO DADOS #############################################
# Importando e tratando os dados
## Dados sociodemográficos
df_dados_sociodemograficos = read_excel('Dados_sociodemograficos_2.xlsx')
df_dados_sociodemograficos = df_dados_sociodemograficos %>%
  filter(UF == 'RJ') %>%
  select(COD7, COD6, NOME, PIB_P_CAP, GINI, POP,
         PERC_URB, PERC_MASC, PERC_BRANCOS, PERC60MAIS, PERC_POP_EDUC_SUP, 
         MEDICOS_100MIL, LEITOS_100MIL)

## Dados sobre o COVID-19
# o arquivo 'case-brasil-cities-time pode ser encontrado em https://github.com/wcota/covid19br/blob/master/cases-brazil-cities-time.csv.gz
df_covid19 = read_csv('cases-brazil-cities-time.csv')
df_covid19 = df_covid19 %>%
  filter(state == 'RJ',
         date == '2021-02-25') %>% # 1 ano desde o 1º caso confirmado de COVID-19 no Brasil (25/02/2020)
  select(city,deaths_per_100k_inhabitants, totalCases_per_100k_inhabitants, deaths_by_totalCases)
colnames(df_covid19) = c('NOME','MORTES_100MIL','CASOS_100MIL','MORTES_P_CASOS')
df_covid19$NOME = gsub('\\/\\S\\S', '', df_covid19$NOME) # removendo /RJ no final do nome dos municípios


## Dados sociodemográficos IpeaDATA
ipeadata_AguaRedeGeral = read_excel('ipeadata_AguaRedeGeral.xlsx')
ipeadata_AguaRedeGeral = ipeadata_AguaRedeGeral %>%
  select(COD7, PERC_AGUACANAL, PERC_SANITARIOS)

ipeadata_BolsaFamilia <- read_excel('ipeadata_BolsaFamilia.xlsx')
ipeadata_BolsaFamilia = ipeadata_BolsaFamilia %>%
  select(COD7, BENEFICIOS_PBF)

ipeadata_Desligamentos <- read_excel('ipeadata_Desligamentos.xlsx')
ipeadata_Desligamentos = ipeadata_Desligamentos %>%
  select(COD7, TRAB_DISPENSADOS)

ipeadata_Homicidios <- read_excel('ipeadata_Homicidios.xlsx')
ipeadata_Homicidios = ipeadata_Homicidios 

ipeadata_Suicidios <- read_excel('ipeadata_Suicidios.xlsx')
ipeadata_Suicidios = ipeadata_Suicidios %>%
  select(COD7, SUICIDIOS)

ipeadata_IDH_Educacao <- read_excel('ipeadata_IDH_Educacao.xlsx')
ipeadata_IDH_Educacao = ipeadata_IDH_Educacao %>%
  select(COD7, IDH_EDU)

ipeadata_IDH_Longevidade <- read_excel('ipeadata_IDH_Longevidade.xlsx')
ipeadata_IDH_Longevidade = ipeadata_IDH_Longevidade %>%
  select(COD7, IDH_LONG)

ipeadata_IDH_Renda <- read_excel('ipeadata_IDH_Renda.xlsx')
ipeadata_IDH_Renda = ipeadata_IDH_Renda %>%
  select(COD7, IDH_RENDA)

#Unindo as variáveis em uma única tabela para corte de 1 ano de COVID-19
df_covid_1_ano_base = merge(df_covid19, df_dados_sociodemograficos, by = 'NOME') 
df_covid_1_ano_base = merge(df_covid_1_ano_base, ipeadata_AguaRedeGeral, by = 'COD7') 
df_covid_1_ano_base = merge(df_covid_1_ano_base, ipeadata_BolsaFamilia, by = 'COD7') 
df_covid_1_ano_base = merge(df_covid_1_ano_base, ipeadata_Desligamentos, by = 'COD7') 
df_covid_1_ano_base = merge(df_covid_1_ano_base, ipeadata_Homicidios, by = 'COD7')
df_covid_1_ano_base = merge(df_covid_1_ano_base, ipeadata_IDH_Educacao, by = 'COD7') 
df_covid_1_ano_base = merge(df_covid_1_ano_base, ipeadata_IDH_Longevidade, by = 'COD7') 
df_covid_1_ano_base = merge(df_covid_1_ano_base, ipeadata_IDH_Renda, by = 'COD7')


## Feature selection: Filtrando variáveis númericas (Removendo os dados categóricos, os logs e os nulos)
df_covid_1_ano = df_covid_1_ano_base

rownames(df_covid_1_ano) <- df_covid_1_ano_base$NOME.x
df_covid_1_ano = df_covid_1_ano %>%
  mutate(BENEFICIOS_PBF_100MIL = BENEFICIOS_PBF*100000/POP,
         TRAB_DISPENSADOS_100MIL = TRAB_DISPENSADOS*100000/POP,
         HOMICIDIOS_100MIL = HOMICIDIOS*100000/POP) %>%
  select(-NOME.x,-NOME.y, -COD7, -COD6, -POP, -Sigla, 
         -BENEFICIOS_PBF, -TRAB_DISPENSADOS, -HOMICIDIOS,-MORTES_100MIL,-CASOS_100MIL)

#Para resolver o problema dos valores ausentes (os NA), poderia ser aplicada uma técnica robusta.
#Mas como esta é uma análise simples ou optei por remover os municípios que tinham algum dado faltando.
dados_simples_1_ano <- na.omit(df_covid_1_ano)

#write_xlsx(dados_simples_1_ano,"Data_COVID_1YEAR.xlsx")

#################################### CLUSTERIZAÇÃO SIMPLES ###########################################
#Para usar o algoritmo k-means para clusterizar os países, é necessário:
# 1. Calcular a distância (dissimilaridade) entre os países;
# 2. Escolher o número de clusteres.

#Para o cálculo da distância, temos um problema: as escalas das colunas são diferentes.
#Por isso, é necessário convertes todos os indicadores a uma escala única de média 0:

df1_escala <- scale(dados_simples_1_ano) #Executa o procedimento de centralizar em zero e alterar a escala para desvio padrão.

#A função scale em R, definida com as configurações padrão, calculará a média e o desvio padrão de todo o vetor, depois “dimensionará” cada elemento por esses valores subtraindo a média e dividindo pelo desvio padrão.
#Esta é uma etapa do processamento prévio de dados onde basicamente ajudará a normalizar os dados dentro de um determinado intervalo. Às vezes, também ajuda a acelerar os cálculos no algoritmo. O objetivo da normalização é alterar os valores das colunas numéricas em um conjunto de dados para uma escala comum, sem distorcer as diferenças nos intervalos de valores.

#Conferindo o output para a capital
df1_escala["Rio de Janeiro",]

#A determinação da quantidade de clusteres não segue uma regra pré-definida e deve ser pensada pelo responsável pela análise.
#Cada projeto de clusterização tem suas próprias particularidades.
#Contudo, alguns métodos analíticos podem ajudar nessa escolha, seja pela minização da soma dos quadrados dos clusteres ou pelo auxílio visual de um dendograma.

#Para determinar o número de clusteres pela minimização da soma dos quadrados dos clusteres (método do cotovelo), observe o gráfico abaixo:
# referencia: http://www.statmethods.net/advstats/cluster.html
# https://medium.com/pizzadedados/kmeans-e-metodo-do-cotovelo-94ded9fdf3a9


#Fixar uma seed para garantir a reproducibilidade da análise:
set.seed(123) # fixando os valores, para que o resultado gerado seja sempre o mesmo

wss <- (nrow(df1_escala)-1)*sum(apply(df1_escala,2,var))
for (i in 2:20) wss[i] <- sum(kmeans(df1_escala,
                                     centers=i)$withinss)
plot(1:20, wss, type="b", xlab="Número de Clusters",
     ylab="Soma dos quadrados dentro dos clusters")

#Pelo método de elbow

fviz_nbclust(x = df1_escala, FUNcluster = kmeans, method = 'wss') +
  geom_vline(xintercept = 4, linetype = 2)

#Ponto que indica o equilíbrio entre maior homogeneidade dentro do cluster e a maior diferença entre clusters:
#É o ponto da curva mais distante de uma reta traçada entre os pontos a0 e a19: 6 clusteres

# Pelo método do dendograma:
#A posição de cada município no dendograma é determinada pela dissimilaridade entre cada um dos outros municípios.

dendo <- df1_escala %>% dist %>% hclust
plot(dendo)
rect.hclust(dendo, k = 3, border = "blue") # é a divisão que pareceu mais adequada
rect.hclust(dendo, k = 4, border = "red")
rect.hclust(dendo, k = 5, border = "purple")
rect.hclust(dendo, k = 6, border = "pink") 
rect.hclust(dendo, k = 7, border = "green") 
rect.hclust(dendo, k = 8, border = "turquoise")

### Método Silhouette para decidir o total de clusters (método para complementar o método de Elbow)
fviz_nbclust(x = df1_escala, FUNcluster = kmeans, method = 'silhouette')
#A partir desse método, teríamos 5 clusters

########### CLUSTERIZAÇÃO COM 6 CLUSTERS ###########
# Criar os clusteres
resultado_cluster <- kmeans(df1_escala, centers = 6)
lista_clusteres <- kmeans(df1_escala, centers = 6)$cluster

#Visualizando os clusteres
df_covid_cluster = cbind(dados_simples_1_ano, cluster = lista_clusteres)
#write_xlsx(data.frame(df_covid_cluster),"DATA_VALUES_SIMPLE_6.xlsx")


# função customizada para calcular a média dos indicadores para cada cluster
cluster.summary <- function(data, groups) {
  x <- round(aggregate(data, list(groups), mean), 2)
  x$qtd <- as.numeric(table(groups))
  # colocar coluna de quantidade na segunda posição
  x <- x[, c(1,21,2,14,15,16,11,12,17)]
  return(x)
}

(tabela <- cluster.summary(df_covid_cluster, lista_clusteres))

# Critérios de cor de acordo com vulnerabilidade
colorir.indicador.bom <- function(x) ifelse(x >= mean(x), style(color = "green"), style(color = "red"))
colorir.indicador.ruim <- function(x) ifelse(x <= mean(x), style(color = "green"), style(color = "red"))

nome_colunas <-  c("Cluster", "Quantidade de municípios do cluster", "Letalidade por COVID-19/100k hab.",
                   "IDH Municipal – Dimensão Educação","IDH Municipal – Dimensão Longevidade",
                   "IDH Municipal – Dimensão Renda","Núm. de leitos/100k hab.", 
                   "População com água canalizada (%)", "Núm. de beneficiários do Programa Bolsa Família/100k hab.")

formattable(
  tabela,
  list(
    MORTES_P_CASOS = formatter("span", style = x ~ colorir.indicador.ruim(x)),
    IDH_EDU = formatter("span", style = x ~ colorir.indicador.bom(x)),
    IDH_LONG = formatter("span", style = x ~ colorir.indicador.bom(x)),
    IDH_RENDA = formatter("span", style = x ~ colorir.indicador.bom(x)),
    LEITOS_100MIL = formatter("span", style = x ~ colorir.indicador.bom(x)),
    PERC_AGUACANAL = formatter("span", style = x ~ colorir.indicador.bom(x)),
    BENEFICIOS_PBF_100MIL = formatter("span", style = x ~ colorir.indicador.ruim(x))    
  ),  col.names = nome_colunas, format = "markdown", pad = 0
)

########### CLUSTERIZAÇÃO COM 5 CLUSTERS ###########
# Criar os clusteres
resultado_cluster_1 <- kmeans(df1_escala, centers = 5)
lista_clusteres_1 <- kmeans(df1_escala, centers = 5)$cluster

#Visualizando os clusteres
df_covid_cluster = cbind(dados_simples_1_ano, cluster = lista_clusteres_1)
#write_xlsx(data.frame(df_covid_cluster),"DATA_VALUES_SIMPLE_5.xlsx")


# função customizada para calcular a média dos indicadores para cada cluster
cluster.summary <- function(data, groups) {
  x <- round(aggregate(data, list(groups), mean), 2)
  x$qtd <- as.numeric(table(groups))
  # colocar coluna de quantidade na segunda posição
  x <- x[, c(1,21,2,14,15,16,11,12,17)]
  return(x)
}

(tabela <- cluster.summary(df_covid_cluster, lista_clusteres_1))

# Critérios de cor de acordo com vulnerabilidade
colorir.indicador.bom <- function(x) ifelse(x >= mean(x), style(color = "green"), style(color = "red"))
colorir.indicador.ruim <- function(x) ifelse(x <= mean(x), style(color = "green"), style(color = "red"))

nome_colunas <-  c("Cluster", "Quantidade de municípios do cluster", "Letalidade por COVID-19/100k hab.",
                   "IDH Municipal – Dimensão Educação","IDH Municipal – Dimensão Longevidade",
                   "IDH Municipal – Dimensão Renda","Núm. de leitos/100k hab.", 
                   "População com água canalizada (%)", "Núm. de beneficiários do Programa Bolsa Família/100k hab.")

formattable(
  tabela,
  list(
    MORTES_P_CASOS = formatter("span", style = x ~ colorir.indicador.ruim(x)),
    IDH_EDU = formatter("span", style = x ~ colorir.indicador.bom(x)),
    IDH_LONG = formatter("span", style = x ~ colorir.indicador.bom(x)),
    IDH_RENDA = formatter("span", style = x ~ colorir.indicador.bom(x)),
    LEITOS_100MIL = formatter("span", style = x ~ colorir.indicador.bom(x)),
    PERC_AGUACANAL = formatter("span", style = x ~ colorir.indicador.bom(x)),
    BENEFICIOS_PBF_100MIL = formatter("span", style = x ~ colorir.indicador.ruim(x))    
  ),  col.names = nome_colunas, format = "markdown", pad = 0
)

########### CLUSTERIZAÇÃO COM 4 CLUSTERS ###########
# Criar os clusteres
resultado_cluster_2 <- kmeans(df1_escala, centers = 4)
lista_clusteres_2 <- kmeans(df1_escala, centers = 4)$cluster

#Visualizando os clusteres
df_covid_cluster = cbind(dados_simples_1_ano, cluster = lista_clusteres_2)
#write_xlsx(data.frame(df_covid_cluster),"DATA_VALUES_SIMPLE_4.xlsx")


# função customizada para calcular a média dos indicadores para cada cluster
cluster.summary <- function(data, groups) {
  x <- round(aggregate(data, list(groups), mean), 2)
  x$qtd <- as.numeric(table(groups))
  # colocar coluna de quantidade na segunda posição
  x <- x[, c(1,21,2,14,15,16,11,12,17)]
  return(x)
}

(tabela <- cluster.summary(df_covid_cluster, lista_clusteres_2))

# Critérios de cor de acordo com vulnerabilidade
colorir.indicador.bom <- function(x) ifelse(x >= mean(x), style(color = "green"), style(color = "red"))
colorir.indicador.ruim <- function(x) ifelse(x <= mean(x), style(color = "green"), style(color = "red"))

nome_colunas <-  c("Cluster", "Quantidade de municípios do cluster", "Letalidade por COVID-19/100k hab.",
                   "IDH Municipal – Dimensão Educação","IDH Municipal – Dimensão Longevidade",
                   "IDH Municipal – Dimensão Renda","Núm. de leitos/100k hab.", 
                   "População com água canalizada (%)", "Núm. de beneficiários do Programa Bolsa Família/100k hab.")

formattable(
  tabela,
  list(
    MORTES_P_CASOS = formatter("span", style = x ~ colorir.indicador.ruim(x)),
    IDH_EDU = formatter("span", style = x ~ colorir.indicador.bom(x)),
    IDH_LONG = formatter("span", style = x ~ colorir.indicador.bom(x)),
    IDH_RENDA = formatter("span", style = x ~ colorir.indicador.bom(x)),
    LEITOS_100MIL = formatter("span", style = x ~ colorir.indicador.bom(x)),
    PERC_AGUACANAL = formatter("span", style = x ~ colorir.indicador.bom(x)),
    BENEFICIOS_PBF_100MIL = formatter("span", style = x ~ colorir.indicador.ruim(x))    
  ),  col.names = nome_colunas, format = "markdown", pad = 0
)

########### CLUSTERIZAÇÃO COM 3 CLUSTERS ###########
# Criar os clusteres
resultado_cluster_3 <- kmeans(df1_escala, centers = 3)
lista_clusteres_3 <- kmeans(df1_escala, centers = 3)$cluster

#Visualizando os clusteres
df_covid_cluster = cbind(dados_simples_1_ano, cluster = lista_clusteres_3)
#write_xlsx(data.frame(df_covid_cluster),"DATA_VALUES_SIMPLE_TESTE.xlsx")


# função customizada para calcular a média dos indicadores para cada cluster
cluster.summary <- function(data, groups) {
  x <- round(aggregate(data, list(groups), mean), 2)
  x$qtd <- as.numeric(table(groups))
  # colocar coluna de quantidade na segunda posição
  x <- x[, c(1,21,2,14,15,16,11,12,17)]
  return(x)
}

(tabela <- cluster.summary(df_covid_cluster, lista_clusteres_3))

# Critérios de cor de acordo com vulnerabilidade
colorir.indicador.bom <- function(x) ifelse(x >= mean(x), style(color = "green"), style(color = "red"))
colorir.indicador.ruim <- function(x) ifelse(x <= mean(x), style(color = "green"), style(color = "red"))

nome_colunas <-  c("Cluster", "Quantidade de municípios do cluster", "Letalidade por COVID-19/100k hab.",
                   "IDH Municipal – Dimensão Educação","IDH Municipal – Dimensão Longevidade",
                   "IDH Municipal – Dimensão Renda","Núm. de leitos/100k hab.", 
                   "População com água canalizada (%)", "Núm. de beneficiários do Programa Bolsa Família/100k hab.")

formattable(
  tabela,
  list(
    MORTES_P_CASOS = formatter("span", style = x ~ colorir.indicador.ruim(x)),
    IDH_EDU = formatter("span", style = x ~ colorir.indicador.bom(x)),
    IDH_LONG = formatter("span", style = x ~ colorir.indicador.bom(x)),
    IDH_RENDA = formatter("span", style = x ~ colorir.indicador.bom(x)),
    LEITOS_100MIL = formatter("span", style = x ~ colorir.indicador.bom(x)),
    PERC_AGUACANAL = formatter("span", style = x ~ colorir.indicador.bom(x)),
    BENEFICIOS_PBF_100MIL = formatter("span", style = x ~ colorir.indicador.ruim(x))    
  ),  col.names = nome_colunas, format = "markdown", pad = 0
)


#################################### VISUALIZAÇÃO GEOGRÁFICA ###########################################
# Importando a planilha de resultados obtida do write_xlsx
resultado_3c <- read_excel("Result_Simple_3C.xlsx")
resultado_4c <- read_excel("Result_Simple_4C.xlsx")
resultado_5c <- read_excel("Result_Simple_5C.xlsx")
resultado_6c <- read_excel("Result_Simple_6C.xlsx")


df_cluster_cod_municipios_0 = resultado_6c %>%
                                  select(COD7,MUNICÍPIO, cluster)
  
df_cluster_cod_municipios_1 = resultado_5c %>%
                                   select(COD7,MUNICÍPIO, cluster)

df_cluster_cod_municipios_2 = resultado_4c %>%
                                    select(COD7,MUNICÍPIO, cluster)

df_cluster_cod_municipios_3 = resultado_3c %>%
                                    select(COD7,MUNICÍPIO, cluster)


df_municipios = read_municipality(code_muni = 'RJ',
                                  year = 2019)

df_municipios_0 = left_join(df_municipios, df_cluster_cod_municipios_0, by = c('code_muni' = 'COD7'))

df_municipios_1 = left_join(df_municipios, df_cluster_cod_municipios_1, by = c('code_muni' = 'COD7'))

df_municipios_2 = left_join(df_municipios, df_cluster_cod_municipios_2, by = c('code_muni' = 'COD7'))

df_municipios_3 = left_join(df_municipios, df_cluster_cod_municipios_3, by = c('code_muni' = 'COD7'))

no_axis = theme(axis.title=element_blank(),
                axis.text=element_blank(),
               axis.ticks=element_blank())

# Visualização da Clusterização com 3 clusters
ggplot() +
  geom_sf(data = df_municipios_3, aes(fill = as.factor(cluster)), color = as.factor('cluster')) +
  scale_fill_manual(name = 'Cluster',
                    values = c('1' = '#984ea3',
                               '2' = '#4daf4a',
                               '3' = 'firebrick 3')) +
  theme_minimal() +
  no_axis

# Visualização da Clusterização com 4 clusteres

ggplot() +
  geom_sf(data = df_municipios_2, aes(fill = as.factor(cluster)), color = as.factor('cluster')) +
  scale_fill_manual(name = 'Cluster',
                    values = c('1' = 'firebrick 3',
                               '2' = 'turquoise',
                               '3' = '#984ea3',
                               '4' = 'gold')) +
  theme_minimal() +
  no_axis

# Visualização da Clusterização com 5 clusteres

ggplot() +
  geom_sf(data = df_municipios_1, aes(fill = as.factor(cluster)), color = as.factor('cluster')) +
  scale_fill_manual(name = 'Cluster',
                    values = c('1' = 'gold',
                               '2' = '#377eb8',
                               '3' = '#984ea3',
                               '4' = '#4daf4a',
                               '5' = 'firebrick 3')) +
  theme_minimal() +
  no_axis

# Visualização da Clusterização com 6 clusteres

ggplot() +
  geom_sf(data = df_municipios_0, aes(fill = as.factor(cluster)), color = as.factor('cluster')) +
  scale_fill_manual(name = 'Cluster',
                    values = c('1' = '#984ea3',
                               '2' = 'firebrick 3',
                               '3' = 'gold',
                               '4' = '#377eb8',
                               '5' = '#4daf4a',
                               '6' = 'turquoise')) +
  theme_minimal() +
  no_axis

#################################### VISUALIZAÇÃO DE DADOS #############################################
# Visualização da Clusterização com 3 clusters

resultado_3c <- resultado_3c %>%
  rename('Mortes de COVID-19 por casos' = MORTES_P_CASOS,
         'PIB per capita' = PIB_P_CAP,
         'Índice GINI' = GINI,
         'Percentual da população em área urbana' = PERC_URB,
         'Percentual da população do sexo masculino' = PERC_MASC,
         'Percentual da população de cor/raça branca' = PERC_BRANCOS,
         'Percentual da população com 60 anos ou mais' = PERC60MAIS,
         'Percentual da população com nível superior completo' = PERC_POP_EDUC_SUP,
         'Médicos por 100 mil habitantes' = MEDICOS_100MIL,
         'Leitos por 100 mil habitantes' = LEITOS_100MIL,
         'Percentual de domicílios com água canalizada' = PERC_AGUACANAL,
         'Percentual de domicílios com instalações sanitárias' = PERC_SANITARIOS,
         'IDH - Dimensão Educação' = IDH_EDU,
         'IDH - Dimensão Longevidade' = IDH_LONG,
         'ÍDH – Dimensão Renda' = IDH_RENDA,
         'Número de benefícios de Bolsa Família por 100 mil habitantes' = BENEFICIOS_PBF_100MIL,
         'Trabalhadores dispensados por 100 mil habitantes' = TRAB_DISPENSADOS_100MIL,
         'Homicídios por 100 mil habitantes' = HOMICIDIOS_100MIL) %>%
  select(-COD7,-MUNICÍPIO)


melted_resultado_3c <- melt(resultado_3c, id.vars = c('cluster'))
ggplot(melted_resultado_3c, 
       aes(x = cluster, 
           y = value,
           fill = factor(cluster))) + 
  scale_fill_manual(name = 'Cluster',
                    values = c('1' = 'light green',
                               '2' = 'coral',
                               '3' = 'light blue')) +
  facet_wrap(~variable, scales = 'free') +
  labs(y = 'valor') +
  geom_boxplot()


# Visualização da Clusterização com 4 clusters

resultado_4c <- resultado_4c %>%
  rename('Mortes COVID-19/casos' = MORTES_P_CASOS,
         'PIB per capita' = PIB_P_CAP,
         'Índice GINI' = GINI,
         '% pop. urbana' = PERC_URB,
         '% pop. sexo masculino' = PERC_MASC,
         '% pop. raça branca' = PERC_BRANCOS,
         '% pop. 60+ anos' = PERC60MAIS,
         '% pop. nível superior completo' = PERC_POP_EDUC_SUP,
         'Médicos/100k.hab.' = MEDICOS_100MIL,
         'Leitos/100K hab.' = LEITOS_100MIL,
         '% domicílios com água canalizada' = PERC_AGUACANAL,
         '% domicílios com instalações sanitárias' = PERC_SANITARIOS,
         'IDH - Educação' = IDH_EDU,
         'IDH - Longevidade' = IDH_LONG,
         'IDH – Renda' = IDH_RENDA,
         'Núm. PBF/100k. hab.' = BENEFICIOS_PBF_100MIL,
         'Trabalhadores dispensados/100k.hab.' = TRAB_DISPENSADOS_100MIL,
         'Homicídios/100k.hab.' = HOMICIDIOS_100MIL) %>%
  select(-COD7,-MUNICÍPIO)


melted_resultado_4c <- melt(resultado_4c, id.vars = c('cluster'))
ggplot(melted_resultado_4c, 
                  aes(x = cluster, 
                  y = value,
                  fill = factor(cluster))) + 
    scale_fill_manual(name = 'Cluster',
                    values = c('1' = 'firebrick 3',
                               '2' = 'turquoise',
                               '3' = '#984ea3',
                               '4' = 'gold')) +

  facet_wrap(~variable,ncol = 4, nrow = 5, scales = 'free') +
  labs(y = 'valor') +
  geom_boxplot()

