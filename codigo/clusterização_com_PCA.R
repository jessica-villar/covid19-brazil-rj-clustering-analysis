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

dados_pca_1_ano <- na.omit(df_covid_1_ano)

df1_escala <- scale(dados_pca_1_ano)

#################################### ANÁLISE DE COMPONENTES PRINCIPAIS #############################################

#Existem dois tipos de PCA: a PCA de covariância e a PCA de correlação. 
#Quando as variáveis foram medidas em unidades diferentes 
#ou quando a variância de cada variável é muito diferente umas das outras 
#devemos usar a PCA de correlação, que é construida usando
#os dados que foram padronizados para ter média 0 e desvio padrão 1.

# Calculando o desvio padrão e variância das variáveis
sapply(dados_pca_1_ano, sd)

round(apply(dados_pca_1_ano,2,var),2) #Veja que a variância de cada variável é muito diferente.
#Portanto, temos que usar uma PCA de correlação para que a variável com maior variância não “domine” a análise.


#Cálculo do PCA com a matriz de covariâncias
# Obtendo as componentes: A função prcomp permite obter as componentes de forma simples.

pca_cov_1_ano = prcomp(dados_pca_1_ano)
summary(pca_cov_1_ano) 

# Como as variáveis estão em diferentes escalas, o mais adequado é utilizar a matriz de correlação.
# PCA com a matriz de correlação (variáveis padronizadas):
#Para construir a PCA de correlação usando prcomp devemos usar o argumento scale=TRUE.

pca_corr_1_ano <- prcomp(dados_pca_1_ano, scale = TRUE)
pca_corr_1_ano
summary(pca_corr_1_ano)

fviz_eig(pca_corr_1_ano) 
#Interpretação (REFERÊNCIA - https://operdata.com.br/blog/analise-de-componentes-principais-pca-calculo-e-aplicacao-no-r/):
#A 1ª componente explica em torno de 27% da variância total dos dados.
#A 2ª componente explica 16% da variância total
#A 3ª componente explica 11,5% da variância total
#A 4ª componente explica 8% da variância total
#A 5ª componente explica 6% da variância total
#Portanto, quase 2/3 das informações contidas nas 18 variáveis do banco de dados podem ser encapsulada nessas 5 componentes. 
#A 6ª componente explica 5,1% da variância total. 
#A 7ª componente explica 4,8% da variância total.
#A 8ª componente explica em torno de 4% da variância total.
#A 9ª componente em torno de 3,9% da variância total.
#A 10ª componente explica em torno de 3% da variância total.
soma = 27+16+11.5+8+6+5.1+4.8+4+3.9+3
soma #Assim, com 10 componentes, 89,30% da variância dos dados é explicada.

# Coeficientes das componentes principais (autovetores da matriz de correlação)
summary(pca_corr_1_ano)$rotation #Pode-se trocar os sinais
#No caso em que todos os coeficientes apresentarem o mesmo sinal (positivo OU negativo) a componente é chamada de índice global. 
#Quando os sinais são opostos a componente é chamada de índice comparativo.

# Quanto mais próxima uma variável for do círculo de correlações, melhor sua representação no mapa fatorial (e mais importante é a variável para a interpretação desses componentes)
# As variáveis próximas ao centro do gráfico são menos importantes para os primeiros componentes.
# No gráfico abaixo os componentes são coloridas de acordo com os valores do coseno quadrado:

# Cos2: é uma medida que indica a qualidade da representação para variáveis no mapa fatorial
fviz_pca_var(pca_corr_1_ano, col.var="cos2") +
  scale_color_gradient2(low="white", mid="blue", 
                        high="red", midpoint=0.5) + theme_minimal()

# Gráfico (mapa fatorial) ajustado por contribuição

fviz_pca_var(pca_corr_1_ano,
             col.var = "contrib", # Cor por contribuições para o PC
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE,     
             legend.title = "Contribuição"
)


#A PCA requer que seja determinado previamente o número de componentes que serão retidos. 
#Nós podemos usar a função fa.parallel para isto.
fa.parallel (dados_pca_1_ano, fa="pc", show.legend=T,
             main="Scree plot with parallel analysis")
# A análise realizada acima sugere 3 componentes (eigen values > 1) 
## critério de "corte" = autovalores (eigenvalue) acima de 1.

View(pca_corr_1_ano$rotation[,1:10])

(tabela_pca <- abs(pca_corr_1_ano$rotation[,1:10]))

#write.csv2(tabela_pca,"data_values_PCA.csv")


#Rodando o PCA pelo pacote psych
# A função PRINCIPAL executa uma PCA de correlação como análise padrão.
pca_psy_1_ano <- principal(dados_pca_1_ano, nfactors=5,
                           rotate='none', scores=TRUE)
str(pca_psy_1_ano)

#PCA	fornece	mapeamento	de	um	espaço	com	N	dimensões	(N	–	nº	variáveis	originais)	para	um	espaço	com	M	dimensões	(onde	M	<	N)	

# para acessar os eingenvalues (autovalores)
pca_psy_1_ano$values

view()

# As	novas	dimensões	são	combinações	lineares	das	variáveis	originais,	sendo	os	coeficientes	destas	no	espaço	original	designado	por	loadings	(P) 
# para visualisar a contribuição de cada variável (peso)
pca_psy_1_ano$loadings

# As	coordenadas	das	observações	nas	novas	variáveis	são	chamada	de	scores (T)
# Para acessar os escores da PCA
SCORES = pca_psy_1_ano$scores

# definimos os loadings como objeto
load <- pca_psy_1_ano$loadings

#As variáveis que são correlacionadas com PC1 e PC2 são as mais importantes para explicar a variabilidade no conjunto de dados. 
#Variáveis que não se correlacionam com nenhum PC ou correlacionadas com as últimas dimensões são variáveis com baixa contribuição e podem ser removidas para simplificar a análise geral.

#####De modo a facilitar o entendimento de qual variável contínua contribui com maior peso em cada PC:
##### Construção gráfica para os loadings 
sorted.loadings1 <- load[order(load[,1]),1] # definimos o objeto que descreve o peso das variáveis para o PC1
Main = "Loadings Plot for PC1"; xlabs = "Variable Loadings" # criamos objetos que descrevem o título e o eixo x da figura
load1 <- dotplot(sorted.loadings1, main=Main,
                 xlab=xlabs, cex=1.5, col="red", pch=16)
# para plotar um gráfico simples de pontos
load1
#### Quais as variáveis que mais contribuem no PC2?
sorted.loadings2 <- load[order(load[,2]),2]
Main="Loadings Plot for PC2"; xlabs="Variable Loadings"
load2<-dotplot(sorted.loadings2, main=Main,
               xlab=xlabs, cex=1.5, col="blue", pch=1)
load2

#################################### CLUSTERIZAÇÃO COM PCA#############################################
bestRKM = tuneclus(dados_pca_1_ano, 3:7, 2:3, method = "RKM", criterion = "asw",
                   dst = "low")
bestRKM # The best solution was obtained for 5 clusters of sizes 27 (32.5%), 27 (32.5%), 17 (20.5%), 10 (12%), 2 (2.4%) in 2 dimensions, for an average Silhouette width value of 0.41. Variables were mean centered and standardized.

comp <- data.frame(pca_corr_1_ano$x[,1:5])


#A determinação da quantidade de clusteres não segue uma regra pré-definida e deve ser pensada pelo responsável pela análise.
#Cada projeto de clusterização tem suas próprias particularidades.
#Contudo, alguns métodos analíticos podem ajudar nessa escolha, seja pela minização da soma dos quadrados dos clusteres ou pelo auxílio visual de um dendograma.

#Para determinar o número de clusteres pela minimização da soma dos quadrados dos clusteres (método do cotovelo), observe o gráfico abaixo:
# referencia: http://www.statmethods.net/advstats/cluster.html
# https://medium.com/pizzadedados/kmeans-e-metodo-do-cotovelo-94ded9fdf3a9


#Fixar uma seed para garantir a reproducibilidade da análise:
set.seed(123) # fixando os valores, para que o resultado gerado seja sempre o mesmo

wss <- (nrow(comp)-1)*sum(apply(comp,2,var))
for (i in 2:20) wss[i] <- sum(kmeans(comp,
                                     centers=i)$withinss)
plot(1:20, wss, type="b", xlab="Número de Clusters",
     ylab="Soma dos quadrados dentro dos clusters")

#Ponto que indica o equilíbrio entre maior homogeneidade dentro do cluster e a maior diferença entre clusters:
#É o ponto da curva mais distante de uma reta traçada entre os pontos a0 e a19: 5 clusteres

# Pelo método do dendograma:
#A posição de cada município no dendograma é determinada pela dissimilaridade entre cada um dos outros municípios.

dendo <- comp %>% dist %>% hclust
plot(dendo)
rect.hclust(dendo, k = 3, border = "blue") 
rect.hclust(dendo, k = 4, border = "red") # é a divisão que pareceu mais adequada
rect.hclust(dendo, k = 5, border = "purple")
rect.hclust(dendo, k = 6, border = "pink") 
rect.hclust(dendo, k = 7, border = "green") 
rect.hclust(dendo, k = 8, border = "turquoise")

### Método Silhouette para decidir o total de clusters (método para complementar o método de Elbow)
fviz_nbclust(x = comp, FUNcluster = kmeans, method = 'silhouette')
#A partir desse método, teríamos 10 clusters

# Criar os clusteres
resultado_cluster_pca <- kmeans(comp, centers = 4)
lista_clusteres_pca <- kmeans(comp, centers = 4)$cluster

#Visualizando os clusteres
df_covid_cluster_pca = cbind(comp, cluster = lista_clusteres_pca)
write_xlsx(data.frame(df_covid_cluster_pca),"Dados_Completos_Valores_PCA_2.xlsx")

# função customizada para calcular a média dos indicadores para cada cluster
cluster.summary <- function(data, groups) {
  x <- round(aggregate(data, list(groups), mean), 2)
  x$qtd <- as.numeric(table(groups))
  # colocar coluna de quantidade na segunda posição
  x <- x[, c(1,8,2,3,4,5,6,7)]
  return(abs(x))
}

(tabela <- cluster.summary(comp, lista_clusteres_pca))

# Critérios de cor de acordo com vulnerabilidade
colorir.indicador <- function(x) ifelse(abs(x) >= mean(abs(x)), style(color = "green"), style(color = "red"))

nome_colunas <-  c("Cluster", "Quantidade de municípios do cluster", "Componente Principal 1",
                   "Componente Principal 2","Componente Principal 3",
                   "Componente Principal 4","Componente Principal 5","Componente Principal 6")

formattable(
  tabela,
  list(
    PC1 = formatter("span", style = x ~ colorir.indicador(x)),
    PC2  = formatter("span", style = x ~ colorir.indicador(x)),
    PC3 = formatter("span", style = x ~ colorir.indicador(x)),
    PC4 = formatter("span", style = x ~ colorir.indicador(x)),
    PC5 = formatter("span", style = x ~ colorir.indicador(x)),
    PC6 = formatter("span", style = x ~ colorir.indicador(x))
  ),  col.names = nome_colunas, format = "markdown", pad = 0
)

#################################### VISUALIZAÇÃO GEOGRÁFICA ###########################################
# Importando a planilha de resultados obtida do write_xlsx
resultado <- read_excel("TCC/Resultado_Clusterização_PCA_4C.xlsx")

df_cluster_cod_municipios = resultado %>%
  select(COD7,MUNICÍPIO, cluster)


df_municipios = read_municipality(code_muni = 'RJ',
                                  year = 2019)

df_municipios_PCA = left_join(df_municipios, df_cluster_cod_municipios, by = c('code_muni' = 'COD7'))

no_axis = theme(axis.title=element_blank(),
                axis.text=element_blank(),
                axis.ticks=element_blank())


# Visualização da Clusterização com 4 clusteres após o PCA

ggplot() +
  geom_sf(data = df_municipios_PCA, aes(fill = as.factor(cluster)), color = as.factor('cluster')) +
  scale_fill_manual(name = 'Cluster',
                    values = c('1' = 'coral',
                               '2' = 'light green',
                               '3' = 'gold',
                               '4' = 'light blue')) +
  theme_minimal() +
  no_axis

