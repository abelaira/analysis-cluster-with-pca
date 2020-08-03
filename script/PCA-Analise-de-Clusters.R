
# ===============================================================================================================
# Análise de Clusters com PCA
# Abril 2020
#
# Diretrizes:
#
# Será usado o R para minerar os dados de interesse. Será seguido o seguinte roteiro na tarefa:
#
#   1. Entendimento do problema: identificar e definir o problema que está sendo abordado.
# Como uma solução de mineração de dados resolverá este problema?
#
#   2. Entendimento dos dados: descrever os dados (e fontes de dados) que darão suporte à mineração de
# dados para solucionar o problema. Fazendo alguns plots com descrições estatísticas. 
# Analisar as correlações entre variáveis e suas distribuições. Essa será a oportunidade para levantar 
# pontos de atenção, como dados faltantes, categorias mal-definidas, desbalanceamento entre categorias e 
# variáveis altamente correlacionadas. 
# As hipóteses e conclusões serão tiradas olhando essas descrições estatísticas.
#
#   3. Preparação dos dados: especificar quais pré-processamentos são necessários para a análise. 
# Incluindo codificações de dados categóricos, normalizações e redução de dimensionalidade. 
# Comparar o dataset antes do pré-processamento e depois.
#
#   4. Modelagem: escolher dois algoritmos de clusterização a serem utilizados. 
# Identificar quais parâmetros podem ser variados nesses algoritmos afim de se obter o melhor resultado.
#
#   5. Avaliação: descubrir o melhor número de clusters e descrever interpretações para eles. 
# Avaliando a diferença de interpretação entre os algoritmos.
#
#   6. Relatório: documentar através de textos e gráficos a análise.
# -------------------------------------------------------------------------------------------------------------


rm(list = ls())

getwd()

# =====================================================================================
# Carregando os pacotes necessários
# -------------------------------------------------------------------------------------
list.packages <- c('data.table',
                   "readxl",
                   'foreign',
                   'stringr',
                   'dplyr',
                   'mice',
                   'ggplot2',
                   'GGally',
                   "factoextra",
                   'gridExtra',
                   "cluster",
                   "clValid")
new.packages <- list.packages[!(list.packages %in% installed.packages()[, "Package"])]
if (length(new.packages)) install.packages(new.packages, dependencies = TRUE)
for (package in list.packages) {
  require(package, character.only = TRUE)
}
# =====================================================================================


# Importando os dados de COVID19 por cidade
covid <- fread('./data/covid19.csv')

# possui um conjunto de dados por cidade
unique(covid$date)

# possui somente dois tipos de locais: cidade e estado
unique(covid$place_type)

# possui 2.382 códigos de cidade sendo uma linha com NA
length(unique(covid$city_ibge_code))
unique(covid$city_ibge_code) %>% anyNA

# possui 20 elementos do tipo cidade com NA
nrow(covid[is_last == TRUE & is.na(city_ibge_code),])
unique(covid[is_last == TRUE & is.na(city_ibge_code),]$place_type)

# Obtendo somente os dados por estado
covid_uf <- covid[is_last == TRUE & place_type == 'state',]
unique(covid_uf$date)
str(covid_uf)
covid_uf

# Obtendo somente os dados por cidade
covid_cidade <- covid[is_last == TRUE & place_type == 'city',]

# a difetença entre o length e o nrow se dá porque existem 20 elementos com NA no city_ibge_code 
# mas esses elementos são de UF diferentes, portanto estão em linhas diferentes.
length(unique(covid_cidade$city_ibge_code))
nrow(covid_cidade)

# Retirando as linhas com atributo city_ibge_code com NA
covid_cidade <- covid_cidade[!is.na(city_ibge_code),]
length(unique(covid_cidade$city_ibge_code))
nrow(covid_cidade)
unique(covid_cidade$date)
str(covid_cidade)


# Importando os dados de cadastro dos Municípios
cidade_df <- read_xls('./data/CODIGO_MUNICIPIO.xls')

# Selecioando os atributos de interesse
cidade <- data.table()
cidade[, ':='(uf = as.integer(cidade_df$UF),
              nome_uf = cidade_df$Nome_UF,
              codigo_cidade = as.integer(cidade_df$Município),
              codigo_ibge_completo = as.integer(cidade_df$`Código Município Completo`),
              nome_cidade = cidade_df$Nome_Município,
              codigo_ibge_parcial = as.integer(str_extract(cidade_df$`Código Município Completo`, '^\\d{6}')))]
str(cidade)

# Importando os dados de leitos em hostpital para o SUS por cidade
hospital <- fread('./data/RecursosFisicosHospitalar.csv')

# Selecionando os atributos de interesse
codigo_ibge <- str_extract(hospital$Município, "^\\d{6}")
hospital[,':='(codigo_ibge = as.integer(codigo_ibge),
               total_leitos = as.integer(Total))]
hospital <- hospital[,c('codigo_ibge', 'total_leitos')]
hospital <- hospital[!is.na(codigo_ibge),]
str(hospital)

# Importando os dados de renda por cidade
renda_df <- read.dbf('./data/RENDABR10.dbf', as.is = TRUE)
str(renda_df)

# Tratando o atributo COR/RAÇA
cor_raca <- case_when(renda_df$CORRACA == 1 ~ 'Branco',
                      renda_df$CORRACA == 2 ~ 'Negro',
                      renda_df$CORRACA == 3 ~ 'Amarelo',
                      renda_df$CORRACA == 4 ~ 'Pardo',
                      renda_df$CORRACA == 5 ~ 'Indígena',
                      renda_df$CORRACA == 0 ~ 'Sem declaração')

# Selecionando os atributos de interesse
renda <- data.table()
renda[, ':='(codigo_ibge = as.integer(renda_df$MUNCOD),
             cor_raca = factor(cor_raca, 
                               levels = c('Branco', 'Negro', 'Amarelo', 'Pardo', 'Indígena', 'Sem declaração')),
             renda_domiciliar = renda_df$NUMRENDA,
             populacao = renda_df$DENRENDA,
             num_pobre = renda_df$NUMPOBRES,
             num_pobre_extremo = renda_df$NUMPOBRESX,
             num_desocupado = renda_df$NUMDESOCUP,
             total_pop_economico = renda_df$DENDESOCUP,
             num_trab_infantil = renda_df$NUMTRABINF,
             total_pop_infantil = renda_df$DENTRABINF)]
str(renda)

# Consolidando os dados em um único Data Set
cidade_hospital <- merge(cidade, hospital, by.x = 'codigo_ibge_parcial', 
                         by.y = 'codigo_ibge', all = TRUE)
str(cidade_hospital)

cidade_hospital_renda <- merge(cidade_hospital, renda, 
                               by.x = 'codigo_ibge_parcial', by.y = 'codigo_ibge', all = TRUE)
str(cidade_hospital_renda)

cidade_hospital_renda <- cidade_hospital_renda[!is.na(codigo_ibge_completo),]

covid_cidade_ibge <- merge(cidade_hospital_renda, covid_cidade, 
                           by.x = 'codigo_ibge_completo', by.y = 'city_ibge_code', all = TRUE)
covid_cidade_ibge <- covid_cidade_ibge[,-c('codigo_ibge_parcial','codigo_ibge_completo','date',
                                           'place_type','is_last','state','city')]
setnames(covid_cidade_ibge, 
         c('confirmed','deaths','estimated_population_2019','confirmed_per_100k_inhabitants','death_rate'),
         c('num_caso_confirmado','num_mortes','pop_estimada_2019','caso_confirmado_100k_habitantes','taxa_mortalidade'))

str(covid_cidade_ibge)

# Descrição Dataset
# -----------------
# Feature =========================== Feature Description
# uf................................: Código unidade federativa
# nome_uf...........................: Nome da unidade federativa
# codigo_cidade.....................: Código IBGE da cidade
# nome_cidade.......................: Nome da cidade
# total_leitos......................: Total de leitos disponíveis pelo SUS
# cor_raca..........................: Cor/Raça
# renda_domiciliar..................: Somatório da renda média domiciliar per capita.
# populacao.........................: População considerada censo IBGE 2010
# num_pobre.........................: População com renda média domiciliar per capita menor que 1/2 salário mínimo.
# num_pobre_extermo.................: População com renda média domiciliar per capita menor que 1/4 salário mínimo.
# num_desocupado....................: População residente economicamente ativa de 16 anos e mais que se encontra sem trabalho.
# total_pop_economico...............: População residente economicamente ativa de 16 anos e mais.
# num_trab_infantil.................: População residente com 10 a 15 anos de idade que se encontra trabalhando ou procurando trabalho.
# total_pop_infantil................: População total residente com 10 a 15 anos de idade.
# num_caso_confirmado...............: Número de casos confirmados.
# num_mortes........................: Número de mortes.
# pop_estimada_2019.................: População estimada para esse município/estado em 2019.
# caso_confirmado_100k_habitantes...: Número de casos confirmados por 100.000 habitantes.
# taxa_mortalidade..................: Taxa de mortalidade (mortes / confirmados).


# Verificando e existência de NA's
anyNA(covid_cidade_ibge)

par(mai = c(0.1,0.01,0.01,0.01))
na_covid <- md.pattern(covid_cidade_ibge, rotate.names = TRUE)
# Nota-se que só existem 9.027 observações completas, ou seja, sem nenhum atributo com NA.

# estruturando um data frame com o resumo dos atributos com NA
na_covid_resumo <- data.frame(atributo = names(na_covid[9,1:19]),
                              perc_na = na_covid[9,1:19]/nrow(covid_cidade_ibge) * 100)
rownames(na_covid_resumo) <- NULL
na_covid_resumo
na_covid[9,1:19]
# O atributo num_mortes possui 14.222 observações com NA. Isso corresponde a cerca de 55,98% das 
# observações com num_morte com NA.

na_covid_resumo <- na_covid_resumo %>% mutate(linha = 1:19, 
                                              tamanho = as.integer(if_else(perc_na>0,10+perc_na/10,0)))
na_covid_resumo


# Gráfico com o percentual de observação com NA's
na_covid_resumo %>% ggplot(aes(x = as.factor(atributo), y = linha)) +
  geom_point(aes(size = perc_na, stroke = tamanho, color = as.factor(atributo), fill = as.factor(atributo)),
             shape = 19, show.legend = FALSE) +
  geom_text(aes(label = paste(round(perc_na,2),"%")), size = 3, fontface = 'bold.italic') +
  scale_y_continuous(breaks = NULL) +
  labs(x = "Atributos", y = "", title = "Percentual de Observação com NA por Atributo") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
# Os atributos caso_confirmado_100k_habitantes (Número de casos confirmados por 100k habitantes), 
# num_caso_confirmado (Número de casos confirmados), num_mortes (Número de mortes), 
# pop_estimada_2019 (População estimada para localidade em 2019) e taxa_mortalidade (Taxa de mortalidade)
# apresentam alto índice de NA's, possuindo mais da metade das observações com dados faltantes.
#
# O atributo total_leitos (Total de leitos disponíveis pelo SUS) apresenta um percentual de NA's 
# ligeiramente mais baixo, ficando em torno de 30%.
#
# Os demais atributos encontram-se com os dados praticamente completos.
#
# Como há o interesse de avaliar como o Número de mortes se relaciona com os demais atributo,
# o fato do Data Set possuir cerca de 50% dos dados para esse atributo faltantes, isso prejudica
# essa análise.
# Portanto, optamos por retirar todas as observações de num_morte que contenha NA.
# Restando um Data Set com 11.183 observações. Prosseguiremos a análise com esse Data Set reduzido, 
# mas sem observações com NA no atributo num_mortes.
# Esse novo Data Set corresponde a 44,02% (100-55,98) do Data Set original.
nrow(na.omit(covid_cidade_ibge))
covid_tratado <- covid_cidade_ibge[!is.na(num_mortes),]
nrow(covid_cidade_ibge)
nrow(covid_tratado)

# Gráfico com a correlação 
ggcorr(covid_tratado[,-c('nome_uf','nome_cidade','cor_raca')],
       geom = 'tile',
       low = "#F21A00",
       high = "#3B9AB2",
       label = TRUE,
       label_size = 4,
       hjust = 0.9,
       label_color = 'gray80')
# O gráfico mostra uma alta correlação entre os atributos renda_domiciliar, populacao, num_pobre,
# num_pobre_extremo, num_desocupado, total_pop_economico, num_trab_infantil e total_pop_infantil.
#
# Também temos uma alta correlação entre num_mortes, num_caso_confirmado, pop_estimada_2019 e
# total_leitos.
#
# Não se nota uma correlação relevante, está em torno de 60%, entre num_mortes e os atributos 
# que possam caracterizar pobreza como os atributos num_pobre, num_pobre_extremo, num_desocupado e
# num_trab_infantil.

# Eliminando o atributo cor_raca e separando as observações entre população branca e demais, 
# construímos um novo Data Set em que o atributo cor_raca é eliminado, restando agora somente
# população branca e não branca.
covid_tratado[,eh_branco := if_else(covid_tratado$cor_raca == 'Branco',TRUE,FALSE)] 
covid_tratado
covid_tratado2 <- covid_tratado[,.(nome_cidade = nome_cidade,
                                   uf = uf,
                                   nome_uf = nome_uf,
                                   total_leitos = total_leitos,
                                   num_caso_confirmado = num_caso_confirmado,
                                   num_mortes = num_mortes,
                                   pop_estimada_2019 = pop_estimada_2019,
                                   caso_confirmado_100k_habitantes = caso_confirmado_100k_habitantes,
                                   taxa_mortalidade = taxa_mortalidade,
                                   renda_domiciliar = sum(renda_domiciliar),
                                   populacao = sum(populacao),
                                   num_pobre = sum(num_pobre),
                                   num_pobre_extremo = sum(num_pobre_extremo),
                                   num_desocupado = sum(num_desocupado),
                                   total_pop_economico = sum(total_pop_economico),
                                   num_trab_infantil = sum(num_trab_infantil),
                                   total_pop_infantil = sum(total_pop_infantil)),
                                by = .(codigo_cidade,eh_branco)] %>% unique
covid_tratado2


# Gráfico com a correlação 
ggcorr(covid_tratado2[,-c('nome_uf','nome_cidade','eh_branco')],
       geom = 'tile',
       low = "#F21A00",
       high = "#3B9AB2",
       label = TRUE,
       label_size = 4,
       hjust = 0.9,
       label_color = 'gray80')
# Com o novo Data Set já percebemos que houve um aumento entre a correlação entre o atributo num_mortes e
# os atributos num_pobre, num_pobre_extremo, num_desocupado e num_trab_infantil.
#
# O tratamento aplicado ao Data Set teve como base o fato que a pobreza no país possui cor, ou seja, 
# a maioria da população pobre é não branca. Ao eliminarmos o atributo cor_raca, agregarmos os atributos 
# que caracterizam a probleza nesse Data Set (num_pobre, num_pobre_extremo, num_desocupado e num_trab_infantil)
# em dois tipos branco e não branco.
#
# Conforme esperado, notamos que o atributo num_mortes está altamente correlacionado com os atributos
# que caracterizam a pobreza nesse Data Set.
# Percebemos que essa doença, faz mais vítimas entre a população mais pobre do país.


summary(covid_tratado2)
# Após a eliminação das observações em que o atributo num_mortes apresentavam NA's, notamos que ainda
# restam bastante observação com NA's no atributo total_leitos e caso_confirmado.

# Distribuição do atributo num_porbre_extremo
covid_tratado2 %>% ggplot(aes(x = eh_branco, y = num_pobre_extremo)) +
  geom_boxplot(aes(fill = as.factor(eh_branco)), outlier.alpha = 0, color = 'gray60', show.legend = FALSE) +
  stat_boxplot(geom = 'errorbar', aes(colour = as.factor(eh_branco)), show.legend = FALSE) +
  geom_jitter(width = 0.3, height = 0.1, color = 'gray60', alpha = 0.3) +
  scale_y_log10(breaks = c(10,1e02,1e03,1e04,1e05,1e06), 
                label = c('10','100','1.000','10.000','100.000','1.000.000')) +
  scale_x_discrete(na.translate = FALSE, breaks = c(FALSE,TRUE), label = c('Não Branco','Branco')) +
  labs(x = 'Cor/Raça da População', y = 'Qtd. Pessoas na Pobreza Extrema')
# Esse gráfico retrata a diferença na quantidade de possoas que estão na probreza extrema entre Brancos e 
# Não Brancos. Nota-se que existem muito mais Não Brancos na pobreza extrema que Branco nas cidades 
# brasileiras.
#
# Com base nessa evidência, pegamos um subconjunto do Data Set em que o foco é nas pessoas Não Brancas.
# Verificamos o comportamento dos atributos num_mortes e num_pobre_extremo associados por cidade.

# Gráfico com o comportamento do atributo num_mortes e num_pobre_extremo por cidade
covid_tratado2[num_mortes >= 10 & eh_branco == FALSE,] %>% 
  ggplot(aes(x = nome_cidade, y = num_pobre_extremo)) +
  geom_col(fill = 'skyblue3') +
  geom_line(aes(x = as.integer(as.factor(nome_cidade)), y = num_mortes*1e03), 
            colour = '#FF0000', size = 0.7) +
  geom_point(aes(y = num_mortes*1e03), colour = '#FF0000', shape = 20) +
  scale_y_continuous(limits = c(0,17e05),
                     breaks = c(0,1e05,2e05,3e05,4e05,5e05,6e05,7e05,8e05,9e05,
                                10e05,11e05,12e05,13e05,14e05,15e05,16e05,17e05),
                     labels = c('0','100.000','200.000','300.000','400.000','500.000','600.000',
                                '700.000','800.000','900.000','1.000.000','1.100.000','1.200.000',
                                '1.300.000','1.400.000','1.500.000','1.600.000','1.700.000'),
                     sec.axis = sec_axis(~ ./1e03,
                                         breaks = c(0,1e02,2e02,3e02,4e02,5e02,6e02,7e02,8e02,9e02,
                                                    10e02,11e02,12e02,13e02,14e02,15e02,16e02,17e02),
                                         labels = c('0','100','200','300','400','500','600',
                                                    '700','800','900','1.000','1.100','1.200',
                                                    '1.300','1.400','1.500','1.600','1.700'),
                                         name = 'Número de Mortes')) +
  labs(x = 'Cidade', y = 'Qtd. Pessoas na Pobreza Extrema (NÃO Brancos)', 
       title = 'Cidades com mais de 10 mortes pelo COVID',
       caption = 'Cor azul representa probreza extrema e vermelha o número de mortes') +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
# O gráfico confirma visualmente uma relação entre num_pobre_extrema e num_mortes apresentada pela matriz
# de correlação, ou seja, em cidades onde o número de pessoas na pobreza extrema é mais alto apresenta um 
# número de mortes pelo COVID mais elevado. No gráfico podemos ver as barras em azul que representam o
# número de pessoas na situação de pobreza extrema com escala no eixo vertical à esquerda e a linha vermelha
# que representa o número de mortes pelo COVID com escala no eixo vertical à direita.


# Na tentativa de identificar padrões ocultos, remover o ruído e redundância no Data Set, iniciamos 
# o procedimento para uma Análise por Componentes Principais (PCA - Principal Component Analysis).
ds_covid <- covid_tratado2 %>% na.omit
pca <- ds_covid[,-c('nome_cidade','nome_uf')] %>% prcomp(scale = TRUE)

summary(pca)
get_eigenvalue(pca)

# Gráfico Scree
fviz_eig(pca, addlabels = TRUE, choice = 'variance',
         barfill = 'skyblue3', barcolor = 'skyblue3', linecolor = 'red', 
         ncp = nrow(pca$rotation), xlab = "Dimensões", ylab = "Porcentagem da Variância Explicada",
         ggtheme = theme_gray(), yticks.by = 5)
# De acordo com o gráfico em função da variância acumulada, vemos que o número de dimensões ideal está
# entre 5 e 7, pois nesse intervalo temos uma variância explicada entre 87,83% e 95,87%.
# Com 5 dimensões explicamos 87,83% da variância, com 6 explicamos 92,23% e 7 explicamos 95,87%. 

# Gráfico Scree
fviz_eig(pca, addlabels = TRUE, choice = 'eigenvalue',
         barfill = 'skyblue3', barcolor = 'skyblue3', linecolor = 'red', 
         ncp = nrow(pca$rotation), xlab = "Dimensões", ylab = "Autovalor",
         ggtheme = theme_gray(), yticks.by = 1)
# Olhando para o gŕafico em função dos autovalores e seguindo o critério da raiz latente ou Kaiser, que 
# descarta dimensões com Autovalor menor que 1, vemos que o número ideal de dimensões estaria entre
# 4 e 5. Apesar da dimensão 5 ser menor que 1, seu Autovalor está muito próximo de 1 (0,9179), 
# assim foi considerado também como ponto de corte.
#
# O critério da raiz latente ou critério de Kaiser estabelece que o número de dimensões deve ser 
# aquela em que o número de autovalores seja maior ou igual à média das variâncias das variáveis analisadas.
# As variáveis estando padronizadas, todas possuem variância igual a 1, portanto o critério descarta
# dimensões que estejam com Autovalor menor que 1. 
# Esse critério faz sentido se pensarmos que Autovalor menor que 1 não explica nem ao menos a variância 
# de uma variável isolada. Portanto, não deve fazer parte do conjunto.
#
# Analisando os dois critérios, optamos pelo corte do PCA em 5 dimensões e, também testaremos um corte
# com 6 dimensões para comparação.

# contribuição das variáveis nas 5 primeiras cargas
g1 <- fviz_contrib(pca, choice = 'var', axes = 1:5,
                   fill = 'skyblue3', color = 'skyblue3', ggtheme = theme_gray(),
                   yticks.by = 1)

# contribuição das variáveis nas 6 primeiras cargas
g2 <- fviz_contrib(pca, choice = 'var', axes = 1:6,
                   fill = 'skyblue3', color = 'skyblue3', ggtheme = theme_gray(),
                   yticks.by = 1)

grid.arrange(g1,
             g2,
             nrow = 2,
             ncol = 1)
# Percebemos uma mudança na contribuição das primeiras variáveis quando cortamos em 5 dimensões e em
# 6. A variável uf passou de última em 5 dimensões para ser a segunda com 6 dimensões. Assim como
# codigo_cidade que tornou-se mais relevante com 6 dimensões. Isso mostra uma contribuição da localização
# mais acentuada quando passamos a ter 6 dimensões.
pca_5dim <- pca$x[,1:5]
pca_6dim <- pca$x[,1:6]

# -----------
# Agrupmaento
# -----------

# ======================================
# Utilizando o Data Set com 5 Dimensoes.
# --------------------------------------
# Padronizando o Data Set
pca_5dim_std <- scale(pca_5dim)

# Calculando as medidas de dissimilaridade
dist_5dim <- dist(pca_5dim_std, method = "euclidean")

# Agrupamento Hierárquico
# -----------------------
set.seed(314)
hier_5dim_comp <- hclust(dist_5dim, method = 'complete')

hier_height <- data.frame(stage = 1:length(hier_5dim_comp$height),
                          height = hier_5dim_comp$height,
                          cum_height = cumsum(hier_5dim_comp$height))

# Gráfico da distância de combinação dos agregados no agrupamento hierárquico
hier_height %>% ggplot(aes(x = stage, y = height)) +
  geom_line(color = 'blue3', size = 0.7) +
  geom_point(color = 'salmon3', shape = 20) +
  geom_hline(yintercept = 7, color = 'red', size = 0.1) +
  geom_hline(yintercept = 11, color = 'red', size = 0.1) +
  scale_y_continuous(breaks = c(0,2,4,6,8,10,12,14,16,18,20,22,24,26,28,30,32,34,36,38,40),
                     labels = c('0','2','4','6','8','10','12','14','16','18','20',
                                '22','24','26','28','30','32','34','36','38','40')) +
  scale_x_continuous(breaks = c(0,500,1000,1500,2000,2500,3000,3500),
                     labels = c('0','500','1.000','1.500','2.000','2.500','3.000','3.500')) +
  labs(x = 'Estágio do agrupamento', y = 'Distância de combinação dos agregados', 
       title = 'Agrupamento Hierárquico')
# O gráfico mostra que entre as distâncias de 7 e 11 temos uma maior separação entre os pontos que
# representam os estágios de agrupamento, indicando que o estágio desse agrupamento foi feito entre
# agregados que tinham pouca similaridade. Assim, se tivéssemos que escolher um ponto para podar 
# a árvore hierárquica do agrupamento, faríamos entre esses pontos.

cutpoint <- hier_height %>% filter(height >= 7 & height <= 11)
grupos <- t(unique(cutree(hier_5dim_comp, h = cutpoint[,"height"])))
apply(grupos, c(1), max)
# Percebemos que entre as distâncias de combinação de agregados no intervalo de 7 a 11, geram
# 14 a 7 grupos no agrupamento hierárquico. Portanto, iremos avaliar o k-Means nesse intervalo 
# de número de grupos.


# Agrupamento k-Means
# -------------------
# Gráfico apontando a escolha da quantidade de grupos
set.seed(314)
fviz_nbclust(pca_5dim_std, kmeans, method = 'silhouette', k.max = 14, 
             diss = dist_5dim) +
  labs(x = 'Número de Grupos', y = 'Silueta Média', title = 'Número Ótimo de Grupos')
# Analisando o gráfico com a simulação do k-Means para o número de grupos de 2 até 14, 
# verficamos que o número de grupos em que a silueta atinge o melhor valor é quando temos 
# 7 grupos. Este valor está dentro do primeiro filtro realizado com o agrupamento hierárquico.
#
# A métrica silueta avaliar o quanto um grupo está coeso, quanto maior o seu valor melhor
# a qualidade do agrupamento.
set.seed(314)
kmeans_5dim <- kmeans(pca_5dim_std, 7, nstart = 100, iter.max = 100)

# Gráfico mostrando a distribuição dos grupos nas duas primeiras dimensões
fviz_cluster(kmeans_5dim, pca_5dim_std, ellipse.alpha = 0.4,
             show.clust.cent = TRUE, ggtheme = theme_gray())
# O gráfico mostra a disponsição dos grupos reduzida a duas dimensões somente. Percebemos 4 grupos 
# bem coesos, formando uma estrutura contínua e 2 grupos mais dispersos com estrutura descontínua. 
# Verficamos também a existência de um grupo que parece abrigar os outliers.

# Grafico da silueta
sil_5dim <- silhouette(kmeans_5dim$cluster, dist_5dim)
fviz_silhouette(sil_5dim)
# O gráfico da silueta para o agrupamento obtido mostra que 2 grupos não apresentam valores
# negativos, mas 4 grupos apresentam, sendo que um mostra um conjunto razoável de volaores negativos 
# que demonstrando que esses grupos possuem observações que não estão bem ajustada ao grupo. 
# Talvez essas observações devessem ser classificadas em outro grupo. 
# Percebemos também a existência de um grupo que parece abrigar os outliers. 
#
# Para tentar minimizar os valores negativos da métrica de Silueta, faremos um agrupamento utilizando
# 13 grupos que foi apontado pela gráfico de "Número Ótimo de Grupos" como sendo o segundo melhor
# Classificado.
set.seed(314)
kmeans_5dim2 <- kmeans(pca_5dim_std, 13, nstart = 100, iter.max = 100)

# Gráfico mostrando a distribuição dos grupos nas duas primeiras dimensões
fviz_cluster(kmeans_5dim2, pca_5dim_std, ellipse.alpha = 0.4,
             show.clust.cent = TRUE, ggtheme = theme_gray())
# O gráfico mostra a disponsição dos grupos reduzida a duas dimensões somente. Parece haver mais outliers
# e percebemos os grupos mais descontínuos.

# Grafico da silueta
sil_5dim2 <- silhouette(kmeans_5dim2$cluster, dist_5dim)
fviz_silhouette(sil_5dim2)
# Dos 13 grupos, 5 grupos apresentam uma quantidade considerável de valores negativos.
# 
# Utilizando a métrica de silueta, nota-se que o agrupamento com 7 grupos gerou um resultado melhor
# tanto no valor da métrica que foi de 0,38, quanto na estrutura dos grupos, pois somente 2 dos 7
# grupos apresentaram uma quantidade considerável de observaçoes com valores negativos.

# ======================================
# Utilizando o Data Set com 6 Dimensoes.
# --------------------------------------
# Padronizando o Data Set
pca_6dim_std <- scale(pca_6dim)

# Calculando as medidas de dissimilaridade
dist_6dim <- dist(pca_6dim_std, method = "euclidean")

# Agrupamento Hierárquico
# -----------------------
set.seed(314)
hier_6dim_comp <- hclust(dist_6dim, method = 'complete')

hier_height <- data.frame(stage = 1:length(hier_6dim_comp$height),
                          height = hier_6dim_comp$height,
                          cum_height = cumsum(hier_6dim_comp$height))

# Gráfico da distância de combinação dos agregados no agrupamento hierárquico
hier_height %>% ggplot(aes(x = stage, y = height)) +
  geom_line(color = 'blue3', size = 0.7) +
  geom_point(color = 'salmon3', shape = 20) +
  geom_hline(yintercept = 6, color = 'red', size = 0.1) +
  geom_hline(yintercept = 9, color = 'red', size = 0.1) +
  scale_y_continuous(breaks = c(0,2,4,6,8,10,12,14,16,18,20,22,24,26,28,30,32,34,36,38,40),
                     labels = c('0','2','4','6','8','10','12','14','16','18','20',
                                '22','24','26','28','30','32','34','36','38','40')) +
  scale_x_continuous(breaks = c(0,500,1000,1500,2000,2500,3000,3500),
                     labels = c('0','500','1.000','1.500','2.000','2.500','3.000','3.500')) +
  labs(x = 'Estágio do agrupamento', y = 'Distância de combinação dos agregados', 
       title = 'Agrupamento Hierárquico')
# O gráfico mostra que entre as distâncias de 6 e 8 temos uma maior separação entre os pontos que
# representam os estágios de agrupamento, indicando que o estágio desse agrupamento foi feito entre
# agregados que tinham pouca similaridade. Assim, se tivéssemos que escolher um ponto para podar 
# a árvore hierárquica do agrupamento, faríamos entre esses pontos.

cutpoint <- hier_height %>% filter(height >= 6 & height <= 9)
grupos <- t(unique(cutree(hier_6dim_comp, h = cutpoint[,"height"])))
apply(grupos, c(1), max)
# Percebemos que entre as distâncias de combinação de agregados no intervalo de 6 a 9, geram
# 17 a 12 grupos no agrupamento hierárquico. Portanto, iremos avaliar o k-Means nesse intervalo 
# de número de grupos.

# Agrupamento k-Means
# -------------------
# Gráfico apontando a escolha da quantidade de grupos
set.seed(314)
fviz_nbclust(pca_6dim_std, kmeans, method = 'silhouette', k.max = 17, 
             diss = dist_6dim) +
  labs(x = 'Número de Grupos', y = 'Silueta Média', title = 'Número Ótimo de Grupos')
# Analisando o gráfico com a simulação do k-Means para o número de grupos de 2 até 17, 
# verficamos que o número de grupos em que a silueta atinge o melhor valor é quando temos 
# 9 grupos. Este valor não está dentro do primeiro filtro realizado com o agrupamento hierárquico.

set.seed(314)
kmeans_6dim <- kmeans(pca_6dim_std, 9, nstart = 100, iter.max = 100)

# Gráfico mostrando a distribuição dos grupos nas duas primeiras dimensões
fviz_cluster(kmeans_6dim, pca_6dim_std, ellipse.alpha = 0.4,
             show.clust.cent = TRUE, ggtheme = theme_gray())
# O gráfico mostra a disponsição dos grupos reduzida a duas dimensões somente. Percebemos somente
# 2 grupos bem coesos, formando uma estrutura contínua, os demais grupos parecem mais dispersos 
# com estrutura descontínua. 
# Verficamos também a existência de um grupo que parece abrigar os outliers.

# Grafico da silueta
sil_6dim <- silhouette(kmeans_6dim$cluster, dist_6dim)
fviz_silhouette(sil_6dim)
# O gráfico da silueta para o agrupamento obtido mostra que alguns grupos não apresentam valores
# negativos ou apresentam poucos conjuntos negativos. Mas 2 grupos apresentam um conjunto razoável 
# de valores negativos, demonstrando que esses grupos possuem observações que não estão bem ajustada 
# ao grupo. 
# Talvez essas observações devessem ser classificadas em outro grupo. 
#
# Para tentar minimizar os valores negativos da métrica de Silueta, faremos um agrupamento utilizando
# 15 grupos que foi apontado pela gráfico de "Número Ótimo de Grupos" como sendo o segundo melhor
# Classificado. Esse valor está dentro do filtro realizado com o grupamento hierárquico.
set.seed(314)
kmeans_6dim2 <- kmeans(pca_6dim_std, 15, nstart = 100, iter.max = 100)

# Gráfico mostrando a distribuição dos grupos nas duas primeiras dimensões
fviz_cluster(kmeans_6dim2, pca_6dim_std, ellipse.alpha = 0.4,
             show.clust.cent = TRUE, ggtheme = theme_gray())
# O gráfico mostra a disponsição dos grupos reduzida a duas dimensões somente. percebemos que muitos grupos 
# continuam a apresentar descontínuidade.

# Grafico da silueta
sil_6dim2 <- silhouette(kmeans_6dim2$cluster, dist_6dim)
fviz_silhouette(sil_6dim2)
# Dos 15 grupos, somente 4 grupos apresentam uma quantidade considerável de valores negativos.
# 
# Utilizando a métrica de silueta, nota-se que o agrupamento com 9 grupos gerou um resultado melhor
# tanto no valor da métrica que foi de 0,34, quanto na estrutura dos grupos, pois somente 2 dos 9
# grupos apresentaram uma quantidade considerável de observaçoes com valores negativos.

# Podando o agrupamento hierárquico com 5 dimensões para conter somente 7 grupos
hier_5dim.cluster <- cutree(hier_5dim_comp, k = 7)

# Pdando o agrupamento hierárquico com 6 dimensões para conter somente 9 grupos
hier_6dim.cluster <- cutree(hier_6dim_comp, k = 9)


# Criando o Data Set final com os agrupamentos
ds_final <- ds_covid
ds_final[, ':='(kmeans_5dim = kmeans_5dim$cluster,
                kmeans_6dim = kmeans_6dim$cluster,
                hier_5dim = hier_5dim.cluster,
                hier_6dim = hier_6dim.cluster)]
ds_final[, perc_pobre_extremo := num_pobre_extremo / sum(num_pobre_extremo), by = nome_cidade]
ds_final

# Gráfico que relaciona pobreza extrema com número de mortes pelo COVID
# ds_final[num_mortes>0,] %>% ggplot(aes(x = num_mortes, y = num_pobre_extremo)) +
#   geom_jitter(aes(color = as.factor(kmeans_5dim)), width = 0.1) +
#   geom_vline(xintercept = 50, color = 'turquoise3') +
#   scale_color_brewer(palette = 'Set1') +
#   scale_x_log10(breaks = c(1,10,50,100,1000),
#                 labels = c('1','10','50','100','1.000')) +
#   scale_y_continuous(breaks = c(0, 0.5e5, 1e5, 1.5e5, 2e5, 2.5e5, 3e5, 3.5e5, 4e5, 4.5e5, 5e5, 5.5e5),
#                      labels = c('0', '50', '100', '150', '200', '250', '300', '350',
#                                 '400', '450', '500', '550')) +
#   labs(x = 'Número de Mortes', y = 'Qtd. Pessoas Pobreza Extrema (em Mil)', color = 'Grupos')
ds_final[num_mortes>0 & num_mortes<=5,] %>% ggplot(aes(x = num_mortes, y = num_pobre_extremo)) +
  geom_jitter(aes(color = as.factor(kmeans_5dim)), width = 0.1) +
  geom_vline(xintercept = 50, color = 'turquoise3') +
  geom_jitter(data = ds_final[num_mortes>50,], 
              aes(x = num_mortes, y = num_pobre_extremo, color = as.factor(kmeans_5dim)), width = 0.1) +
  scale_color_brewer(palette = 'Set1') +
  scale_x_log10(breaks = c(1,10,50,100,1000),
                labels = c('1','10','50','100','1.000')) +
  scale_y_log10(breaks = c(1e1,1e2,1e3,1e4,1e5),
                labels = c('10','100','1.000','10.000','100.000')) +
  labs(x = 'Número de Mortes', y = 'Qtd. Pessoas Pobreza Extrema', color = 'Grupos')
# O gráfico mostra que a relação entre pobreza exterma e número de mortes pelo COVID é crescente,
# ou seja, quanto maior a pobreza maior o número de mortos. Outra informação que o gráfico traz é
# que são poucas cidades que possuem um número de mortes maior que 50 e que o número de mortes
# está relacionado com a pobreza extrema.


# Gráfico mostrando a situação para as cidades com número de mortos maior que 50 mortes
limite_inferior <- 50
ds_final[num_mortes >= limite_inferior & eh_branco == FALSE,] %>% 
  ggplot(aes(x = as.factor(nome_cidade), y = num_mortes)) +
  geom_col(aes(fill = as.factor(kmeans_5dim))) +
  geom_col(data = ds_final[num_mortes >= limite_inferior,],
               aes(x = as.factor(nome_cidade), y = perc_pobre_extremo*1e3, color = as.factor(eh_branco)), 
           width = 0, size = 2, alpha = 0, position = position_dodge(width = 0.7)) +
  scale_y_continuous(breaks = c(0, 200, 400, 600, 800, 1000, 1200, 1400, 1600, 1800),
                     labels = c('0', '200', '400', '600', '800', 
                                '1.000', '1.200', '1.400', '1.600', '1.800'),
                     sec.axis = sec_axis(~ ./10,
                                         breaks = c(0,10,20,30,40,50,60,70,80,90,100),
                                         labels = c('0','10','20','30','40','50','60',
                                                    '70','80','90','100'),
                                         name = 'Percentual Pobreza Extrema (%)')) +
  scale_color_manual(values = c('khaki4','gray80'), breaks = c(FALSE,TRUE), 
                    labels = c('Não Branco','Branco')) +
  scale_fill_viridis_d() +
  labs(x = 'Cidade', y = 'Número de Mortes', color = 'Cor/Raça', fill = 'Grupos',
       title = 'Cidades com Número de Mortes > 50') +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
# O gráfico mostra que as cidades com número de mortes maior que 50 possuem um percentual de 
# pobreza de pessoas não brancas maior que de pessoas brancas. Com exceção de São Paulo que
# aponta para um equilíbrio.
#
# Percebemos também que a relação entre o atributo num_mortes e demais atributos que expressam
# a pobreza não foram determinantes no agrupamento. Apesar dessa relação ser importante e essas
# variáveis estarem bem correlacionadas, essa relação por si só não responde pelo agrupamento obtido.
# Esse fato nos leva a supor que existam outras variáveis também são muito determinantes para fazer
# a separação dos grupos.

