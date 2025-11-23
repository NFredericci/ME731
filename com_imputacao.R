# Pacotes

library(tidyverse)
library(readxl)
library(psych)
library(missMDA)
library(factoextra)
library(cluster)

# Leitura, seleC'C#o e manipulaC'C#o dos dados

giffen <- read_excel("caminho", sheet = "vGdata")

data <- giffen %>% select(-c(`Van Giffen ID_1`, `Van Giffen ID_2`, BAI_ID, 
                            RMO_ID, Other_ID, `Alternate spelling`, `Object location`,
                            Comments, `Year collected`, Longitude, Latitude, `Plis sourc`, 
                            `Erster Hypoc`, `SS stud`, PM4, `Start Range`, `End Range`, 
                            ba, `A-S (S')`))

moda_start <- names(sort(table(data$`Start Period`), decreasing = TRUE)[1])
data$`Start Period`[is.na(data$`Start Period`)] <- moda_start

moda_end <- names(sort(table(data$`End Period`), decreasing = TRUE)[1])
data$`End Period`[is.na(data$`End Period`)] <- moda_end

data$`Start Period` <- data$`Start Period` %>%
  fct_collapse(
    "Iron Age" = c(
      "Early Iron Age",
      "Middle Iron Age",
      "Late Iron Age",
      "Iron Age" 
    ),
    "Roman Age" = c(
      "Roman Age",
      "Middle Roman Age"
    ),
    "Middle Ages" = c(
      "Middle Ages", 
      "Early Middle Ages B",
      "Early Middle Ages D"
    ),
    other_level = NULL )

ordem_final_start <- c(
  "Iron Age", 
  "Roman Age", 
  "Middle Ages")

data$`Start Period` <- data$`Start Period` %>%
  fct_relevel(ordem_final_start)

niveis_para_agrupar_medios <- c(
  "Early Middle Ages D",
  "Middle Ages",
  "Late Middle Ages B",
  "Late Middle Ages A",
  "Late Middle Ages")

data$`End Period`<- data$`End Period` %>%
  fct_collapse(
    "Middle Ages" = niveis_para_agrupar_medios,
    other_level = NULL )

ordem_final_end <- c(
  "Roman Age",
  "Middle Ages",
  "Modern Era")

data$`End Period` <- data$`End Period` %>%
  fct_relevel(ordem_final_end)

data %>% group_by(`Start Period`) %>% summarise(n = n ())
data %>% group_by(`End Period`) %>% summarise(n = n ())

data <- data %>% select(-c(`End Period`))

# EDA

data_medias <- data %>% select(-c(ID, `Site location`, `Skeletal part`)) %>%  group_by(`Start Period`) %>% summarise_all(mean, na.rm = TRUE)

data_longo <- data_medias %>%
  pivot_longer(
    cols = -`Start Period`,   
    names_to = "Variavel",
    values_to = "Media") %>%
  filter(!is.na(Media))

data_longo %>%
  ggplot(aes(x = `Start Period`, y = Media, group = 1)) +
  geom_line(color = "#0072B2") + 
  geom_point(color = "#D55E00", size = 3) +
  facet_wrap(~ Variavel, scales = "free_y") +
  labs(
    title = "Media das Medidas Morfologicas ao Longo do Periodo",
    x = "Periodo",
    y = "Media"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

## Tentativa de agrupar por local (a visualizaC'C#o ficou ruim)

data %>% group_by(`Site location`) %>% summarise(q = n())

data_local <- data %>% select(-c(ID, `Skeletal part`)) %>%  group_by(`Site location`, `Start Period`) %>% summarise_all(mean, na.rm = TRUE)

data_longo2 <- data_local %>%
  pivot_longer(
    cols = -c(`Site location`, `Start Period`),   
    names_to = "Variavel",
    values_to = "Media") %>%
  filter(!is.na(Media)) 

data_longo2 %>%
  ggplot(aes(x = `Start Period`, y = Media, color = `Site location`)) +
  geom_line() + 
  geom_point(size = 3) +
  facet_wrap(~ Variavel, scales = "free_y") +
  labs(
    title = "Media das Medidas Morfologicas ao Longo do Periodo",
    x = "Periodo",
    y = "Media"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Analise com Data Imputing nas medidas
# Esta analise foi feita em paralelo com a analise sem Data Imputing, apenas retirando os NAs encontrados. 
# Ao final das analises, os resultados obtidos pouco se diferenciavam; logo, fiquei com a analise sem Data Imputing no resultado final. 
# Assim, nao ha os possiveis erros das informacoes adicionadas pela tecnica; apenas as dificuldades da diminuicao de dados.

medidas <- data %>% select(-c(ID, `Site location`, `Start Period`, `Skeletal part`, `End Period`))

length(checkNA(medidas)) 

nb <- estim_ncpPCA(medidas)
nb$ncp 
matriz_imputada <- imputePCA(medidas, ncp = nb$ncp)$completeObs

## PCA

pca_resultado <- prcomp(matriz_imputada, scale. = TRUE)
summary(pca_resultado)
escores_pca <- pca_resultado$x

## AF

round(cor(matriz_imputada), 2) 

factanal(x = matriz_imputada, factors = 2)

plot(fa(matriz_imputada, nfactors = 2))

### adicionando os perC-odos na analise

data_analise <- data.frame(matriz_imputada)
data_analise$periodo <- data2$`Start Period`
data_analise$scores <- escores_pca

modelo_pc1 <- aov(scores[,"PC1"] ~ periodo, data = data_analise)
modelo_pc2 <- aov(scores[,"PC2"] ~ periodo, data = data_analise)
modelo_pc3 <- aov(scores[,"PC3"] ~ periodo, data = data_analise)
modelo_pc4 <- aov(scores[,"PC4"] ~ periodo, data = data_analise)
modelo_pc5 <- aov(scores[,"PC5"] ~ periodo, data = data_analise)
modelo_pc6 <- aov(scores[,"PC6"] ~ periodo, data = data_analise)
modelo_pc7 <- aov(scores[,"PC7"] ~ periodo, data = data_analise)
modelo_pc8 <- aov(scores[,"PC8"] ~ periodo, data = data_analise)
modelo_pc9 <- aov(scores[,"PC9"] ~ periodo, data = data_analise)
modelo_pc10 <- aov(scores[,"PC10"] ~ periodo, data = data_analise)
modelo_pc11 <- aov(scores[,"PC11"] ~ periodo, data = data_analise)
modelo_pc12 <- aov(scores[,"PC12"] ~ periodo, data = data_analise)
modelo_pc13 <- aov(scores[,"PC13"] ~ periodo, data = data_analise)
modelo_pc14 <- aov(scores[,"PC14"] ~ periodo, data = data_analise)
modelo_pc15 <- aov(scores[,"PC15"] ~ periodo, data = data_analise)
modelo_pc16 <- aov(scores[,"PC16"] ~ periodo, data = data_analise)
modelo_pc17 <- aov(scores[,"PC17"] ~ periodo, data = data_analise)
modelo_pc18 <- aov(scores[,"PC18"] ~ periodo, data = data_analise)
modelo_pc19 <- aov(scores[,"PC19"] ~ periodo, data = data_analise)
modelo_pc20 <- aov(scores[,"PC20"] ~ periodo, data = data_analise)

lista_modelos <- list(
  modelo_pc1, modelo_pc2, modelo_pc3, modelo_pc4, modelo_pc5,
  modelo_pc6, modelo_pc7, modelo_pc8, modelo_pc9, modelo_pc10,
  modelo_pc11, modelo_pc12, modelo_pc13, modelo_pc14, modelo_pc15,
  modelo_pc16, modelo_pc17, modelo_pc18, modelo_pc19, modelo_pc20)

nomes_pcs <- paste0("PC", 1:20)
names(lista_modelos) <- nomes_pcs

resultados_df <- data.frame(
  PC = nomes_pcs,
  P_valor = NA,
  F_estatistico = NA)

for (i in 1:length(lista_modelos)) {
  resumo_anova <- summary(lista_modelos[[i]])
  
  p_val <- resumo_anova[[1]]$'Pr(>F)'[1]
  
  f_stat <- resumo_anova[[1]]$'F value'[1]
  
  resultados_df[i, "P_valor"] <- p_val
  resultados_df[i, "F_estatistico"] <- f_stat
}

resultados_significativos <- resultados_df %>%
  filter(P_valor <= 0.05) %>%
  arrange(P_valor) 

resultados_significativos

Periodo_vetor <- as.factor(data_analise$periodo) 

model <- factanal(x = matriz_imputada, factors = 2, scores = "regression")

plot(model$scores[,1], model$scores[,2], 
     col = Periodo_vetor, 
     pch = 19,
     xlab = "Fator 1 (Tamanho)", 
     ylab = "Fator 7 (Forma/Proporcao Residual)")

arrows(0, 0, temp[,1], temp[,2], length = 0.1, col = "red")

legend("topright", 
       legend = levels(Periodo_vetor), 
       col = 1:length(levels(Periodo_vetor)), 
       pch = 19)

### quais pares diferem?

TukeyHSD(modelo_pc1)


# ClusterizaC'C#o com Data Imputing

df_imputado <- as_tibble(matriz_imputada)
dados_cluster_scaled <- scale(df_imputado)

fviz_nbclust(dados_cluster_scaled, kmeans, method = "wss") 

K_escolhido <- 2 
set.seed(204355) 
modelo_kmeans <- kmeans(dados_cluster_scaled, centers = K_escolhido, nstart = 25)

fviz_cluster(modelo_kmeans, data = dados_cluster_scaled,
             ellipse.type = "convex",
             palette = "jco",
             ggtheme = theme_minimal(),
             main = paste("K-means Clustering (K =", K_escolhido, ")"))

df_imputado$Cluster_Kmeans <- modelo_kmeans$cluster

medias_kmeans <- df_imputado %>%
  group_by(Cluster_Kmeans) %>%
  summarise(across(everything(), mean, na.rm = TRUE)) #

medias_kmeans

d <- dist(dados_cluster_scaled, method = "euclidean")

modelo_hclust <- hclust(d, method = "ward.D2")

plot(modelo_hclust, main = "Dendrograma (Clustering Hierarquico)",
     xlab = "Amostras", ylab = "Distancia")

grupos_hclust <- cutree(modelo_hclust, k = 3)

fviz_cluster(list(data = dados_cluster_scaled, cluster = grupos_hclust),
             ellipse.type = "convex",
             palette = "jco",
             ggtheme = theme_minimal(),
             main = "HCLUST (K=3) Clustering")

df_imputado$Cluster_Hclust <- grupos_hclust

medias_hclust <- df_imputado %>%
  group_by(Cluster_Hclust) %>%
  summarise(across(everything(), mean, na.rm = TRUE))

medias_hclust

## comparaC'C#o das medidas entre os clusters

medias_hclust <- medias_hclust %>% select(-Cluster_Kmeans)

medias_clusters <- data.frame(medias_hclust) 

medias_clusters$Cluster_Hclust <- as.factor(medias_clusters$Cluster_Hclust)

medias_long <- medias_clusters %>%
  pivot_longer(
    cols = -Cluster_Hclust,             
    names_to = "Medida",         
    values_to = "Valor_Medio")

ggplot(
  medias_long, 
  aes(x = Medida, y = Valor_Medio, group = Cluster_Hclust, color = Cluster_Hclust)) + 
  geom_line(size = 1.2) + geom_point(size = 3) + 
  labs(
    title = "Grafico de Perfil dos Clusters de Cranios (Todas as 22 Medidas)",
    subtitle = "Cluster 3 (Caes Maiores) vs. Cluster 2 (Caes Menores)",
    x = "Medida do Cranio",
    y = "Media da Medida (Centroide)") + 
  theme_minimal(base_size = 14) + 
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5),
    axis.text.x = element_text(angle = 55, hjust = 1)) +
  scale_color_brewer(palette = "Dark2")

medias_clusters_2 <- data.frame(medias_kmeans) 

medias_clusters_2$Cluster_Kmeans <- as.factor(medias_clusters_2$Cluster_Kmeans)

medias_long_2 <- medias_clusters_2 %>%
  pivot_longer(
    cols = -Cluster_Kmeans,            
    names_to = "Medida",         
    values_to = "Valor_Medio")

ggplot(
  medias_long_2, 
  aes(x = Medida, y = Valor_Medio, group = Cluster_Kmeans, color = Cluster_Kmeans)) +
  geom_line(size = 1.2) +
  geom_point(size = 3) +
  labs(
    title = "Grafico de Perfil dos Clusters de Cranios (2 Clusters)",
    subtitle = "Comparacao das medias entre o Cluster 1 (Maiores) e Cluster 2 (Menores)",
    x = "Medida do Cranio",
    y = "Media da Medida (Centroide)") +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5),
    axis.text.x = element_text(angle = 55, hjust = 1)) +
  scale_color_brewer(palette = "Dark2")




