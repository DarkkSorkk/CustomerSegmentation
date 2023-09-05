library(tidyverse)
library(cluster)

# Carregar e pré-processar os dados
df <- read.csv('retail_sales_dataset.csv')
df_scaled <- scale(df)

# Codificar a coluna "Gender"
df$Gender <- ifelse(df$Gender == "Male", 1, 2)


library(dplyr)

df <- df %>%
  mutate(Product.Category = recode(Product.Category, 
                                   "Beauty" = 1, 
                                   "Clothing" = 2, 
                                   "Electronics" = 3))

# Remover Customer.ID
df <- df %>% select(-Customer.ID)

# Transformar Date para tipo Date e extrair características
df$Date <- as.Date(df$Date)
df$Year <- as.integer(format(df$Date, "%Y"))
df$Month <- as.integer(format(df$Date, "%m"))
df$Day <- as.integer(format(df$Date, "%d"))

# Remover coluna Date original
df <- df %>% select(-Date)

# Backup do dataframe
df_copy <- df

#Escalonar os dados
df_scaled <- scale(df_copy)

#Aplicar o método do cotovelo para encontrar o número ideal de clusters
wcss <- vector()
for (i in 1:10) {
  kmeans_model <- kmeans(df_scaled, centers=i)
  wcss[i] <- kmeans_model$tot.withinss
}

plot(1:10, wcss, type="b", xlab="Número de Clusters", ylab="WCSS")



# Executando o algoritmo K-means com 8 clusters
final_kmeans_model <- kmeans(df_scaled, centers=8)

# Adicionando os rótulos dos clusters ao dataframe original
df$Cluster <- final_kmeans_model$cluster

# Ver os primeiros registros para confirmar
head(df)

aggregate(.~Cluster, df, mean)

boxplot(Age ~ Cluster, data=df)

# Média de cada variável para cada cluster
cluster_means <- aggregate(.~Cluster, df, mean)
print("Médias dos Clusters:")
print(cluster_means)

# Desvio padrão de cada variável para cada cluster
cluster_sd <- aggregate(.~Cluster, df, sd)
print("Desvios Padrão dos Clusters:")
print(cluster_sd)

library(ggplot2)
ggplot(df, aes(x=factor(Cluster), fill=factor(Gender))) + 
  geom_bar(position="dodge") +
  ggtitle("Distribuição de Gênero por Cluster")

