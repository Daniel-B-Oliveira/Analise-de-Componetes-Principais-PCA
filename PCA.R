library(ggplot2)
library(dplyr)

impute_missing <- function(data){
  data_imputed <- data
  for (i in 1:ncol(data)){
    if (is.numeric(data[[i]])){
      data_imputed[[i]][is.na(data_imputed[[i]])] <- mean(data_imputed[[i]], na.rm = TRUE)
    }
  }
  return(data_imputed)
}

run_pca_manual <- function(data, scale_data = TRUE) {
  data <- impute_missing(data)
  
  if (scale_data) data <- scale(data, scale = TRUE)
  
  cov_matrix <- cov(data)

  eigens <- eigen(cov_matrix, symmetric = TRUE)

  scores <- data %*% eigens$vectors
  
  return(list(eigenvalues = eigens$values, eigenvectors = eigens$vectors, scores = scores))
}

plot_pca_static <- function(scores, group_col = NULL) {
  pca_df <- data.frame(scores)
  colnames(pca_df) <- c("PC1", "PC2")
  
  if (!is.null(group_col)) pca_df$Group <- group_col
  
  ggplot(pca_df, aes(x = PC1, y = PC2, color = Group)) +
    geom_point(size = 3) +
    labs(title = "PCA - Plot Estático") +
    theme_minimal()
}

data(iris)

iris_data <- iris |>
  select(-Species)

pca_result <- run_pca_manual(iris_data, scale_data = TRUE)

plot_pca_static(pca_result$scores, group_col = iris$Species)

#Códigos não incluidos no relatório 

# 1. Variância explicada de cada PC
variancia_explicada <- pca_result$eigenvalues / sum(pca_result$eigenvalues) * 100
cat("Variância explicada:\nPC1:", variancia_explicada[1], "%\nPC2:", variancia_explicada[2], "%\n")

# 2. Autovetores (loadings)
cat("\nAutovetores:\n")
loadings <- data.frame(
  Variavel = colnames(iris_data),
  PC1 = round(pca_result$eigenvectors[,1], 3),
  PC2 = round(pca_result$eigenvectors[,2], 3)
)
print(loadings)

# 3. Primeiros scores (projeções)
cat("\nPrimeiras observações nos componentes:\n")
print(head(pca_result$scores[,1:2]))

# Função para plotar gráficos de PCA com diferentes combinações de componentes
plot_pca_components <- function(pca_scores, components = c(1, 2), group_col = NULL) {
  # Verifica se os componentes fornecidos estão dentro do intervalo de componentes disponíveis
  if (max(components) > ncol(pca_scores)) {
    stop("Componentes solicitados excedem o número de componentes disponíveis.")
  }
  
  # Extrai as colunas dos componentes especificados
  pca_data <- pca_scores[, components]
  
  # Cria o gráfico de dispersão
  plot(pca_data, col = group_col, pch = 19, 
       xlab = paste("PC", components[1]), ylab = paste("PC", components[2]),
       main = paste("Gráfico de PCA - Componentes", components[1], "vs", components[2]))
}
