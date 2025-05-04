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