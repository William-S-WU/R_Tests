library(tidyverse)  
library(corrplot)

getwd()
print(getwd())
current_directory <- getwd()

df <- read.csv("wine-quality.csv")
head(df)
column_names <- colnames(df)
print(column_names)
anova <- aov(quality ~ ., data = df)
summary(anova)
summary(df$citric.acid)

summary(df)
mean(df$citric.acid)
sd(df$citric.acid)
plot(df$citric.acid, df$quality, main="Scatter Plot")
hist(df$citric.acid)
cor(df$citric.acid, df$volatile.acidity)  

#simple tests
t.test(df$quality ~ df$citric.acid)
wilcox.test(df$quality ~ df$citric.acid)

#model linear
model <- lm(quality ~ citric.acid, data=df)
summary(model)  # Check results
multi_model <- lm(quality ~ . , data=df)
summary(multi_model)


#PCA 
typeof(column_names)
pca_result <- prcomp(df, scale=TRUE)
summary(pca_result)
pca_result$rotation
pca_result$x

biplot(pca_result)

explained_variance <- pca_result$sdev^2 / sum(pca_result$sdev^2) * 100
print(explained_variance)

#PCA visualization
x_label <- paste0("Principal Component 1 (", round(explained_variance[1], 2), ")%")
y_label <- paste0("Principal Component 2 (", round(explained_variance[2], 2), ")%")

pca_data <- data.frame(pca_result$x)
pca_data$quality <- df$quality
ggplot(pca_data, aes(x = PC1, y = PC2, color = as.factor(quality))) +
  geom_point() +
  labs(title = 'PCA of Wine Quality Data', x = x_label, y = y_label, color = 'Quality') +
  theme_minimal()



#eigen values
eigenvalues <- pca_result$sdev^2
print(eigenvalues)

loadings <- pca_result$rotation
print(loadings)

dataEigenvalues_loading <- data.frame(Eigenvalues=eigenvalues, Loadings=t(loadings))


corrplot(loadings, method="circle") #correlation plot of loadings