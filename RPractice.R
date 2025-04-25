library(tidyverse)  
library(corrplot)
library(caret)

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
print(explained_variance) #11

#PCA visualization
x_label <- paste0("Principal Component 1 (", round(explained_variance[1], 2), ")%")
y_label <- paste0("Principal Component 2 (", round(explained_variance[2], 2), ")%")

pca_data <- data.frame(pca_result$x)
print(pca_data)
pca_data_new <- pca_data[, -c(12)]#remove lowest PC
print(pca_data_new)
pca_data$quality <- df$quality
pca_data_new$quality <- df$quality
ggplot(pca_data, aes(x = PC1, y = PC2, color = as.factor(quality))) +
  geom_point() +
  labs(title = 'PCA of Wine Quality Data', x = x_label, y = y_label, color = 'Quality') +
  theme_minimal()



# Create an 80-20 train-test split For PCA model 
train_index <- createDataPartition(pca_data$quality, p = 0.8, list = FALSE)
train_data <- pca_data[train_index, ]
test_data <- pca_data[-train_index, ]



pca_model <- lm(quality ~ . , train_data)
summary(pca_model)
#eigen values
eigenvalues <- pca_result$sdev^2
print(eigenvalues)

loadings <- pca_result$rotation
print(loadings)

dataEigenvalues_loading <- data.frame(Eigenvalues=eigenvalues, Loadings=t(loadings))


corrplot(loadings, method="circle") #correlation plot of loadings

#predictions
predictions_lm_pca <- predict(pca_model, test_data)
plot_data_pca <- data.frame(Actual = test_data$quality, Predicted = predictions_lm_pca)

summary(pca_model)$coefficients

p1 <- ggplot(pca_model, aes(x = test_data$quality, y = predictions_lm_pca)) +
  geom_point(color = 'skyblue') +
  geom_smooth(method = "lm", color = 'red', se = FALSE) +
  ggtitle('PCA Data: Actual vs Predicted Values') +
  xlab('Actual Quality') +
  ylab('Predicted Quality') +
  theme_minimal()

print(p1)

