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


model <- lm(quality ~ citric.acid, data=df)
summary(model)  # Check results
multi_model <- lm(quality ~ . , data=df)
summary(multi_model)

typeof(column_names)
pca_result <- prcomp(df, scale=TRUE)
summary(pca_result)
pca_result$rotation
pca_result$x

biplot(pca_result)

eigenvalues <- pca_result$sdev^2
print(eigenvalues)

loadings <- pca_result$rotation
print(loadings)

data.frame(Eigenvalues=eigenvalues, Loadings=t(loadings))


corrplot(loadings, method="circle")