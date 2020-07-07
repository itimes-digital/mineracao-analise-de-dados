head(iris)

table(iris$Species)

library(ggplot2)

idx <- sample(1:nrow(iris), 40)
iris_sample <- iris[idx, ]

# Elimina o classificador
iris_sample$Species <- NULL

cl <- kmeans(iris_sample, 3)

cl

cl$centers

cl$iter

cl$cluster[cl$cluster == 1] = 'versicolor'
cl$cluster[cl$cluster == 2] = 'virginica'
cl$cluster[cl$cluster == 3] = 'setosa'

table(cl$cluster, iris$Species[idx])

# 3 grupos
cl2 <- kmeans(iris[, 3:4], 3)

cl2$cluster

table(cl2$cluster, iris$Species)

cl2$cluster[cl2$cluster == 3] = 0
cl2$cluster[cl2$cluster == 2] = 3
cl2$cluster[cl2$cluster == 1] = 2
cl2$cluster[cl2$cluster == 0] = 1

table(cl2$cluster, iris$Species)

ggplot(iris, aes(Petal.Length, Petal.Width, color = as.factor(cl2$cluster))) +
  geom_point()

# grafico alternativo
plot(iris[, c('Petal.Length', 'Petal.Width')], col = cl2$cluster)
# plot cluster centers
# posição dos centróides
points(cl2$centers[, c('Petal.Length', 'Petal.Width')], col = c(1:3), pch = 8, cex = 2)

# 6 grupos
cl3 <- kmeans(iris[, 3:4], 6)

cl3$cluster

table(cl3$cluster, iris$Species)

ggplot(iris, aes(Petal.Length, Petal.Width, color = as.factor(cl3$cluster))) +
  geom_point()

# grafico alternativo
plot(iris[, c('Petal.Length', 'Petal.Width')], col = cl3$cluster)

points(cl3$centers[,c('Petal.Length', 'Petal.Width')], col = c(1:6), pch = 8)





