head(iris)

# Somas das frequencias
table(iris$Species)

library(ggplot2)

# 40 elementos selecionados aleatóriamente
idx <- sample(1:nrow(iris), 40)

irisSample <- iris[idx, ]

irisSample$Species <- NULL

head(irisSample)

hc <- hclust(dist(irisSample), method = 'complete')

plot(hc, hang = -1, labels = iris$Species[idx])

# cut tree into 3 clusters 
rect.hclust(hc, k = 3)

groups <- cutree(hc, k = 3)

groups[groups == 1] <- 'setosa'
groups[groups == 2] <- 'virginica'
groups[groups == 3] <- 'versicolor'

table(groups, iris$Species[idx])

# linkage distance is used
hc2 <- hclust(dist(iris[, 3:4]))
groups2 <- cutree(hc2, 3)

table(groups2, iris$Species)

groups2[groups2 == 3] <- 0
groups2[groups2 == 2] <- 3
groups2[groups2 == 0] <- 2

table(groups2, iris$Species)

names(iris)

ggplot(iris, aes(Petal.Length, Petal.Width, color = Species)) +
  geom_point(alpha = 0.4, size = 3.5) + 
  geom_point(col = groups2) +
  scale_color_manual(values = c('black', 'red', 'green'))


