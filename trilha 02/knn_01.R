library(class)
set.seed(1984)

# Dados de teste
L <- sample(1:nrow(iris), round(nrow(iris)/3))
test <- iris[L, 1:4]

# Restantes dos dados são treinamento
train <- iris[-L, 1:4]

# classificação
cl <- factor(iris[-L, 5])

fit <- knn(train, test, cl, k = 3);fit

fit[1:length(L)]

conf_matrix <- table(fit[1:length(L)], factor(iris[L, 5]))
print(conf_matrix)

# pega valores na diagonal
diag(conf_matrix)

cat('Accuracy: ', sum(diag(conf_matrix))/sum(conf_matrix) * 100, '%')

fit

attr(fit, 'knn.index')