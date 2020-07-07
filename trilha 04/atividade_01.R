install.packages('party')
library(party)
library(grid)

L <- sample(1:nrow(iris), round(nrow(iris)/3))

train <- iris[-L, ]
test <- iris[L, ]

train

# Species é a classe
fit <- ctree(Species ~., train)
fit

predict_test <- predict(fit, newdata = test)

conf_matrix <- table(test$Species, predict_test)
print(conf_matrix)

cat('Accuracy: ', sum(diag(conf_matrix))/sum(conf_matrix) * 100, '%')

plot(fit)
