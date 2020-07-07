mtcars$mpg

mpg_discrete = cut(mtcars$mpg, 3, include.lowest = TRUE, 
                   labels = c('Baixo', 'Médio', 'Alto'))

mpg_discrete

my_mtcars <- cbind(mtcars[, 1:3], mpg_discrete)

head(my_mtcars)

tail(my_mtcars)

fld_setosa = ifelse(iris$Species == 'setosa', 1,0)
fld_versicolor = ifelse(iris$Species == 'versicolor', 1,0)
fld_virginica = ifelse(iris$Species == 'virginica', 1,0)
my_iris = cbind(iris, fld_setosa, fld_versicolor, fld_virginica)
my_iris[base::sample(1:nrow(iris), 7),]

# Função para normalização dos dados
normalize <- function(x){
  num <- x - min(x)
  denom <- max(x) - min(x)
  return (num/denom)
}
# comando lapply() é usado para aplicar uma função do tipo f(vetor)
# para um grupo de valores
iris_normalized = as.data.frame(lapply(iris[,1:4], normalize))

head(iris_normalized)

summary(iris_normalized)

