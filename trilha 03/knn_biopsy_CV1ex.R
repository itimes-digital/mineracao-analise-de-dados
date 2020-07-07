#
# Atividade de Aprofundamento 
# Tarefa 1 = Knn Cross Validation
#
# Altere o código abaixo que avalia modelos knn para k=3,4 e com 5 partições
# para avaliar modelos knn com k=1:12 e para que as partições de teste que tenham
# apenas 1 elemento.
#
# Em seguida responda a questão 1 e 2 do questionário.
#
# NÃO É NECESSÁRIO POSTAR ESSE CÓDIGO
#
install.packages('lattice')
install.packages('ggplot2')
install.packages('e1071')
library(MASS)
library(class)
library(lattice)
library(ggplot2)
library(caret) # for Cross Validation functions
library(e1071)

# Problemas com a carga das libraries? Execute antes
# 
# install.packages(" nome da library ") 
#
# não esqueça das aspas

biopsy_ = na.omit(biopsy[,-c(1)]) # 1 = ID é eliminado assim como os valores NA  

# Cross validation em 5 partes com 'Summary of sample sizes: 547, 546, 546, 547, 546' 
ctrl <- trainControl(method="repeatedcv", number=683, repeats=1)

# Definição do número de k (vizinhos) para o modelo
nn_grid <- expand.grid(k=c(1:12))
nn_grid

best_knn <- train(class~., data=biopsy_,
                  method="knn",
                  trControl=ctrl, 
                  preProcess = c("center", "scale"),  # standardize
                  tuneGrid=nn_grid)

print(best_knn) 