#
# Atividade de Aprofundamento 
# Tarefa 1 = Naive Bayes e Decision Tree Credit Approval
#
# Esse código executa os modelos de Naive Bayes e Decision Tree para o 
# dataframe de Credit Approval. Empregue ele para responder a questão 1 do 
# questionário e faça a alteração necessária no treinamento da Decision
# Tree para responder a questão 2.
#
# NÃO É NECESSÁRIO POSTAR ESSE CÓDIGO do Credit Approval 
#
# Tarefa 2 = Naive Bayes e Decision Tree Mushrooms
#
# Empregue esse cóigo como base para execução dos modelos Naive Bayes e Decision Tree para o 
# dataframe mushrooms.
#
# O arquivo pode ser obtido em:
# https://www.openml.org/data/get_csv/24/dataset_24_mushroom.arff
# 
# Em seguida responda as questões de 3 a 10 do questionário.
#
# ATENÇÃO: É NECESSÁRIO POSTAR ESSE CÓDIGO do mushrooms! 
#


# Verifique outras libraries necessárias na sua execução
library(e1071)
library(party)
library(dummies)

credit = read.csv("https://www.openml.org/data/get_csv/29/dataset_29_credit-a.arff",header=T)


# Elimina valores NA
credit[credit[]=="?"]=NA
credit=na.omit(credit)

table(credit$V1)

credit$class <- ifelse(credit$class == "+", 1, 0)

# factor determina como uma variável categórica
classe <- factor(credit$class)

str(classe)

# com o levels foi definido os valores nessas categorias 
levels(classe) <- c("-", "+")

# Comparação de quantidade de valores antes inputar os dados na base
table(classe)
table(credit$class)

credit$class <- classe

summary(credit)

# MANTER O SEED PARA GARANTIR AS RESPOSTAS DO QUESTIONÁRIO 


credit <- credit[, c('A5', 'A7', 'A8', 'A9', 'A10',  'A11',  'A14', 'class')]
credit = cbind(credit, dummy('A5', data = credit, sep = ".", drop = TRUE, fun = as.integer, verbose = FALSE))
credit = cbind(credit, dummy('A7', data = credit, sep = ".", drop = TRUE, fun = as.integer, verbose = FALSE))
credit = cbind(credit, dummy('A9', data = credit, sep = ".", drop = TRUE, fun = as.integer, verbose = FALSE))
credit = cbind(credit, dummy('A10', data = credit, sep = ".", drop = TRUE, fun = as.integer, verbose = FALSE))

excluir <- c('A5', 'A7', 'A9', 'A10')

credit <- credit[,!(names(credit)%in% excluir)]
credit$A14 <- as.double(credit$A14)

RNGversion("3.5.2")
set.seed(1987)

#
#
# Decision Tree 
#

L <- sample(1:nrow(credit),round(nrow(credit)/3))
train <- credit[-L,]
test <- credit[L,]

fitTree = ctree(class~.,data=train)

# predict Output 
predTree = predict(fitTree, test)
predTree

# matriz de confusão
c_matrix = table(predTree,test$class)
print(c_matrix)

cat('Accuracy Dtree: ', sum(diag(c_matrix))/sum(c_matrix)*100, ' %', "\n")

plot(fitTree)