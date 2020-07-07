#
# Atividade de Aprofundamento 
# Tarefa 2 = Logit Cross Validation
#
# Altere o código abaixo para obter avaliar a acuracidade do modelo logístico
# emprega partições de teste que tenham apenas 1 elemento.
#
# Em seguida responda a questão 3 e 4 do questionário.
#
# NÃO É NECESSÁRIO POSTAR ESSE CÓDIGO
#
install.packages("ROCR")
install.packages("dummies")
install.packages("caret") 
install.packages("e1071") 
library(ROCR)
library(dummies)
library(caret) # for Cross Validation functions
library(e1071)

biopsy_ = na.omit(biopsy[,-c(1)]) # 1 = ID

table(biopsy_$class)

# Altere para numérico 0 ou 1
biopsy_$class = ifelse(biopsy_$class == 'malignant', 1,0)  

# Altere o valor de number= para que as partições de teste que tenham apenas 1 elemento  
ctrl <- trainControl(method="repeatedcv", number= 683, repeats=1)

fit <- train(class~., data=biopsy_,
             method="glm", 
             family="binomial",
             trControl=ctrl, 
             preProcess = c("center", "scale"))
fit

predict_test = predict(fit, newdata=biopsy_, type="raw")

# predict_test contem valores 0-1 com a probabilidade de "malignant" ou "benign"
# altere predict_test para um vetor contendo os valores "malignant","benign"
#
# sugestão: empregue elseif para atribuir 1="malignant" ou 0="benign" (valores numéricos) conforme o 
# valor seja >0.5
#
predict_test = ifelse(predict_test > 0.5, 'malignant','benign')  

# Gere a matrix de confusão 
c_matrix = table(biopsy_$class, predict_test)
print(c_matrix)

# Calcule a acuracidade
cat('Accuracy: ', sum(diag(c_matrix))/sum(c_matrix)*100, ' %', "\n")

# ROC curve 
predict_test = ifelse(predict_test == 'malignant', 1, 0) 
# biopsy_$class e predict_test estão com o mesmo tipo 
pr=prediction(predict_test,biopsy_$class) 
prf=performance(pr, measure="tpr", x.measure="fpr")
plot(prf,colorize=TRUE)

# Area under ROC curve 
auc=performance(pr, measure="auc")
auc=auc@y.values[[1]]
auc