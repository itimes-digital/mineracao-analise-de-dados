library(ROCR)
library(e1071)
library(dummies)

credit_fraud <- read.csv('D:/estudo-machine-learning/mineracao-analise-de-dados/trilha 04/data/qconlondon2016_sample_data.csv')

head(credit_fraud)

# Ajustar as horas para ser numeral
credit_fraud$charge_time = unclass(as.POSIXct(as.Date(credit_fraud$charge_time)))

credit_fraud = cbind(credit_fraud, dummy('card_country', data = credit_fraud, sep = ".", drop = TRUE, fun = as.integer, verbose = FALSE))

names(credit_fraud)

credit_fraud <- credit_fraud[c("fraudulent", 
                               "charge_time",
                               "amount",
                               "card_use_24h",
                               "card_country.AU",
                               "card_country.GB",
                               "card_country.US")]

#credit_fraud$fraudulent = ifelse(credit_fraud$fraudulent == 'True', 1, 0)

credit_fraud$amount <- cut(credit_fraud$amount, 20)
credit_fraud$charge_time <- cut(credit_fraud$charge_time, 20)
credit_fraud$card_use_24h <- cut(credit_fraud$card_use_24h, 20)

set.seed(1984)

T <- sample(1:nrow(credit_fraud), round(0.3 * nrow(credit_fraud)))

credit_fraud_test <- credit_fraud[T, ]
credit_fraud_train <- credit_fraud[-T, ]


fit <- naiveBayes(fraudulent ~., data = credit_fraud_train, laplace = 1)

fit$apriori

fit$tables

predict_test <- predict(fit, newdata = credit_fraud_test, type = 'raw') > 0.05

length(predict_test[,2])
length(credit_fraud_test$fraudulent)

conf_matrix_2 <- table(credit_fraud_test$fraudulent, predict_test[,2])
print(conf_matrix_2)

cat('Accuracy: ', sum(diag(conf_matrix_2)) / sum(conf_matrix_2) * 100, '%')

# ROC curve

predict_test[,2] <- ifelse(predict_test[,2] == TRUE, 1, 0) 
credit_fraud_test$fraudulent <- ifelse(credit_fraud_test$fraudulent == 'True', 1, 0)

pr = prediction(as.numeric(predict_test[,2]), as.numeric(credit_fraud_test$fraudulent))

prf = performance(pr, measure = "tpr", x.measure = 'fpr')
plot(prf, colorize=TRUE)

auc=performance(pr, measure = 'auc')
auc = auc@y.values[[1]]
auc





