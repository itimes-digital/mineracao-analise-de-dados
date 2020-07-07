install.packages("ROCR")
library(ROCR)

# definir um seed para que a execução seja sempre o mesmo valor
set.seed(1984)

# carregamento de valores
credit_fraud = read.csv('https://raw.githubusercontent.com/jcpsantos/Aprendizado_M-quina/master/qconlondon2016_sample_data.csv')
head(credit_fraud)

summary(credit_fraud)

# agrupa quantidades das observações
table(credit_fraud$card_country)

#dummy
country_us = ifelse(credit_fraud$card_country == 'US', 1, 0)
country_gb = ifelse(credit_fraud$card_country == 'GB', 1, 0)
country_au = ifelse(credit_fraud$card_country == 'AU', 1, 0)

credit_fraud <- cbind(credit_fraud, country_us, country_gb, country_au)

credit_fraud$fraudulent = ifelse(credit_fraud$fraudulent == 'True', 1, 0)

# Ajustar as horas para ser numeral
credit_fraud$charge_time = unclass(as.POSIXct(as.Date(credit_fraud$charge_time)))

# retirar a coluna card_country
#colnames(credit_fraud)
credit_fraud <- credit_fraud[,c("fraudulent", 
                               "charge_time",
                               "amount",
                               "card_use_24h",
                               "country_us",
                               "country_gb",
                               "country_au")]

# retirar a coluna card_country
credit_fraud <- credit_fraud[,c(1:3, 5:8)]
#colnames(credit_fraud)

# slice dos dados de 30% para teste, a variável T recebe os índices dos dados
T = sample(1:nrow(credit_fraud), round(0.3 * nrow(credit_fraud)))

# 30% dos dados para teste
credit_fraud_test = credit_fraud[T,]

# 70% dos dados para treinamento
credit_fraud_train = credit_fraud[-T,]

table(credit_fraud$fraudulent)

# o ponto "." indica todas as variáveis
fit = glm(fraudulent~., data = credit_fraud_train, family = binomial("logit"))

# Definição das variéveis ao modelo após o '~'
#fit = glm(fraudulent~amount + card_use_24h + country_us + country_gb + country_au, 
#          data = credit_fraud_train, family = binomial("logit"))

predict_test = predict(fit, newdata = credit_fraud_test, type = "response") > 0.5

conf_matrix = table(credit_fraud_test$fraudulent, predict_test)

print(conf_matrix)

cat('Accuracy: ', sum(diag(conf_matrix))/sum(conf_matrix) * 100, ' %')

exp(coef(fit))

summary(fit)

pr = prediction(as.numeric(predict_test), as.numeric(credit_fraud_test$fraudulent))
prf = performance(pr, measure = "tpr", x.measure = "fpr")
plot(prf, colorize = TRUE)

auc = performance(pr, measure = "auc")
auc = auc@y.values[[1]]
auc

