head(iris)

# linhas
nrow(iris)

# dimensão - linhas e colunas
dim(iris)

# todas as linhas da coluna 2
iris[,2]
iris[,'Sepal.Width']
iris[2:4,'Sepal.Width']

# nome das colunas ou atributos
names(iris)

str(iris)

# 5 indices aleatóriamente
indice1 <- sample(1:nrow(iris), 5);indice1

# Dados do indice
iris[indice1,]

# Dados restantes diferente do indice
iris[-indice1,]

summary(iris)

boxplot(iris$Petal.Length)

boxplot(iris)

# combinar gráficos
par(mfrow=c(2,2))
hist(iris[, 1])
hist(iris[, 2])
hist(iris[, 3])
hist(iris[, 4])

par(mfrow=c(2,2))
plot(density(iris[,1]))
plot(density(iris[,2]))
plot(density(iris[,3]))
plot(density(iris[,4]))

var(iris$Sepal.Length) > var(iris$Sepal.Width)

sd(iris$Sepal.Length) > sd(iris$Sepal.Width)

# O desvio padrão é a raiz quadrada da variância
sd(iris$Sepal.Length) == sqrt(var(iris$Sepal.Length))

indice2 <- base::sample(1:nrow(iris), 30)
my_30_iris_species <- base::table(iris[indice2,]$Species)
my_30_iris_species

par(mfrow=c(1,2))
pie(my_30_iris_species)
barplot(my_30_iris_species)

cov(iris$Sepal.Length, iris$Petal.Length)

cov(iris[,1:4])

cor(iris[,1:4])

pairs(iris)

par(mfrow=c(1,2))
with(iris, 
     plot(Sepal.Length, 
          Sepal.width, 
          col=Species, 
          pch=as.numeric(Species)))
plot(iris$Sepal.Length, iris$Sepal.Width,
col=iris$Species, pch=as.numeric(iris$Species))
with(iris, 
     plot(Petal.Length, 
          Petal.width, 
          col=Species, 
          pch=as.numeric(Species)))
plot(iris$Petal.Length, iris$Petal.Width,
col=iris$Species, pch=as.numeric(iris$Species))

boxplot.stats(iris$Sepal.Width)

# visão gráfica dos outliers
boxplot(iris$Sepal.Width)

x = c(1,2,3,NA)
is.na(x)
y = c('a', NA, 'b', 'c')
mydata = data.frame(cbind(x, y))
mydata$x <- as.numeric(mydata$x)
mydata

# Excluindo valores ausentes em funções
mean(mydata$x, na.rm = TRUE)
# 2

# verifica se tem NA, retorna true, pega o valor de X como NA e troca para média
mydata[is.na(mydata$x),]$x <- mean(mydata$x, na.rm = TRUE)
mydata

complete.cases(mydata)

# elimina a linha que contêm NA
mydata[complete.cases(mydata),]

# elimina a linha que contêm NA
na.omit(mydata)

my_df1 <- data.frame(c(1,2,3), c(55,32,34))
names(my_df1) <- c('id', 'value1')
my_df2 <- data.frame(c(1,2,3), c('a', 'b', 'c'))
names(my_df2) <- c('id','value2')
my_df_join = merge(my_df1, my_df2, by = c('id'))
my_df_join

order(my_df_join$value1)

my_df_join[order(my_df_join$value1),]





