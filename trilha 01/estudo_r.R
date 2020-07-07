x <- 1;
y <- 2;
print(x + y)

# Em R poder ser também x = 1

help(plot)

X = 10
Y <- 1

class(X)
class(Y)

cat("Valores de x e y são: ", x, " e ", y, 
    "\nvalores de X e Y são: ", X, " e ", Y)

x = c(1, 2, 3);x
y <- seq(1,10,0.4);y
z = c(1:10);z
nomes <- c('julia', 'aline', 'cris');nomes

# NA and Inf
x = c(1, 2, 3, NA, 5, 6, 1/0, 7, 8, 9, 10);x

# Basic statistics

x = c(1:3)
mean(x)
min(x)
max(x)
sum(x)
sd(x)
x = c(1, NA, 3)
mean(x)
var(x)
median(x)
mean(x, na.rm = TRUE)
var(x, na.rm = TRUE)
median(x, na.rm = TRUE)
x <- c(1, 1/0, 3)
mean(x)

# quartil
x = c(1, 2, 3, 4, 5)
quantile(x)
var(x)
sd(x)

y = c(1, 2, 3, 4)
quantile(y)

z = c(0:10)
quantile(z)

summary(x)

# basic plots
par(mfrow=c(1, 2)) # combine 2 graphs
x = rnorm(1345) # random normal distibuition
boxplot(x) # com presença de outliers
hist(x)


par(mfrow=c(1, 3)) # combine 3 graphs
x = rnorm(100)
y = rnorm(100)

barplot(x, y)
boxplot(x, y)
plot(x, y) # scatter plot


x <- c('a', 'b', 'c', 'a', 'd', 'a', 'b')
table(x) # frequência de valores de um atributo

# overview command
help(mtcars)
head(mtcars)
tail(mtcars)
colnames(mtcars)
nrow(mtcars)
ncol(mtcars)

# selectin rows
mtcars['Mazda RX4',]
mtcars[1,]
mtcars[c('Mazda RX4', 'Mazda RX4 Wag'),]

# seleciona a 1ª e 2ª linha
mtcars[1:2,]

# seleciona a 1ª, 5ª e 7ª a 18ª
mtcars[c(1, 5, 7:18),]

# seleciona todas as linhas em que o atributo cyl é igual ou maior que 6
# após a virgula, determina todas as colunas
mtcars[mtcars$cyl >= 6,]

# seleciona todas as linhas em que o atributo cyl é igual a 6 
# com as colunas, cyl e mpg
mtcars[mtcars$cyl == 6, c('cyl', 'mpg')]
mtcars$mpg
mtcars[,1]
mtcars[,'mpg']
mtcars[,'cyl']
mtcars[1]
mtcars['mpg']

mtcars[, c('mpg', 'cyl', 'disp')]
mtcars[, c(1, 2, 4:7)]
mtcars[, c(1:3)]
# linhas e colunas específicas
mtcars[c('Valiant', 'Cadillac Fleetwood'), c(1:3)]
mtcars['Mazda RX4', 2]
mtcars['Mazda RX4', c(2, 3)]
mtcars[1, 2]
mtcars[mtcars$cyl > 6, 'mpg']
mtcars[mtcars$cyl > 6, c('mpg', 'cyl')]
mtcars[mtcars$cyl > 6,]$mpg

my_mtcars_v1 <- mtcars[c(1,2,3,15:18), c('mpg', 'cyl', 'carb')]
my_mtcars_v1

my_mtcars_v2 <- mtcars[mtcars$cyl > 6 & mtcars$disp > 100, c(1:3)];my_mtcars_v2

# logical index
logic = mtcars$cyl > 6 & mtcars$disp > 100;logic

# filtrando pelo retorno de true ou false do logic
mtcars[logic, c(1, 2, 3)]

iris_table <- iris[iris$Petal.Width == 0.2,]

iris_table$Species == 'setosa'
iris_table$Species == 'versicolor'
iris_table$Species == 'virginica'

iris_table <- iris[iris$Petal.Length >= 1.5 & iris$Petal.Length <= 4.5,]

iris_table$Species == 'setosa'
iris_table$Species == 'versicolor'
iris_table$Species == 'virginica'

mean(iris_table$Petal.Length)

summary(iris)

cor(iris$Petal.Length, iris$Petal.Width)

iris$setosa = ifelse(iris$Species == 'setosa', 1, 0)

iris$setosa

x <- c(1, 2, 3)

y <- x**2

z <- y[x == y]

z

x <- c(1, 2, 3)

y <- x + 1

a <- 1:12

x <- matrix(a, 3, 4)

x[,2]

x[3,2]

a <- 1:4

x <- matrix(a, 2, 2)

x

x <- seq(1, 10, 0.5)
mean(x)
median(x)
