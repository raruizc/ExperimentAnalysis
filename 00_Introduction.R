# Introdução à linguagem R ------------------------------------------------
# 
# R é uma linguagem de programação e um ambiente de software livre voltado para 
# estatística e gráficos. 
# Foi desenvolvida por estatísticos para facilitar a análise de dados, 
# modelagem estatística, visualização e computação científica.
# 
# Por que usar R?
#  1) Poderoso para Análise de Dados: Oferece uma vasta gama de técnicas 
#   estatísticas e gráficas.
#  2) Gratuito e Open Source: Qualquer pessoa pode usar, modificar e distribuir.
#  3) Comunidade Ativa: Possui uma grande comunidade de usuários e desenvolvedores
#   que contribuem com pacotes adicionais, tutoriais e suporte.
#  4) Extensível: Com milhares de pacotes disponíveis no CRAN  (Comprehensive R Archive Network), 
#   você pode encontrar ferramentas para praticamente qualquer tipo de  análise de dados.
#  5) Reprodutibilidade: Facilita a criação de scripts que documentam 
#   todas as etapas da análise, promovendo a reprodutibilidade.

#Comandos Básicos e Operações --------------------------------------------------

## Operadores ------------------------------------------------------------------

2 + 2   #soma
3 - 1   #subtração
4 * 5   #multiplicação
12 / 3  #divisão
7 ^ 2   #exponenciação
0:100   #sequências

3 == 3  #comparação de igualdade
5 != 0  #comparação de diferenças
2 > 9   #maior do que
1 < 8   #menor do que
5 >= 5  #maior ou igual
4 <= 1  #menor ou igual

# Outros valores importantes:

TRUE #logical
T #logical
FALSE #logical
F #logical
NA #logical
NULL #NULL
Inf #numeric
-Inf #numeric


## Criando Variáveis -----------------------------------------------------------
# Você pode criar variáveis para armazenar dados:

# Atribuindo valores a variáveis
a <- 5
b <- 10

# Soma de variáveis
c <- a + b
print(c)

## Vetores --------------------------------------------------------------------- 

#Vetores são uma sequência de elementos do mesmo tipo:

# Criação de um vetor
numeros <- c(1, 2, 3, 4, 5)
print(numeros)

# Operações com vetores
soma_numeros <- sum(numeros)
media_numeros <- mean(numeros)
print(soma_numeros)
print(media_numeros)

## Data Frames -----------------------------------------------------------------

# Criação de um data frame
dados <- data.frame(
  ID = 1:5,
  Nome = c("Ana", "Bruno", "Carla", "Daniel", "Eva"),
  Idade = c(23, 25, 30, 22, 35)
)
print(dados)

# Seleção de uma coluna
print(dados$Nome)

# Seleção de uma linha
print(dados[1,])

# Seleção de uma célula
print(dados[1, "Nome"])

## Introdução à manipulação de dados ---------------------------------------

# Há alguns banco de dados interessantes já armazenadas no R compiladas em
# um pacote chamado 'datasets'

# Para ver a documentação ou buscar ajuda a respeito de algum pacote:

help(package = "datasets")

# Para carregarmos uma base de dados interna, utilizamos a função data():

data("iris")

# Para melhor entendermos o que há na base de dados chamada 'iris', pode-
# mos comandar o seguinte:

?iris

### Funções são tipos especiais de objetos no R, cujo nome vem seguido de 
# parêntesis. Equivalem a ordens diretas à máquina.

# Funções são algoritmos. Cada algoritmo possui suas próprias atribuições
# e decisões. e.g: Para a função c(), seu # objetivo é o de concatenar valores.
# Às funções head() e tail() no lugar da função View():

head(x = iris)
tail(x = iris)

head(iris, n = 1)
tail(iris, n = 10)

# Outra função interessante é a função str():

str(iris)

# Também podemos obter informações a respeito do número de linhas e de colu-
# nas da base de dados com as funções nrow(), ncol() e dim():

nrow(iris)
ncol(iris)
dim(iris)

# Podemos, ainda, ter acesso aos nomes das variáveis da base de dados com o
# auxílio da função names():

names(iris)

# Podemos deletar a base de dados do nosso ambiente de trabalho com a função
# rm():

rm(iris)

# Voltando a carregar a base de dados iris
data("iris")

#Podemos acessar uma variável da nossa base de dados com o operador $:
iris$Sepal.Length
iris$Species
#Também podemos acessar uma variável com o uso do operador [ , ]:
iris[, 1]

#Há, ainda, a função attach() que facilita bastante!
attach(iris)
Species
detach(iris)

#Podemos acessar as observações com o uso semelhante do operador [ , ]:
iris[1, ]

#Assim, é possível acessar valores específicos ao combinarmos o aprendido:
iris[2, 1]
iris[4, 5]

#Podemos, ainda, combinar as posições com o nome das variáveis:
iris[1, "Species"]

#Não podemos, porém, comandar o seguinte:
iris[, "Sepal.Width" : "Petal.Length"]

#E agora? A declaração abaixo funcionará?
iris[ ,c("Sepal.Width", "Petal.Length", "Species")]

#Outra forma interessante de seleção de valores:
iris[, c(3:5)]
iris[, -c(3:5)]


#A função which() pode ser uma boa opção na seleção de valores. 
#Vamos supor que a intenção seja que o R filtre todos os indivíduos cujo valor para 
#a variável Sepal.Width seja igual a 3.6:
iris[which(iris$Sepal.Width == 3.6), ]

#E se quiséssemos os indivíduos cujo valor para a variável Petal.Length seja diferente de 1.4:
iris[which(iris$Petal.Length != 1.4), ]

#Nesse momento é oportuno apresentar dois novos operadores:
# & #significa "e"
# | #em regra, significa "ou".

#Ind. cujo valor para a variável Sepal.Width seja igual a 3.6 E com a variável Petal.Length
#menor do que 1.7:
iris[which(iris$Sepal.Width == 3.6 & iris$Petal.Length < 1.7), ]

#Ind. cujo valor para a variável Sepal.Width seja igual a 3.6 OU com a variável Petal.Length 
#menor do que 1.7:
iris[which(iris$Sepal.Width == 3.6 | iris$Petal.Length < 1.7), ]

# Para mais informação podemos revisar a documentação do pacote dplyr
## É um pacote contigo no tidyverse
### Contem funções úteis para a manipulação/preparação de banco de dados
install.packages("dplyr")
library(dplyr)

### Referência: 
# https://dplyr.tidyverse.org/
# https://github.com/rstudio/cheatsheets/blob/master/data-transformation.pdf
# Wickham, H. & Grolemund, G. R for Data Science: https://r4ds.had.co.nz/index.html

## Funções Básicas de Estatísticas ---------------------------------------------
# Criação de um vetor com números
numeros <- c(1, 2, 3, 4, 5)

## Ou podemos selecionar de um Dataframe os valores
iris_comprimento_petala <- iris$Petal.Length


# Cálculo de média
media <- mean(iris_comprimento_petala)
print(media)

# Cálculo de mediana
mediana <- median(iris_comprimento_petala)
print(mediana)

# Cálculo de desvio padrão
desvio_padrao <- sd(iris_comprimento_petala)
print(desvio_padrao)

# Gráfico de barras
barplot(iris_comprimento_petala, main = "Gráfico de Barras", 
        xlab = "Índice", ylab = "Valores")

# Gráfico de dispersão
plot(iris_comprimento_petala, main = "Gráfico de Dispersão", 
     xlab = "Índice", ylab = "Valores")


