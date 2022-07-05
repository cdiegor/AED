
### Leitura do conjunto de dados ###
dados <- read.csv("2022.csv", sep=",")
names(dados)

### Transformando o score em número ###
dados$Happiness.score <- as.numeric( 
                            gsub(",", "", dados$Happiness.score) )

### Sumário, histograma e boxplot da variável score ###
summary(dados$Happiness.score)
hist(dados$Happiness.score)
boxplot(dados$Happiness.score)

### Gráfico de pontos da variável score ###
dotchart(dados$Happiness.score)

### Gráfico de pontos da variável score top 20###
dotchart(head(dados$Happiness.score, 20), labels = head(dados$Country, 20))

### Gráfico de pontos da variável score últimos 20###
dotchart(tail(dados$Happiness.score, 20), labels = tail(dados$Country, 20))


### Curva de Lorenz e índice de Gini ###
library(ineq)
clorenz = Lc(dados$Happiness.score)
plot(clorenz)
Gini(dados$Happiness.score)

### Correlação entre score e corrupção ###

### Transformando percepção da corrupção em número ###
dados$Explained.by..Perceptions.of.corruption <- as.numeric( 
  gsub(",", ".", dados$Explained.by..Perceptions.of.corruption) )

### Calculando a correlação entre a corrupção e o score ###
cor(dados$Happiness.score, 
    dados$Explained.by..Perceptions.of.corruption,
    use = "complete")

plot(dados$Happiness.score, 
     dados$Explained.by..Perceptions.of.corruption)

subset(dados, Country=="Brazil")

### Gráfico de pares ###

### Transformando variáveis em número ###

dados$Explained.by..Generosity<- as.numeric( 
  gsub(",", ".", dados$Explained.by..Generosity) )
dados$Explained.by..Freedom.to.make.life.choices<- as.numeric( 
  gsub(",", ".", dados$Explained.by..Freedom.to.make.life.choices) )
dados$Explained.by..Healthy.life.expectancy<- as.numeric( 
  gsub(",", ".", dados$Explained.by..Healthy.life.expectancy) )
dados$Explained.by..Social.support<- as.numeric( 
  gsub(",", ".", dados$Explained.by..Social.support) )
dados$Explained.by..Generosity<- as.numeric( 
  gsub(",", ".", dados$Explained.by..Generosity) )

dados$Whisker.high <- as.numeric( 
  gsub(",", "", dados$Whisker.high) )
dados$Whisker.low <- as.numeric( 
  gsub(",", "", dados$Whisker.low) )
dados$Explained.by..GDP.per.capita <- as.numeric( 
  gsub(",", "", dados$Explained.by..GDP.per.capita) )
dados$Dystopia..1.83....residual <- as.numeric( 
  gsub(",", "", dados$Dystopia..1.83....residual) )

pairs(dados)

cor(dados$Happiness.score, 
    dados$Explained.by..Healthy.life.expectancy, 
    use="complete")

cor(dados$Happiness.score, 
    dados$Explained.by..Freedom.to.make.life.choices, 
    use="complete")

cor(dados$Happiness.score, 
    dados$Explained.by..Social.support, 
    use="complete")
