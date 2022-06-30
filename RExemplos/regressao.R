# Exemplos com regressão

Acuidade <-read.table(
  "http://wiki.icmc.usp.br/images/0/0f/Acuidade.txt", header=TRUE)

X <- Acuidade$idade
Y <- Acuidade$tempo
plot(X, Y, pch=16)
lm(Y~X)
abline(lm(Y~X), col=2)
summary(lm(Y~X))



dados = read.csv("http://www.icmc.usp.br/~cibele/Dados/Housing_Dec2010.csv",
                   header = TRUE, sep = ";")

names(dados)
summary(dados)
attach(dados)
table(year)
barplot(table(year), xlab = "Ano", ylab = "Número de países",
        las = 2)
countryarea[year == 1976]
countryarea[year == 1998] 
dotchart(total, labels = countryarea, xlab = "Média de pessoas/cômodo", 
          pch = 20, cex = 0.7, cex.lab = 1.5)
ordem = order(total, decreasing = TRUE)
dotchart(total[ordem], labels = countryarea[ordem], xlab = "Média de pessoas/cômodo",
         pch = 20, cex = 0.7, cex.lab = 1.5)
plot(year, total, xlab = "Ano", ylab = "Média de pessoas/cômodo", pch = 20)
abline(lm(total ~ year), lty = 2) 
cor(year, total)
cor(year, total, use = "complete")

plot(rural, urban, xlab = "Média de pessoas/cômodo - rural",
     ylab = "Média de pessoas/cômodo - urbano", pch = 20)
abline(0, 1, lty = 2)
cor(rural, urban,
    use = "complete")
