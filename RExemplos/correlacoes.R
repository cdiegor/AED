#Covariância e correlação

x = c(5.5,6.7,9.5,4.2,9.0,11.6,4.5,9.6,6.2,11.6,8.8,8.6,7.8,4.8,
        10.1)
y = c(11.6,11.3,17.5,9.1,15.7,16.9,8.1,21.2,11.7,18.7,13.9,15.0,
        11.6,7.0, 15.6)
plot(x, y, pch = 20)
cov(x, y)
cor(x, y)

##### Exemplo USAArrests ####
? USArrests

# Apresentação
class(USArrests)
names(USArrests)
rownames(USArrests)

# Sumário
summary(USArrests)

# Mostrando os pares
pairs(USArrests, pch = 20)

# Definindo uma ordem e renomeando os dados
ordem = c("Murder", "Assault",
          "Rape", "UrbanPop")
nomes = c("Homicídio", "Assalto",
          "Estupro", "População \n urbana (%)")

# Refazendo os pares com nova ordem e nomes
pairs(USArrests[, ordem], pch = 20,
      labels = nomes)

# Mostrando as covariâncias e correlações
cov(USArrests[, ordem])
cor(USArrests[, ordem])

# Um novo painel
# Triangular superior mostrando os valores.
panel.cor = function(x, y,
                     digits = 3)
{
  usr = par("usr")
  on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  r = cor(x, y)
  text(0.5, 0.5, round(r,
                       digits), cex = 1.5)
}

pairs(USArrests[, ordem],
      labels = nomes, upper.panel =
        panel.cor)

# Um painel sem a parte inferior
pairs(USArrests[, ordem],
      labels = nomes, lower.panel =
        NULL)


# Um painel com linhas de correlações e tendências
pairs(USArrests[, ordem],
      labels = nomes, upper.panel =
        panel.smooth, lower.panel =
        panel.cor) 

pairs(USArrests[, ordem], labels = nomes, upper.panel =
        panel.smooth, lower.panel = panel.cor, diag.panel = panel.hist) 

