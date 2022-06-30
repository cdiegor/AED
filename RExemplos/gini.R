# Medidas de desigualdade

library(ineq)
x = c(2.8, 13.7, 6.8, 12.1, 1.1, 5.9, 4.5, 9.6, 2.3, 28.9, 6.7, 0.4,
      5.6, 8.0, 10.3)

summary(x)
clorenz = Lc(x)

(jmax = which.max(clorenz$p
                  - clorenz$L))

(Lmax = clorenz$p[jmax] -
    clorenz$L[jmax])

Gini(x)
names(clorenz)

plot(clorenz, main = "", ylab = "q")
segments(clorenz$p[jmax], clorenz$L[jmax], clorenz$p[jmax],
         clorenz$p[jmax],lty = 2)
