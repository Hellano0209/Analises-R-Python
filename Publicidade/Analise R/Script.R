setwd("C:\\Users\\hella\\Google Drive\\Projetos\\Projetos Academicos Profissional\\Análise de Dados\\Relatórios\\Publicidade")
file = "advertising.csv"
dados = read.csv(file, head = T, sep = ";", dec = ",")
dados = dados[!(dados$Radio == 0), ]

names(dados)

cbind(
  c(summary(dados$Sales), sd=sd(dados$Sales)),
  c(summary(dados$TV), sd=sd(dados$TV)),
  c(summary(dados$Radio), sd=sd(dados$Radio)),
  c(summary(dados$Newspaper), sd=sd(dados$Newspaper))
)

x11()
par(mfrow = c(2,2))

plot(dados$Sales ~ dados$TV, 
     main = "(a) TV v.s Vendas")
lines(lowess(dados$Sales ~ dados$TV), col = "red")

hist(dados$Sales, probability = T,
     main = "(b) Histograma Vendas")
lines(density(dados$Sales), col = "red")

plot(dados$Sales ~ dados$Radio,
     main = "(c) Rádio v.s Vendas")
lines(lowess(dados$Sales ~ dados$Radio), col = "red")

plot(dados$Sales ~ dados$Newspaper,
     main = "(d) Jonal v.s Vendas")
lines(lowess(dados$Sales ~ dados$Newspaper), col = "red")


x11()
par(mfrow = c(2,2))

plot(log(dados$Sales) ~ log(dados$TV), 
     main = "(a) TV v.s Vendas")
lines(lowess(log(dados$Sales) ~ log(dados$TV)), col = "red")

hist(log(dados$Sales), probability = T,
     main = "(b) Histograma Vendas")
lines(density(log(dados$Sales)), col = "red")

plot(log(dados$Sales) ~ log(dados$Radio),
     main = "(c) Rádio v.s Vendas")
lines(lowess(log(dados$Sales) ~ log(dados$Radio)), col = "red")

plot(log(dados$Sales) ~ log(dados$Newspaper),
     main = "(d) Jonal v.s Vendas")
lines(lowess(log(dados$Sales) ~ log(dados$Newspaper)), col = "red")

  
############
# Modelo 1 #
############
mod1 = glm(Sales ~TV +  Radio + Newspaper, 
           family = Gamma(link = "identity"), 
           data = dados)
summary(mod1)

x11()
par(mfrow = c(2,2))
plot(resid(mod1, "deviance") ~ dados$TV,
     main = "(a) TV v.s. Resíduos")
lines(lowess(resid(mod1, "deviance") ~ dados$TV))
plot(resid(mod1, "deviance") ~ dados$Radio,
     main = "(b) Rádio v.s. Resíduos")
lines(lowess(resid(mod1, "deviance") ~ dados$Radio))
plot(resid(mod1, "deviance") ~ dados$Newspaper,
     main = "(c) Jornal v.s. Resíduos")
lines(lowess(resid(mod1, "deviance") ~ dados$Newspaper))

hnp(mod1, resid.type = "deviance", halfnormal = F)

############
# Modelo 2 #
############
mod2 = glm(Sales ~TV +  Radio + Newspaper, 
           family = inverse.gaussian(link = "identity"), 
           data = dados)
summary(mod2)

x11()
par(mfrow = c(2,2))
plot(resid(mod2, "deviance") ~ dados$TV,
     main = "(a) TV v.s. Resíduos")
lines(lowess(resid(mod2, "deviance") ~ dados$TV))
plot(resid(mod2, "deviance") ~ dados$Radio,
     main = "(b) Rádio v.s. Resíduos")
lines(lowess(resid(mod2, "deviance") ~ dados$Radio))
plot(resid(mod2, "deviance") ~ dados$Newspaper,
     main = "(c) Jornal v.s. Resíduos")
lines(lowess(resid(mod2, "deviance") ~ dados$Newspaper))

hnp(mod2, resid.type = "deviance", halfnormal = F)

############
# Modelo 3 #
############
mod3 = glm(Sales ~TV +  Radio + Newspaper, 
           family = gaussian(link = "identity"), 
           data = dados)
summary(mod3)

x11()
par(mfrow = c(2,2))
plot(resid(mod3, "deviance") ~ dados$TV,
     main = "(a) TV v.s. Resíduos")
lines(lowess(resid(mod3, "deviance") ~ dados$TV))
plot(resid(mod3, "deviance") ~ dados$Radio,
     main = "(b) Rádio v.s. Resíduos")
lines(lowess(resid(mod3, "deviance") ~ dados$Radio))
plot(resid(mod3, "deviance") ~ dados$Newspaper,
     main = "(c) Jornal v.s. Resíduos")
lines(lowess(resid(mod3, "deviance") ~ dados$Newspaper))

hnp(mod3, resid.type = "deviance", halfnormal = F)

############
# Modelo 4 #
############
mod4 = glm(Sales ~ log(TV) +  log(Radio) + log(Newspaper), 
           family = Gamma(link = "log"), 
           data = dados)
summary(mod4)

x11()
par(mfrow = c(2,2))
plot(resid(mod4, "deviance") ~ dados$TV,
     main = "(a) TV v.s. Resíduos")
lines(lowess(resid(mod4, "deviance") ~ dados$TV))
plot(resid(mod4, "deviance") ~ dados$Radio,
     main = "(b) Rádio v.s. Resíduos")
lines(lowess(resid(mod4, "deviance") ~ dados$Radio))
plot(resid(mod4, "deviance") ~ dados$Newspaper,
     main = "(c) Jornal v.s. Resíduos")
lines(lowess(resid(mod4, "deviance") ~ dados$Newspaper))

hnp(mod4, resid.type = "deviance", halfnormal = F)

############
# Modelo 5 #
############
mod5 = glm(Sales ~ log(TV) +  log(Radio) + log(Newspaper), 
           family = inverse.gaussian(link = "log"), 
           data = dados)
summary(mod5)

x11()
par(mfrow = c(2,2))
plot(resid(mod5, "deviance") ~ dados$TV,
     main = "(a) TV v.s. Resíduos")
lines(lowess(resid(mod5, "deviance") ~ dados$TV))
plot(resid(mod5, "deviance") ~ dados$Radio,
     main = "(b) Rádio v.s. Resíduos")
lines(lowess(resid(mod5, "deviance") ~ dados$Radio))
plot(resid(mod5, "deviance") ~ dados$Newspaper,
     main = "(c) Jornal v.s. Resíduos")
lines(lowess(resid(mod5, "deviance") ~ dados$Newspaper))

hnp(mod5, resid.type = "deviance", halfnormal = F)

############
# Modelo 6 #
############
mod6 = glm(Sales ~ log(TV) +  log(Radio) + log(Newspaper),
           family = gaussian("log"),
           data = dados)
summary(mod6)

x11()
par(mfrow = c(2,2))
plot(resid(mod6, "deviance") ~ dados$TV,
     main = "(a) TV v.s. Resíduos")
lines(lowess(resid(mod6, "deviance") ~ dados$TV))
plot(resid(mod6, "deviance") ~ dados$Radio,
     main = "(b) Rádio v.s. Resíduos")
lines(lowess(resid(mod6, "deviance") ~ dados$Radio))
plot(resid(mod6, "deviance") ~ dados$Newspaper,
     main = "(c) Jornal v.s. Resíduos")
lines(lowess(resid(mod6, "deviance") ~ dados$Newspaper))

hnp(mod6, resid.type = "deviance", halfnormal = F)

AIC(mod1, mod2, mod3, mod4, mod5, mod6)

################################
# MODELOS DE MÉDIA E DISPERÇÃO #
################################
mod7 = dglm(Sales ~ log(TV) +  Radio,
            ~ TV + I(TV^2) + Radio + I(Radio^2),
            family = quasi(link = "log", variance = "mu^2"), 
            data = dados)
summary(mod7)
coeftest(mod7)
coeftest(mod7$dispersion.fit)

x11()
par(mfrow = c(2,2))
plot(resid(mod7, "deviance") ~ dados$TV,
     main = "(a) TV v.s. Resíduos")
lines(lowess(resid(mod7, "deviance") ~ dados$TV))
plot(resid(mod7, "deviance") ~ dados$Radio,
     main = "(b) Rádio v.s. Resíduos")
lines(lowess(resid(mod7, "deviance") ~ dados$Radio))
plot(resid(mod7, "deviance") ~ dados$Newspaper,
     main = "(c) Jornal v.s. Resíduos")
lines(lowess(resid(mod7, "deviance") ~ dados$Newspaper))

hnp(mod7, resid.type = "deviance", halfnormal = F)

####################################
# MODELOS DE QUASE-VEROSSIMILHANÇA #
####################################
mod7 = glm(Sales ~ log(TV) +  Radio,
          family = quasi(link = "log", variance = "mu^2"), 
          data = dados)
summary(mod7)

x11()
par(mfrow = c(2,2))
plot(resid(mod7, "deviance") ~ dados$TV,
     main = "(a) TV v.s. Resíduos")
lines(lowess(resid(mod7, "deviance") ~ dados$TV))
plot(resid(mod7, "deviance") ~ dados$Radio,
     main = "(b) Rádio v.s. Resíduos")
lines(lowess(resid(mod7, "deviance") ~ dados$Radio))
plot(resid(mod7, "deviance") ~ dados$Newspaper,
     main = "(c) Jornal v.s. Resíduos")
lines(lowess(resid(mod7, "deviance") ~ dados$Newspaper))

hnp(mod7, resid.type = "deviance", halfnormal = F)




# Observações Influentes: 9, 53, 92, 133, 109
mod1.9 = glm(Sales ~log(TV) +  Radio + Newspaper, family = Gamma(link = "log"), 
             data = dados[-9,])
mod1.53 = glm(Sales ~log(TV) +  Radio + Newspaper, family = Gamma(link = "log"), 
             data = dados[-53,])
mod1.92 = glm(Sales ~log(TV) +  Radio + Newspaper, family = Gamma(link = "log"), 
              data = dados[-92,])
mod1.109 = glm(Sales ~log(TV) +  Radio + Newspaper, family = Gamma(link = "log"), 
    data = dados[-109,])
mod1.133 = glm(Sales ~log(TV) +  Radio + Newspaper, family = Gamma(link = "log"), 
              data = dados[-133,])
mod.all = glm(Sales ~log(TV) +  Radio + Newspaper, family = Gamma(link = "log"), 
              data = dados[-c(9,53,92,109,133),])

round(rbind(coef(mod1), coef(mod1.9), coef(mod1.53), coef(mod1.92), 
      coef(mod1.109), coef(mod1.133), coef(mod.all)),5)

# Observações Aberrantes: 2, 10, 26, 38, 77, 176
mod1.2 = glm(Sales ~log(TV) +  Radio + Newspaper, family = Gamma(link = "log"), 
             data = dados[-2,])
mod1.10 = glm(Sales ~log(TV) +  Radio + Newspaper, family = Gamma(link = "log"), 
              data = dados[-10,])
mod1.26 = glm(Sales ~log(TV) +  Radio + Newspaper, family = Gamma(link = "log"), 
              data = dados[-26,])
mod1.38 = glm(Sales ~log(TV) +  Radio + Newspaper, family = Gamma(link = "log"), 
               data = dados[-38,])
mod1.77 = glm(Sales ~log(TV) +  Radio + Newspaper, family = Gamma(link = "log"), 
               data = dados[-77,])
mod1.176 = glm(Sales ~log(TV) +  Radio + Newspaper, family = Gamma(link = "log"), 
              data = dados[-176,])
mod.all = glm(Sales ~log(TV) +  Radio + Newspaper, family = Gamma(link = "log"), 
               data = dados[-c(2,10,26,38,77,176),])

round(rbind(coef(mod1), coef(mod1.2), coef(mod1.10), coef(mod1.26), 
            coef(mod1.38), coef(mod1.77), coef(mod1.176), coef(mod.all)),10)

mod.allall = glm(Sales ~log(TV) +  Radio + Newspaper, family = Gamma(link = "log"), 
                 data = dados[-c(9,53,92,109,133,2,10,26,38,77,176),])
coef(mod.allall)
