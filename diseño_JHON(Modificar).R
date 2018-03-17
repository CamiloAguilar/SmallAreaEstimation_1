rm(list = ls())
options(scipen=999)
library(survey)
library(TeachingSampling)

########### Base de datos (muestra)
### Marco
data("BigLucy")
set.seed(100318)

## Diseño estratificado
Nh <- summary(BigLucy$Zone)
muh <- tapply(BigLucy$Taxes, BigLucy$Zone, mean)
sigmah <- tapply(BigLucy$Taxes, BigLucy$Zone, sd)


BigLucy <- BigLucy[order(BigLucy$Zone),]

n_h <- ss4stm(Nh, muh, sigmah, DEFFh=1.4, conf = 0.95, rme = 0.05)$nh

table(BigLucy$Zone)

?sampling::strata
set.seed(100318)  
BigLucy <- BigLucy[order(BigLucy$Zone),]

indica_estrato <- sampling::strata(data=BigLucy, stratanames="Zone", size=n_h, method="srswor", description=FALSE)

muestra_ESTMAS <- BigLucy[indica_estrato$ID_unit,]  
muestra_ESTMAS <- sampling::getdata(BigLucy,indica_estrato) # construye las factores de expansi?n


#y:income
#Variable agrupamiento Level y por el dominio ISO (no, yes)
#agregar los Nh a cada estrato
tamanosEstrato<- as.data.frame(table(BigLucy$Zone))  # convierte la tabla en una base
names(tamanosEstrato) <- c("Zone", "N_h")  # le coloca estos nombres
muestra_ESTMAS<- merge(muestra_ESTMAS, tamanosEstrato)

tamanosmuestra<- as.data.frame(table(muestra_ESTMAS$Zone))  # convierte la tabla en una base
names(tamanosmuestra) <- c("Zone", "n_h")  # le coloca estos nombres
muestra_ESTMAS<- merge(muestra_ESTMAS, tamanosmuestra)


head(muestra_ESTMAS)


# Dise?o de muestra
diseno_ESTMAS <- svydesign(ids=~1, strata = ~Zone, fpc = ~N_h, data=muestra_ESTMAS)

table(muestra_ESTMAS$Level)
table(muestra_ESTMAS$ISO)

table(muestra_ESTMAS$ISO,muestra_ESTMAS$Level)
table(BigLucy$ISO,BigLucy$Level)






#Estimador Sint?tico
#variable de agrupaci?n Level (big, medium, small)
#dominio ISO

#Estimador Directo
svyby(~Income, ~Level, diseno_ESTMAS, FUN = svymean)

#guardar en vector la segunda columna
y_g <-svyby(~Income, ~Level, diseno_ESTMAS, FUN = svymean)[,2]

D<-length(unique(muestra_ESTMAS$ISO))
G<-length(unique(muestra_ESTMAS$Level))
Ypron<- t(y_g * t(matrix(1,nrow = D,ncol = G)))
N_dg <- table(BigLucy$ISO, BigLucy$Level)

totalProm<-Ypron*N_dg


YsinteticoxDominio <- rowSums(totalProm) #Estimaciones por dominio

aggregate(Income ~ ISO, data = BigLucy, FUN = sum) #total poblacional por Dominios
aggregate(Income ~ Level, data = BigLucy, FUN = sum) #total poblacional por Variable de agrupamiento



#Estimaci?n de la varianza

VarY_g <-svyby(~Income, ~Level, diseno_ESTMAS, FUN = svymean)[,3]^2

VarYpron<- t(VarY_g * t(matrix(1,nrow = D,ncol = G)))
N_dg2 <- table(BigLucy$ISO, BigLucy$Level)^2

A<- VarYpron*N_dg2
VarYSintet_d <- rowSums(A) #estiamci?n por dominio
VarYSintet_d
sqrt(VarYSintet_d)/YsinteticoxDominio*100


########### Estimador GREG


muestra_ESTMAS$IsoSpam <- paste(muestra_ESTMAS$ISO,muestra_ESTMAS$SPAM, sep = "_")
unique(muestra_ESTMAS$IsoSpam)

BigLucy$IsoSpam <- paste(BigLucy$ISO,BigLucy$SPAM, sep = "_")
unique(BigLucy$IsoSpam)


muestra_ESTMAS$fexp <- muestra_ESTMAS$N_h/muestra_ESTMAS$n_h

#HeterocedÃ¡stico
modeloGREG <- lm(Income ~ Taxes + Employees + Level, data=muestra_ESTMAS,
                 weights = muestra_ESTMAS$fexp*(1/muestra_ESTMAS$Employees))  

summary(modeloGREG) 

e <- modeloGREG$residuals
# Crear g
modeloU <- lm(Income ~ Taxes + Employees + Level,
              data = BigLucy)

X_U <-  model.matrix(modeloU) #MAtrix de dise?o
X_s <- model.matrix(modeloGREG)

# Dise?o de muestra
diseno_ESTMAS <- svydesign(ids=~1, strata = ~Zone, fpc = ~N_h, data=muestra_ESTMAS)

fexp_k <- weights(diseno_ESTMAS) # 1/pi_k
# summary(pi_k)
W <- diag(fexp_k)
g <- rep(NA, nrow(muestra_ESTMAS))


# Dominio ISO - SPAM = "no_no"

SumUd_X <- as.matrix(colSums(X_U[BigLucy$IsoSpam == "no_no", ]))
SumSd_X <- as.matrix(colSums(X_s[muestra_ESTMAS$IsoSpam == "no_no", ]*
                               fexp_k[muestra_ESTMAS$IsoSpam == "no_no"]))

z_dk <- as.numeric(muestra_ESTMAS$IsoSpam == "no_no")
#i = 2
for(i in 1:nrow(muestra_ESTMAS)){
  g[i] <-   z_dk[i] + t(SumUd_X - SumSd_X) %*%
    solve(t(X_s) %*% W %*% X_s) %*% as.matrix(X_s[i,])
}
summary(g)

yGreg_no_no <- sum(muestra_ESTMAS$Income *  g  * fexp_k)
aggregate(Income ~ IsoSpam, FUN = sum, data = BigLucy)

muestra_ESTMAS$U <- g * modeloGREG$residuals
diseno_ESTMAS <- svydesign(ids=~1, strata = ~Zone, fpc = ~N_h, data=muestra_ESTMAS)

svytotal(~U, diseno_ESTMAS)
100 * svytotal(~U, diseno_ESTMAS) / yGreg_no_no