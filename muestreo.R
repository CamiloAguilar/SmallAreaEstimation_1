#***************************
# LIBRERIAS ####
#***************************

library(survey)
library(dplyr)
library(TeachingSampling)
library(samplesize4surveys)
options(scipen = 999)

#*************************************
# 1. ESTIMACIÓN RAZÓN PARA INCOME #### 
#*************************************

# Cargar la muestra
mue <- readRDS("./data/muestra_2etapas.rds")
muestra <- readRDS("./data/muestra_2etapas.rds")
sum(mue$fexp) # N gorro

# Real de BigLucy
data("BigLucy"); nrow(BigLucy)
mean(BigLucy$Income)

# diseño muestral
diseno <- svydesign(ids =~ Segments + ID, strata=~ estrato_segmento, fpc=~ N_h + Ni, data=mue)

#****************************************
# 1.a. Estimador de razón para Taxes #### 
#****************************************

# Estimador de razon de dominios --> dominio=SPAM

# Variable auxiliar Taxes
Rd_Income_Taxes <- svyby(~Income, denominator=~Taxes, ~SPAM, diseno, FUN=svyratio)
# Coeficiente de variación
cv_Rd_Income_Taxes <- cv(svyby(~Income, denominator=~Taxes, ~SPAM, diseno, FUN=svyratio))*100
# Tabla con el coeficiente
Income_Taxes <- data.frame(Rd_Income_Taxes, cv.Income.Taxes=c(cv_Rd_Income_Taxes))

#********************************************
# 1.b. Estimador de razón para Employees #### 
#********************************************

# Estimador de razon de dominios --> dominio=SPAM

# Variable auxiliar Employees
Rd_Income_Employees <- svyby(~Income, denominator=~Employees, ~SPAM, diseno, FUN=svyratio)
# Coeficiente de variación
cv_Rd_Income_Employees <- cv(svyby(~Income, denominator=~Employees, ~SPAM, diseno, FUN=svyratio))*100
# Tabla con el coeficiente
Income_Employees <- data.frame(Rd_Income_Employees, cv.Income.Employees=c(cv_Rd_Income_Employees))

#*************************************************************
# 1.c. Estimador de razón globales para Taxes & Employees #### 
#*************************************************************

# TAXES
# Estimador de razón global para Taxes
Rd_Income_Taxes_global <- svyratio(~Income, denominator=~Taxes, design =  diseno)
# Coeficiente de variación
cv_Income_Taxes_global <- cv(svyratio(~Income, denominator=~Taxes, design =  diseno))*100
# Tabla
Income_Taxes_global <- data.frame(Income.Taxes = Rd_Income_Taxes_global$ratio, 
                                  se.Income.Taxes = Rd_Income_Taxes_global$var,
                                  cv.Income.Taxes = cv_Income_Taxes_global)
names(Income_Taxes_global) = c("Income.Taxes", "se.Income.Taxes", "cv.Income.Taxes")

# EMPLOYEES
# Estimador de razón global para Taxes
Rd_Income_Employees_global <- svyratio(~Income, denominator=~Employees, design =  diseno)
# Coeficiente de variación
cv_Income_Employees_global <- cv(svyratio(~Income, denominator=~Employees, design =  diseno))*100
# Tabla
Income_Employees_global <- data.frame(Income.Employees = Rd_Income_Employees_global$ratio, 
                                  se.Income.Employees = Rd_Income_Employees_global$var,
                                  cv.Income.Employees = cv_Income_Employees_global)
names(Income_Employees_global) = c("Income.Employees", "se.Income.Employees", "cv.Income.Employees")

#****************************************************
# 1.d. Estimador promedio para Taxes & Employees #### 
#****************************************************

# Estimador promedio para promedio por dominio=SPAM
mean_Income_dominio <- svyby(~Income, ~SPAM, diseno, FUN=svymean)
cv_mean_Income_dominio <- cv(svyby(~Income, ~SPAM, diseno, FUN=svymean))*100
# Estimador promedio global
Income_global <- svymean(~Income, diseno)
cv_Income_global <- cv(svymean(~Income, diseno))*100
# Tabla
mean_income <- data.frame(mean_Income_dominio, cv=cv_mean_Income_dominio)
global_mean_income <- data.frame(SPAM="Total",Income=as.data.frame(Income_global)$mean, 
                                 se=as.data.frame(Income_global)$Income, 
                                 cv=as.data.frame(cv_Income_global)$Income)
Mean_Income <- rbind(mean_income, global_mean_income)

#****************************************
# 2. Total Income posestratificación #### 
#****************************************

muestra <- readRDS("./data/muestra_2etapas.rds")

#**********************************************
# 2.a. Total Income con var auxiliar Level #### 
#**********************************************

muestra$Spam_Level <- paste(muestra$SPAM, muestra$Level, sep="_")
table(muestra$Spam_Level)

diseno <- svydesign(ids =~ Segments + ID, strata =~ estrato_segmento, 
                    fpc =~ N_h + Ni, data = muestra)

Spam_Level_est <- svyby(~Income, ~Spam_Level, diseno, FUN=svytotal)
Spam_Level_cv <- 100 * cv(svyby(~Income, ~Spam_Level, diseno, FUN=svytotal))

Spam_Level_est_table <- data.frame(Spam_Level_est, cv = Spam_Level_cv)

#*****************************************
# 2.b. Estimación global por dominios #### 
#*****************************************

# SPAM
Spam_est <- svyby(~Income, ~SPAM, diseno, FUN=svytotal)
cv_spam_est <- 100 * cv(Spam_est <- svyby(~Income, ~SPAM, diseno, FUN=svytotal)) 
spam_est_table <- data.frame(Spam_est, cv = cv_spam_est)

# Level
Level_est <- svyby(~Income, ~Level, diseno, FUN=svytotal)
cv_Level_est <- 100 * cv(Spam_est <- svyby(~Income, ~Level, diseno, FUN=svytotal)) 
level_est_table <- data.frame(Level_est, cv = cv_Level_est)

#*********************************
# 2c. Estimación del promedio ####
#*********************************

# Promedio
Spam_Level_est_mean <- svyby(~Income, ~Spam_Level, diseno, FUN=svymean)
cv_Spam_Level_mean <- 100 * cv(svyby(~Income, ~Spam_Level, diseno, FUN=svymean))
Spam_Level_est_mean_table <- data.frame(Spam_Level_est_mean, cv = cv_Spam_Level_mean)

# Global
# SPAM
Spam_est_mean <- svyby(~Income, ~SPAM, diseno, FUN=svymean)
cv_spam_est_mean <- 100 * cv(Spam_est <- svyby(~Income, ~SPAM, diseno, FUN=svymean)) 
spam_est_table_mean <- data.frame(Spam_est_mean, cv = cv_spam_est_mean)

# Level
Level_est_mean <- svyby(~Income, ~Level, diseno, FUN=svymean)
cv_Level_est_mean <- 100 * cv(Spam_est <- svyby(~Income, ~Level, diseno, FUN=svymean)) 
level_est_table <- data.frame(Level_est_mean, cv = cv_Level_est_mean)

#**********************************************************
# 3. Diseño estratificado estrato Zone, semilla=100318 ####
#**********************************************************

# Marco Muestral
data("BigLucy")
set.seed(100318)

# Diseño estratificado
Nh <- summary(BigLucy$Zone)
muh <- tapply(BigLucy$Taxes, BigLucy$Zone, mean)
sigmah <- tapply(BigLucy$Taxes, BigLucy$Zone, sd)

BigLucy <- BigLucy[order(BigLucy$Zone),]

n_h <- ss4stm(Nh, muh, sigmah, DEFFh=1.4, conf = 0.95, rme = 0.05)$nh

table(BigLucy$Zone)

?sampling::strata
set.seed(100318)  

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












