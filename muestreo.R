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

# Base poblacional de BigLucy
data("BigLucy"); nrow(BigLucy)
mean(BigLucy$Income)

# diseño muestral
diseno <- svydesign(ids =~ Segments + ID, strata=~ estrato_segmento, fpc=~ N_h + Ni, data=mue)

#****************************************
# 1.a. Estimador de razón para Taxes #### 
#****************************************

# Estimador de razon de dominios --> dominio=SPAM

# Variable auxiliar Taxes
(Rd_Income_Taxes <- svyby(~Income, denominator=~Taxes, ~SPAM, diseno, FUN=svyratio))
# Coeficiente de variación
(cv_Rd_Income_Taxes <- cv(svyby(~Income, denominator=~Taxes, ~SPAM, diseno, FUN=svyratio))*100)
# Tabla con el coeficiente
(Income_Taxes <- data.frame(Rd_Income_Taxes, cv.Income.Taxes=c(cv_Rd_Income_Taxes)))

# Ratio real para Income/Taxes por dominios en Biglucy
Rd_IT_real <- BigLucy %>% 
              group_by(SPAM) %>%
              summarise(Income=sum(Income), Taxes=sum(Taxes)) %>%
              mutate(Ratio=Income/Taxes) %>%
              select(-(Income), -(Taxes))
as.data.frame(Rd_IT_real)

#********************************************
# 1.b. Estimador de razón para Employees #### 
#********************************************

# Estimador de razon de dominios --> dominio=SPAM

# Variable auxiliar Employees
(Rd_Income_Employees <- svyby(~Income, denominator=~Employees, ~SPAM, diseno, FUN=svyratio))
# Coeficiente de variación
(cv_Rd_Income_Employees <- cv(svyby(~Income, denominator=~Employees, ~SPAM, diseno, FUN=svyratio))*100)
# Tabla con el coeficiente
(Income_Employees <- data.frame(Rd_Income_Employees, cv.Income.Employees=c(cv_Rd_Income_Employees)))

# Ratio real para Income/Employees por dominios en Biglucy
Rd_IE_real <- BigLucy %>% 
              group_by(SPAM) %>%
              summarise(Income=sum(Income), Employees=sum(Employees)) %>%
              mutate(Ratio=Income/Employees) %>%
              select(-(Income), -(Employees))
as.data.frame(Rd_IE_real)

#*************************************************************
# 1.c. Estimador de razón globales para Taxes & Employees #### 
#*************************************************************

#************
# TAXES
#************
# Estimador de razón global para Taxes
(Rd_Income_Taxes_global <- svyratio(~Income, denominator=~Taxes, design =  diseno))
# Coeficiente de variación
(cv_Income_Taxes_global <- cv(svyratio(~Income, denominator=~Taxes, design =  diseno))*100)
# Tabla
Income_Taxes_global <- data.frame(Income.Taxes = Rd_Income_Taxes_global$ratio, 
                                  se.Income.Taxes = Rd_Income_Taxes_global$var,
                                  cv.Income.Taxes = cv_Income_Taxes_global)
names(Income_Taxes_global) = c("Income.Taxes", "se.Income.Taxes", "cv.Income.Taxes")

# Ratio global real para Income/Taxes 
(Rd_IT_real_global <- sum(BigLucy$Income)/sum(BigLucy$Taxes))

#************
# EMPLOYEES
#************
# Estimador de razón global para Taxes
(Rd_Income_Employees_global <- svyratio(~Income, denominator=~Employees, design =  diseno))
# Coeficiente de variación
(cv_Income_Employees_global <- cv(svyratio(~Income, denominator=~Employees, design =  diseno))*100)
# Tabla
Income_Employees_global <- data.frame(Income.Employees = Rd_Income_Employees_global$ratio, 
                                  se.Income.Employees = Rd_Income_Employees_global$var,
                                  cv.Income.Employees = cv_Income_Employees_global)
names(Income_Employees_global) = c("Income.Employees", "se.Income.Employees", "cv.Income.Employees")

# Ratio global real para Income/Employees 
(Rd_IE_real_global <- sum(BigLucy$Income)/sum(BigLucy$Employees))


#****************************************************
# 1.d. Estimador del Income promedio #### 
#****************************************************

# Estimador del promedio por dominio=SPAM
(mean_Income_dominio <- svyby(~Income, ~SPAM, diseno, FUN=svymean))
(cv_mean_Income_dominio <- cv(svyby(~Income, ~SPAM, diseno, FUN=svymean))*100)
# Promedio real por dominio
Media_IS_real <- BigLucy %>% 
                 group_by(SPAM) %>%
                 summarise(Income=sum(Income), Empresas=n()) %>%
                 mutate(Income_promedio=Income/Empresas) %>%
                 select(-(Income), -(Empresas))
as.data.frame(Media_IS_real)

# Estimador del promedio global
Income_global <- svymean(~Income, diseno)
cv_Income_global <- cv(svymean(~Income, diseno))*100
# Tabla
mean_income <- data.frame(mean_Income_dominio, cv=cv_mean_Income_dominio)
global_mean_income <- data.frame(SPAM="Total",Income=as.data.frame(Income_global)$mean, 
                                 se=as.data.frame(Income_global)$Income, 
                                 cv=as.data.frame(cv_Income_global)$Income)
Mean_Income <- rbind(mean_income, global_mean_income)
# Promedio global real
sum(BigLucy$Income)/nrow(BigLucy)

#******************************************************
# 2. Estimador de postestratificación por dominios #### 
#******************************************************

# Lectura de los datos de la muestra seleccionada
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

#**********
# SPAM
#**********
Spam_est <- svyby(~Income, ~SPAM, diseno, FUN=svytotal)
cv_spam_est <- 100 * cv(Spam_est <- svyby(~Income, ~SPAM, diseno, FUN=svytotal)) 
spam_est_table <- data.frame(Spam_est, cv = cv_spam_est)

#**********
# Level
#**********
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

#*****************************
# 3. Diseño Estratificado ####
#*****************************

#************************************************************
# 3.a. Diseño estratificado estrato Zone, semilla=100318 ####
#************************************************************

# Marco Muestral
data("BigLucy")
set.seed(100318)

# Descripción Diseño Estratificado
Nh <- table(BigLucy$Zone)
muh <- aggregate(Taxes ~ Zone, FUN=mean, data=BigLucy)$Taxes
sh <- aggregate(Taxes ~ Zone, FUN=sd, data=BigLucy)$Taxes
conf <- 0.95
rme <- 0.03

n_h <- ss4stm(Nh, muh, sigmah=sh, DEFFh=1, conf=conf, rme=rme)$nh
sum(n_h)

# Tamaños muestra y factores de expansión
set.seed(100318)  
estrato <- sampling::strata(data=BigLucy, stratanames="Zone", 
                                   size=n_h, method="srswor", description=FALSE)
muestra_3 <- BigLucy[estrato$ID_unit,]  
muestra_3 <- sampling::getdata(BigLucy,estrato)

# Para Income
# Variable agrupamiento Level y por el dominio ISO (no, yes)
# Agregar los Nh a cada estrato

Tamanos_Estrato <- as.data.frame(table(BigLucy$Zone))
names(Tamanos_Estrato) <- c("Zone", "N_h")  
muestra <- merge(muestra_3, Tamanos_Estrato)
length(muestra_3$Zone)

Tamanos_muestra<- as.data.frame(table(muestra$Zone))
names(Tamanos_muestra) <- c("Zone", "n_h")
muestra <- merge(muestra_3, Tamanos_muestra)
length(muestra_3$Zone)

# Diseño de muestra
diseno_estra <- svydesign(ids=~1, strata = ~Zone, fpc = ~N_h, data=muestra_3)

# Muestra
table(muestra_3$ISO,muestra$Level)

# Real
table(BigLucy$ISO,BigLucy$Level)

#******************************
# 3.a. Estimador Sintetico ####
#******************************

# Estimador Sintético Dominop: ISO 
# Estimador Directo

est_income_total <- svyby(~Income, ~Level, diseno_estra, FUN = svytotal)
est_income_mean <- svyby(~Income, ~Level, diseno_estra, FUN = svymean)

# Estimador directo HT
Ybarra_g <- svyby(~Income, ~Level, diseno_estra, FUN = svymean)[,2]

D <- length(unique(muestra$ISO))
G <- length(unique(muestra$Level))
Ybarra_pron <- t(Ybarra_g * t(matrix(1,nrow = D,ncol = G)))
N_dg <- table(BigLucy$ISO, BigLucy$Level)
totalProm <- Ybarra_pron*N_dg

# Estimaciones por dominio
Ysynth_d <- rowSums(totalProm) 

# Total poblacional por Dominios
aggregate(Income ~ ISO, data = BigLucy, FUN = sum) 
aggregate(Income ~ ISO, data = BigLucy, FUN = sum)$Income 
# Total poblacional por Variable de agrupamiento
agg_leve <- aggregate(Income ~ Level, data = BigLucy, FUN = sum) 

Estimador_Sintetico <- data.frame(ISO=names(totalProm[,1]), Big=totalProm[,1], 
                                  Medium=totalProm[,2], Small=totalProm[,3],
                                  Total=aggregate(Income ~ ISO, data = BigLucy, FUN = sum)$Income)

total <- data.frame(ISO="Total", Big=agg_leve[1,2], Medium=agg_leve[1,2], Small=agg_leve[1,2], Total=sum(agg_leve[,2]))

Estimador_Sinte <- rbind(Estimador_Sintetico, total)

#Estimación de la varianza

VarY_g <- svyby(~Income, ~Level, diseno_estra, FUN = svymean)[,3]^2
VarYpron <- t(VarY_g * t(matrix(1,nrow = D,ncol = G)))
N_dg2 <- table(BigLucy$ISO, BigLucy$Level)^2
A <- VarYpron*N_dg2

#estiamción por dominio
VarYSintet_d <- rowSums(A) 
VarYSintet_d
sqrt(VarYSintet_d)/Ysynth_d*100

Estimador_var_Sintetico <- data.frame(ISO=names(A[,1]), Big=A[,1], 
                                  Medium=A[,2], Small=A[,3],
                                  Total=VarYSintet_d)

var_total <- data.frame(ISO="Total", Big=sum(Estimador_var_Sintetico$Big), 
                        Medium=sum(Estimador_var_Sintetico$Medium), 
                        Small=sum(Estimador_var_Sintetico$Small), 
                        Total=sum(Estimador_var_Sintetico$Total))

Estimador_var_Sinte <- rbind(Estimador_Sintetico, total)

Estimador_sd_Sinte <- data.frame(ISO=Estimador_var_Sinte[,1],
                                 (sqrt(Estimador_var_Sinte[,c(2,3,4,5)])/Estimador_Sinte[,c(2,3,4,5)])*100)

#*************************
# 3.b. Estimador GREG ####
#*************************

muestra$IsoSpam <- paste(muestra$ISO,muestra$SPAM, sep = "_")
unique(muestra$IsoSpam)

BigLucy$IsoSpam <- paste(BigLucy$ISO,BigLucy$SPAM, sep = "_")
unique(BigLucy$IsoSpam)

# Calculo de FEXP
muestra$fexp <- muestra$N_h/muestra$n_h
  
# Heterocedástico
mod_GREG <- lm(Income ~ Taxes + Employees + Level, data=muestra,
                 weights = muestra$fexp*(1/muestra$Employees))

summary(mod_GREG) 

e <- mod_GREG$residuals

# Crear g
mod_U <- lm(Income ~ Taxes + Employees + Level,
              data = BigLucy)

X_U <-  model.matrix(mod_U) #MAtrix de dise?o
X_s <- model.matrix(mod_GREG)

# Dise?o de muestra
diseno_estratificado <- svydesign(ids=~1, strata = ~Zone, fpc = ~N_h, data=muestra)
fexp_k <- weights(diseno_estratificado) # 1/pi_k

# summary(pi_k)
W <- diag(fexp_k)
g <- rep(NA, nrow(muestra))


# Dominio ISO - SPAM = "no_no"

SumUd_X <- as.matrix(colSums(X_U[BigLucy$IsoSpam == "no_no", ]))
SumSd_X <- as.matrix(colSums(X_s[muestra$IsoSpam == "no_no", ]*
                               fexp_k[muestra$IsoSpam == "no_no"]))

z_dk <- as.numeric(muestra$IsoSpam == "no_no")

# i = 2
for(i in 1:nrow(muestra)){
  g[i] <-   z_dk[i] + t(SumUd_X - SumSd_X) %*%
    solve(t(X_s) %*% W %*% X_s) %*% as.matrix(X_s[i,])
}
summary(g)

yGreg_no_no <- sum(muestra$Income *  g  * fexp_k)
aggregate(Income ~ IsoSpam, FUN = sum, data = BigLucy)

muestra$U <- g * mod_GREG$residuals
diseno_ESTMAS <- svydesign(ids=~1, strata = ~Zone, fpc = ~N_h, data=muestra)

svytotal(~U, diseno_ESTMAS)
100 * svytotal(~U, diseno_ESTMAS) / yGreg_no_no












