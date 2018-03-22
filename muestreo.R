#***************************
# LIBRERIAS ####
#***************************

library(survey)
library(dplyr)
library(reshape2)
library(stringr)
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
mean_income <- data.frame(Income_global, cv=cv_Income_global)
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

# Tabla resultados de estimación
spam <- NULL
level <- NULL
p <- str_split(Spam_Level_est$Spam_Level, pattern = "_")
for (i in 1:length(p)) {
  spam <- c(spam, p[[i]][1])
  level <- c(level, p[[i]][2])
}
Spam_Level_est_table <- data.frame(SPAM=spam, Level=level, Income=Spam_Level_est$Income, cv = Spam_Level_cv)
Spam_Level_est_table <- melt(Spam_Level_est_table, id.vars= c("SPAM", "Level"), 
                             measure.vars = c("Income", "cv")) %>%
                        dcast(SPAM ~ Level + variable, value.var = "value")
Spam_Level_est_table

# Resultados poblacionales
Spam_Level_real_table <- BigLucy %>%
                         group_by(SPAM, Level) %>%
                         summarise(Income=sum(Income)) %>%
                         dcast(SPAM ~ Level, value.var = "Income")
Spam_Level_real_table    

#*****************************************
# 2.b. Estimación global por dominios #### 
#*****************************************

#**********
# SPAM
#**********
(Spam_est <- svyby(~Income, ~SPAM, diseno, FUN=svytotal))
(cv_spam_est <- 100 * cv(Spam_est <- svyby(~Income, ~SPAM, diseno, FUN=svytotal)))
spam_est_table <- data.frame(Spam_est, cv = cv_spam_est)
# Resultado poblacional
Spam_real <- BigLucy %>%
             group_by(SPAM) %>%
             summarise(Total_Income=sum(Income))
data.frame(Spam_real)

#**********
# Level
#**********
(Level_est <- svyby(~Income, ~Level, diseno, FUN=svytotal))
(cv_Level_est <- 100 * cv(Spam_est <- svyby(~Income, ~Level, diseno, FUN=svytotal)))
level_est_table <- data.frame(Level_est, cv = cv_Level_est)
# Resultado poblacional
Level_est <- BigLucy %>%
             group_by(Level) %>%
             summarise(Total_Income=sum(Income))
data.frame(Level_est)


#*********************************
# 2c. Estimación del promedio ####
#*********************************

#**************
# Promedio
#**************
(Spam_Level_est_mean <- svyby(~Income, ~Spam_Level, diseno, FUN=svymean))
(cv_Spam_Level_mean <- 100 * cv(svyby(~Income, ~Spam_Level, diseno, FUN=svymean)))
Spam_Level_est_mean_table <- data.frame(Spam_Level_est_mean, cv = cv_Spam_Level_mean)

# Tabla resultados de estimación
spam <- NULL
level <- NULL
p <- str_split(Spam_Level_est_mean_table$Spam_Level, pattern = "_")
for (i in 1:length(p)) {
  spam <- c(spam, p[[i]][1])
  level <- c(level, p[[i]][2])
}
Spam_Level_est_mean_table <- data.frame(SPAM=spam, Level=level, Income=Spam_Level_est_mean$Income, 
                                        cv = cv_Spam_Level_mean)
Spam_Level_est_mean_table <- melt(Spam_Level_est_mean_table, id.vars= c("SPAM", "Level"), 
                             measure.vars = c("Income", "cv")) %>%
                             dcast(SPAM ~ Level + variable, value.var = "value")
Spam_Level_est_mean_table

# Resultados poblacionales
Spam_Level_real_mean_table <- BigLucy %>%
                              group_by(SPAM, Level) %>%
                              summarise(Income_mean=mean(Income)) %>%
                              dcast(SPAM ~ Level, value.var = "Income_mean")
Spam_Level_real_mean_table    


#**************
# Global
#**************
# SPAM
(Spam_est_mean <- svyby(~Income, ~SPAM, diseno, FUN=svymean))
(cv_spam_est_mean <- 100 * cv(Spam_est <- svyby(~Income, ~SPAM, diseno, FUN=svymean)))
(spam_est_table_mean <- data.frame(Spam_est_mean, cv = cv_spam_est_mean))
# SPAM poblacional
Spam_real_mean <- BigLucy %>%
                  group_by(SPAM) %>%
                  summarise(Income_mean=mean(Income))
data.frame(Spam_real_mean)

# Level
(Level_est_mean <- svyby(~Income, ~Level, diseno, FUN=svymean))
(cv_Level_est_mean <- 100 * cv(Spam_est <- svyby(~Income, ~Level, diseno, FUN=svymean)))
(level_est_table <- data.frame(Level_est_mean, cv = cv_Level_est_mean))
# Level poblacional
Level_real_mean <- BigLucy %>%
                   group_by(Level) %>%
                   summarise(Income_mean=mean(Income))
data.frame(Level_real_mean)


#************************************************************
# 3 Diseño estratificado estrato Zone ####
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
muestra_3 <- merge(muestra_3, Tamanos_Estrato)
length(muestra_3$Zone)

Tamanos_muestra<- as.data.frame(table(muestra$Zone))
names(Tamanos_muestra) <- c("Zone", "n_h")
muestra_3 <- merge(muestra_3, Tamanos_muestra)
length(muestra_3$Zone)
head(muestra_3)

# Diseño de muestra
diseno_estra <- svydesign(ids=~1, strata = ~Zone, fpc = ~N_h, data=muestra_3)
#muestra_3$fex <- weights(diseno_estra)

# Tamaños de Muestra por ISO y Level
table(muestra_3$ISO, muestra_3$Level)

# Número de empresas por ISO y Level
table(BigLucy$ISO,BigLucy$Level)

#******************************
# 3a. Estimador Sintético ####
#******************************
# Variable de agrupación: Level (Big, Medium, Small)
# Dominio: ISO

# Estimador directo HT
Ybarra_g <- svyby(~Income, ~Level, diseno_estra, FUN = svymean)[,2]

D <- length(unique(muestra_3$ISO))
G <- length(unique(muestra_3$Level))

# Matriz con promedios por columna
Ybarpron <- t(Ybarra_g * t(matrix(1,nrow = D,ncol = G)))

# Tamaño de las celdas
N_dg <- table(BigLucy$ISO, BigLucy$Level)

# Estimaciones por dominio
total_Income <- Ybarpron*N_dg
Ysynth_d <- rowSums(total_Income) 

# Estimador sintético  
Estimador_Sintetico <- data.frame(ISO=names(total_Income[,1]), Big=total_Income[,1], 
                                  Medium=total_Income[,2], Small=total_Income[,3],
                                  ISO_estimado=Ysynth_d)
rownames(Estimador_Sintetico)<-NULL
Estimador_Sintetico


#***************************
#Estimación de la varianza
#***************************
VarY_g <- svyby(~Income, ~Level, diseno_estra, FUN = svymean)[,3]^2 #Varianza por Level
VarYtotal <- t(VarY_g * t(matrix(1,nrow = D,ncol = G)))
N_dg2 <- table(BigLucy$ISO, BigLucy$Level)^2
A <- VarYtotal*N_dg2

# Varianza por dominio
VarYSynth_d <- rowSums(A) 
sd_Ysynth_d <- sqrt(VarYSynth_d)/Ysynth_d*100
sd_Ysynth_d

#**************
# Tabla final
#**************
Estimador_Sintetico$sd_ISO <- sd_Ysynth_d
Estimador_Sintetico



#*************************
# 3.b. Estimador GREG ####
#*************************

# Cruce de la variable ISO y SPAM
muestra_3$ISO_SPAM <- paste(muestra_3$ISO, muestra_3$SPAM, sep = "_")
table(muestra_3$ISO_SPAM)

BigLucy$ISO_SPAM <- paste(BigLucy$ISO, BigLucy$SPAM, sep = "_")
table(BigLucy$ISO_SPAM)

# Redefinimos el diseño incluyendo la nueva variable
diseno_strata <- svydesign(ids=~1, strata = ~Zone, fpc = ~N_h, data=muestra_3)
muestra_3$fex <- weights(diseno_strata)

#*****************************************  
# Modelo Heterocedástico
#*****************************************
mod_GREG <- lm(Income ~ Level + Employees + Taxes, data=muestra_3, 
               weights = muestra_3$fex*(1/muestra_3$Employees))
e <- mod_GREG$residuals
summary(mod_GREG) 

# Crear g
mod_U <- lm(Income ~ Level + Employees + Taxes, data = BigLucy)

# Matriz de diseño
X_U <-  model.matrix(mod_U) 
X_s <- model.matrix(mod_GREG)
W <- diag(muestra_3$fex)
g <- rep(NA, nrow(muestra_3))

#*********************************
# Dominio ISO - SPAM = "no_no"
#*********************************
SumUd_X <- as.matrix(colSums(X_U[BigLucy$ISO_SPAM == "no_no", ]))
SumSd_X <- as.matrix(colSums(X_s[muestra_3$ISO_SPAM == "no_no", ]*
                               muestra_3$fex[muestra_3$ISO_SPAM == "no_no"]))

z_dk <- as.numeric(muestra_3$ISO_SPAM == "no_no")

# Calculamos el factor de calibración g
for(i in 1:nrow(muestra_3)){
  g[i] <- z_dk[i] + t(SumUd_X - SumSd_X) %*%
    solve(t(X_s) %*% W %*% X_s) %*% as.matrix(X_s[i,])
}
summary(g)

yGreg_no_no <- sum(muestra_3$Income *  g  * muestra_3$fex)
saveRDS(yGreg_no_no, file = "./results/yGreg_no_no.rds")
# Resultado poblacional:
aggregate(Income ~ ISO_SPAM, FUN = sum, data = BigLucy)

# Varianza
muestra_3$U <- g * mod_GREG$residuals
diseno_ESTMAS <- svydesign(ids=~1, strata = ~Zone, fpc = ~N_h, data=muestra_3)

svytotal(~U, diseno_ESTMAS)
cv_no_no <- 100 * svytotal(~U, diseno_ESTMAS) / yGreg_no_no

greg_no_no <- c(yGreg_no_no, cv_no_no)
saveRDS(greg_no_no, file = "./results/greg_no_no.rds")


#************************************
# Dominio ISO - SPAM = "no_yes"
#************************************
SumUd_X <- as.matrix(colSums(X_U[BigLucy$ISO_SPAM == "no_yes", ]))
SumSd_X <- as.matrix(colSums(X_s[muestra_3$ISO_SPAM == "no_yes", ]*
                               muestra_3$fex[muestra_3$ISO_SPAM == "no_yes"]))

z_dk <- as.numeric(muestra_3$ISO_SPAM == "no_yes")

# Calculamos el factor de calibración g
for(i in 1:nrow(muestra_3)){
  g[i] <-   z_dk[i] + t(SumUd_X - SumSd_X) %*%
    solve(t(X_s) %*% W %*% X_s) %*% as.matrix(X_s[i,])
}
summary(g)

yGreg_no_yes <- sum(muestra_3$Income *  g  * muestra_3$fex)
saveRDS(yGreg_no_yes, file = "./results/yGreg_no_yes.rds")
# Resultado poblacional:
aggregate(Income ~ ISO_SPAM, FUN = sum, data = BigLucy)

# Varianza
muestra_3$U2 <- g * mod_GREG$residuals
diseno_ESTMAS <- svydesign(ids=~1, strata = ~Zone, fpc = ~N_h, data=muestra_3)

svytotal(~U2, diseno_ESTMAS)
cv_no_yes <- 100 * svytotal(~U2, diseno_ESTMAS) / yGreg_no_yes

greg_no_yes <- c(yGreg_no_yes, cv_no_yes)
saveRDS(greg_no_yes, file = "./results/greg_no_yes.rds")

#**********************************
# Dominio ISO - SPAM = "yes_yes"
#**********************************
SumUd_X <- as.matrix(colSums(X_U[BigLucy$ISO_SPAM == "yes_yes", ]))
SumSd_X <- as.matrix(colSums(X_s[muestra_3$ISO_SPAM == "yes_yes", ]*
                               muestra_3$fex[muestra_3$ISO_SPAM == "yes_yes"]))

z_dk <- as.numeric(muestra_3$ISO_SPAM == "yes_yes")

# Calculamos el factor de calibración g
for(i in 1:nrow(muestra_3)){
  g[i] <-   z_dk[i] + t(SumUd_X - SumSd_X) %*%
    solve(t(X_s) %*% W %*% X_s) %*% as.matrix(X_s[i,])
}
summary(g)

yGreg_yes_yes <- sum(muestra_3$Income *  g  * muestra_3$fex)
saveRDS(yGreg_yes_yes, file = "./results/yGreg_yes_yes.rds")
# Resultado poblacional:
aggregate(Income ~ ISO_SPAM, FUN = sum, data = BigLucy)

# Varianza
muestra_3$U3 <- g * mod_GREG$residuals
diseno_ESTMAS <- svydesign(ids=~1, strata = ~Zone, fpc = ~N_h, data=muestra_3)

svytotal(~U3, diseno_ESTMAS)
cv_yes_yes <- 100 * svytotal(~U3, diseno_ESTMAS) / yGreg_yes_yes

greg_yes_yes <- c(yGreg_yes_yes, cv_yes_yes)
saveRDS(greg_yes_yes, file = "./results/greg_yes_yes.rds")

#*********************************
# Dominio ISO - SPAM = "yes_no"
#*********************************
SumUd_X <- as.matrix(colSums(X_U[BigLucy$ISO_SPAM == "yes_no", ]))
SumSd_X <- as.matrix(colSums(X_s[muestra_3$ISO_SPAM == "yes_no", ]*
                               muestra_3$fex[muestra_3$ISO_SPAM == "yes_no"]))

z_dk <- as.numeric(muestra_3$ISO_SPAM == "yes_no")

# Calculamos el factor de calibración g
for(i in 1:nrow(muestra_3)){
  g[i] <-   z_dk[i] + t(SumUd_X - SumSd_X) %*%
    solve(t(X_s) %*% W %*% X_s) %*% as.matrix(X_s[i,])
}
summary(g)

yGreg_yes_no <- sum(muestra_3$Income *  g  * muestra_3$fex)
saveRDS(yGreg_yes_no, file = "./results/yGreg_yes_no.rds")
# Resultado poblacional
aggregate(Income ~ ISO_SPAM, FUN = sum, data = BigLucy)

# Varianza
muestra_3$U4 <- g * mod_GREG$residuals
diseno_ESTMAS <- svydesign(ids=~1, strata = ~Zone, fpc = ~N_h, data=muestra_3)

svytotal(~U4, diseno_ESTMAS)
cv_yes_no <- 100 * svytotal(~U4, diseno_ESTMAS) / yGreg_yes_no

greg_yes_no <- c(yGreg_yes_no, cv_yes_no)
saveRDS(greg_yes_no, file = "./results/greg_yes_no.rds")

#********************************************
# Resultado final por cada dominio SPAM-ISO
#********************************************
esti_greg <- abs(data.frame(greg_no_no,greg_no_yes,greg_yes_yes,greg_yes_no))
rownames(esti_greg)<-c("Income", "cv")
names(esti_greg)<-c("Income_no_no", "Income_no_yes", "Income_yes_yes", "Income_yes_no")
esti_greg

saveRDS(esti_greg, file = "./results/esti_greg.rds")









