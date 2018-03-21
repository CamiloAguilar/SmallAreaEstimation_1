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
muestra_3 <- merge(muestra_3, Tamanos_Estrato)
length(muestra_3$Zone)

Tamanos_muestra<- as.data.frame(table(muestra$Zone))
names(Tamanos_muestra) <- c("Zone", "n_h")
muestra_3 <- merge(muestra_3, Tamanos_muestra)
length(muestra_3$Zone)
head(muestra_3)

# Diseño de muestra
diseno_estra <- svydesign(ids=~1, strata = ~Zone, fpc = ~N_h, data=muestra_3)

# Tamaños de Muestra por ISO y Level
table(muestra_3$ISO,muestra_3$Level)

# Número de empresas por ISO y Level
table(BigLucy$ISO,BigLucy$Level)

#******************************
# 3a. Estimador Sintético ####
#******************************
# Variable de agrupación: Level (Big, Medium, Small)
# Dominio: ISO

# Estimador Directo
est_income_total <- svyby(~Income, ~Level, diseno_estra, FUN = svytotal)
est_income_mean <- svyby(~Income, ~Level, diseno_estra, FUN = svymean)

# Estimador directo HT
Ybarra_g <- svyby(~Income, ~Level, diseno_estra, FUN = svymean)[,2]

D <- length(unique(muestra_3$ISO))
G <- length(unique(muestra_3$Level))
Ybarra_total <- t(Ybarra_g * t(matrix(1,nrow = D,ncol = G)))
N_dg <- table(BigLucy$ISO, BigLucy$Level)
total_Income <- Ybarra_total*N_dg

# Estimaciones por dominio
Ysynth_d <- rowSums(total_Income) 

# Total poblacional por Dominios
agg_ISO <- BigLucy %>% group_by(ISO) %>% summarise(Income_total=sum(Income))

# Total poblacional por Variable de agrupamiento
agg_leve <- BigLucy %>% group_by(Level) %>% summarise(Income_total=sum(Income))

# Estimador sintético  
Estimador_Sintetico <- data.frame(ISO=names(total_Income[,1]), Big=total_Income[,1], 
                                  Medium=total_Income[,2], Small=total_Income[,3],
                                  Total_poblacional=agg_ISO$Income_total)

total <- data.frame(ISO="Total_poblacional", Big=agg_leve$Income_total[1], Medium=agg_leve$Income_total[2], 
                    Small=agg_leve$Income_total[3], Total_poblacional=sum(agg_leve$Income_total))
#names(total) <- names(Estimador_Sintetico)

Estimador_Sinte <- rbind(Estimador_Sintetico, total)
rownames(Estimador_Sinte)<-NULL
Estimador_Sinte


#***************************
#Estimación de la varianza
#***************************
VarY_g <- svyby(~Income, ~Level, diseno_estra, FUN = svymean)[,3]^2 #Varianza por Level
VarYtotal <- t(VarY_g * t(matrix(1,nrow = D,ncol = G)))
N_dg2 <- table(BigLucy$ISO, BigLucy$Level)^2
A <- VarYtotal*N_dg2

# Varianza por dominio
VarYSintet_d <- rowSums(A) 
VarYSintet_d
sqrt(VarYSintet_d)/Ysynth_d*100

Estimador_var_Sintetico <- data.frame(ISO=names(A[,1]), Big=A[,1], 
                                  Medium=A[,2], Small=A[,3],
                                  Total=VarYSintet_d)

var_total <- data.frame(ISO="Total", Big=sum(Estimador_var_Sintetico$Big, na.rm = T), 
                        Medium=sum(Estimador_var_Sintetico$Medium, na.rm = T), 
                        Small=sum(Estimador_var_Sintetico$Small, na.rm = T), 
                        Total=sum(Estimador_var_Sintetico$Total, na.rm = T))

Estimador_var_Sinte <- rbind(Estimador_Sintetico, total)

Estimador_sd_Sinte <- data.frame(ISO=Estimador_var_Sinte[,1],
                                 (sqrt(Estimador_var_Sinte[,c(2,3,4,5)])/Estimador_Sinte[,c(2,3,4,5)])*100)
Estimador_sd_Sinte


#*************************
# 3.b. Estimador GREG ####
#*************************

muestra_3$ISO_SPAM <- paste(muestra_3$ISO, muestra_3$SPAM, sep = "_")
table(muestra_3$ISO_SPAM)

BigLucy$ISO_SPAM <- paste(BigLucy$ISO, BigLucy$SPAM, sep = "_")
table(BigLucy$ISO_SPAM)

# Calculo de FEXP
#muestra_3$fexp <- muestra_3$N_h/muestra_3$n_h
muestra_3$fexp <-weights(diseno_estra)

#*****************************************  
# Modelo Heterocedástico
#*****************************************
mod_GREG <- lm(Income ~ Level + Employees + Taxes, data=muestra_3, 
               weights = muestra_3$fexp*(1/muestra_3$Employees))
e <- mod_GREG$residuals
summary(mod_GREG) 

# Crear g
mod_U <- lm(Income ~ Level + Employees + Taxes, data = BigLucy)

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












