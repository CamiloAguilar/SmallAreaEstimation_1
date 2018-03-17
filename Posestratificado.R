#*******************************************************************************************************
#****************************** Estimador de postestratificación ***************************************
#*******************************************************************************************************

library(survey)
library(TeachingSampling)
library(ggplot2)
rm(list = ls())

## 1. Lectura info ####
#*********************************

# Base poblacional
data("BigLucy")

# Muestra estratificada MAS
muestra <- readRDS("./data/muestra_2Etapas.rds")

#*******************************************
## 1. Total Income con var auxiliar Level ####
#*******************************************
muestra$Spam_Level <- paste(muestra$SPAM, muestra$Level, sep="_")
head(muestra$Spam_Level)

diseno <- svydesign(ids =~ Segments + ID, 
                    strata =~ estrato_segmento, 
                    fpc =~ N_h + Ni, data = muestra)

Spam_Level_est <- svyby(~Income, ~Spam_Level, diseno, FUN=svytotal)
Spam_Level_cv <- 100 * cv(svyby(~Income, ~Spam_Level, diseno, FUN=svytotal))

#*****************************************************
## 2b. Estimación global por dominios ####
#*****************************************************
# SPAM
Spam_est <- svyby(~Income, ~SPAM, diseno, FUN=svytotal)
100 * cv(Spam_est <- svyby(~Income, ~SPAM, diseno, FUN=svytotal)) 

# Level
Level_est <- svyby(~Income, ~Level, diseno, FUN=svytotal)
100 * cv(Spam_est <- svyby(~Income, ~Level, diseno, FUN=svytotal)) 


#*****************************************************
## 2c. Estimación del promedio ####
#*****************************************************

# Posestratificado
SpamLevel_mean <- svyby(~Income, ~Spam_Level, diseno, FUN=svymean)
SpamLevel_mean_cv <- 100 * cv(svyby(~Income, ~Spam_Level, diseno, FUN=svymean))

# Por dominio SPAM
Spam_mean <- svyby(~Income, ~SPAM, diseno, FUN=svymean)
Spam_mean_cv <- 100 * cv(svyby(~Income, ~SPAM, diseno, FUN=svymean))

#*****************************************************
## 2d Estimación promedio Global ####
#*****************************************************
Income_mean <- svymean(~Income, diseno)
100 * cv(svymean(~Income, diseno))


