#*******************************************************************************************************
#******************************* Estimador de razón por dominios ***************************************
#*******************************************************************************************************

library(survey)
library(TeachingSampling)
library(ggplot2)

## 1. Lectura info ####
#*********************************

# Base poblacional
data("BigLucy")

# Muestra estratificada MAS
muestra_estrat <- readRDS("./data/muestra_2Etapas.rds")

## 2. Var auxiliar Taxes ####
#*********************************
# Y: Income
# X: Taxes
# Dominios: SPAM

# Relación variable auxiliar
f <- ggplot(muestra_estrat, aes(Taxes, Income)) +
  ggtitle("Relación de 'Income' vs 'Taxes'") +
  xlab("Taxes") + 
  ylab("Income")
f + geom_point(colour = "deepskyblue4", size = 3)

#**************************
# Estimador de razón
#**************************
# total poblacional para 'Taxes'
t_x <- sum(BigLucy$Taxes)

diseno_ESTMAS <- svydesign(id =~Segments + ID, strata =~ estrato_segmento, fpc = ~N_h + Ni, 
                           data = muestra_estrat)

# Estimador de razón combinada
diseno_razon <- calibrate(diseno_ESTMAS, ~Taxes-1, population = t_x, 
                          variance = 1)

# Comparación de estimador HT vs razón
(estimador_HT <- svytotal(~Income, diseno_ESTMAS))
100 * SE(estimador_HT)/estimador_HT[1]

(estimador_razon <- svytotal(~Income, diseno_razon))
100 * SE(estimador_razon)/estimador_razon[1]








