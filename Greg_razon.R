#***************************
# LIBRERIAS ####
#***************************

library(survey)
library(dplyr)
library(TeachingSampling)
options(scipen = 999)

#*************************
# PUNTO 1 #### 
#*************************

# Cargar la muestra
mue <- readRDS("muestra_2etapas.rds")
sum(mue$fexp) # N gorro

# Real de BigLucy
data("BigLucy"); nrow(BigLucy)
mean(BigLucy$Income)

# diseño muestral
diseno <- svydesign(ids =~ Segments + ID, strata=~ estrato_segmento, fpc=~ N_h + Ni, data=mue)

#*************************
# PUNTO 1a #### 
#*************************

# Estimador de razon de dominios --> dominio=SPAM

# Variable auxiliar Taxes
svyby(~Income, denominator=~Taxes, ~SPAM, diseno, FUN=svyratio)
cv(svyby(~Income, denominator=~Taxes, ~SPAM, diseno, FUN=svyratio))*100

#*************************
# PUNTO 1b #### 
#*************************

# Estimador de razon de dominios --> dominio=SPAM

# Variable auxiliar Employees
svyby(~Income, denominator=~Employees, ~SPAM, diseno, FUN=svyratio)
cv(svyby(~Income, denominator=~Employees, ~SPAM, diseno, FUN=svyratio))*100

#*************************
# PUNTO 1c #### 
#*************************

# Estimador de razón global para Taxes
svyratio(~Income, denominator=~Taxes, design =  diseno)
cv(svyratio(~Income, denominator=~Taxes, design =  diseno))*100

# Estimador de razón global para Employess
svyratio(~Income, denominator=~Employees, design =  diseno)
cv(svyratio(~Income, denominator=~Employees, design =  diseno))*100

#*************************
# PUNTO 1d #### 
#*************************

# Estimador promedio para promedio por dominio=SPAM
svyby(~Income, ~SPAM, diseno, FUN=svymean)
cv(svyby(~Income, ~SPAM, diseno, FUN=svymean))*100

# Estimador promedio global
svymean(~Income, diseno)
cv(svymean(~Income, diseno))*100