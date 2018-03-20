#***************************
# LIBRERIAS ####
#***************************

library(survey)
library(dplyr)
library(TeachingSampling)
options(scipen = 999)

#*************************************
# 1. ESTIMACIÓN RAZÓN PARA INCOME #### 
#*************************************

# Cargar la muestra
mue <- readRDS("./data/muestra_2etapas.rds")
sum(mue$fexp) # N gorro

# Base poblacional 'BigLucy'
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


#**************************************
# 1.c. Estimador de razón globales #### 
#**************************************

#************
# TAXES
#************
# Estimador de razón global para Taxes
(Rd_Income_Taxes_global <- svyratio(~Income, denominator=~Taxes, design =  diseno))
# Coeficiente de variación
(cv_Income_Taxes_global <- cv(svyratio(~Income, denominator=~Taxes, design =  diseno))*100)
Income_Taxes_global <- data.frame(Income.Taxes = Rd_Income_Taxes_global$ratio, 
                                  se.Income.Taxes = Rd_Income_Taxes_global$var,
                                  cv.Income.Taxes = cv_Income_Taxes_global)
names(Income_Taxes_global) = c("Income.Taxes", "se.Income.Taxes", "cv.Income.Taxes")

# Ratio global real para Income/Taxes 
(Rd_IT_real_global <- sum(BigLucy$Income)/sum(BigLucy$Taxes))


#************
# EMPLOYEES
#************
# Estimador de razón global para Employess
svyratio(~Income, denominator=~Employees, design =  diseno)
cv(svyratio(~Income, denominator=~Employees, design =  diseno))*100

# Ratio global real para Income/Employees 
(Rd_IE_real_global <- sum(BigLucy$Income)/sum(BigLucy$Employees))


#*******************************************
# 1d. Estimadores del Income promedio #### 
#*******************************************

# Estimador promedio para promedio por dominio=SPAM
(svyby(~Income, ~SPAM, diseno, FUN=svymean))
(cv(svyby(~Income, ~SPAM, diseno, FUN=svymean))*100)
# Promedio real por dominio
Media_IS_real <- BigLucy %>% 
                 group_by(SPAM) %>%
                 summarise(Income=sum(Income), Empresas=n()) %>%
                 mutate(Income_promedio=Income/Empresas) %>%
                 select(-(Income), -(Empresas))
as.data.frame(Media_IS_real)


# Estimador promedio global
svymean(~Income, diseno)
cv(svymean(~Income, diseno))*100
# Promedio global real
sum(BigLucy$Income)/nrow(BigLucy)

