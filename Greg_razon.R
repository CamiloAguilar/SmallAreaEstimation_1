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
muestra <- readRDS("./data/muestra_2Etapas.rds")

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

#***********************************
# 1a Estimador de razón - con TAXES ####
#***********************************

# Estimar el ingreso por el dominio de Level

diseno <- svydesign(ids =~ Segments + ID, 
                    strata =~ estrato_segmento, 
                    fpc =~ N_h + Ni, data = muestra)

# total estimado de Income directo por dominio
Income_d_est <- svyby(~Income, ~SPAM, diseno, FUN = svytotal)
Income_d_est_cv <- 100 * cv( svyby(~Income,~SPAM, diseno, FUN = svytotal))

# Total de taxes en población Biglucy
Xd <- BigLucy %>%
      group_by(SPAM) %>%
      summarise(Taxes_d = sum(Taxes))
Xd <- as.data.frame(Xd)

# total estimado de Taxes directo por dominio
Taxes_d_est <- svyby(~Taxes, ~SPAM, diseno, FUN = svytotal)
Taxes_d_est_cv <- 100 * cv( svyby(~Taxes,~SPAM, diseno, FUN = svytotal))

# Estimador de razón por dominios

# Para SPAM=yes
Income_yes <- (Income_d_est$Income[which(Income_d_est$SPAM=="yes")] * 
              Xd$Taxes_d[which(Xd$SPAM=="yes")]) / 
              Taxes_d_est$Taxes[which(Taxes_d_est$SPAM=="yes")]
Income_yes

# Para SPAM=no
Income_no <- (Income_d_est$Income[which(Income_d_est$SPAM=="no")] * 
              Xd$Taxes_d[which(Xd$SPAM=="no")]) / 
              Taxes_d_est$Taxes[which(Taxes_d_est$SPAM=="no")]
Income_no

# Revisamos contra BigLucy real
Income_d_real <- BigLucy %>%
                   group_by(SPAM) %>%
                   summarise(Income_d_real=sum(Income))
Income_d_real

#******************************
# Varianza de la estimación
#******************************
muestra$R_d <- muestra$Income / muestra$Taxes
diseno <- svydesign(ids =~ Segments + ID, 
                    strata =~ estrato_segmento, 
                    fpc =~ N_h + Ni, data = muestra)

R_d_expandido <- data.frame(SPAM=rep(muestra$SPAM, floor(muestra$fexp)), 
                            R_d=rep(muestra$R_d, floor(muestra$fexp)))

Var_yes <- Xd$Taxes_d[Xd$SPAM=="yes"]^2 * sd(R_d_expandido$R_d[R_d_expandido$SPAM=="yes"])
cv_yes <- sqrt(Var_yes) / Income_yes * 100

Var_no <- Xd$Taxes_d[Xd$SPAM=="no"]^2 * sd(R_d_expandido$R_d[R_d_expandido$SPAM=="no"])
cv_no <- sqrt(Var_no) / Income_yes * 100

## Tabla de resultados
res1 <- data.frame(SPAM=c("no", "sisas"),
                   Income_dir=c(Income_no,Income_yes),
                   cv_Income_dir=Income_d_est_cv,
                   Taxes_dir=Taxes_d_est$Taxes,
                   cv_Taxes_dir=Taxes_d_est_cv,
                   Rd=c(Income_no,Income_yes)/Taxes_d_est$Taxes,
                   cv_Rd=c(cv_no, cv_yes))
res1

