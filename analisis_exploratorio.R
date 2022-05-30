################################################################################
#####################        ANÁLISIS EXPLORATORIO         #####################
################################################################################
library(polycor)
library(lavaan)
library(tidyverse)
library(psych)
library(lmtest)
library(plm)
library(lme4)
library(sandwich)
library(AER)
library(texreg)
library(survey)
library(MCMCglmm)
library(mice)
rm(list=ls())

user <- "Camila Ortiz"
#user <- "Matias"

setwd(paste0("C:/Users/", user, "/Dropbox/Artículo Polarización 2.0/Datos"))

#Para leer modelos pgmm con texreg
source(paste0("C:/Users/", user, "/Dropbox/Proyectos/Confianza social y politica/Codigo/pgmm_summary_fix.R"))


##-------------------------------Correlaciones--------------------------------##
load("Elsoc_Wide_2016_2021_REC.Rdata")

#Seleccionar muestra 1
elw <- subset(elw, muestra==1)

#How many waves did respondents respond
elw$n_enc <- apply(!is.na(elw[,grep("m0_sexo",names(elw))]), 1, sum, na.rm=T)
table(elw$n_enc)

#How many L-R questions did respondents respond
elw$n_lrscale <- apply(!is.na(elw[,grep("izqder_", names(elw))]), 1, sum, na.rm=T)
sum(table(elw$n_lrscale[elw$n_enc>=4]))

#How many poeple responde waves 3, 4 and 5
izqder3 <- with(elw, apply(cbind(!is.na(izqder_w03), !is.na(izqder_w04), 
                                 !is.na(izqder_w05)), 1, sum))
table(izqder3)

#Manifiesta posición en escala ideológica
with(elw, cor(cbind(elw$pos_ideo_w01, elw$pos_ideo_w02, elw$pos_ideo_w03, 
                    elw$pos_ideo_w04, elw$pos_ideo_w05), use="pairwise.complete.obs"))
#       [,1]      [,2]      [,3]      [,4]      [,5]
#[1,] 1.0000000 0.2749832 0.2326817 0.2404695 0.2211018
#[2,] 0.2749832 1.0000000 0.3255666 0.3425908 0.2746526
#[3,] 0.2326817 0.3255666 1.0000000 0.3500249 0.2640187
#[4,] 0.2404695 0.3425908 0.3500249 1.0000000 0.3101675
#[5,] 0.2211018 0.2746526 0.2640187 0.3101675 1.0000000

#Posición ideológica (continua)
with(elw, cor(cbind(elw$izqder_w01, elw$izqder_w02, elw$izqder_w03, 
                    elw$izqder_w04, elw$izqder_w05), use="pairwise.complete.obs"))
#          [,1]      [,2]      [,3]      [,4]      [,5]
#[1,] 1.0000000 0.5843306 0.5161926 0.5358415 0.4686988
#[2,] 0.5843306 1.0000000 0.6138224 0.5370899 0.5233336
#[3,] 0.5161926 0.6138224 1.0000000 0.5967131 0.5366317
#[4,] 0.5358415 0.5370899 0.5967131 1.0000000 0.5917871
#[5,] 0.4686988 0.5233336 0.5366317 0.5917871 1.0000000

#Identificación con movimiento social
with(elw, cor(cbind(elw$mov_izq_w01, elw$mov_izq_w02, elw$mov_izq_w03, 
                    elw$mov_izq_w04, elw$mov_izq_w05), use="pairwise.complete.obs"))
#          [,1]      [,2]       [,3]      [,4]       [,5]
#[1,] 1.0000000 0.2139437 0.18287333 0.1478955 0.12868686
#[2,] 0.2139437 1.0000000 0.21899441 0.1981120 0.13354296
#[3,] 0.1828733 0.2189944 1.00000000 0.1590340 0.09765072
#[4,] 0.1478955 0.1981120 0.15903401 1.0000000 0.17612544
#[5,] 0.1286869 0.1335430 0.09765072 0.1761254 1.00000000




#-----------------------------Pooled Cross-Lagged------------------------------#
load("Elsoc_Long_2016_2021_REC.Rdata")

#Subset 2 olas mínimo
ell <- subset(ell, muestra==1)
#ell <- subset(ell, tipo_atricion<=1) 

#Recodificar variables time-invariant
ell <- ell[order(ell$idencuesta, ell$time), ]
ell$hombre_w01 <- ell$hombre[ell$time==1][factor(ell$idencuesta)]
ell$edad_w01 <- ell$edad[ell$time==1][factor(ell$idencuesta)]
ell$educF_w01 <- ell$educF[ell$time==1][factor(ell$idencuesta)]
ell$religid_w01 <- ell$religid[ell$time==1][factor(ell$idencuesta)]
ell$religid_w01 <- relevel(ell$religid_w01, "Catolico")
levels(ell$religid_w01)[2] <- levels(ell$religid_w01)[5]
ell$practica_w01 <- ell$practica[ell$time==1][factor(ell$idencuesta)]

#Dummy para identificacion de izquierda
ell$pos_izq <- as.numeric(ell$izqderF=="Izquierda")
ell$pos_izq_w <- ell$pos_izq - ave(ell$pos_izq, ell$idencuesta, na.rm=T)
table(ell$pos_izq, ell$izqderF, exclude = NULL)
table(ell$pos_izq_w, ell$pos_izq)

#Within Dummy for Social Movement valoration
ell$mov_izq_w <- ell$mov_izq - ave(ell$mov_izq, ell$idencuesta, na.rm=T)
table(ell$mov_izq_w)

#Reescalar variables principales
ell$izqder01 <- (ell$izqder - min(ell$izqder, na.rm = T)) / 
  (max(ell$izqder, na.rm = T) - min(ell$izqder, na.rm = T))

summary(cbind(ell$pos_ideo, ell$izqder01, ell$mov_izq, ell$pos_izq))

#Moderadores: estudiante universitario, confianza institucional, Interés en política
ell$InteresPoli_bw <- ave(ell$InteresPoli, ell$idencuesta, na.rm=T)
ell$InteresPoli_bin_bw <- as.numeric(ell$InteresPoli_bw>=mean(ell$InteresPoli_bw, na.rm=T))
table(ell$InteresPoli_bin_bw)

#Modelos A: Menciona Posición Ideológica de Izq e Identificación con Movimiento de Izquierda
a1 <- plm(pos_izq ~ lag(pos_izq) + lag(mov_izq) +
            hombre_w01 + edad_w01 + educF_w01 + practica_w01 + time, data=ell, 
            index = c("idencuesta", "time"), model = "pooling")

a2 <- plm(pos_izq ~ lag(pos_izq) + mov_izq + 
            hombre_w01 + edad_w01 + educF_w01 + practica_w01 + time, data=ell, 
          index = c("idencuesta", "time"), model = "pooling")

a3 <- plm(pos_izq ~ lag(pos_izq) + lag(mov_izq) + mov_izq + 
            hombre_w01 + edad_w01 + educF_w01 + practica_w01 + time, data=ell, 
          index = c("idencuesta", "time"), model = "pooling")

a4 <- plm(mov_izq ~ lag(mov_izq) + lag(pos_izq) + 
            hombre_w01 + edad_w01 + educF_w01 + practica_w01 + time, data=ell, 
          index = c("idencuesta", "time"), model = "pooling")

a5 <- plm(mov_izq ~ lag(mov_izq) + pos_izq + 
            hombre_w01 + edad_w01 + educF_w01 + practica_w01 + time, data=ell, 
          index = c("idencuesta", "time"), model = "pooling")

a6 <- plm(mov_izq ~ lag(mov_izq) + lag(pos_izq) + pos_izq +
            hombre_w01 + edad_w01 + educF_w01 + practica_w01 + time, data=ell, 
          index = c("idencuesta", "time"), model = "pooling")

rse_a1 <- sqrt(diag(vcovHC(a1, type="HC0",cluster="group")))
rse_a2 <- sqrt(diag(vcovHC(a2, type="HC0",cluster="group")))
rse_a3 <- sqrt(diag(vcovHC(a3, type="HC0",cluster="group")))
rse_a4 <- sqrt(diag(vcovHC(a4, type="HC0",cluster="group")))
rse_a5 <- sqrt(diag(vcovHC(a5, type="HC0",cluster="group")))
rse_a6 <- sqrt(diag(vcovHC(a6, type="HC0",cluster="group")))

pval_a1 <- (1 - pnorm(abs(coef(a1))/rse_a1))*2
pval_a2 <- (1 - pnorm(abs(coef(a2))/rse_a2))*2
pval_a3 <- (1 - pnorm(abs(coef(a3))/rse_a3))*2
pval_a4 <- (1 - pnorm(abs(coef(a4))/rse_a4))*2
pval_a5 <- (1 - pnorm(abs(coef(a5))/rse_a5))*2
pval_a6 <- (1 - pnorm(abs(coef(a6))/rse_a6))*2

screenreg(list(a1,a2,a3,a4,a5,a6), stars = c(0.1,0.05,0.01),
          override.se = list(rse_a1, rse_a2, rse_a3, rse_a4, rse_a5, rse_a6),
          override.pvalues = list(pval_a1, pval_a2, pval_a3, pval_a4, pval_a5, pval_a6),
          single.row = F, digits=2,
          custom.model.names = c("Menciona Ideo 1", "Menciona Ideo 2", "Menciona Ideo 3",
                                 "Mov Izquierda 1", "Mov Izquierda 2", "Mov Izquierda 3"))

#Modelos B: Posición Ideológica (continua)e Identificación con Movimiento de Izquierda

b1 <- plm(izqder01 ~ lag(izqder01) + lag(mov_izq) + 
            hombre_w01 + edad_w01 + educF_w01 + practica_w01 + time, data=ell, 
          index = c("idencuesta", "time"), model = "pooling")

b2 <- plm(izqder01 ~ lag(izqder01) + mov_izq + 
            hombre_w01 + edad_w01 + educF_w01 + practica_w01 + time, data=ell, 
          index = c("idencuesta", "time"), model = "pooling")

b3 <- plm(izqder01 ~ lag(izqder01) + lag(mov_izq) + mov_izq + 
            hombre_w01 + edad_w01 + educF_w01 + practica_w01 + time, data=ell, 
          index = c("idencuesta", "time"), model = "pooling")

b4 <- plm(mov_izq ~ lag(mov_izq) + lag(izqder01) + 
            hombre_w01 + edad_w01 + educF_w01 + practica_w01 + time, data=ell, 
          index = c("idencuesta", "time"), model = "pooling")

b5 <- plm(mov_izq ~ lag(mov_izq) + izqder01 + 
            hombre_w01 + edad_w01 + educF_w01 + practica_w01 + time, data=ell, 
          index = c("idencuesta", "time"), model = "pooling")

b6 <- plm(mov_izq ~ lag(mov_izq) + lag(izqder01) + izqder01 +
            hombre_w01 + edad_w01 + educF_w01 + practica_w01 + time, data=ell, 
          index = c("idencuesta", "time"), model = "pooling")

rse_b1 <- sqrt(diag(vcovHC(b1, type="HC0",cluster="group")))
rse_b2 <- sqrt(diag(vcovHC(b2, type="HC0",cluster="group")))
rse_b3 <- sqrt(diag(vcovHC(b3, type="HC0",cluster="group")))
rse_b4 <- sqrt(diag(vcovHC(b4, type="HC0",cluster="group")))
rse_b5 <- sqrt(diag(vcovHC(b5, type="HC0",cluster="group")))
rse_b6 <- sqrt(diag(vcovHC(b6, type="HC0",cluster="group")))

pval_b1 <- (1 - pnorm(abs(coef(b1))/rse_b1))*2
pval_b2 <- (1 - pnorm(abs(coef(b2))/rse_b2))*2
pval_b3 <- (1 - pnorm(abs(coef(b3))/rse_b3))*2
pval_b4 <- (1 - pnorm(abs(coef(b4))/rse_b4))*2
pval_b5 <- (1 - pnorm(abs(coef(b5))/rse_b5))*2
pval_b6 <- (1 - pnorm(abs(coef(b6))/rse_b6))*2

screenreg(list(b1,b2,b3,b4,b5,b6), stars = c(0.1,0.05,0.01),
          override.se = list(rse_b1, rse_b2, rse_b3, rse_b4, rse_b5, rse_b6),
          override.pvalues = list(pval_b1, pval_b2, pval_b3, pval_b4, pval_b5, pval_b6),
          single.row = F, digits=2,
          custom.model.names = c("Ideologia 1", "Ideologia 2", "Ideologia 3",
                                 "Mov Izquierda 1", "Mov Izquierda 2", "Mov Izquierda 3"))

##Subset 2 olas mínimo (incluye no consecutivas)
#ell2= filter(ell, !is.na(ell$pos_izq) | !is.na(ell$mov_izq))
#ell3= filter(ell, !is.na(ell$izqder01) | !is.na(ell$mov_izq))


###--- MODELO PRIMERAS DIFERENCIAS ---#######

mlin1 <- plm(diff(pos_izq) ~ diff(lag(mov_izq)) + time,
             data=ell, index = c("idencuesta", "time"), model = "pooling")
mlin2 <- plm(diff(pos_izq) ~ diff(mov_izq) + time,
             data=ell, index = c("idencuesta", "time"), model = "pooling")
mlin3 <- plm(diff(pos_izq) ~ diff(lag(mov_izq)) + diff(mov_izq) + time,
             data=ell, index = c("idencuesta", "time"), model = "pooling")
screenreg(l=list(mlin1,mlin2,mlin3))

#Heterogeneity by political interest
mlin3a <- plm(diff(pos_izq) ~ diff(mov_izq) + time,
              data=ell, index = c("idencuesta", "time"), model = "pooling",
              InteresPoli_bw>mean(ell$InteresPoli_bw, na.rm=T))
mlin3b <- plm(diff(pos_izq) ~ diff(mov_izq) + time,
              data=ell, index = c("idencuesta", "time"), model = "pooling",
              InteresPoli_bw<=mean(ell$InteresPoli_bw, na.rm=T))
mlin3c <- plm(diff(pos_izq) ~ diff(mov_izq) + diff(mov_izq):InteresPoli_bin_bw + time,
              data=ell, index = c("idencuesta", "time"), model = "pooling")
screenreg(l=list(mlin3a, mlin3b, mlin3c))


mlin4 <- plm(diff(izqder) ~ diff(lag(mov_izq)) +  time,
             data=ell, index = c("idencuesta", "time"), model = "pooling")
mlin5 <- plm(diff(izqder) ~ diff(mov_izq) + time,
             data=ell, index = c("idencuesta", "time"), model = "pooling")
mlin6 <- plm(diff(izqder) ~ diff(lag(mov_izq)) + diff(mov_izq) + time,
             data=ell, index = c("idencuesta", "time"), model = "pooling")
screenreg(l=list(mlin4,mlin5,mlin6))

###--- MODELO ARRELLANO BOND ---#######
mab1 <- pgmm(pos_izq ~ lag(pos_izq) + lag(mov_izq) | 
               lag(pos_izq, 2:99) + lag(mov_izq, 2:99), 
            data = ell, effect = "twoways", index = c("idencuesta", "time"))
mab2 <- pgmm(pos_izq ~ lag(pos_izq) + mov_izq | 
               lag(pos_izq, 2:99) + lag(mov_izq, 1:99), 
             data = ell, effect = "twoways", index = c("idencuesta", "time"))
mab3 <- pgmm(pos_izq ~ lag(pos_izq) + mov_izq + lag(mov_izq) | 
               lag(pos_izq, 2:99) + lag(mov_izq, 1:99) + lag(mov_izq, 2:99), 
             data = ell, effect = "twoways", index = c("idencuesta", "time"))
screenreg(l=list(mab1,mab2,mab3))

#Heterogeneity by political interest
mab3a <- pgmm(pos_izq ~ lag(pos_izq) + mov_izq | 
               lag(pos_izq, 2:99) + lag(mov_izq, 1:99), 
              data = subset(ell, InteresPoli_bw<mean(ell$InteresPoli_bw, na.rm=T)),
              effect = "twoways", index = c("idencuesta", "time"))
mab3b <- pgmm(pos_izq ~ lag(pos_izq) + mov_izq  | 
               lag(pos_izq, 2:99) + lag(mov_izq, 1:99), 
             data = subset(ell, InteresPoli_bw>=mean(ell$InteresPoli_bw, na.rm=T)),
             effect = "twoways", index = c("idencuesta", "time"))
screenreg(l=list(mab3a, mab3b))


###--- MODELO ARRELLANO BOND ---#######
mab4 <- pgmm(izqder ~ lag(izqder) + lag(mov_izq) + practica + ingreso | 
               lag(izqder, 2:99) + lag(mov_izq, 2:99), 
             data = ell, effect = "twoways", index = c("idencuesta", "time"))
mab5 <- pgmm(izqder ~ lag(izqder) + mov_izq  + practica + ingreso | 
               lag(izqder, 2:99) + lag(mov_izq, 1:99), 
             data = ell, effect = "twoways", index = c("idencuesta", "time"))
mab6 <- pgmm(izqder ~ lag(izqder) + mov_izq + lag(mov_izq) + practica + ingreso | 
               lag(izqder, 2:99) + lag(mov_izq, 1:99) + lag(mov_izq, 2:99), 
             data = ell, effect = "twoways", index = c("idencuesta", "time"))
screenreg(l=list(mab4,mab5,mab6))

#Heterogeneity by political interest
mab6a <- pgmm(izqder ~ lag(izqder) + mov_izq + practica + ingreso | 
                lag(izqder, 2:99) + lag(mov_izq, 1:99), 
              data = subset(ell, InteresPoli_bw>mean(ell$InteresPoli_bw, na.rm=T)),
              effect = "twoways", index = c("idencuesta", "time"))
mab6b <- pgmm(izqder ~ lag(izqder) + mov_izq + practica + ingreso | 
                lag(izqder, 2:99) + lag(mov_izq, 1:99), 
              data = subset(ell, InteresPoli_bw<=mean(ell$InteresPoli_bw, na.rm=T)),
              effect = "twoways", index = c("idencuesta", "time"))
screenreg(l=list(mab6a, mab6b))


summary(pgmm(izqder ~ lag(izqder) + mov_izq + mov_izq : InteresPoli_bin_bw | 
               lag(izqder, 2:99), data = ell, effect = "twoways", 
        index = c("idencuesta", "time")))



##----Exploraroty Analsis ----###

prop.table(xtabs(~izqderF + mov_izq, data=ell),2)
prop.table(xtabs(~izqderF + mov_izq, data=subset(ell, InteresPoli_bw>2)),2)
prop.table(xtabs(~izqderF + mov_izq, data=subset(ell, InteresPoli<=2)),2)

with(subset(ell, InteresPoli_bw>=2), cor(pos_izq, mov_izq, use = "complete.obs"))
with(subset(ell, InteresPoli_bw<2), cor(pos_izq, mov_izq, use = "complete.obs"))


##----Bayesian Multinomial Mixed Effects ----###

elmcmc <- pdata.frame(ell, index = c("idencuesta", "time"))
elmcmc$izqderF <- relevel(elmcmc$izqderF, "Izquierda")
elmcmc$mov_izq_t1 <- plm::lag(elmcmc$mov_izq, 1)
elmcmc$mov_izq_t1_w <- ave(elmcmc$mov_izq_t1, elmcmc$idencuesta, 
                             FUN=function(x) mean(x, na.rm=T))

elmcmc <- na.omit(subset(elmcmc, select=c("izqderF", "mov_izq", "mov_izq_w", 
                                        #"hombre_w01", "edad_w01", "educF_w01", "practica_w01", 
                                        "time", "idencuesta", "InteresPoli_bw", "pos_izq")))

summary(elmcmc)

elmcmc$InteresPoli_bin_bw <- as.numeric(
  ifelse(elmcmc$InteresPoli_bw>mean(elmcmc$InteresPoli_bw, na.rm=T), 1, 0)
  )
table(elmcmc$InteresPoli_bin_bw)

#Model Izq-Der Multinomial
k <- length(levels(ell$izqderF))
I <- diag(k-1)
J <- matrix(rep(1, (k-1)^2), c(k-1, k-1))
prior1 = list(R = list(V=0.5*(I+J), fix=1), 
              G = list(G1=list(V=diag(3), nu=3)))
m1.mul = MCMCglmm(izqderF ~ - 1 + trait*mov_izq_w*InteresPoli_bin_bw + trait*time - 
                    mov_izq_w * InteresPoli_bin_bw - time,
                  random=~us(trait):idencuesta, 
                  rcov =~us(trait):units,
                  data=elmcmc, family= "categorical", 
                  prior=prior1, pr=TRUE, nitt=35000, thin=30, burnin=5000, 
                  verbose=TRUE, saveX=FALSE, saveZ=FALSE, saveXL=FALSE)
summary(m1.mul)
summary(m1.mul$Sol[,1:24])
autocorr.diag(m1.mul$Sol[,1:12])
autocorr.diag(m1.mul$VCV)
plot(m1.mul$Sol[,1:12])

#Rescale Coefficients
c2 <- ((16 * sqrt(3))/(15 * pi))^2
sigma_rescale <- function(sigma_resid) sqrt((1 + c2*sigma_resid) / (1 + c2*1))

summary(m1.mul$Sol[,"traitizqderF.Ninguna/Ind:mov_izq_w"]*sigma_rescale(0))
summary(m1.mul$Sol[,"traitizqderF.Derecha:mov_izq_w"]*sigma_rescale(0))
summary(m1.mul$Sol[,"traitizqderF.Centro:mov_izq_w"]*sigma_rescale(0))

#Location effects: izqderF ~ -1 + trait * mov_izq_w + trait * InteresPoli + trait * mov_izq_w * InteresPoli + trait * time - mov_izq_w - mov_izq_t1_w - mov_izq_w * InteresPoli - time 

#                                                post.mean  l-95% CI  u-95% CI eff.samp  pMCMC    

##Multinomial Logit
library(nnet)
screenreg(multinom(izqderF ~ mov_izq_w * InteresPoli_bin_bw, data=elmcmc), single.row = T)

#Pseudo Mixed Multinomial
screenreg(l=list(
  glmer(izqderF=="Ninguna/Ind" ~ mov_izq_w * InteresPoli_bin_bw + factor(time) + 
          (1| idencuesta), data=subset(elmcmc, izqderF!="Centro" & izqderF!="Derecha"),
        family = binomial, nAGQ=0),
  glmer(izqderF=="Derecha" ~ mov_izq_w * InteresPoli_bin_bw + factor(time) + 
          (1| idencuesta), data=subset(elmcmc, izqderF!="Centro" & izqderF!="Ninguna/Ind"),
        family = binomial, nAGQ=0),
  glmer(izqderF=="Centro" ~ mov_izq_w * InteresPoli_bin_bw + factor(time) + 
          (1| idencuesta), data=subset(elmcmc, izqderF!="Derecha" & izqderF!="Ninguna/Ind"),
        family = binomial, nAGQ=0)))

#LPM Mixed Multinomial
screenreg(l=list(
lmer(izqderF=="Ninguna/Ind" ~ mov_izq_w * InteresPoli_bin_bw + factor(time) + 
        (1| idencuesta), data=subset(elmcmc, izqderF!="Centro" & izqderF!="Derecha")),
lmer(izqderF=="Derecha" ~ mov_izq_w * InteresPoli_bin_bw + factor(time) + 
        (1| idencuesta), data=subset(elmcmc, izqderF!="Centro" & izqderF!="Ninguna/Ind")),
lmer(izqderF=="Centro" ~ mov_izq_w * InteresPoli_bin_bw + factor(time) + 
        (1| idencuesta), data=subset(elmcmc, izqderF!="Derecha" & izqderF!="Ninguna/Ind"))))

xtabs(~izqderF+mov_izq_w, data=subset(elmcmc, izqderF!="Centro" & izqderF!="Derecha"))
xtabs(~izqderF+mov_izq_w, data=subset(ell, izqderF!="Centro" & izqderF!="Derecha"))


##Identify cases used in AB analysis
summary(plm(diff(izqder) ~ diff(lag(izqder)) | lag(izqder, 2), data = ell,
            index = c("idencuesta", "time")))
summary(pgmm(izqder ~ lag(izqder)  | lag(izqder, 2:99), data = ell, 
             effect = "twoways", index = c("idencuesta", "time")))

dd <- with(ell, data.frame(izqder, idencuesta, time))
dd <- pdata.frame(dd, index = c("idencuesta", "time"))
dd$izqder_t1 <- plm::lag(dd$izqder)
dd$izqder_t2 <- plm::lag(dd$izqder,2)
dd <- na.omit(dd)
dd$time <- as.numeric(dd$time)
head(dd, 30)
dim(dd)

table(dd$time)
length((unique(dd$idencuesta)))

#¿Quienes responden modelo autoregresivo?
# En total 1031 entrevistados que respondieron posicion izq-der en olas 1 al 3,
# olas 1 al 4 y 1 al 5.



###################################################################################
### MULTIPLE IMPUTATION MODEL OF LEFT-RIGHT SCALE WITH AUXILIARY VARIABLES ########
###################################################################################

#Left-right and autoritarian attitudes
cor(cbind(elw[, grep("izqder_w0", names(elw))], elw[, grep("gobfirme_w", names(elw))]),
    use="pairwise.complete.obs") #0.21 - 0.27
cor(cbind(elw[, grep("izqder_w0", names(elw))], elw[, grep("mandfuerte_w", names(elw))]),
    use="pairwise.complete.obs") #0.20 - 0.26
cor(cbind(elw[, grep("izqder_w0", names(elw))], elw[, grep("obediencia_w", names(elw))]),
    use="pairwise.complete.obs") #0.15 - 0.26
cor(cbind(elw[, grep("izqder_w0", names(elw))], elw[, grep("disciplina_w", names(elw))]),
    use="pairwise.complete.obs") #0.16 - 0.23
cor(cbind(elw[, grep("izqder_w0", names(elw))], elw[, grep("autori_w", names(elw))]),
    use="pairwise.complete.obs") #0.25 - 0.30                                             *

#Left-right and distributive attitudes
cor(cbind(elw[, grep("izqder_w0", names(elw))], elw[, grep("difingreso_w", names(elw))]),
    use="pairwise.complete.obs") #(-0.11) - (-0.02)
cor(cbind(elw[, grep("izqder_w0", names(elw))], elw[, grep("IndMerito_w", names(elw))]),
    use="pairwise.complete.obs") #0.03 - 0.16

#Left-right and Overall Life Evaluation
cor(cbind(elw[, grep("izqder_w0", names(elw))], elw[, grep("LifeEval_w", names(elw))]),
    use="pairwise.complete.obs") # ~0.08

#Left-right and Trust in the Police
cor(cbind(elw[, grep("izqder_w0", names(elw))], elw[, grep("confCara_w", names(elw))]),
    use="pairwise.complete.obs") # 0.08 - 0.37                                            *

#Left-right and Attend to Demostrations                                                   *
cor(cbind(elw[, grep("izqder_w0", names(elw))], elw[, grep("AsisMarc_w", names(elw))]),   
    use="pairwise.complete.obs") # (-0.18) - (-0.28) 

#Left-right and National Identity
cor(cbind(elw[, grep("izqder_w0", names(elw))], elw[, grep("IdNacional_w", names(elw))]),
    use="pairwise.complete.obs") #0.08 - 0.13

#Left-right and Social Change is Possible
cor(cbind(elw[, grep("izqder_w0", names(elw))], elw[, grep("cambiosocial_w", names(elw))]),
    use="pairwise.complete.obs") # ~ (-0.1)

##Left-right and Come from a Rich Family
cor(cbind(elw[, grep("izqder_w0", names(elw))], elw[, grep("famrica_w", names(elw))]),
    use="pairwise.complete.obs") # < (-0.1)

##Left-right and Subjective Social Status
cor(cbind(elw[, grep("izqder_w0", names(elw))], elw[, grep("ess_w", names(elw))]),
    use="pairwise.complete.obs") # 0.05 - 0.16

##Left-right and Vote Valuation
cor(cbind(elw[, grep("izqder_w0", names(elw))], elw[, grep("ValorVotoIn_w", names(elw))]),
    use="pairwise.complete.obs") # < 0.1

##Left-right and Police Evict Occupied Schools                                              *
cor(cbind(elw[, grep("izqder_w0", names(elw))], elw[, grep("DesalojoCarab_w", names(elw))]),
    use="pairwise.complete.obs") # < (0.18 - 0.32)                      
##Left-right and Police repress demostrations                                               *
cor(cbind(elw[, grep("izqder_w0", names(elw))], elw[, grep("RepresionCarab_w", names(elw))]),
    use="pairwise.complete.obs") #0.07- 0.21
##Left-right and Justification for police violence Index
cor(cbind(elw[, grep("izqder_w0", names(elw))], elw[, grep("ViolCarab_w", names(elw))]),
    use="pairwise.complete.obs") #0.17- 0.30


##Left-right and Workers block streets                                                      *
cor(cbind(elw[, grep("izqder_w0", names(elw))], elw[, grep("TrabajBloqueo_w", names(elw))]),
    use="pairwise.complete.obs") # (-0.05) - (-0.26)
##Left-right and Students stone police #¿                                                   *
cor(cbind(elw[, grep("izqder_w0", names(elw))], elw[, grep("PiedrasCarab_w", names(elw))]),
    use="pairwise.complete.obs") # (-0.07) - (-0.27)
##Left-right and Justification for demonstrators violence Index
cor(cbind(elw[, grep("izqder_w0", names(elw))], elw[, grep("ViolManif_w", names(elw))]),
    use="pairwise.complete.obs") #(-0.07)- (-0.30)

##Left-right and Taxes are High
cor(cbind(elw[, grep("izqder_w0", names(elw))], elw[, grep("paga_imp_w", names(elw))]),
    use="pairwise.complete.obs") # < 0.05

##Left-right and trust in Labor Unions
cor(cbind(elw[, grep("izqder_w0", names(elw))], elw[, grep("satisdemo_w", names(elw))]),
    use="pairwise.complete.obs") 


#ISSUES tienen 2 o 3 olas
#Justicia distributiva no tiene ola 5