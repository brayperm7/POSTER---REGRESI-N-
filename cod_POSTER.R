install.packages("hot.deck")
library("HotDeckImputation")
require(mice)
require(lattice)
library("hot.deck")
base<- read.csv("diabetes1.csv", header = T)
base
head(base)
#imputación de datos y se excluye de las covariables la fecha del control  y si era diabetico, debido a que para todos es si.
# tambien se sacó glicemia post prandial debido a la baja proporcion de muestreados 
hot.deck(data = base, m = 46, method = c("best.cell", "p.draw"), cutoff = NULL,sdCutoff = 1, 
         optimizeSD = FALSE, optimStep = 0.1, optimStop = 5, weightedAffinity = FALSE, 
         impContinuous = c("HD", "mice"), IDvars = base$Glicemia.de.ayuno)
#impute.CPS_SEQ_HD(DATA = base, covariates = c(Edad,Grupo.de.edad,Genero,Etnia,Zona,
                                              #Afiliacion.SGSSS,Escolaridad,Escolaridad,
                                              #Regimen.de.Afiliaci.n,Fumador.Activo,
                                              #Hipertensi.n.Arterial.Sistemica,HTA...DM,
                                              #Clasificacion.de.Diabetes.o.del.ultimo.estado.de.Glicemia,
                                              #Complicaciones..y.Lesiones.en.Organo.Blanco,
                                              #Antecedentes.Fliar..Enfermedad.Coronaria,
                                              #Tension.SISTOLICA,Tension.DIASTOLICA,HTA.COMPENSADOS,
                                              #Colesterol.Total,Colesterol.HDL,Trigliceridos,Colesterol.LDL,
                                              #CALCULO.DE.RIESGO.DE.Framingham....a.10.a.os.,
                                              #Clasificaci.n.de.RCV.Global,Hemoglobina.A1C,DM.COMPENSADO,
                                              #HTA.Y.DM.COMPENSADA, Perimetro.Abdominal, Clasificaci.n.per.metro.abdominal, 
                                              #Peso,Talla,base$))

# análisis descriptivo base
# Densidad de la variable respuesta,
glic<-na.omit(base$Glicemia.de.ayuno)
plot(density(glic), xlim=c(0, 500), lwd=3,
     main='', ylab='Densidad', ylim=c(0, 0.03),
     xlab='glicemia de ayuno')
rug(base$Glicemia.de.ayuno)

# Aplicacion de fitDist ---------------------------------------------------
require(gamlss)
fit <- fitDist(Glicemia.de.ayuno, data=base, type="realplus")
fit$fits
fit
fitDist(Glicemia.de.ayuno, data=base, type="realplus")

# Histograma y mejores ajustes con exp, wei, ga y pareto
par(mfrow=c(1, 2))
fitexGAU <- histDist(y=base$Glicemia.de.ayuno, family=exGAUS, main="exGAUS", )
fitEXP <- histDist(y=base$Glicemia.de.ayuno, family=EXP, main="EXP")
fitpareto2 <- histDist(y=base$Glicemia.de.ayuno, family=PARETO2, main="PARETO2")
fitpareto2o <- histDist(y=base$Glicemia.de.ayuno, family=PARETO2o, main="PARETO2O")

# Mejorando los graficos anteriores, FIGURA 3
par(mfrow=c(1, 2))
hist(base$Glicemia.de.ayuno  ,
     main=expression(paste("Exponencial Gaussiana(", mu, "=73.12)(", sigma, "=3.185)(", nu, "=3.487)")),
     ylab='Densidad', freq=F, breaks=50, xlim=c(0, 600),
     xlab='glicemia en ayuno ', ylim=c(0, 0.05))
curve(dexGAUS(x, mu=fitexGAU$mu.coef, sigma = fitexGAU$sigma.coefficients, nu = fitexGAU$sigma.coefficients), add=T, lwd=2)

hist(base$Glicemia.de.ayuno,
     main=expression(paste("Exponencial(", mu, "=73.12)")),
     ylab='Densidad', freq=F, breaks=100, xlim=c(0, 600),
     xlab='glicemia en ayuno ', ylim=c(0, 0.05))
curve(dEXP(x, mu=log(fitEXP$mu.coef) ), add=T, lwd=3)

# Diagramas de dispersion en 2d -------------------------------------------
# -------------------------------------------------------------------------
pairs(base)

# Mejorando el grafico
panel.reg <- function (x, y)
{
  points(x, y, pch=20)
  abline(lm(y ~ x), lwd=2, col='dodgerblue2')
}

# proporción de personas a las que se le pudo tomar la muestra de glicemia post prandial
muestreados <- na.omit(base$Glicemia..Pos.prandial)
pro_postprandial <- length(muestreados)/length(base$Glicemia..Pos.prandial)
pro_postprandial

M_0<- gamlss(base$ ~ ., data = )

# Debido a el # de covariables se hace necesario identificar rapidamente cuales de ellas 
# son realmente significativas y de esta manera trabajar 