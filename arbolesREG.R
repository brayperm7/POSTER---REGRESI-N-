install.packages("rpart")
install.packages("rpart.plot")
install.packages("rpart.util")
install.packages("randomForest")
install.packages(hot.)
require(magrittr)
require(dplyr)
library(rpart)
library(rpart.plot)
library(randomForest)

base_training <- read.csv("diabetes_training.csv", header = T)
base_training
base_test <- read.csv("diabetes_test.csv", header = T) 

str(base_training)
basetest.imputed <- rfImpute( base_training$Glicemia.de.ayuno  ~ ., base_training)
base_dep_trai <- base_training %>% dplyr::select(Edad, Grupo.de.edad, Genero, Etnia, Zona, Afiliacion.SGSSS, Escolaridad, Regimen.de.Afiliaci.n, Fumador.Activo, Hipertensi.n.Arterial.Sistemica, HTA...DM, 
                                Clasificacion.de.Diabetes.o.del.ultimo.estado.de.Glicemia, Complicaciones..y.Lesiones.en.Organo.Blanco, Antecedentes.Fliar..Enfermedad.Coronaria, Tension.SISTOLICA, 
                                Tension.DIASTOLICA, HTA.COMPENSADOS, Colesterol.Total, Colesterol.HDL, Trigliceridos, Colesterol.LDL, CALCULO.DE.RIESGO.DE.Framingham....a.10.a.os., Clasificaci.n.de.RCV.Global,
                                Hemoglobina.A1C, DM.COMPENSADO, HTA.Y.DM.COMPENSADA, Glicemia.de.ayuno, Perimetro.Abdominal, Clasificaci.n.per.metro.abdominal, Peso, Talla, IMC, Creatinina,
                                Microalbuminuria, Proteinuria, Calculo.de..TFG.corregida..Cockcroft.Gault., Estadio.IRC, Remisiones.Especialidad, Farmacos..Antihipertensivos, Estatina, Antidiabeticos, Adherencia.al.tratamiento) %>% na.omit

base_dep_trai
#base_dep_test <- base_test %>% dplyr::select(Edad, Grupo.de.edad, Genero, Etnia, Zona, Afiliacion.SGSSS, Escolaridad, Regimen.de.Afiliaci.n, Fumador.Activo, Hipertensi.n.Arterial.Sistemica, HTA...DM, 
                                                          Clasificacion.de.Diabetes.o.del.ultimo.estado.de.Glicemia, Complicaciones..y.Lesiones.en.Organo.Blanco, Antecedentes.Fliar..Enfermedad.Coronaria, Tension.SISTOLICA, 
                                                          Tension.DIASTOLICA, HTA.COMPENSADOS, Colesterol.Total, Colesterol.HDL, Trigliceridos, Colesterol.LDL, CALCULO.DE.RIESGO.DE.Framingham....a.10.a.os., Clasificaci.n.de.RCV.Global,
                                                          Hemoglobina.A1C, DM.COMPENSADO, HTA.Y.DM.COMPENSADA, Glicemia.de.ayuno, Perimetro.Abdominal, Clasificaci.n.per.metro.abdominal, Peso, Talla, IMC, Creatinina,
                                                          Microalbuminuria, Proteinuria, Calculo.de..TFG.corregida..Cockcroft.Gault., Estadio.IRC, Remisiones.Especialidad, Farmacos..Antihipertensivos, Estatina, Antidiabeticos, Adherencia.al.tratamiento) %>% na.omit

#base_dep_test 
m1<- rpart(base_training$Glicemia.de.ayuno ~ . , data = base_training, method = "anova")
m1
graficom1 <- rpart.plot(m1, type = 1, digits = 3, fallen.leaves = T)
graficom1

p1 <- predict(m1, base_test)
p1

ECM <- sum(p1 - base_dep$Glicemia.de.ayuno)^2 *(1/ nrow(base_dep))
ECM



