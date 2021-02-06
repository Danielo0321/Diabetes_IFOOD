# DESCARGA DE LOS DATOS DE LA PÁGINA https://archive.ics.uci.edu/ml/datasets/Diabetes+130-US+hospitals+for+years+1999-2008#
# Y LOS ALMACENO EN LA DIRECCIÓN C:\Users\WORK\Documents\Archivos Rstudio\dataset_diabetes\diabetic_data.csv

setwd("~/Archivos Rstudio/dataset_diabetes")
diabetes <- read.csv("diabetic_data.csv")
# View(diabetes)

# ELIMINANDO COLUMNAS O FILAS INCOMPLETAS O IRRELEVANTES #########

diabetes1 <- subset(diabetes,select=-c(encounter_id, patient_nbr, examide,citoglipton,weight, payer_code, medical_specialty)) 
diabetes2 <- diabetes1[diabetes1$race != "?",] # No. of observations drops by 2273
diabetes2 <- diabetes2[diabetes2$diag_1 != "?",] # No of observations drops by 21
diabetes2 <- diabetes2[diabetes2$diag_2 != "?",] # No of observations drops by 358
diabetes2 <- diabetes2[diabetes2$diag_3 != "?",] # No of observations drops by 1453
# View(diabetes2)

###### PROYECCIÓN DE DATOS A ENTORNO BINARIO DONDE 1 CORRESPONDE A LA REINCIDENCIA EN MENOS DE 30 DÍAS
###### Y 0 CORREPONDE A >30 O NO REINCIDENCIA

diabetes2$readmittedbin <- ifelse(diabetes2$readmitted == "<30",1,0)

########### CREANDO UN NUEVO CONJUNTO DE DATOS PARA INICIAR A TRABAJAR
diabetes3 <- cbind(diabetes2[c(7:13,17)], lapply(diabetes2[c(1:6,14:16,18:44)],factor))
head(diabetes3)
# View(diabetes3)

## ATRIBUYENDO UNA NUEVA PROPORCIÓN A LA NUEVA TABLA
table(diabetes3$readmitted)
prop.table(table(diabetes3$readmitted))
table(diabetes3$readmittedbin)
prop.table(table(diabetes3$readmittedbin))

# Organización de datos por raza
racewise <- table(diabetes3$readmittedbin,diabetes3$race)
racewise

# AfricanAmerican Asian Caucasian Hispanic Other
# 0           16741   560     66567     1777  1342
# 1            2140    65      8512      207   142

# Organización de datos por género
genderwise <- table(diabetes3$readmittedbin,diabetes3$gender)
genderwise

# Female  Male Unknown/Invalid
# 0  46831 40155               1
# 1   6002  5064               0

# Organización de datos por edad
agewise <- table(diabetes3$readmittedbin,diabetes3$age)
agewise

# [0-10) [10-20) [20-30) [30-40) [40-50) [50-60) [60-70) [70-80) [80-90) [90-100)
# 0     64     435    1265    3140    8265   15059   19349   22302   14690     2418
# 1      1      31     213     408    1000    1638    2460    3004    2012      299

# Organización de datos por tiempo en el hospital
timinhoswise <- table(diabetes3$readmittedbin,diabetes3$time_in_hos)
timinhoswise

# 1     2     3     4     5     6     7     8     9    10    11    12    13
# 0 12363 14791 15201 11845  8519  6396  4961  3661  2524  1954  1618  1231  1038
# 1  1127  1650  1848  1589  1180   924   733   615   404   333   191   193   147
# 
# 14
# 0   885
# 1   132

# Organización de datos por el tipo de atención médica
Adminidwise <- table(diabetes3$readmittedbin,diabetes3$admission_type_id)
Adminidwise

# 1     2     3     4     5     6     7     8
# 0 46115 15547 16279     9  4172  4560    20   285
# 1  6063  1996  1915     1   489   575     0    27

# Organización de datos por el tipo de alta
dischargewise <- table(diabetes3$readmittedbin,diabetes3$discharge_disposition_id)
dischargewise

# Organización de datos por el tipo nivel o suministro de insulina
insulinwise <- table(diabetes3$readmittedbin,diabetes3$insulin)
insulinwise
# 0 10182 41285  26055  9465
# 1  1661  4658   3313  1434


# GRAFICANDO RESULTADOS (READMISIÓN DE PACIENTES) 
Number_patients <- table(diabetes3$readmitted)
plot(Number_patients,col ="lightblue", xlab = " Readmission Days ", main= " Frequency of Readmission", lwd =20,pch=18)

# GRAFICANDO RESULTADOS (SE EVIDENCIA QUE APENAS EL 11% DE LOS PACIENTES REGRESAN NUEVAMENTE AL HOSPITAL EN MENOS DE 30 DÍAS)
# IDEALMENTE SE BUSCA QUE EL CONJUNTO DE DATOS SEA SUFICIENTEMENTE RICO EN CASOS TANTO <30 COMO >30 O NUNCA 
# DE AQUÍ EN ADELANTE SE PUEDEN HACER MÁS TIPOS DE ANÁLISIS, SIN EMBARGO, NO SERÍAN DE UTILIDAD PARA APLICARLO EN LA VIDA REAL
# DADO QUE ESTARIAMOS HABLANDO DE MODELOS ENTRENADOS APENAS PARA UN TIPO ESPECÍFICO DE CASOS.

Number_patients_bin <- table(diabetes3$readmittedbin)
plot(Number_patients_bin,col ="lightblue", xlab = " Readmission Days ", main= " Frequency of Readmission", lwd =20,pch=18)


###### SEGMENTANDO EL CONJUNTO DE DATOS DE ENTRENAMIENTO
set.seed(111)
inTrain <- createDataPartition(diabetes3$readmittedbin, p=.2, list=FALSE)
objTrain <-diabetes3[inTrain,]
objTest <- diabetes3[-inTrain,]
table(objTrain$readmittedbin)

# 0     1 
# 17398  2214 

# NORMALIZANDO LA CANTIDAD DE DATOS USADOS EN EL CONJUNTO ANTERIOR
prop.table(table(objTrain$readmittedbin))

# 0         1 
# 0.8871099 0.1128901

# SEGMENTACIÓN DE DATOS USADOS PARA EL ENTRENAMIENTO
table(objTest$readmittedbin)

# 0     1 
# 69589  8852 

# NORMALIZANDO LA CANTIDAD DE DATOS USADOS EN EL CONJUNTO ANTERIOR
prop.table(table(objTest$readmittedbin))

# 0         1 
# 0.8871509 0.1128491

#### IDENTIFICACIÓN DE DESBALANCE EN EL CONJUNTO DE DATOS
cfit <- rpart(readmitted ~ time_in_hospital + num_lab_procedures + num_procedures + num_medications + number_outpatient + number_emergency + number_inpatient + race + age + admission_type_id + discharge_disposition_id + admission_source_id + number_diagnoses + max_glu_serum + A1Cresult + metformin + insulin, data = objTrain, method="class", minsplit = 20, minbucket = 5, cp = 0.001)

head(predict(cfit))

# <30       >30        NO
# 7  0.08661236 0.3034122 0.6099754
# 10 0.08661236 0.3034122 0.6099754
# 12 0.08661236 0.3034122 0.6099754
# 16 0.08661236 0.3034122 0.6099754
# 18 0.08661236 0.3034122 0.6099754
# 30 0.08661236 0.3034122 0.6099754

# GRAFICANDO RESULTADOS MEDIANTE ÁRBOLES DE DECISIÓN
par(mar=c(1,1,0.25,1))
plot(cfit, branch = 0.4,uniform = TRUE, compress = TRUE)
text(cfit, pretty = 0)

# 
rpart.predict <- predict(cfit, newdata = objTrain, type="class")
tail(rpart.predict)

# 101743 101748 101751 101753 101756 101766 
# NO     NO     NO     NO     NO     NO 
# Levels: <30 >30 NO

#accuracy.meas(objTest$readmitted, rpart.predict[,2])
cf <-confusionMatrix(rpart.predict, objTrain$readmitted)
cf

# Confusion Matrix and Statistics
# 
# Reference
# Prediction  <30  >30   NO
# <30    0    0    0
# >30  761 1901 1105
# NO  1453 4944 9448
# 
# Overall Statistics
# 
# Accuracy : 0.5787          
# 95% CI : (0.5717, 0.5856)
# No Information Rate : 0.5381          
# P-Value [Acc > NIR] : < 2.2e-16       
# 
# Kappa : 0.1544          
# 
# Mcnemar's Test P-Value : < 2.2e-16       
# 
# Statistics by Class:
# 
#                      Class: <30 Class: >30 Class: NO
# Sensitivity              0.0000    0.27772    0.8953
# Specificity              1.0000    0.85384    0.2939
# Pos Pred Value              NaN    0.50465    0.5963
# Neg Pred Value           0.8871    0.68798    0.7067
# Prevalence               0.1129    0.34902    0.5381
# Detection Rate           0.0000    0.09693    0.4817
# Detection Prevalence     0.0000    0.19208    0.8079
# Balanced Accuracy        0.5000    0.56578    0.5946

#Mean error rate
mean.error.rate.rpart <- 1- cf$overall[1]
mean.error.rate.rpart

# Accuracy 
# 0.4213237

par(mar=c(3,3,3,3))
plotcp(cfit,lty = 3, col = 1)
printcp(cfit)

# Classification tree:
#   rpart(formula = readmitted ~ time_in_hospital + num_lab_procedures + 
#           num_procedures + num_medications + number_outpatient + number_emergency + 
#           number_inpatient + race + age + admission_type_id + discharge_disposition_id + 
#           admission_source_id + number_diagnoses + max_glu_serum + 
#           A1Cresult + metformin + insulin, data = objTrain, method = "class", 
#         minsplit = 20, minbucket = 5, cp = 0.001)
# 
# Variables actually used in tree construction:
#   [1] admission_source_id      age                      discharge_disposition_id
# [4] num_lab_procedures       num_medications          num_procedures          
# [7] number_emergency         number_inpatient         race                    
# 
# Root node error: 9059/19612 = 0.46191
# 
# n= 19612 
# 
# CP nsplit rel error  xerror      xstd
# 1 0.0310189      0   1.00000 1.00000 0.0077070
# 2 0.0216359      1   0.96898 0.98554 0.0076984
# 3 0.0097141      2   0.94735 0.94768 0.0076693
# 4 0.0041395      3   0.93763 0.93840 0.0076607
# 5 0.0028701      6   0.92151 0.93355 0.0076560
# 6 0.0014350      8   0.91577 0.93564 0.0076581
# 7 0.0011039      9   0.91434 0.93542 0.0076578
# 8 0.0010000     11   0.91213 0.93564 0.0076581


# The plot shows 3 branches as optimum. The CP value for 3 branches would be around 0.0014. Pruning and replotting the tree:

# VALIDACIÓN CRUZADA PARA EL ÁRBOL DE DECISIÓN
cfit.tree <- tree(readmitted ~ time_in_hospital + num_lab_procedures + num_procedures + num_medications + number_outpatient + number_emergency + number_inpatient + race + age + admission_type_id + discharge_disposition_id + admission_source_id + number_diagnoses + max_glu_serum + A1Cresult + metformin + insulin, data = objTrain, method="class")
cv.cfit.tree <- cv.tree(cfit.tree, FUN = prune.misclass)
cv.cfit.tree

# $size
# [1] 2 1
# 
# $dev
# [1] 8778 9071
# 
# $k
# [1] -Inf  281
# 
# $method
# [1] "misclass"
# 
# attr(,"class")
# [1] "prune"         "tree.sequence"


### NUEVA SEGMENTACIÓN O PODA DE ÁRBOL PARA CONTINUAR CON EL ANÁLISIS
prune.cfit.tree <- prune.misclass(cfit.tree, best = 4)

# Warning message:
#   In prune.tree(tree = cfit.tree, best = 4, method = "misclass") :
#   best is bigger than tree size

#plot(prune.cfit.tree)
text(prune.cfit.tree, pretty = 0)

# DIAGRAMA DE ÁRBOL DESPUES DE LA SEGMENTACIÓN O PODA

cfit2 = prune(cfit, cp = 0.0014)

par(mar=c(1,1,0.25,1))
plot(cfit2, branch = 0.4,uniform = TRUE, compress = TRUE)
text(cfit2)

# CÁLCULO DE LA MATRIZ DE CONFUSIÓN A PARTIR DE LOS RESULTADOS DEL ÁRBOL PODADO

#Prediction on test set
rpart.prune.predict <- predict(cfit2, newdata = objTest,type = "class")

cf.prune <-confusionMatrix(rpart.prune.predict,objTest$readmitted)

#Mean error rate
mean.error.rate.rpart.prune <- 1- cf.prune$overall[1]
mean.error.rate.rpart.prune

# Accuracy 
# 0.4278247

cf.prune$table

# Reference
# Prediction   <30   >30    NO
# <30     0     0     0
# >30  3135  8058  4961
# NO   5717 19746 36824

# ÁRBOL DE DECISIÓN CON DOS TIPOS DE RESPUESTA
cfit_bin <- rpart(readmittedbin ~ time_in_hospital + num_lab_procedures + num_procedures + num_medications + number_outpatient + number_emergency + number_inpatient + race + age + admission_type_id + discharge_disposition_id + admission_source_id + number_diagnoses + max_glu_serum + A1Cresult + metformin + insulin, data = objTrain, method="class", minsplit = 1, minbucket = 1, cp = 0.001)

par(mar=c(2,2,0.25,1))
plot(cfit_bin, branch = 0.4,uniform = TRUE, compress = TRUE)
text(cfit_bin, pretty = 0)

#How to read plotcp - http://www.wekaleamstudios.co.uk/posts/classification-trees-using-the-rpart-function/#m3mLNpeke0I
rpart.predict_bin <- predict(cfit_bin, newdata = objTrain,type="prob")

View(objTrain)
head(rpart.predict_bin)

View(rpart.predict_bin)
accuracy.meas(objTrain$readmittedbin, rpart.predict_bin[,2])

# Call: 
#   accuracy.meas(response = objTrain$readmittedbin, predicted = rpart.predict_bin[, 
#                                                                                  2])
# 
# Examples are labelled as positive when predicted is greater than 0.5 
# 
# precision: 0.763
# recall: 0.048
# F: 0.045

roc.curve(objTrain$readmittedbin, rpart.predict_bin[,2], plotit = T)
# Area under the curve (AUC): 0.584


par =TRUE


str(rpart.predict_bin)
# num [1:19612, 1:2] 0.905 0.905 0.905 0.905 0.905 ...
# - attr(*, "dimnames")=List of 2
# ..$ : chr [1:19612] "7" "10" "12" "16" ...
# ..$ : chr [1:2] "0" "1"

str(objTrain$readmittedbin)
# Factor w/ 2 levels "0","1": 1 1 2 1 1 1 1 1 1 1 ...

# cf_bin <-confusionMatrix(rpart.predict_bin, objTrain$readmittedbin)
# cf_bin
# # Mean error rate
# mean.error.rate.rpart_bin <- 1- cf_bin$overall[1]
# mean.error.rate.rpart_bin
# par(mar=c(3,3,3,3))
# plotcp(cfit_bin,lty = 3, col = 1)
# printcp(cfit_bin)

##### The plot shows 34 branches as optimum. The CP value for 2 branches would be around 0.001. Pruning and replotting the tree:
#After pruning
cfit2_bin = prune(cfit_bin, cp = 0.0001)

par(mar=c(.5,.5,.5,.5))
plot(cfit2_bin, branch = 0.4,uniform = TRUE, compress = TRUE)
text(cfit2_bin, pretty=0)

head(predict(cfit2_bin))

# 0         1
# 7  0.9054046 0.0945954
# 10 0.9054046 0.0945954
# 12 0.9054046 0.0945954
# 16 0.9054046 0.0945954
# 18 0.9054046 0.0945954

#Prediction on training  set
rpart.prune.predict2_bin <- predict(cfit2_bin, newdata = objTrain,type = "class")

cf.prune_bin <-confusionMatrix(rpart.prune.predict2_bin,objTrain$readmittedbin)
cf.prune_bin

# Confusion Matrix and Statistics
# 
# Reference
# Prediction     0     1
# 0 17365  2108
# 1    33   106
# 
# Accuracy : 0.8908
# 95% CI : (0.8864, 0.8952)
# No Information Rate : 0.8871
# P-Value [Acc > NIR] : 0.05041
# 
# Kappa : 0.0778
# 
# Mcnemar's Test P-Value : < 2e-16
# 
#             Sensitivity : 0.99810
#             Specificity : 0.04788
#          Pos Pred Value : 0.89175
#          Neg Pred Value : 0.76259
#              Prevalence : 0.88711
#          Detection Rate : 0.88543
#    Detection Prevalence : 0.99291
#       Balanced Accuracy : 0.52299
# 
#        'Positive' Class : 0

#Mean error rate
mean.error.rate.rpart.prune2 <- 1- cf.prune_bin$overall[1]
mean.error.rate.rpart.prune2

# Accuracy 
# 0.1091679 

# over sampling
table(objTrain$readmittedbin)

# 0     1 
# 17398  2214 


data_balanced_over <- ovun.sample(readmittedbin ~ time_in_hospital + num_lab_procedures + num_procedures + num_medications + number_outpatient + number_emergency + number_inpatient + race + age + admission_type_id + discharge_disposition_id + admission_source_id + number_diagnoses + max_glu_serum + A1Cresult + metformin + insulin, data = objTrain, method = "over", N = 34794)$data
table(data_balanced_over$readmittedbin)

# 0     1 
# 17398 17396

# under sampling
data_balanced_under <- ovun.sample(readmittedbin ~ time_in_hospital + num_lab_procedures + num_procedures + num_medications + number_outpatient + number_emergency + number_inpatient + race + age + admission_type_id + discharge_disposition_id + admission_source_id + number_diagnoses + max_glu_serum + A1Cresult + metformin + insulin, data = objTrain, method = "under", N = 4428, seed=1)$data
table(data_balanced_under$readmittedbin)

# 0    1 
# 2214 2214

# Balanced sampling

data_balanced_both <- ovun.sample(readmittedbin ~ time_in_hospital + num_lab_procedures + num_procedures + num_medications + number_outpatient + number_emergency + number_inpatient + race + age + admission_type_id + discharge_disposition_id + admission_source_id + number_diagnoses + max_glu_serum + A1Cresult + metformin + insulin, data = objTrain, method = "both", N = 19610, seed=1)$data
table(data_balanced_both$readmittedbin)

# 0    1 
# 9847 9763

# ROSE SYTHETIC DATA BALANCING
data.rose <- ROSE(readmittedbin ~ time_in_hospital + num_lab_procedures + num_procedures + num_medications + number_outpatient + number_emergency + number_inpatient + race + age + admission_type_id + discharge_disposition_id + admission_source_id + number_diagnoses + max_glu_serum + A1Cresult + metformin + insulin, data = objTrain,seed=1)$data
table(data.rose$readmittedbin)

# 0    1 
# 9848 9764

# build decision tree models-Rose

cfit.rose <- rpart(readmittedbin ~ time_in_hospital + num_lab_procedures + num_procedures + num_medications + number_outpatient + number_emergency + number_inpatient + race + age + admission_type_id + discharge_disposition_id + admission_source_id + number_diagnoses + max_glu_serum + A1Cresult + metformin + insulin, data = data.rose)
head(data.rose)

# time_in_hospital num_lab_procedures num_procedures num_medications
# 1        5.6881038           68.64722     1.12725591       19.766359
# 2        8.9294231           46.66806     2.25758926       29.753958
# 3        0.9139547           19.03643    -0.05469272       12.701929
# 4        1.0302780           15.14254     0.14820962        8.627319
# 5        1.1633738           43.09546     0.12154749        8.711719
# 6        8.2764111           68.08642     3.01913368        8.082345
# number_outpatient number_emergency number_inpatient number_diagnoses
# 1        -0.1844017      -0.12894458       1.21143437         8.657900
# 2         0.4041118       0.30335962      -0.15421890         8.130813
# 3         0.1600705       0.15040256       0.11662756         8.345056
# 4         0.4107458       1.06564092      -0.31555765         8.089226
# 5         0.7988783       0.40242918      -0.74410725         9.229554
# 6         0.4375493       0.05104021       0.05146796         8.860479
# race      age admission_type_id discharge_disposition_id
# 1       Caucasian  [80-90)                 3                       22
# 2       Caucasian  [60-70)                 3                        1
# 3       Caucasian  [60-70)                 1                        1
# 4       Caucasian  [80-90)                 2                        1
# 5       Caucasian  [60-70)                 1                        1
# 6 AfricanAmerican [90-100)                 1                        3
# admission_source_id max_glu_serum A1Cresult metformin insulin readmittedbin
# 1                   7          None      None        No      Up             0
# 2                   1          None      None        No      Up             0
# 3                   7          None        >8    Steady      Up             0
# 4                   7          None      None        No  Steady             0
# 5                   7          None        >8        No      No             0
# 6                   7          None      None        No  Steady             0


rpart.predict.rose <- predict(cfit.rose, newdata = data.rose)
par(2,2,2,2)

# [[1]]
# NULL
# 
# [[2]]
# NULL
# 
# [[3]]
# NULL
# 
# [[4]]
# NULL

roc.curve(data.rose$readmittedbin, rpart.predict.rose[,2], col= redblue(10000), add =TRUE)
par =TRUE


#Prediction on rose set
rpart.prune.predict3_bin <- predict(cfit.rose, newdata = data.rose,type = "class")

cf.prune_bin <-confusionMatrix(rpart.prune.predict3_bin,objTrain$readmittedbin)
cf.prune_bin

# Confusion Matrix and Statistics
# 
# Reference
# Prediction    0    1
# 0 8833 1166
# 1 8565 1048
# 
# Accuracy : 0.5038          
# 95% CI : (0.4968, 0.5108)
# No Information Rate : 0.8871          
# P-Value [Acc > NIR] : 1               
# 
# Kappa : -0.0077         
# 
# Mcnemar's Test P-Value : <2e-16          
#                                           
#             Sensitivity : 0.5077          
#             Specificity : 0.4734          
#          Pos Pred Value : 0.8834          
#          Neg Pred Value : 0.1090          
#              Prevalence : 0.8871          
#          Detection Rate : 0.4504          
#    Detection Prevalence : 0.5098          
#       Balanced Accuracy : 0.4905          
#                                           
#        'Positive' Class : 0

#Mean error rate
mean.error.rate.rpart.prune2 <- 1- cf.prune_bin$overall[1]
mean.error.rate.rpart.prune2

# Accuracy 
# 0.4961758 

############ Decision tree models-over sampling
cfit.over <- rpart(readmittedbin ~ time_in_hospital + num_lab_procedures + num_procedures + num_medications + number_outpatient + number_emergency + number_inpatient + race + age + admission_type_id + discharge_disposition_id + admission_source_id + number_diagnoses + max_glu_serum + A1Cresult + metformin + insulin,  data = data_balanced_over)
rpart.predict.over <- predict(cfit.over, newdata = data_balanced_over)

# plot(, colorize = TRUE)
# plot(perf2, add = TRUE, colorize = TRUE)
# roc.curve(data_balanced_over$readmittedbin, rpart.predict.over[,2], add =TRUE, col = greenred(2) )
# str(rpart.predict.over)
# confusionMatrix(data_balanced_over$readmittedbin, rpart.predict.over[,2])

# ######## decision tree model-undersampling
cfit.under <- rpart(readmittedbin ~ time_in_hospital + num_lab_procedures + num_procedures + num_medications + number_outpatient + number_emergency + number_inpatient + race + age + admission_type_id + discharge_disposition_id + admission_source_id + number_diagnoses + max_glu_serum + A1Cresult + metformin + insulin, data = data_balanced_under)
rpart.predict.under <- predict(cfit.over, newdata = data_balanced_under)
par(new=TRUE)

## Warning in par(new = TRUE): calling par(new=TRUE) with no plot
#roc.curve(data_balanced_under$readmittedbin, rpart.predict.under[,2], add =TRUE, col = bluered(2))

# decision tree model-both under and over sampling
cfit.both <- rpart(readmittedbin ~ time_in_hospital + num_lab_procedures + num_procedures + num_medications + number_outpatient + number_emergency + number_inpatient + race + age + admission_type_id + discharge_disposition_id + admission_source_id + number_diagnoses + max_glu_serum + A1Cresult + metformin + insulin, data = data_balanced_both)
rpart.predict.both <- predict(cfit.both, newdata = data_balanced_both)
#roc.curve(data_balanced_both$readmittedbin, rpart.predict.both[,2], add =TRUE, col = redblue(5))

# ROC curve comparison verificar por qué no funciona
img1 <-  rasterGrob(as.raster(readPNG("~/Archivos Rstudio/dataset_diabetes/ROC curve comparison.png")), interpolate = FALSE)
#img1
grid.arrange(img1,ncol = 1)

# Analyze the data using random forests. Report the mean error rate and the confusion matrix.
rf.diabetes_bin <- randomForest(readmittedbin ~ time_in_hospital + num_lab_procedures + num_procedures + num_medications + number_outpatient + number_emergency + number_inpatient + race + age + admission_type_id + discharge_disposition_id + admission_source_id + number_diagnoses + max_glu_serum + A1Cresult + metformin + insulin, data = objTrain,importance=TRUE)
rf.diabetes_bin

# Call:
#   randomForest(formula = readmittedbin ~ time_in_hospital + num_lab_procedures +      num_procedures + num_medications + number_outpatient + number_emergency +      number_inpatient + race + age + admission_type_id + discharge_disposition_id +      admission_source_id + number_diagnoses + max_glu_serum +      A1Cresult + metformin + insulin, data = objTrain, importance = TRUE) 
# Type of random forest: classification
# Number of trees: 500
# No. of variables tried at each split: 4
# 
# OOB estimate of  error rate: 11.33%
# Confusion matrix:
#   0  1 class.error
# 0 17352 46 0.002643982
# 1  2177 37 0.983288166

rf.predict_bin <- predict(rf.diabetes_bin,newdata =objTest)

#Plotting the errors from Random Forest model:
par(mar=c(3,3,3,3))
plot(rf.diabetes_bin, type="l")

# VERIFICAR ESTOS RESULTADOS
varImpPlot(rf.diabetes_bin,main = "Important Variables")

importance(rf.diabetes_bin)

# 0           1 MeanDecreaseAccuracy
# time_in_hospital         28.382796  -8.6072684            25.655897
# num_lab_procedures       27.373541  -7.4169385            23.891613
# num_procedures           24.028916  -4.8389483            21.271202
# num_medications          37.357838 -11.8978961            33.417657
# number_outpatient        12.114245  -2.2352286            10.422747
# number_emergency         21.394554   6.5900970            22.465689
# number_inpatient         31.196982  33.6520358            40.066962
# race                      4.966195  -3.3495752             3.552354
# age                      13.402293  -0.9687571            12.607425
# admission_type_id        33.817368  -9.1096163            31.322629
# discharge_disposition_id 48.424089  16.1427535            50.191499
# admission_source_id      35.292640 -13.9683893            32.290460
# number_diagnoses         23.914965  -5.7847326            21.707132
# max_glu_serum            11.747073  -2.9774675            11.228884
# A1Cresult                 5.328055  -4.7399076             3.714464
# metformin                 3.284802  -4.5120878             1.469623
# insulin                  12.931488  -2.5193358            11.142779
# MeanDecreaseGini
# time_in_hospital                331.19780
# num_lab_procedures              583.28712
# num_procedures                  235.65644
# num_medications                 497.32761
# number_outpatient               124.11725
# number_emergency                 94.87624
# number_inpatient                221.12108
# race                            112.83261
# age                             302.92001
# admission_type_id               178.24566
# discharge_disposition_id        273.77829
# admission_source_id             159.68681
# number_diagnoses                204.98526
# max_glu_serum                    40.81234
# A1Cresult                       105.67592
# metformin                        93.32326
# insulin                         194.42330

# Confusion Matrix and the mean error rate:

rf.cm_bin <- confusionMatrix(rf.predict_bin,objTest$readmittedbin)
rf.cm_bin$table

# Reference
# Prediction     0     1
# 0 69443  8729
# 1   146   123

#Mean error rate
mean.error.rate.rf <- (1- rf.cm_bin$overall[1])
mean.error.rate.rf

# Accuracy 
# 0.1131424 

# Random on three class response variable
rf.diabetes <- randomForest(readmitted ~ time_in_hospital + num_lab_procedures + num_procedures + num_medications + number_outpatient + number_emergency + number_inpatient + race + age + admission_type_id + discharge_disposition_id + admission_source_id + number_diagnoses + max_glu_serum + A1Cresult + metformin + insulin, data = objTrain,importance=TRUE)
rf.diabetes

# Call:
#   randomForest(formula = readmitted ~ time_in_hospital + num_lab_procedures +      num_procedures + num_medications + number_outpatient + number_emergency +      number_inpatient + race + age + admission_type_id + discharge_disposition_id +      admission_source_id + number_diagnoses + max_glu_serum +      A1Cresult + metformin + insulin, data = objTrain, importance = TRUE) 
# Type of random forest: classification
# Number of trees: 500
# No. of variables tried at each split: 4
# 
# OOB estimate of  error rate: 43.4%
# Confusion matrix:
#   <30  >30   NO class.error
# <30  66  813 1335   0.9701897
# >30  79 2435 4331   0.6442659
# NO   29 1925 8599   0.1851606

# GRAFICANDO LOS RESULTADOS

rf.predict <- predict(rf.diabetes,newdata =objTest)

#Plotting the errors from Random Forest model:
par(mar=c(3,3,3,3))
plot(rf.diabetes, type="l")

# IMPORTANCIA DE LAS VARIABLES
varImpPlot(rf.diabetes,main = "Important Variables")
importance(rf.diabetes)

# <30        >30         NO MeanDecreaseAccuracy
# time_in_hospital         -3.8719084 -6.2277449 23.4962514           14.3198703
# num_lab_procedures       -2.1191807 -5.6690938 26.1519774           17.3987478
# num_procedures           -4.4204705 -6.2565703 15.6875008            7.8577299
# num_medications          -7.4761596 -1.1324919 27.0622720           19.7249782
# number_outpatient        -1.6254091  3.7721043 25.9402481           21.8810129
# number_emergency          7.4347522  2.7656014 34.6626449           29.8041299
# number_inpatient         32.4293747 12.9989401 76.9459511           73.2258398
# race                      0.9109629  1.0897190  0.9999686            1.6582053
# age                      -0.1693514  3.7225255 16.6729949           15.5773613
# admission_type_id        -7.2635613 -3.0486199 28.8785227           23.2896666
# discharge_disposition_id 15.2405845 21.7723779 49.1572375           58.0951746
# admission_source_id      -5.6734504 -4.0556522 41.7736857           36.5407356
# number_diagnoses         -1.1751347  3.6960623 22.5060336           19.7790859
# max_glu_serum             0.1613820 -2.0187603 18.8208174           15.1790977
# A1Cresult                -1.9279560  1.1180715  7.6651554            6.0264217
# metformin                -5.1923591  0.6525803  2.1773337            0.9358518
# insulin                  -1.5927402  0.3006251 13.6684123           10.4811320
# MeanDecreaseGini
# time_in_hospital                1009.8656
# num_lab_procedures              1653.6790
# num_procedures                   667.8786
# num_medications                 1414.5414
# number_outpatient                282.8296
# number_emergency                 199.1503
# number_inpatient                 521.7718
# race                             351.7820
# age                              891.9975
# admission_type_id                471.3160
# discharge_disposition_id         820.6830
# admission_source_id              400.2833
# number_diagnoses                 603.5402
# max_glu_serum                    107.0560
# A1Cresult                        319.6482
# metformin                        286.6185
# insulin                          584.6098

# Confusion Matrix and the mean error rate:

rf.cm <- confusionMatrix(rf.predict,objTest$readmitted)
rf.cm$table

# Reference
# Prediction   <30   >30    NO
# <30   211   255   133
# >30  3369 10243  7330
# NO   5272 17306 34322

#Mean error rate
mean.error.rate.rf <- (1- rf.cm$overall[1])
# This gives error rate
mean.error.rate.rf

# Accuracy 
# 0.4291761 