#### Let's try to predict strokes, diabetes and hypertension

dataset = read.csv('health_data.csv')

### Data exploration
xx = list(1:15)   
for(x in xx){
  boxplot(dataset[,x], dataset = dataset)$out
}
boxplot(dataset[,13:15], dataset = dataset, names)

### so we have outliers in BMI, MentalHlth, PhysHlt
#### Class Imbalance on Stroke, we'll fix that later. 
##### Also a Data Imbalance porblem

names(dataset)

## reviewing data quality

library("dplyr")                   
str(dataset)
table(dataset$Diabetes)

### Let's remove outliers by capping them
dataset$BMI[dataset$BMI > 41] = 40
dataset$BMI[dataset$BMI < 14] = 15
dataset$MentHlth[dataset$MentHlth > 5] = 4
dataset$PhysHlth[dataset$PhysHlth > 14] = 13

## Reviewing if the outliers have dissapeared
xx = list(1:15)   
for(x in xx){
  boxplot(ds_final[,x], dataset = ds_final)
}

table(ds_final$HvyAlcoholConsump)

## Reviewing Correlations
round(cor(dataset),2)
round(cor(ds_final),2)

## The correlations are very poor, only a few Features have a little of predictive power

# Shuffle before split.
ds_final = dataset[sample(1:nrow(dataset)), ]

### Scale the data
ds_final[,c("Age", "BMI", "GenHlth", "PhysHlth", "MentHlth")] = scale(ds_final[,c("Age", "BMI", "GenHlth", "PhysHlth", "MentHlth")]) 

# Let's divide the dataset intro three
library(caTools)
set.seed(123)
split = sample.split(ds_final$Diabetes, SplitRatio =0.8)
training_set_diab = subset(ds_final, split =T)
test_set_diab = subset(ds_final, split =F)

split = sample.split(ds_final$Hypertension, SplitRatio =0.8)
training_set_hyp = subset(ds_final, split =T)
test_set_hyp = subset(ds_final, split =F)

split = sample.split(ds_final$Stroke, SplitRatio =0.8)
training_set_stroke = subset(ds_final, split =T)
test_set_stroke = subset(ds_final, split =F)

##### I'll use PCA to check which features can help to predict ##################################
library(caret)
library(e1071)

###############################################################################################
############Now we process the data with PCA's################################################
#### First I'll start with  7 PCA's

### DIABETES
pca_diab = preProcess(x=training_set_diab[,-16],
                      method = "pca",
                      pcaComp=7)

training_set_diab1 = predict(pca_diab, training_set_diab)
training_set_diab1 = training_set_diab1[,c(2,3,4,5,6,7,8,1)]

test_set_diab1 = predict(pca_diab, test_set_diab)
test_set_diab1 = test_set_diab1[,c(2,3,4,5,6,7,8,1)]

## MODEL
classifier_diabetes = naiveBayes(training_set_diab1[,-8],
                        y = training_set_diab1$Diabetes)

y_pred_d = predict(classifier_diabetes, newdata = test_set_diab1[,-8])

### Confusion Matrix
cm_D= table(test_set_diab1[,8],y_pred_d)
accuracy_DIAB = (cm_D[1,1] + cm_D[2,2]) / (cm_D[1,1] + cm_D[2,2] + cm_D[1,2] + cm_D[2,1])

################################################################################################

### HYPERTENSION

pca_hyp = preProcess(x=training_set_hyp[,-17],
                      method = "pca",
                      pcaComp=7)

training_set_hyp1 = predict(pca_hyp, training_set_hyp)
training_set_hyp1 = training_set_hyp1[,c(2,3,4,5,6,7,8,1)]

test_set_hyp1 = predict(pca_hyp, test_set_hyp)
test_set_hyp1 = test_set_hyp1[,c(2,3,4,5,6,7,8,1)]

### Model
classifier_hyp = naiveBayes(training_set_hyp1[,-8],
                        y = training_set_hyp1$Hypertension)

y_pred_hyp = predict(classifier_hyp, newdata = test_set_hyp1[,-8])

### Confusion Matrix
cm_H= table(test_set_hyp1[,8],y_pred_hyp)
accuracy_H = (cm_H[1,1] + cm_H[2,2]) / (cm_H[1,1] + cm_H[2,2] + cm_H[1,2] + cm_H[2,1])

#####################################################################

### STROKE
pca_s = preProcess(x=training_set_stroke[,-18],
                      method = "pca",
                      pcaComp=7)

training_set_stroke1 = predict(pca_s, training_set_stroke)
training_set_stroke1 = training_set_stroke1[,c(2,3,4,5,6,7,8,1)]

test_set_stroke1 = predict(pca_s, test_set_stroke)
test_set_stroke1 = test_set_stroke1[,c(2,3,4,5,6,7,8,1)]

### MODEL
classifier_s = naiveBayes(training_set_stroke1[,-8],
                        y = training_set_stroke1$Stroke)

y_pred_s = predict(classifier_s, newdata = test_set_stroke1[,-8])

### Confusion Matrix
cm_s = table(test_set_stroke1[,8],y_pred_s)
accuracy_s = (cm_s[1,1] + cm_s[2,2]) / (cm_s[1,1] + cm_s[2,2] + cm_s[1,2] + cm_s[2,1])

########################################################################
#### Let's print the results. 
print("DIABETESSSSSSSSS")
print(cm_D)
print(accuracy_DIAB)
print("HYPOERTENSOSKSDASD")
print(cm_H)
print(accuracy_H)
print("STROKEKEKEKEKEKEK")
print(cm_s)
print(accuracy_s)




###################------- NOW 3 PCAS ------------ ##########################

pca_diab = preProcess(x=training_set_diab[,-16],
                      method = "pca",
                      pcaComp=10)

training_set_diab1 = predict(pca_diab, training_set_diab)
training_set_diab1 = training_set_diab1[,c(2,3,4,5,6,7,8,9,10,11,1)]

test_set_diab1 = predict(pca_diab, test_set_diab)
test_set_diab1 = test_set_diab1[,c(2,3,4,5,6,7,8,9,10,11,1)]

classifier_diabetes = naiveBayes(training_set_diab1[,-11],
                                 y = training_set_diab1$Diabetes)

y_pred_d = predict(classifier_diabetes, newdata = test_set_diab1[,-11])

cm_D= table(test_set_diab1[,11],y_pred_d)
accuracy_DIAB = (cm_D[1,1] + cm_D[2,2]) / (cm_D[1,1] + cm_D[2,2] + cm_D[1,2] + cm_D[2,1])

########################################################################################
pca_hyp = preProcess(x=training_set_hyp[,-17],
                     method = "pca",
                     pcaComp=10)

training_set_hyp1 = predict(pca_hyp, training_set_hyp)
training_set_hyp1 = training_set_hyp1[,c(2,3,4,5,6,7,8,9,10,11,1)]

test_set_hyp1 = predict(pca_hyp, test_set_hyp)
test_set_hyp1 = test_set_hyp1[,c(2,3,4,5,6,7,8,9,10,11,1)]

classifier_hyp = naiveBayes(training_set_hyp1[,-11],
                            y = training_set_hyp1$Hypertension)

y_pred_hyp = predict(classifier_hyp, newdata = test_set_hyp1[,-11])

### Confusio Matrix
cm_H= table(test_set_hyp1[,11],y_pred_hyp)
accuracy_H = (cm_H[1,1] + cm_H[2,2]) / (cm_H[1,1] + cm_H[2,2] + cm_H[1,2] + cm_H[2,1])

#########################################################################################################

pca_s = preProcess(x=training_set_stroke[,-18],
                   method = "pca",
                   pcaComp=10)

training_set_stroke1 = predict(pca_s, training_set_stroke)
training_set_stroke1 = training_set_stroke1[,c(2,3,4,5,6,7,8,9,10,11,1)]

test_set_stroke1 = predict(pca_s, test_set_stroke)
test_set_stroke1 = test_set_stroke1[,c(2,3,4,5,6,7,8,9,10,11,1)]

classifier_s = naiveBayes(training_set_stroke1[,-11],
                          y = training_set_stroke1$Stroke)

y_pred_s = predict(classifier_s, newdata = test_set_stroke1[,-11])

cm_s = table(test_set_stroke1[,11],y_pred_s)
accuracy_s = (cm_s[1,1] + cm_s[2,2]) / (cm_s[1,1] + cm_s[2,2] + cm_s[1,2] + cm_s[2,1])

######################################################################################
#### printing the results

print("Diabetes")
print(cm_D)
print(accuracy_DIAB)
print("Hypertension")
print(cm_H)
print(accuracy_H)
print("Stroke")
print(cm_s)
print(accuracy_s)

####################################################################################
