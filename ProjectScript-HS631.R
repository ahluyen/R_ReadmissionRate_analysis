setwd("/Users/Andy/Documents/USFFall2018/HS631")

library(ggplot2)
library(corrplot)
library(pwr)
install.packages("car")
library(car)
library(pscl)
library(stats)
library(base)
install.packages("dplyr")
install.packages("rlang")
library (dplyr)

diabetic <- read.csv(file="diabetic_data.csv", header =T, na.strings = '?') #remove all missing values with NA. 
summary(diabetic)

#########Part 1: EDA ##############

#Remove repeated measure: 
#diabetic <- diab
diabetic <- diabetic %>% group_by(patient_nbr) %>% filter(encounter_id == min(encounter_id)) 
summary(diabetic)

#Medical Specialty: too many factor levels in Medical Specialty (~72), collapse to 4: 
summary(diabetic$medical_specialty) 

diabetic$medical_specialty <- as.character(diabetic$medical_specialty)

Surgery_collapse <- which(diabetic$medical_specialty=="Surgery-General" |diabetic$medical_specialty=="Surgery-Neuro" |
                    diabetic$medical_specialty=="Surgery-Plastic" | diabetic$medical_specialty =="Surgery-Thoracic" |
                    diabetic$medical_specialty=="SurgicalSpecialty" | diabetic$medical_specialty =="Surgery-Cardiovascular"|
                    diabetic$medical_specialty =="Surgery-Colon&Rectal" | diabetic$medical_specialty == "Surgery-Maxillofacial"|
                    diabetic$medical_specialty =="Surgery-Pediatric" | diabetic$medical_specialty =="Surgery-PlasticwithinHeadandNeck" |
                    diabetic$medical_specialty == "Surgery-Vascular" | diabetic$medical_specialty =="Surgery-Cardiovascular/Thoracic")

diabetic$medical_specialty[Surgery_collapse] <- "Surgery" 

keep_ind <- which(diabetic$medical_specialty=="Emergency/Trauma" |diabetic$medical_specialty=="Cardiology" |
                    diabetic$medical_specialty=="InternalMedicine" | diabetic$medical_specialty=="Surgery")  

diabetic$medical_specialty[-keep_ind] <- NA 

diabetic$medical_specialty <- as.factor(diabetic$medical_specialty)

lev <- c("Cardiology", "Emergency/Trauma", "InternalMedicine", "Surgery")
diabetic$medical_specialty <- factor(diabetic$medical_specialty, levels = lev)

summary(diabetic$medical_specialty) # too many NAs (~48,120) 

##Distribution of medical_specialty: 
#plot(diabetic$medical_specialty)

g_MS <- ggplot(data = subset(diabetic, !is.na(diabetic$medical_specialty)), aes(x = medical_specialty, y = frequency(medical_specialty))) + 
        geom_bar(stat = "identity", color = "blue") + ggtitle("Distribution of Medical Specialty") + 
        xlab("Type of Medical Specialty") + ylab("Count")
g_MS

#Collapse admission_type_id: NULL, Not Available, and not mapped into NA
summary(diabetic$admission_type_id)

diabetic$admission_type_id <- as.numeric(as.character(diabetic$admission_type_id))
diabetic$admission_type_id[diabetic$admission_type_id==5] <- NA #Not Available
diabetic$admission_type_id[diabetic$admission_type_id==6] <- NA #NULL 
diabetic$admission_type_id[diabetic$admission_type_id==8] <- NA #Not Mapped

summary(diabetic$admission_type_id)

diabetic$admission_type_id[diabetic$admission_type_id==1] <- 'Emergency'
diabetic$admission_type_id[diabetic$admission_type_id==2] <- 'Urgent'
diabetic$admission_type_id[diabetic$admission_type_id==3] <- 'Elective'
diabetic$admission_type_id[diabetic$admission_type_id==4] <- 'Newborn'
diabetic$admission_type_id[diabetic$admission_type_id==7] <- 'Trauma Center'

diabetic$admission_type_id <- as.factor(diabetic$admission_type_id)

summary(diabetic$admission_type_id)
#plot(diabetic$admission_type_id, main = "Distribution of Admission Type", xlab = "Admission Type")

which(diabetic$admission_type_id == "Trauma Center")

g_AT <- ggplot(data = subset(diabetic, !is.na(diabetic$admission_type_id)), aes(x = admission_type_id, y = frequency(admission_type_id))) + 
  geom_bar(stat = "identity", color = "blue") + ggtitle("Distribution of Admission Type") + 
  xlab("Type of Admission") + ylab("Count")
g_AT

##Weight and Age are definitely in the wrong order, reorder needed.

#Reordering weight:
summary(diabetic$weight) #97% of weight value is missing, consider drop out this variable.
lev2 <- c('[0-25)','[25-50)','[50-75)','[75-100)','[100-125)','[125-150)','[150-175)','[175-200)','>200')
diabetic$weight <- factor(diabetic$weight, levels = lev2)

#Reordering Age: 
summary(diabetic$age)
lev3 <- c('[0-10)','[10-20)','[20-30)','[30-40)','[40-50)','[50-60)','[60-70)','[70-80)','[80-90)','[90-100)')
diabetic$age <- factor(diabetic$age, levels = lev3)
#plot(diabetic$age)

g_Age <- ggplot(data = diabetic, aes(x = age, y = frequency(age))) + 
  geom_bar(stat = "identity", color = "blue") + xlab("Age Level") + ylab("Count")
g_Age

#Collapse payer code: lot of missing info - consider using this variable. 
summary(diabetic$payer_code)
diabetic$payer_code <- as.character(diabetic$payer_code)
diabetic$payer_code[diabetic$payer_code == 'SP'] <- "Self-paid"
diabetic$payer_code[diabetic$payer_code == 'MC'] <- "Medi-care"
keep_ind2 <- which(diabetic$payer_code == 'Self-paid'| diabetic$payer_code == 'Medi-care'|
                   diabetic$payer_code == 'MD')
diabetic$payer_code[-keep_ind2] <- "Commercial Insurance"

diabetic$payer_code <- as.factor(diabetic$payer_code)
summary(diabetic$payer_code)
#plot(diabetic$payer_code, main = "Distribution of Payer Type")

g_Payer <- ggplot(data = diabetic, aes(x = payer_code, y = frequency(payer_code))) + 
  geom_bar(stat = "identity", color = "blue") + ggtitle("Distribution of Payer Type") +
  xlab("Payer Type") + ylab("Count")
g_Payer

#Collapse Discharge_disposition_id: 

class(diabetic$discharge_disposition_id)
summary(diabetic$discharge_disposition_id)
diabetic$discharge_disposition_id <- as.character(diabetic$discharge_disposition_id)

diabetic$discharge_disposition_id[diabetic$discharge_disposition_id == 1] <- "Discharged/Home"

discharge_collapse <- which(diabetic$discharge_disposition_id == 2 | diabetic$discharge_disposition_id == 3
                   |diabetic$discharge_disposition_id == 4 | diabetic$discharge_disposition_id == 5
                   |diabetic$discharge_disposition_id == 6 | diabetic$discharge_disposition_id== 8
                   |diabetic$discharge_disposition_id== 15 | diabetic$discharge_disposition_id== 16
                   |diabetic$discharge_disposition_id== 17 | diabetic$discharge_disposition_id== 30 
                   |diabetic$discharge_disposition_id== 27 | diabetic$discharge_disposition_id== 28
                   |diabetic$discharge_disposition_id== 29 | diabetic$discharge_disposition_id== 22
                   |diabetic$discharge_disposition_id== 23 | diabetic$discharge_disposition_id== 24)  
diabetic$discharge_disposition_id[discharge_collapse] <- "Discharged/Transfer" 

summary(diabetic$discharge_disposition_id)

hospice_collapse <- which(diabetic$discharge_disposition_id == 13 | diabetic$discharge_disposition_id == 14)  
diabetic$discharge_disposition_id[hospice_collapse] <- "Hospice" 

NA_collapse <- which(diabetic$discharge_disposition_id== 18 | diabetic$discharge_disposition_id == 25
                     |diabetic$discharge_disposition_id == 26)  
diabetic$discharge_disposition_id[NA_collapse] <- NA

Expired_collapse <- which(diabetic$discharge_disposition_id== 11 | diabetic$discharge_disposition_id == 19
                           |diabetic$discharge_disposition_id == 20, diabetic$discharge_disposition_id == 21)
diabetic$discharge_disposition_id[Expired_collapse] <- "Expired"

Other_collapse <- which(diabetic$discharge_disposition_id== 7 | diabetic$discharge_disposition_id == 9
                          |diabetic$discharge_disposition_id == 12 | diabetic$discharge_disposition_id == 10)
diabetic$discharge_disposition_id[Other_collapse] <- "Other"

diabetic$discharge_disposition_id <- as.factor(diabetic$discharge_disposition_id)

summary(diabetic$discharge_disposition_id)
#plot(diabetic$discharge_disposition_id)

g_dis <- ggplot(data = subset(diabetic, !is.na(diabetic$discharge_disposition_id)), aes(x = discharge_disposition_id, y = frequency(discharge_disposition_id))) + 
  geom_bar(stat = "identity", color = "blue") + ggtitle("Discharge Type") + 
  xlab("Discharge Type") + ylab("Count")
g_dis

#Race:
summary(diabetic$race)
#plot(diabetic$race)

g_race <- ggplot(data = subset(diabetic, !is.na(diabetic$race)), aes(x = race, y = frequency(race))) + 
  geom_bar(stat = "identity", color = "blue") + ggtitle("Distribution of Race") +
  xlab("Race") + ylab("Count")
g_race

# #Gender: 
summary(diabetic$gender)
#plot(diabetic$gender)

g_gender <- ggplot(data = diabetic, aes(x = gender, y = frequency(gender))) + 
  geom_bar(stat = "identity", color = "blue") + ggtitle("Distribution of Gender") +
  xlab("Gender") + ylab("Count")
g_gender

summary(diabetic$time_in_hospital)
class(diabetic$time_in_hospital)

male <- diabetic[diabetic$gender == "Male",]
female <- diabetic[diabetic$gender == "Female",]

male$time_in_hospital
female$time_in_hospital

t.test(female$time_in_hospital,male$time_in_hospital, alternative = "two.sided",
       conf.level = 0.95, var.equal = TRUE)

#pwr.t.test(n1 = length(male), n2 = length(female), d = .2, sig.level = .05)

summary(diabetic$time_in_hospital)

g9 <- ggplot(data = diabetic) 
g9 + geom_bar(aes(x=time_in_hospital, fill=factor(gender)), position = "dodge") +
              ggtitle("Relationship between Length of Stay and Gender") + 
              xlab("Length of Stay") + ylab("Gender")

#Readmitted: 
summary(diabetic$readmitted)
#plot(diabetic$readmitted)

g_readmit <- ggplot(data = diabetic, aes(x = readmitted, y = frequency(readmitted))) + 
  geom_bar(stat = "identity", color = "blue") + ggtitle("Readmission") +
  xlab("Readmission Type") + ylab("Count")
g_readmit

#Insulin: need to be reordered
summary(diabetic$insulin)

lev4 <- c("No", "Down", "Steady", "Up")
diabetic$insulin <- factor(diabetic$insulin, levels = lev4)

#Other Medications:
summary(diabetic$glipizide)
diabetic$glipizide <- factor(diabetic$glipizide, levels = lev4)

summary(diabetic$metformin)
diabetic$metformin <- factor(diabetic$metformin, levels = lev4)

summary(diabetic$glimepiride)
diabetic$glimepiride <- factor(diabetic$glimepiride, levels = lev4)

summary(diabetic$glyburide)
diabetic$glyburide <- factor(diabetic$glyburide, levels = lev4)

summary(diabetic$rosiglitazone)
diabetic$rosiglitazone <- factor(diabetic$rosiglitazone, levels = lev4)

#Glucose serum test result: Mostly "None"
summary(diabetic$max_glu_serum)
plot(diabetic$max_glu_serum)

#A1C Result: Mostly "None"
summary(diabetic$A1Cresult)
plot(diabetic$A1Cresult)

#Diagnosis: 
summary(diabetic$diag_1)
summary(diabetic$diag_2)
summary(diabetic$diag_3)

#Length of Stay: 
summary(diabetic$time_in_hospital)
hist(diabetic$time_in_hospital, main = "Distribution of Length of Stay", 
     xlab = 'Length of Stay')

#Change:
summary(diabetic$change)
diabetic$change <- as.character(diabetic$change)
diabetic$change[diabetic$change == "Ch"] <- "Yes"
diabetic$change <- as.factor(diabetic$change)
summary(diabetic$change)

#Other variables:
summary(diabetic$diabetesMed)
summary(diabetic$number_inpatient)
summary(diabetic$number_diagnoses)
summary(diabetic$number_outpatient)
summary(diabetic$number_emergency)
summary(diabetic$num_lab_procedures)
summary(diabetic$num_medications)
summary(diabetic$num_procedures)
summary(diabetic$num_lab_procedures)

#num_medications + LOS
cor.test(diabetic$time_in_hospital, diabetic$num_medications)

#num_diagnoses + LOS
cor.test(diabetic$time_in_hospital,diabetic$number_diagnoses)
##Length of stay and number of diagnoses has a positively correlation with each other. 

gD <- ggplot(data = diabetic, aes(x= number_emergency, y=time_in_hospital, col = gender)) + geom_point()


cor.test(diabetic$time_in_hospital, diabetic$num_lab_procedures)
cor.test(diabetic$time_in_hospital, diabetic$number_emergency)
cor.test(diabetic$time_in_hospital, diabetic$number_outpatient)
cor.test(diabetic$time_in_hospital, diabetic$number_inpatient)

#Relationship between Readmission and Admission Type: 
g <- ggplot(data = diabetic) 
g + geom_bar(aes(x = admission_type_id, fill = readmitted, na.rm = TRUE),position = "dodge") 
  + ggtitle("Relationship between Readmission and Admission Type")  
  + xlab("Admission Type") + ylab("Frequency")

chisq.test(table(diabetic$admission_type_id, diabetic$readmitted))
##      Pearson's Chi-squared test

##data:  table(diabetic$admission_type_id, diabetic$readmitted)
##X-squared = 142.34, df = 8, p-value < 2.2e-16

##Chi-squared test gave us a really small p-value meaning we have enough 
##evidence to reject the null hypothesis and accept the alternative hypothesis,
##which is admission type and readmitted are not indenpendent, there is a relationship
##between 2 variables. 

##Relationship between A1C result and readmission: 
g <- ggplot(data = diabetic) 
g + geom_bar(aes(x = A1Cresult, fill = readmitted), position = "dodge") 
  + ggtitle("Relationship between A1Cresult and Admission Type")  
  + xlab("A1Cresult") + ylab("Frequency")

chisq.test(table(diabetic$A1Cresult, diabetic$readmitted))
##      Pearson's Chi-squared test

##data:  table(diabetic$A1Cresult, diabetic$readmitted)
##X-squared = 37.131, df = 6, p-value = 1.66e-06

##Same as above 

#LOS + Admission Type: 
g + geom_histogram(aes(x=time_in_hospital, fill=factor(admission_type_id)), binwidth=5,position="dodge")

#Age + LOS: 
g3 <- ggplot(diabetic, aes(x=age, y = time_in_hospital))
g3 + geom_boxplot() + ggtitle("Relationship between Age and Length of Stay")  + xlab("Age")+ ylab("Length of Stay")

#wilcox.test(as.numeric(time_in_hospital) ~ age, data=diabetic)

kruskal.test(as.numeric(time_in_hospital) ~ age, data=diabetic)

#LOS + Admission Type Boxplot:
g4 <- ggplot(diabetic, aes(x=admission_type_id, y = time_in_hospital))
g4 + geom_boxplot() + ggtitle("Relationship between Admission Type and Length of Stay")  + xlab("Admission Type")+ ylab("Length of Stay")

kruskal.test(as.numeric(time_in_hospital) ~ admission_type_id, data=diabetic)


#Discharge and Readmission: 
g5 <- ggplot(data = diabetic) 
g5 + geom_bar(aes(x = discharge_disposition_id, fill = readmitted), position = "dodge") 
+ ggtitle("Relationship between Discharge Type and Admission Type")  
+ xlab("Discharge Type") + ylab("Frequency")

chisq.test(table(diabetic$discharge_disposition_id, diabetic$readmitted))

#Pearson's Chi-squared test

#data:  table(diabetic$discharge_disposition_id, diabetic$readmitted)
#X-squared = 1698.2, df = 8, p-value < 2.2e-16

chisq.test(diabetic$admission_type_id,diabetic$medical_specialty)

##> chisq.test(diabetic$admission_type_id,diabetic$new_med_spec)

##    Pearson's Chi-squared test

##data:  diabetic$admission_type_id and diabetic$new_med_spec
##X-squared = 3862.4, df = 6, p-value < 2.2e-16


#####Linear Regression######

fit1 <- lm(time_in_hospital~ age + gender + race + admission_type_id + medical_specialty 
           + discharge_disposition_id + A1Cresult + 
             + insulin + max_glu_serum + number_diagnoses + num_lab_procedures + number_emergency
           + number_outpatient + number_inpatient + diabetesMed + change, data = diabetic)
summary(fit1)

#Take out insignificant variables: rosiglitazone, metformin, change, diabetesMed, 
#number_inpatient, gender, age, max_glu_serum, A1Cresult, num_outpatient

fit2 <- lm(time_in_hospital~ race + admission_type_id + medical_specialty 
           + discharge_disposition_id
           + insulin + number_diagnoses + num_lab_procedures + number_emergency , data = diabetic)
summary(fit2)

# --> final intepretable model for now. 


####### Logistic Regression ###########

#pR2 function: 
pR2 <- function(LogModel) {
  dev <- LogModel$deviance 
  nullDev <- LogModel$null.deviance 
  modelN <-  length(LogModel$fitted.values)
  R.l <-  1 -  dev / nullDev
  R.cs <- 1- exp ( -(nullDev - dev) / modelN)
  R.n <- R.cs / ( 1 - ( exp (-(nullDev / modelN))))
  cat("Pseudo R^2 for logistic regression\n")
  cat("Hosmer and Lemeshow R^2  ", round(R.l, 4), "\n")
  cat("Cox and Snell R^2        ", round(R.cs, 4), "\n")
  cat("Nagelkerke R^2           ", round(R.n, 4),    "\n")
}

summary(diabetic$readmitted)
class(diabetic$readmitted)

readmit_lt30_idx <- which(diabetic$readmitted=="<30")

no_readmit_idx <- which(diabetic$readmitted=="NO")

diabetic_reg2 <- diabetic[c(readmit_lt30_idx,no_readmit_idx) , ]

summary(diabetic_reg2$readmitted)

#1: < 30 vs. "No"

fit_readmit <- glm(readmitted~age + race + gender + admission_type_id + discharge_disposition_id 
                   + insulin + max_glu_serum + number_diagnoses + num_lab_procedures 
                   + number_emergency + number_outpatient + number_inpatient + diabetesMed + change
                   + time_in_hospital, family=binomial(), data = diabetic_reg2)
summary(fit_readmit)

##take out insignificant variables: age, race, gender, max_glu_serum, number_lab_procedures,
##number_outpatient, change

fit_readmit_2 <- glm(readmitted~admission_type_id + discharge_disposition_id 
                    + insulin + number_diagnoses 
                    + number_emergency + number_inpatient + diabetesMed
                    + time_in_hospital, family=binomial(), data = diabetic_reg2)
summary(fit_readmit_2)

##take out insulin

fit_readmit_3 <- glm(readmitted~admission_type_id + discharge_disposition_id 
                + number_diagnoses 
                + number_emergency + number_inpatient + diabetesMed
                + time_in_hospital, family=binomial(), data = diabetic_reg2)
summary(fit_readmit_3)

exp(coef(fit_readmit_3))

#odd
exp(5.450854e-01)-1 #0.7247

exp(cbind(OR=coef(fit_readmit_3), confint(fit_readmit_3)))

vif(fit_readmit_3)
exp(coef(fit_readmit_3))
pR2(fit_readmit_3)


#2 >30 and "No" vs <30

diabetic2 <- diabetic
summary(diabetic2$readmitted)
diabetic2$readmitted <- as.character(diabetic2$readmitted)
readmit_collapse <- which(diabetic2$readmitted == "NO" | diabetic2$readmitted == ">30")
diabetic2$readmitted[readmit_collapse] <- ">30 or No"

diabetic2$readmitted <- as.factor(diabetic2$readmitted)

summary(diabetic2$readmitted)

fit_readmit_4 <- glm(readmitted~age + race + gender + admission_type_id + discharge_disposition_id 
                     + insulin + A1Cresult + max_glu_serum + number_diagnoses + num_lab_procedures 
                     + number_emergency + number_outpatient + number_inpatient + diabetesMed + change
                     + time_in_hospital, family=binomial(), data = diabetic2)
summary(fit_readmit_4)

##take out insignificant variables: age, race, gender, admission_type_id, A1Cresult, max_glu,
##number_lab_procedures, number_emergency, number_outpatient, change, time_in_hospital

fit_readmit_5 <- glm(readmitted~discharge_disposition_id 
                     + insulin + number_diagnoses 
                     + number_inpatient + diabetesMed
                     , family=binomial(), data = diabetic2)
summary(fit_readmit_5)

#take out insulin: 

fit_readmit_6 <- glm(readmitted~discharge_disposition_id 
                     + number_diagnoses 
                     + number_inpatient + diabetesMed
                     , family=binomial(), data = diabetic2)
summary(fit_readmit_6)

exp(coef(fit_readmit_6))
#odd
exp(5.275409e-01) - 1 #0.6947
exp(cbind(OR=coef(fit_readmit_6), confint(fit_readmit_6)))

 
vif(fit_readmit_6)
exp(coef(fit_readmit_6))
pR2(fit_readmit_6)
