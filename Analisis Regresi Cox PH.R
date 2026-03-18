# Import library
library(survival)
library(survminer)
library(readr)
library(psych)

# Masukkan data
data = read.csv("/Users/nadira/Downloads/processed_heart.csv", sep = ";")

# Pemodelan Regresi Cox PH
modelcoxph = coxph(Surv(time, DEATH_EVENT) ~ age + anaemia + creatinine_phospho + 
                diabetes + as.factor(ejection_fraction) + high_blood_pressure + as.factor(platelets) + 
                serum_creatinine + serum_sodium + sex + smoking, data = data)
summary(modelcoxph)

# Pemilihan Model Terbaik (backward) Manual 
# variabel dihilangkan berdasarkan p-value terbesar
model1 = coxph(Surv(time, DEATH_EVENT) ~ age + anaemia + creatinine_phospho + diabetes 
               + as.factor(ejection_fraction) + high_blood_pressure + as.factor(platelets)
               + serum_creatinine + serum_sodium + sex + smoking, data = data)
summary(model1)

model2 = coxph(Surv(time, DEATH_EVENT) ~ age + anaemia + creatinine_phospho + 
                 diabetes + as.factor(ejection_fraction) + high_blood_pressure + 
                 serum_creatinine + serum_sodium + sex + smoking, data = data)
summary(model2)

model3 = coxph(Surv(time, DEATH_EVENT) ~ age + anaemia + creatinine_phospho + 
                 diabetes + as.factor(ejection_fraction) + high_blood_pressure + 
                 serum_creatinine + serum_sodium + sex, data = data)
summary(model3)

model4 = coxph(Surv(time, DEATH_EVENT) ~ age + anaemia + creatinine_phospho + 
                 diabetes + as.factor(ejection_fraction) + high_blood_pressure + 
                 serum_creatinine + serum_sodium, data = data)
summary(model4)

model5 = coxph(Surv(time, DEATH_EVENT) ~ age + anaemia + creatinine_phospho + 
                 as.factor(ejection_fraction) + high_blood_pressure + 
                 serum_creatinine + serum_sodium, data = data)
summary(model5)

model6 = coxph(Surv(time, DEATH_EVENT) ~ age + creatinine_phospho + as.factor(ejection_fraction)
               + high_blood_pressure + serum_creatinine + serum_sodium, data = data)
summary(model6)

model7 = coxph(Surv(time, DEATH_EVENT) ~ age + creatinine_phospho + as.factor(ejection_fraction)
               + high_blood_pressure + serum_sodium, data = data)
summary(model7)

model8 = coxph(Surv(time, DEATH_EVENT) ~ age + as.factor(ejection_fraction) + high_blood_pressure
               + serum_sodium, data = data)
summary(model8)

AIC(model1, model2, model3, model4, model5, model6, model7, model8)

# Pemilihan Model Terbaik (backward) dengan Kode R
step(modelcoxph, direction = "backward")