library(KMsurv)
library(survival)
library(ggplot2)
library(survminer)
library(readr)

heart1 = read.csv("/Users/HP/Downloads/processed_heart.csv")


--- Uji Asumsi ---

mv_fit1 <- coxph(Surv(time,DEATH_EVENT) ~ age + anaemia + creatinine_phosphokinase + as.factor(ejection_fraction) + high_blood_pressure + serum_creatinine + serum_sodium, data = heart1)
ccox <- cox.zph(mv_fit1)
ccox


----- Plot log(H(t)) -----
  
# anaemia
plot(survfit(Surv(time, DEATH_EVENT) ~ creatinine_phosphokinase, data=heart1),
       fun="cloglog", lty=1:2, mark.time=FALSE,
       xlab="Waktu Survival, T", ylab="log(H(t))")

legend(1, 1, lty=1:2, legend=c("0", "1"), bty='n')

# creatinine_phosphokinase
plot(survfit(Surv(time, DEATH_EVENT) ~ creatinine_phosphokinase, data=heart1),
     fun="cloglog", lty=1:2, mark.time=FALSE,
     xlab="Waktu Survival, T", ylab="log(H(t))")

legend(1, 1, lty=1:2, legend=c("0", "1"), bty='n')

# ejection_fraction
plot(survfit(Surv(time, DEATH_EVENT) ~ ejection_fraction, data=heart1),
     fun="cloglog", lty=1:3, mark.time=FALSE,
     xlab="Waktu Survival, T", ylab="log(H(t))")

legend(1, 1, lty=1:3, legend=c("0", "1", "2"), bty='n')

# high_blood_pressure
plot(survfit(Surv(time, DEATH_EVENT) ~ high_blood_pressure, data=heart1),
     fun="cloglog", lty=1:2, mark.time=FALSE,
     xlab="Waktu Survival, T", ylab="log(H(t))")

legend(1, 1, lty=1:2, legend=c("0", "1"), bty='n')

# serum_creatinine
plot(survfit(Surv(time, DEATH_EVENT) ~ serum_creatinine, data=heart1),
     fun="cloglog", lty=1:2, mark.time=FALSE,
     xlab="Waktu Survival, T", ylab="log(H(t))")

legend(1, 1, lty=1:2, legend=c("0", "1"), bty='n')

# serum_sodium
plot(survfit(Surv(time, DEATH_EVENT) ~ serum_sodium, data=heart1),
     fun="cloglog", lty=1:2, mark.time=FALSE,
     xlab="Waktu Survival, T", ylab="log(H(t))")

legend(1, 1, lty=1:2, legend=c("0", "1"), bty='n')
