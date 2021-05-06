library(survival)
library(survminer)

rak <- ovarian
rak.surv <- Surv(time = ovarian$futime, event = ovarian$fustat)
rak.surv  

#ggsurvplot(rak.fit, data=rak, pval = TRUE)

rak.hist <- hist(rak$age)

rak$age <- ifelse(rak$age >= 55, 1, 0) #granica podziału ===> 55 lat

# AGE
rak.fit.age <- survfit(rak.surv ~ age, data=rak)
summary(rak.fit.age)

rak.result.age <- survfit(rak.surv ~ age, data = rak)

g1 <- ggsurvplot(rak.result.age, data=rak, pval = TRUE)
g1

# RESID.DS
rak.fit.resid <- survfit(rak.surv ~ resid.ds, data=rak)
summary(rak.fit.resid)

rak.result.resid <- survfit(rak.surv ~ resid.ds, data = rak)

g2 <- ggsurvplot(rak.result.resid, data=rak, pval = TRUE)
g2

# ECOG.PS
rak.fit.ecog <- survfit(rak.surv ~ ecog.ps, data=rak)
summary(rak.fit.resid)

rak.result.ecog <- survfit(rak.surv ~ ecog.ps, data = rak)

g3 <- ggsurvplot(rak.result.ecog, data=rak, pval = TRUE)
g3

#H0: Funkcje przeżycia w różnych grupach nie różnią się statystycznie od siebie.

# AGE: S(t) różnią się statystycznie w różnych gruupach.
# Resid: S(t) nie różnią się staystycznie w grupach.
# Ecog: S(t) nie różnią się statystycznie w grupach.