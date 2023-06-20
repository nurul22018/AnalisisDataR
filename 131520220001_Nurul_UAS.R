#Mengaktifkan paket yang dibutuhkan
library(dplyr)
library(ggplot2)
library(gplots)
library(nlme)
library(AICcmodavg)
library(car)
library(geepack)
library(lme4)
library(nortest)

#1. Membuat boxplot untuk melakukan analisis perbedaan Bart1 berdasarkan Group
boxplot(stroke$Bart1~stroke$Group,xlab = "Intervention Group", ylab = "Barthel Index Score 1", 
        col = c("#F4AED6","#C9F3D3", "#EBE1D9"), 
        main = "Barthel Index Score 1 according to Intervention Group")


#2. Mengecek normalitas data Bart1
shapiro.test(stroke$Bart1)


#3. Menghitung perbedaat Bart1 dan Bart8 dan memasukkannya sebagai Bart_diff
stroke_wide <- stroke_wide %>%
  mutate(Bart_diff=Week8-Week1) %>%
  as.data.frame()


#4. Mengecek normalitas data Bart_diff
shapiro.test(stroke_wide$Bart_diff)


#5. Mengecek kesamaan varians Bart_diff antar Group
bartlett.test(Bart_diff ~ Group, stroke_wide)


#6. Membuat plot untuk mean dan 95% CI dari Bart_diff berdasarkan Group
#a. Menghitung mean dan 95% CI
dt <- stroke_wide %>%
  dplyr::group_by(Group) %>%
  dplyr::summarise(
    mean = mean(Bart_diff),
    lci = t.test(Bart_diff, conf.level = 0.95)$conf.int[1],
    uci = t.test(Bart_diff, conf.level = 0.95)$conf.int[2])

#b. Membuat plot
pl1 <- ggplot(data=dt)
pl1 <- pl1 + geom_line(aes(x = Group, y = mean), group = 1)
pl1 <- pl1 + geom_point(aes(x=Group, y=mean), color = "red")
pl1 <- pl1 + geom_errorbar(aes(x=Group, ymin=lci, ymax=uci), width = 0.4, color = "red", size = 1)
pl1 <- pl1 + geom_text(aes(x=Group, y=lci, label = round(lci,1)), size=2, vjust=1)
pl1 <- pl1 + geom_text(aes(x=Group, y=uci, label = round(uci,1)), size=2, vjust=-1)
pl1 <- pl1 + theme_classic()
pl1 <- pl1 + labs(title = "Mean Plot with 95% CI")
pl1 <- pl1 + labs(x = "Group", y = "Barthel Index Score Mean")

pl1


#7. Melakukan uji ANOVA untuk membandingkan mean Bart_diff antar Group
st_wide_aov <- aov(stroke_wide$Bart_diff~factor(stroke_wide$Group))
summary(st_wide_aov)


#8. Melakukan analisis model linear regresi dengan explanatory variables Time, Group, dan interaksi Time dan Group
mlr1 <- lm(Ability ~ Time + Group + 
             Time*Group, data = stroke_long)
summary(mlr1)


#9. Melakukan analisis model linear regresi dengan explanatory variables Time, Group, tanpa interaksi Time dan Group
mlr2 <- lm(Ability ~ Time + Group, data = stroke_long)
summary(mlr2)


#10. Menghitung AIC model nomor 8 dan 9
models <- list(mlr1, mlr2)
mod.names <- c('ability.time.group.timegroup', 'ability.time.group')
aictab(cand.set = models, modnames = mod.names)


#12. Melakukan analisis mixed model dengan explanatory variable Time, Group, dan random intercept
rnd <- lme(Ability~as.numeric(Time) + as.factor(Group), data = stroke_long,
           random=~1|Subject)
summary(rnd)


#13. Melakukan analisis dengan GEE dengan explanatory variables Time dan Group
#a. Struktur korelasi Exchangeable 
gee1 <- geeglm(Ability~as.numeric(Time) + as.factor(Group),family=gaussian,
               data=stroke_long,id=as.factor(Subject),wave=as.numeric(Time),corst="exchangeable")
summary(gee1)

#b. Struktur korelasi Auto Regressive
gee2 <- geeglm(Ability~as.numeric(Time) + as.factor(Group),family=gaussian,
               data=stroke_long,id=as.factor(Subject),wave=as.numeric(Time),corst="ar1")
summary(gee2)

#c. Struktur korelasi Unstructured
gee3 <- geeglm(Ability~as.numeric(Time) + as.factor(Group),family=gaussian,
               data=stroke_long,id=as.factor(Subject),wave=as.numeric(Time),corst="unstructured")
summary(gee3)


#14. Menghitung AIC dari model GLS
#a. Struktur korelasi Exchangeable 
exch <- corCompSymm(form = ~ 1 | Subject)
gls1 <- gls(Ability~as.numeric(Time) + as.factor(Group),
            data=stroke_long,
            correlation=exch)
summary(gls1)

#b. Struktur korelasi Auto Regressive
ar1 <- corAR1(form = ~ 1 | Subject)
gls2 <- gls(Ability~as.numeric(Time) + as.factor(Group),
            data=stroke_long,
            correlation=ar1)
summary(gls2)

#c. Struktur korelasi Unstructured
un <- corSymm(form = ~ 1 | Subject)
gls3 <- gls(Ability~as.numeric(Time) + as.factor(Group),
            data=stroke_long,
            correlation=un)
summary(gls3)


#15. Membuat tabel perbandingan AIC dari model nomor 9 dan 14
aic = AIC(mlr3,gls1,gls2,gls3)

