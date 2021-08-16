library(openxlsx)
library(pROC)
library(ROCit)

dat = read.xlsx("Lab Statistics/Data/ROC data for John Pearson June21.xlsx")

roc(dat$Death.at.1.yr,dat$Marker.B)

rocA=roc(dat$Death.at.1.yr,dat$Marker.A)
rocB=roc(dat$Death.at.1.yr,dat$Marker.B)

roc.test(rocA, rocB, method="specificity", specificity=0.9)


Test <- factor(dat$Marker.B<125,labels=c("+","-"))
# GOT normal range value from https://my.clevelandclinic.org/health/diagnostics/16814-nt-prob-type-natriuretic-peptide-bnp
epiR::epi.tests(table(Test,dat$Death.at.1.yr))


plot(rocA,print.thres="best", print.thres.best.method="youden", legacy.axes = TRUE)
plot(rocB,print.thres="best", print.thres.best.method="youden", add=T, col='blue',legacy.axes = TRUE)


