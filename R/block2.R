## ----setup, include=FALSE-------------------------------------------------------------------------------------------------------------------------------
knitr::opts_chunk$set(echo = TRUE, comment=NA, fig.align = 'center')


## ----datasets, results='hide',error=FALSE, warning=FALSE, message=FALSE,echo=F--------------------------------------------------------------------------
## Read excelsheets
library(openxlsx)

## Packages used for 'data wrangling'
library(tidyverse)
library(data.table)

## Extra stats package
library(rstatix) ## used to help format stats into dfs for ggplot
library(DescTools) ## For Dunnett's test
library(epitools)
library(epiR)
library(mlbench)
library(pROC) ## For ROC curves

## Packages for plotting and table outputs
library(ggplot2)
library(ggsci)
library(ggpubr)
library(ggrepel)
library(cowplot)
library(kableExtra) ## used for to make nicer looking tables


## ----global_functions, echo=FALSE-----------------------------------------------------------------------------------------------------------------------
bxplot <- function(...){ggplot(...)+geom_boxplot(width=0.4, size=1, fatten=1, colour="grey70") +
  geom_point(colour="steelblue", size=1.2, alpha=0.5) +
  theme_bw(base_size=14)}

p.siginf<- function(vec){
  out<-c()
  for(i in 1:length(vec)){
    if(vec[i] < 0.001){
      out <- c(out, "***")
    }else if( vec[i] < 0.01){
      out <- c(out, "**")
    } else if(vec[i] <0.05){
      out <- c(out, "*")
    }else{
      out <- c(out, "ns")
    }
  }     
  return(out)
}


## ----2by2, echo=F---------------------------------------------------------------------------------------------------------------------------------------
data.frame(C1=rep('Exposure',3), C2 = c("Yes","No", "Totals"), plus=c('A', 'C', "A+C"), minus=c('B', 'D',"C+D"), Totals = c("A+B", "C+D", "N")) %>%
  kbl(col.names=c("","", "Yes","No", "Totals"), caption = "2x2 contingency table",align = 'c') %>% 
  kable_classic(html_font = "Cambria", font_size=16) %>% 
  collapse_rows(columns=1) %>% add_header_above(c("","", "Outcome" =2, "")) 


## ----2by2di, echo=F-------------------------------------------------------------------------------------------------------------------------------------
data.frame(C1=rep('Test',3), C2 = c("Yes","No", "Totals"), plus=c('True Positive', 'False Negative', "Number with disease"), minus=c('False Positives', 'True negatives',"Number without disease"), Totals = c("Number who test positive", "Number who test negative", "N")) %>%
  kbl(col.names=c("","", "Yes","No", "Totals"), caption = "2x2 contingency table",align = 'c', table.attr =  'style= "line-height:3ch;"') %>% 
  kable_classic(html_font = "Cambria", font_size=16) %>% 
  collapse_rows(columns=1) %>% add_header_above(c("","", "Disease" =2, "")) %>% footnote(c("Sensitivity = TP/(TP+FN),   Specificity=TN/(FP+TN)", "Positive Predicitve Value=TP/(TP+FP),  Negative Predictive Value=TN/(TN+FN)"), general_title ="" )


## -------------------------------------------------------------------------------------------------------------------------------------------------------
bwt <- MASS::birthwt # load data
bwt.cont<- table(bwt$smoke, bwt$low, dnn =c("Smoke", "Low BW")) # frquency of events 0=no, 1=yes

## the epiR functions require a particular order (e.g. first column is outcome positive, first row is expsoure positive)
bwt.cont <- bwt.cont[2:1,2:1]


## ----bwt2by2, echo=F------------------------------------------------------------------------------------------------------------------------------------
data.frame(C1=rep('Smoking',2), C2 = c("Yes","No"), plus=c('30', '29'), minus=c('44', '86')) %>%
kbl(col.names=c("","", "Yes", "No"),caption = "Frequency of low birth weight and mother smoking status",align = 'c', table.attr =  'style= "width: 45%;"') %>%
    kable_classic(html_font = "Cambria", font_size=16) %>% 
  collapse_rows(columns=1) %>% add_header_above(c("","", "Low birth weight (<2.5kg)" =2)) 



## ----oddsRatio------------------------------------------------------------------------------------------------------------------------------------------
epi.2by2(dat = bwt.cont, method="cohort.count", conf.level = .95)


## ---- expected, echo=F----------------------------------------------------------------------------------------------------------------------------------
data.frame(C1=rep('Smoking',2), C2 = c("Yes","Yes","No","No"), C3 = c("Obs","Exp", "Obs","Exp"), plus=c('30', '23.10', '29', '35.90'), minus=c('44','50.90', '86','79.10')) %>% kbl(col.names=c("","","", "Yes", "No"),caption = "Frequency of observed and exprected child born with low birth weight and smoking status of the mother",align = 'c', table.attr =  'style= "width: 60%;line-height: 3ch;"') %>%  kable_classic(html_font = "Cambria", font_size=16) %>% 
  collapse_rows(columns=1:2) %>% add_header_above(c("","","", "Low birth weight (<2.5kg)" =2))


## ----chisq----------------------------------------------------------------------------------------------------------------------------------------------
chisq.test(bwt.cont, correct = F)
## Too inspect the expected values
chisq.test(bwt.cont, correct = F)$expected


## ----exampleRoc, eval=F---------------------------------------------------------------------------------------------------------------------------------
# example of how to plot ROC curves
## Load data important to detect dates
dat = read.xlsx("../Data/ROCdat.xlsx", sheet = 1, detectDates = T)
## Calculate age (round-down to nearest year). -- Date.ofBirth wasn't detected as a date on import
dat$Age <- as.numeric(floor((dat$Baseline_Date - as.Date(dat$Date.of.Birth, "%d/%m/%Y")) /365.25))

## filter out missing data
# unsure what -1 is...
dat = dat %>% filter(EDNTpBNP > -1 & !is.na(GSDia1HF_New_At))

roc1=roc(dat$GSDia1HF_New_At, dat$EDNTpBNP)
ggroc(roc1)

# similar to ggplot aesthetics can be altered.



## ----coords, eval=F-------------------------------------------------------------------------------------------------------------------------------------
## Examples of how to extract 'best' threshold 
# determine Youdens statistic
coords(roc1, best.method = "youden", 'best')

# threshold at the point closest to the top-left part of the plot
coords(roc1, best.method = "closest.topleft", 'best')



## ----ROCproBNP, fig.cap="A) The impact of proBNP thresholds on the classification of individiuals with and without heart failues (HF). Individuals about the thresholds (red line) would return a positive result. B) ROC curve for proBNP levels and heart failure diagnosis with Youden statistic and value it top-left most corner.", message=F, echo=F----
## Load data important to detect dates
dat = read.xlsx("../Data/ROCdat.xlsx", sheet = 1, detectDates = T)
## Calculate age (round-down to nearest year). -- Date.ofBirth wasn't detected as a date on import
dat$Age <- as.numeric(floor((dat$Baseline_Date - as.Date(dat$Date.of.Birth, "%d/%m/%Y")) /365.25))

## filter out missing data
# unsure what -1 is...
dat = dat %>% filter(EDNTpBNP > -1 & !is.na(GSDia1HF_New_At))

t.size = 8

p1<-ggplot(dat, aes(x=GSDia1HF_New_At, y= EDNTpBNP )) + geom_point() + geom_hline(yintercept = 10090, color='red') + theme_bw() + xlab("HF") + 
  ggtitle("Threshold = 10000")+ theme(plot.title = element_text(size = t.size)) +ylab('NT-proBNP (pg/mL)')
p2<-ggplot(dat, aes(x=GSDia1HF_New_At, y= EDNTpBNP )) + geom_point() + geom_hline(yintercept = 5920, color= 'red') +theme_bw()+ xlab("HF")+ 
  ggtitle("Threshold = 5900")+ theme(plot.title = element_text(size = t.size))+ylab('NT-proBNP (pg/mL)')
p3<-ggplot(dat, aes(x=GSDia1HF_New_At, y= EDNTpBNP )) + geom_point() + geom_hline(yintercept = 4280, color= 'red') +theme_bw()+ xlab("HF")+ 
  ggtitle("Threshold = 4280")+ theme(plot.title = element_text(size = t.size))+ylab('NT-proBNP (pg/mL)')
p4<-ggplot(dat, aes(x=GSDia1HF_New_At, y= EDNTpBNP )) + geom_point() + geom_hline(yintercept = 1926, color= 'red') +theme_bw()+ xlab("HF")+ 
  ggtitle("Threshold = 1900")+ theme(plot.title = element_text(size = t.size))+ylab('NT-proBNP (pg/mL)')

roc1=roc(dat$GSDia1HF_New_At, dat$EDNTpBNP)
youden= coords(roc1, best.method = "youden", 'best') %>% mutate(label=paste(round(threshold,3), " (", round(specificity,3), ", ", round(sensitivity,3), ")", sep=""))
tl = coords(roc1, best.method = "t", 'best') %>% mutate(label=paste(round(threshold,3), " (", round(specificity,3), ", ", round(sensitivity,3), ")", sep=""))


y.tl = rbind(youden,tl)
y.tl$name = c('Youden','Topleft') 

p5<-ggroc(roc1, size=1) + theme_bw() +
  geom_point(data=y.tl, aes(x=specificity, y=sensitivity, color=name), size=3) + 
  geom_text(data=y.tl, aes(x=specificity, y=sensitivity, label = label, color=name), nudge_x= .2, nudge_y = -.035,show.legend = FALSE)+
  scale_color_manual(values=c('red','black')) + theme(legend.title = element_blank())

top <- plot_grid(p1,p2,p3,p4, nrow=1)
plot_grid(top, p5, nrow=2, rel_heights = c(1,1.5), labels=LETTERS[1:2])


## ----ROCproBNP2, echo=F, results='asis',  message=F, comment=F, warning=F, fig.cap="Contingency table corresponding to Figure 1A"-----------------------

tmp <- dat
tmp$GSDia1HF_New_At <- factor(tmp$GSDia1HF_New_At, levels =c("Y","N"))

threshold = c(10090, 5920,4280,1926)
title = "Threshold at"
pos=c('float_left', "float_left", "float_right", 'float_right')
lst=list()
for(i in 1:4){
  lst[[i]] = as.data.frame.matrix(table(tmp$GSDia1HF_New_At, tmp$EDNTpBNP < threshold[i])) %>% rownames_to_column()%>% 
  add_column(c("HF","HF"),.before="rowname") %>% kbl(col.names=c("","","Y","N"), table.attr =  'style= "width: 23%;"') %>% 
  kable_classic(html_font = "Cambria", font_size=14, position=pos[i],full_width=T) %>% 
  collapse_rows(columns=1) %>% add_header_above(c("", "","proBNP"=2)) 
}

cat('<p class=caption style="font-size: initial !important;color: #222222; text-align: center;font-family: Cambria;" id="tab:ROCproBNP2"> Table: Contingency table corresponding to Figure 1A </p>')

print(lst[[1]] %>% add_header_above(c("Threshold cutoff= 10090"=4)))
print(lst[[2]]%>% add_header_above(c("Threshold cutoff= 5920"=4)))
print(lst[[3]]%>% add_header_above(c("Threshold cutoff= 4280"=4)))
print(lst[[4]]%>% add_header_above(c("Threshold cutoff= 1926"=4)))



## ----NormalproBNP, echo=F-------------------------------------------------------------------------------------------------------------------------------
nor <- data.frame(Age=c("0-50", "50-74", "75+"), Normal = c("< 125", "< 125", "< 450"), HF= c("> 450", "> 900", "> 900"))
kbl(nor, col.names=c("Age","Normal (pg/mL)","HF (pg/mL)"), caption = "NT-proBNP normal-range and heart failure across age") %>% kable_classic(html_font = "Cambria", font_size=14)



## ----proBNPage, echo=F, message=F, fig.cap="Comparison of diagnostic potential of proBNP in individuals over 75 compared to under 75."------------------
roc2<-roc(dat$"GSDia1HF_New_At"[dat$Age < 75],dat$"EDNTpBNP"[dat$Age < 75])
roc3<-roc(dat$"GSDia1HF_New_At"[dat$Age >= 75],dat$"EDNTpBNP"[dat$Age >= 75])

## Calculate Youden
youden2 = coords(roc2, best.method = "youden", 'best') %>% mutate(label=paste(round(threshold,3), " (", round(specificity,3), ", ", round(sensitivity,3), ")", sep=""))
youden3 = coords(roc3, best.method = "youden", 'best') %>% mutate(label=paste(round(threshold,3), " (", round(specificity,3), ", ", round(sensitivity,3), ")", sep=""))
youden2 = rbind(youden2,youden3)
youden2$name = 'Youden' 


## Calculate TopLeft
tl2 = coords(roc2, best.method = "t", 'best') %>% mutate(label=paste(round(threshold,3), " (", round(specificity,3), ", ", round(sensitivity,3), ")", sep=""))
tl3 = coords(roc3, best.method = "t", 'best') %>% mutate(label=paste(round(threshold,3), " (", round(specificity,3), ", ", round(sensitivity,3), ")", sep=""))
tl2 = rbind(tl2,tl3)
tl2$name = 'TopLeft' 

points <- rbind(youden2,tl2)
points$col = rep(c('blue','orange'),each=2)

## Generate AUC
AUC = data.frame(AUC=paste(c("< 75","> 75"),"AUC ", c(round(auc(roc2),3),round(auc(roc3),3))))
AUC$AUC.x = .15
AUC$AUC.y = c(.05,.1)
AUC$name =c("x","y")

my_col=c(x='black',y='red', Youden="blue", TopLeft="orange")
ggroc(list(x=roc2,y=roc3), show.legend=F, size=1) + theme_bw() +
  geom_text(data=AUC,aes(x=AUC.x, y=AUC.y, label = AUC), show.legend = F) +
  geom_point(data=points, aes(x=specificity, y=sensitivity,color=name),size=3) +
  geom_text(data=points,aes(x=specificity, y=sensitivity, label = label), nudge_x= .15, nudge_y = -.025, show.legend = F) +
  scale_color_manual(values=my_col,breaks=c('Youden','TopLeft')) +theme(legend.title=element_blank())


## ----roc.test-------------------------------------------------------------------------------------------------------------------------------------------

roc.test(roc2,roc3)


