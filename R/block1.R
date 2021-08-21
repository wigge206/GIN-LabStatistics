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

## Packages for plotting and table outputs
library(ggplot2)
library(ggsci)
library(ggpubr)
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


## ----ReadData, echo=T-----------------------------------------------------------------------------------------------------------------------------------
## Ensure the path to data is correct
sheetNames<-getSheetNames("../Data/DummyData_MTT.xlsx") ## Gives you a list of sheetNames in a given excel workbook

## Read a single sheet (the first sheet) from a workbook 
# for details on function type ?function (e.g. ?read.xlsx)
Exp1 <- read.xlsx("../Data/DummyData_MTT.xlsx", sheet=sheetNames[1]) # [1] denotes which sheet to read in

## Read all the sheets and store as a list
KD_data<-lapply(sheetNames,function(i) read.xlsx("../Data/DummyData_MTT.xlsx", sheet=i))
names(KD_data) <- c(paste("Exp",1:3, sep=""), 'normalised')


## ----KDdata, warning=F, echo=F--------------------------------------------------------------------------------------------------------------------------
## select from a list the second experiment and limit it to one cell (AN3CAs)
## Also remove the column the labels cells
dat <- KD_data %>% `[[`('Exp2') %>% filter(cellLine == "AN3CA") %>% select(!cellLine)

## Add column-wise means and sd
dat <- rbind(dat,colMeans(dat[1:6]))
dat<- rbind(dat,apply(dat[,1:6],2,sd))
rownames(dat) <- c(paste("Rep",1:3, sep=""), "mean", "sd")

## Format for output in the html document. 
dat %>% kbl(digit=3, format='html',caption = "Abs570nm (minus blanks) for a single experiment", table.attr =  'style= "width: 70%;"') %>% kable_classic( html_font = "Cambria", font_size=16)  %>% row_spec(3, extra_css = "border-bottom: 1px solid")


## ----KDdataExp, echo=T,warning=F, comment=F,  message=F, fig.width=6, fig.height=4, fig.cap = "Assessment of cell viability by MTT assay for three indepedent replicates across two cell lines. Quantitative analysis of MTT is represented as the mean +/- SD of three technical replicates"----
KD_all_exp <- KD_data # copy to prevent overwriting
KD_all_exp[['normalised']] <- NULL  # remove the normalised dataframe from the list

## Bind all expreiments together (with labels), transform data, group by each cell lines, expt (.id) and condition (name) and caluculate the means and sd. Plot condition (name) and mean value (mean), split into each expt with fact_grid() option. 
rbindlist(KD_all_exp, idcol = T) %>% pivot_longer(2:7)%>% group_by(cellLine, name,.id) %>% summarise(mean=mean(value), sd = sd(value))%>% ggplot(aes(name, mean)) +geom_errorbar(aes(x=name, ymin=mean-sd, ymax=mean+sd), width=.2)+ geom_col(width=.7) + facet_grid(.id~cellLine) +theme_bw(base_size = 10)+ylab("A560-A670") +xlab(NULL)+theme(axis.text.x = element_text(angle = 45, hjust = 1))


## ----KDdataSummary, echo=F,warning=F, comment=F,  message=F---------------------------------------------------------------------------------------------
## Select the normalised data summarise and format for plotting
KD_data %>% `[[`('normalised') %>% pivot_longer(1:6) %>% group_by(cellLine, name)%>% summarise(mean=mean(value), sd = sd(value), n=n()) %>% filter(name != "CellsOnly") %>% kbl(digit=3, format = 'html', caption = "MTT assay normalised to untreated WT", table.attr =  'style= "margin-top: 60px;margin-bottom: 60px;text-align:center; width: 50%;"') %>% kable_classic(html_font = "Cambria", font_size=16)



## ----normalDist, warning=F, echo=F, fig.cap="Relationship of standard deviation and normal distrubtions", fig.height=3----------------------------------
par(mar=c(3,3,2,1))
x<-rnorm(10)
plot(seq(-3.2,3.2,length=50),dnorm(seq(-3,3,length=50),0,1),type="l",xlab="",ylab="",ylim=c(0,0.5))
segments(x0 = c(-3,3),y0 = c(-1,-1),x1 = c(-3,3),y1=c(1,1))
text(x=0,y=0.45,labels = expression("99.7% of the data within 3" ~ sigma),cex=0.8)
arrows(x0=c(-2,2),y0=c(0.45,0.45),x1=c(-3,3),y1=c(0.45,0.45))
segments(x0 = c(-2,2),y0 = c(-1,-1),x1 = c(-2,2),y1=c(0.4,0.4))
text(x=0,y=0.3,labels = expression("95% of the data within 2" ~ sigma), cex=0.8)
arrows(x0=c(-1.5,1.5),y0=c(0.3,0.3),x1=c(-2,2),y1=c(0.3,0.3))
segments(x0 = c(-1,1),y0 = c(-1,-1),x1 = c(-1,1),y1=c(0.25,0.25))
text(x=0,y=0.15,labels = expression("68% of the data within 1" * sigma),cex=0.8)



## ----exp2, warning=F, echo=F, fig.cap="Assessment of cell viability by MTT assay for one experiment. Quantitative analysis of MTT represented as the mean +/- SD of three techical replicates",  fig.height=3----
dat[1:3,] %>% pivot_longer(1:6) %>% group_by(name)%>%summarise(mean=mean(value), sd=sd(value)) %>% 
  ggplot(aes(name,mean)) + geom_errorbar(aes(ymin=mean-sd, ymax=mean+sd), width =.2)+ ylab("A560nm - A670nm") + xlab(NULL)+
  geom_col(width=.5) + geom_text(aes(label=signif(mean, digits=2)), nudge_y = -0.05, color='white')+ theme_bw()


## ----SEM, echo=F,fig.cap="A) Individual data points for each experiment colored by experimental condition. B) Assessment of cell viability by MTT assay. Quantitative analysis of MTT represented as the mean +/- SE for three replicates realtive to untreated cells.",  fig.height=6, fig.width=10, message=F----

x <- KD_data
x[['normalised']] <- NULL
p1<-rbindlist(x, idcol = T) %>% pivot_longer(2:7) %>% filter(name != "CellsOnly")%>%
  ggplot(aes(.id, value, col = name)) + geom_point(size=2) +coord_flip()+ theme_bw()+ scale_color_npg() +
  theme(legend.position = 'top') + ylab(NULL)+ xlab(NULL) + facet_wrap(~cellLine, nrow = 2, scales="free") +theme(legend.title = element_blank())



p2<-KD_data %>% `[[`('normalised') %>% pivot_longer(1:6) %>% filter(name != "CellsOnly") %>% 
  add_column(y = 0) %>% mutate(names=name) %>% unite(label, name,Exp, sep="_") %>%
  ggplot(aes(x = value, y = y, label = label, col = names)) + 
  geom_point() + theme_classic()+ xlab(NULL)+
  geom_text(hjust = 'left', angle = 90, size = 4.5, nudge_y = .001) +
  ylim(0, 0.05) +theme(axis.title.y =element_blank(),
                      axis.text.y = element_blank(),
                      axis.ticks.y=element_blank(),
                      axis.line = element_blank(),
                      legend.position = "none") +
  geom_hline(yintercept = 0) + facet_wrap(~cellLine, scales = 'free') + scale_color_npg()

left<-plot_grid(p1,p2, rel_heights = c(1.6,1), nrow=2 ,labels=c("A", "B"), align = 'h', axis='t')

p3<-KD_data %>% `[[`('normalised') %>% pivot_longer(1:6) %>% 
  group_by(cellLine, name)%>% summarise(mean=mean(value), sd = sd(value), n=n(), se = sd(value)/sqrt(n())) %>% filter(name != "CellsOnly") %>%
  ggplot(aes(name, mean)) + geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width =.2)+ ylab("Relative cell proliferation") + xlab(NULL)+
  geom_col(width=.7) + geom_text(aes(label=round(mean, digits=2), size=1.25), nudge_y = -0.1, color='white', size=3)+ theme_bw() +facet_wrap(~cellLine)+
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size=10))

#plot_grid(left,p3, ncol=2, labels=c("","C"))
plot_grid(p1 +theme(legend.position = 'right'),p3 , ncol=1, labels = LETTERS[1:2], axis = 'lr', align='v', rel_widths = c(1.3,1))



## ----CI-Zscore, echo=F----------------------------------------------------------------------------------------------------------------------------------
zScore <- data.frame(CI= c(90,95,99), Z=c(1.645, 1.96,2.576))

zScore %>% kbl(format = 'html', caption = "Z-value for commonly used CI", table.attr =  'style= "text-align:center; width: 30%;"') %>% kable_classic(html_font = "Cambria", font_size=16)


## ----CI-tscore, echo=F----------------------------------------------------------------------------------------------------------------------------------
tScore <- data.frame(df= c(2,3,4,5,8,10,20,50,100), 
                     CI95=c(4.303, 3.182, 2.776, 2.571, 2.306, 2.228, 2.086, 2.009, 1.984), 
                     CI99= c(9.925,5.841,4.604,4.032, 3.355, 3.169, 2.845, 2.678, 2.626))

tScore %>% kbl(format = 'html', caption = "t-table for some commonly used df", table.attr =  'style= " width: 40%;"', col.names = c('df',"95%", "99%")) %>% kable_classic(html_font = "Cambria", font_size=16) %>% footnote(general = "df=n-1", general_title = "", escape = F)


## ----t-dist, echo=F, fig.height=3.5, fig.width=8.5 , fig.align='center', fig.cap="Effects of degrees of freedom (left) and an example of one-tailed test (right)."----
par(mfrow=c(1,2), mar=c(4,4,4,1))

x <- seq(-4, 4, length=1000)
hx <- dnorm(x)

degf <- c(1, 2, 5, 10)
colors <- c("red", "blue", "darkgreen", "gold", "black")
labels <- c("df=1", "df=2", "df=5", "df=10", "normal")

t.value <- 2.12

plot(x, hx, type="l", lty=1, lwd=2, xlab="t statistic",
     ylab="Density", main="Comparison of t Distributions")

for (i in 1:4){
    lines(x, dt(x,degf[i]), lwd=2, col=colors[i])
}
legend("topright", title="Distributions",
       labels, lwd=1.5, lty=c(1, 1, 1, 1, 1), col=colors, cex=0.75)

hy <-dt(x,df=9)
p <- round(pt(t.value, 9, lower=F), 3)
plot(x,hy, type='l', lwd=3, main=expression(paste(bolditalic("p"), bold("-value for "),bolditalic("T = 2.12, df = 9"))),
     xlab="t statistic", ylab="Density", yaxs='i', ylim=c(min(hy),.4))
polygon(c(x[x>=t.value], t.value), c(hy[x>=t.value], hy[x==max(x)]), col = '#52accc')
abline(v=t.value, lty=2, col='#455a64')
#text(x=t.value*1.15, y= min(hy)*3, label=t.value, cex=.8)
mtext(t.value, at =t.value+0.3, side=1,line=0, cex=.75)
text(x=t.value*1.45, y= max(hy)*.95, label= paste("p = ", p), cex=0.75)


## ----tTest----------------------------------------------------------------------------------------------------------------------------------------------
## Select the nomalised data and remove the cellsOnly data, limit to AN3CAs only.
test_data<- KD_data %>% `[[`('normalised') %>% pivot_longer(1:6) %>% filter(name != "CellsOnly", cellLine == "AN3CA") 

## t.test: Scramble treated vs siRNA1 treated cells
## as an example the t.test using the formula is written below but commented out
# values.both <- test_data %>% filter(name%in%c("siRNA1","Scramble"))
# t.test(value~name, data=values.both)

## Generate vectors for each group
values.scramble <- test_data %>% filter(name =="Scramble") %>% pull("value")
values.siRNA1 <- test_data %>% filter(name =="siRNA1") %>% pull("value")

## t.test
t.test(values.scramble,values.siRNA1)

## pairwise comparisons of all groups
pairwise_test<- compare_means(value~name, data=test_data, method="t.test")



## ----tTestTable, echo=F---------------------------------------------------------------------------------------------------------------------------------

pairwise_test[,2:6]%>% kbl(digit=3, caption = "Results from pairwise_test") %>% kable_classic(full_width = F, html_font = "Cambria", font_size=16)



## ----t.testMultiple-------------------------------------------------------------------------------------------------------------------------------------
## Select the nomalised data and remove the cellsOnly and Reagents data, limit to AN3CAs only.
# Scramble vs each siRNA
test_data<- KD_data %>% `[[`('normalised') %>% pivot_longer(1:6) %>% filter(name != "CellsOnly", cellLine == "AN3CA", name != "Reagents") 

pairwise_test<- compare_means(value~name, data=test_data, method="t.test", ref.group = 'Scramble')


## ----tTestTable2, echo=F--------------------------------------------------------------------------------------------------------------------------------
pairwise_test[,c(2:6,8)]%>% kbl(digit=3, caption = "Results from pairwise_test-siRNA compared to scramble control") %>% kable_classic(full_width = F, html_font = "Cambria", font_size=16)


## ----one-wayano, comment=NA-----------------------------------------------------------------------------------------------------------------------------
# anova using the formula syntax
a1 <-aov(weight ~ feed,data = chickwts)
summary(a1)


## ----bp2, echo=F, fig.height=3, fig.width=5, fig.cap="Significant difference (ANOVA) in chick weights and six week diet on various feed supplement.", fig.align='center'----
bxplot(chickwts, aes(x=feed, y=weight)) + stat_compare_means(method='anova', size=3.5, label.y = 415) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + xlab(NULL) + ylab("weight (g)")


## ----TukeysCode, eval=F, warning=F----------------------------------------------------------------------------------------------------------------------
## tukey_hsd function from rstatix
stat.test<-aov(weight ~ feed,data = chickwts) %>% tukey_hsd()


## ----Tukeys,  fig.align='center', fig.height=4, echo=F, warning=F, fig.cap="Tukey's multiple comparisons. A) Difference between pairwise comparison with 95% CI. B) The effect of feed on chick weight. Statistical analysis by one-way ANOVA with Tukey's post-hoc test. * p < 0.5, ** p < 0.001, p < 0.0001 "----
stat.test<-aov(weight ~ feed,data = chickwts) %>% tukey_hsd()

stat.test2 <- stat.test[stat.test$p.adj.signif != "ns",]
stat.test3 <- stat.test
stat.test3$comparison <- paste(stat.test3$group1, stat.test3$group2,sep="-")
p0 <-ggplot(stat.test3, aes(x=comparison, y=estimate)) + geom_point() +coord_flip() +theme_bw(base_size = 12) + geom_errorbar(aes(ymin=conf.low, ymax=conf.high)) +theme(axis.title.y = element_blank()) + geom_hline(yintercept = 0, linetype=2, colour ='red')+
  theme(plot.margin=unit(c(12.5,5.5,5.5,5.5),"points"))

p1 <- bxplot(chickwts, aes(x=feed, y=weight))  +
  stat_pvalue_manual(stat.test2, label = "p.adj.signif", hide.ns = T, y.position =
                                    max(chickwts$weight)*seq(1.1, 1.8,0.1)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +xlab(NULL)

plot_grid(p0,p1, ncol=2,  align = "h", axis = "b", labels = "AUTO")


## ----Dunnetts-------------------------------------------------------------------------------------------------------------------------------------------
## Make some data
set.seed(123)
low <- rnorm(1000, mean= 95, sd =2.5)
med <- rnorm(1000, mean= 125, sd =5)
high <- rnorm(1000, mean= 150, sd =9)
score = c(sample(med, 8, replace = T),sample(high, 2, replace = T),
          sample(low, 9, replace = T),sample(med, 1, replace = T),
          sample(low, 4, replace = T),sample(med, 6, replace = T),
          sample(med, 1, replace = T),sample(high, 9, replace = T),
          sample(med, 7, replace = T),sample(high, 3, replace = T),
          sample(low, 3, replace = T),sample(low, 7, replace = T),
          sample(high, 7, replace = T),sample(med, 3, replace = T))
dunnetts <- data.frame(Technique = as.factor(rep(c("Existing", "drugA", "drugB","drugC", "drugD","drugE","drugF"), each = 10)),
                       Score = score)

summary(aov(Score ~ Technique, data = dunnetts))
stat.test<-DunnettTest(Score ~ Technique, data = dunnetts,control = "Existing")
stat.test



## ----dunnetBxplot, echo=F,  fig.width=6, fig.height=3, fig.cap="Effect of treatment on blood haemoglobin in amenic individuals. Statistical analysis by one-way ANOVA with Dunnett post-hoc test. * p < 0.5, ** p < 0.01, p < 0.001"----
stat.test2 <- as.data.frame(stat.test$Existing)
stat.test2$group1 <- as.factor(unlist(lapply(strsplit(rownames(stat.test$Existing),"-"), function(i) i[1])))
stat.test2$group2 <- as.factor(unlist(lapply(strsplit(rownames(stat.test$Existing),"-"), function(i) i[2])))
stat.test2$p.format <- round(stat.test2$pval, 3)
stat.test2$p.signif <- p.siginf(stat.test2$pval)
dunnetts$Technique <- as.factor(dunnetts$Technique)

dunnetts %>% bxplot(aes(x=fct_inorder(Technique), y=Score)) + 
  stat_pvalue_manual(stat.test2,label = "p.signif", hide.ns = T, y.position = c(max(dunnetts$Score*1.02),max(dunnetts$Score*1.05),max(dunnetts$Score*1.08),max(dunnetts$Score*1.11))) + 
  ylab(expression("Increase in blood \nhaemoglobin (%)")) +xlab(NULL) +
  theme(plot.margin=unit(c(5.5,5.5,5.5,18.5),"points"))

