### Provided by Dr. Andrew Chang 
### Related publication: Chang, A., Kragness, H.E., Tsou, W., Thiede, A., Bosnyak, D.J., Trainor, L.J. (in press) Body sway predicts romantic interest in speed dating. Social Cognitive and Affective Neuroscience
### https://doi.org/10.1093/scan/nsaa093
###
### R version 3.6.1 (2019-07-05)
### Platform: x86_64-w64-mingw32/x64 (64-bit)
### Running under: Windows 7 x64 (build 7601) Service Pack 1
### The versions of the R and packages for obtaining the statistical results were reported in the paper.


# load packages
library(lme4)
library(car)
library(multcomp)
library(remef)
library(TMB)
library(sjPlot)
library(sjmisc)
library(sjlabelled)
library(ggplot2)
library(snakecase)
library(ggpubr)
require("lattice")
devtools::install_github('bcjaeger/r2glmm')
install.packages('r2glmm')
library(r2glmm)
library("heplots", lib.loc="~/R/win-library/3.4")


# load data

datingData = read.csv('C:/Users/Andrew Chang/Desktop/SpeedDatingOSF_20201218.csv',header=T) 



sapply(datingData, class)

datingM = datingData[datingData$gender=='M',]
datingF = datingData[datingData$gender=='F',]


#####################################
### body sway predicts attraction ###
#####################################

m=glmer(meetAgain~1+Attractiveness+gcOUT+ccOUT+(1+Attractiveness+gcOUT+ccOUT|sub)+(1|partner),datingData, family = binomial(link = 'logit'),control = glmerControl(  optCtrl=list(maxfun=1e5), calc.derivs = FALSE)) 
m=lmer(meetAgain_9points~1+Attractiveness+gcOUT+ccOUT+(1+Attractiveness+gcOUT+ccOUT|sub)+(1|partner),datingData,control = lmerControl( calc.derivs = FALSE))
m=lmer(ShortTerm~1+Attractiveness+gcOUT+ccOUT+(1+Attractiveness+gcOUT+ccOUT|sub)+(1|partner),datingData,control = lmerControl( calc.derivs = FALSE))
m=lmer(LongTerm~1+Attractiveness+gcOUT+ccOUT+(1+Attractiveness+gcOUT+ccOUT|sub)+(1|partner),datingData,control = lmerControl( calc.derivs = FALSE)) 

# reverse GC and CC
m=glmer(meetAgain~1+Attractiveness+gcIN+ccIN+(1+Attractiveness+gcIN+ccIN|sub)+(1|partner),datingData, family = binomial(link = 'logit'),control = glmerControl(  optCtrl=list(maxfun=1e5), calc.derivs = FALSE)) 
m=lmer(ShortTerm~1+Attractiveness+gcIN+ccIN+(1+Attractiveness+gcIN+ccIN|sub)+(1|partner),datingData,control = lmerControl( calc.derivs = FALSE))
m=lmer(LongTerm~1+Attractiveness+gcIN+ccIN+(1+Attractiveness+gcIN+ccIN|sub)+(1|partner),datingData,control = lmerControl( calc.derivs = FALSE))

# split gender
m=glmer(meetAgain~1+Attractiveness+gcOUT+ccOUT+(1+Attractiveness+gcOUT+ccOUT|sub)+(1|partner),datingM, family = binomial(link = 'logit'),control = glmerControl(  optCtrl=list(maxfun=1e5), calc.derivs = FALSE)) 
m=lmer(ShortTerm~1+Attractiveness+gcOUT+ccOUT+(1+Attractiveness+gcOUT+ccOUT|sub)+(1|partner),datingM,control = lmerControl( calc.derivs = FALSE))
m=lmer(LongTerm~1+Attractiveness+gcOUT+ccOUT+(1+Attractiveness+gcOUT+ccOUT|sub)+(1|partner),datingM,control = lmerControl( calc.derivs = FALSE)) 
m=glmer(meetAgain~1+Attractiveness+gcOUT+ccOUT+(1+Attractiveness+gcOUT+ccOUT|sub)+(1|partner),datingF, family = binomial(link = 'logit'),control = glmerControl(  optCtrl=list(maxfun=1e5), calc.derivs = FALSE)) 
m=lmer(ShortTerm~1+Attractiveness+gcOUT+ccOUT+(1+Attractiveness+gcOUT+ccOUT|sub)+(1|partner),datingF,control = lmerControl( calc.derivs = FALSE))
m=lmer(LongTerm~1+Attractiveness+gcOUT+ccOUT+(1+Attractiveness+gcOUT+ccOUT|sub)+(1|partner),datingF,control = lmerControl( calc.derivs = FALSE)) 

(r2 = r2beta(m, method = 'kr', partial = T))
Anova(m)
plotM = plot_model(m, type = "std",se=TRUE,line.size=1.5,dot.size=4,show.values = TRUE,p.threshold = c(0.0166, 0.01, 0.001),order.terms = c(1,2,3),color="forestgreen")+ylim(-0.1,0.7)+font_size(labels.y = 15)





##############################################
### groove and gender on romantic interest ###
##############################################

aa = aggregate(datingData$meetAgain,list(datingData$sub,datingData$grooveLevel),mean)
aa = aggregate(datingData$LongTerm,list(datingData$sub,datingData$grooveLevel),mean)
aa = aggregate(datingData$ShortTerm,list(datingData$sub,datingData$grooveLevel),mean)


tempGenderData =  aa[aa$Group.2=='H',1]
tempGenderData2 <- substr(tempGenderData, 0, 1)
tempData = data.frame(aa[aa$Group.2=='H',3], aa[aa$Group.2=='L',3], tempGenderData2)
colnames(tempData) <- c("HighGroove","LowGroove","gender")

#ANOVA
groove = c("High","Low")
g.idata = data.frame(groove)


m.mlm = lm(cbind(HighGroove,LowGroove)~1+gender,data=tempData)
m.aov = Anova(m.mlm, idata=g.idata, idesign=~groove, type="II")
summary(m.aov, multivariate=F)
etasq(m.aov, anova = TRUE)# effect size




#####################################################
### Does groovy music predict body sway coupling? ###
#####################################################

mGC=lmer(gcOUT~1+gender*grooveLevel+(1+gender*grooveLevel|sub)+(1|partner),datingData,control = lmerControl( calc.derivs = FALSE))
Anova(mGC)
plot.mGC = plot_model(mGC, type = "std",se=TRUE,line.size=1.5,dot.size=4,show.values = TRUE,p.threshold = c(0.0166, 0.01, 0.001),order.terms = c(1,2,3),color="forestgreen")+ylim(-0.1,0.7)+font_size(labels.y = 15)
(r2.mGC = r2beta(mGC, method = 'kr', partial = T))
mCC=lmer(ccOUT~1+gender*grooveLevel+(1+gender*grooveLevel|sub)+(1|partner),datingData,control = lmerControl( calc.derivs = FALSE))
Anova(mCC)
plot.mCC = plot_model(mCC, type = "std",se=TRUE,line.size=1.5,dot.size=4,show.values = TRUE,p.threshold = c(0.0166, 0.01, 0.001),order.terms = c(1,2,3),color="forestgreen")+ylim(-0.1,0.7)+font_size(labels.y = 15)
(r2.mCC = r2beta(mCC, method = 'kr', partial = T))

