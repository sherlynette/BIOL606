# BIOL606
#Packages
library(nlme)
library(car)
library(rcompanion)
library(multcomp)
library(sciplot)

data=BIOL606_Perez_Castro
summary(data)

#Shapiro-Wilk Normality Test
#The null-hypothesis of this test is that the population is normally distributed.
shapiro.test(data$SIR)
#SIR p-value =
shapiro.test(data$GWC)
#GWC p-value =
#For positive skew—, common transformations include square root, cube root, and log.For negative skew—, common transformations include square root (constant – x), cube root (constant – x), and log (constant – x)
plotNormalHistogram(data$GWC)
GWC_log = log(data$GWC)
shapiro.test(GWC_log)
#GWC_log p-value =
plotNormalHistogram(GWC_log)

#qqnorm.lme {nlme} Diagnostic plots for assessing the normality of residuals and random effects in the linear mixed-effects fit are obtained. 
#quantile-quantile (Q-Q) plot to compare the model's residuals to "ideal" normal observations. 

#factors
data$plot<-as.factor(data$plot)
data$Year<-as.factor(data$Year)
data$Veg<-as.factor(data$Veg)
data$Treatment<-as.factor(data$Treatment)
data$Season<-as.factor(data$Season)

M1<-lme(GWC~Treatment*Veg*Season*Year,random=~1|plot,data=data,na.action=na.exclude)
qqnorm(resid(M1),ylab="Residuals")
qqline(resid(M1))
M1_log<-lme(GWC_log~Treatment*Veg*Season*Year,random=~1|plot,data=data,na.action=na.exclude)
qqnorm(resid(M1_log),ylab="Residuals")
qqline(resid(M1_log))

shapiro.test(resid(M1))
shapiro.test(resid(M1_log))

#Only long-tailed distributions cause large inaccuracies for linear models. It is possible to transform log-normal distributions, but mild non-normality can be safely ignored. 


M3<-lme(SIR~Veg*Treatment*Season*Year,random=~1|plot,data=data,na.action=na.exclude)
#Quantile-Quantile Plots
plotNormalHistogram(data$SIR)
qqnorm(resid(M3),ylab="Residuals")
qqline(resid(M3))
shapiro.test(resid(M3))
#p-value =

#Checking the homogeneity of variance assumption. Plotting residuals vs. fitted values is a good way to check whether the variance in the data are constant. The vertical scatter surrounding a horizontal line running through y=0 should be random. 

M1<-lme(GWC~Treatment*Veg*Season*Year,random=~1|plot,data=data,na.action=na.exclude)
plot(fitted(M1),resid(M1))
abline(h=0)
M3<-lme(SIR~Treatment*Veg*Season*Year,random=~1|plot,data=data,na.action=na.exclude)
plot(fitted(M3),resid(M3))
abline(h=0)

#Testing data for homogeneity of variance
#Bartlett’s test - If the data is normally distributed, this is the best test to use. It is sensitive to data which is not non-normally distribution; it is more likely to return a “false positive” when the data is non-normal.
#Levene’s test - this is more robust to departures from normality than Bartlett’s test. It is in the car package. The null hypothesis that the population variances are equal (called homogeneity of variance or homoscedasticity).
leveneTest(SIR ~ Treatment*Veg*Season*Year, data=data)
#p-value =
leveneTest(GWC ~ Treatment*Veg*Season*Year, data=data)
#p-value =

bartlett.test(GWC ~ interaction(Treatment,Veg,Season,Year), data=data)
#p-value =
bartlett.test(SIR ~ interaction(Treatment,Veg,Season,Year), data=data)
#p-value =


#Analysis of variance (ANOVA) 
M2<-lme(MBC~Treatment*Veg*Season*Year,random=~1|plot,data=data,na.action=na.exclude)
Anova(M2)
summary(pairwise <- glht(M1,linfct=mcp(Treatment="Tukey")))
summary(pairwise <- glht(M1,linfct=mcp(Season="Tukey")))
summary(pairwise <- glht(M1,linfct=mcp(Year="Tukey")))



#This model have 4 main effects, 6 2-way interactions, 4 3-way interactions, and 1 4-way interaction.How do you interpret lower order effects (main or 2-way) if there is a strong and significant 3-way or 4-way interaction. 
###Graphs typically show 2 factors within each panel. How would you try and visualize all 4 factors?  What makes it OK to present simplified graphics?


#subsets
Ex <- subset(data, Veg==0)
Na <- subset(data, Veg==1)
N <- subset(data, Treatment==1.5)
D <- subset(data,Treatment==1)
ED <- subset(data,Treatment==0.5)
W <- subset(data, Season=='Winter')
Sp <- subset(data, Season=='Spring')
Su <- subset(data, Season=='Summer')
Y1 <-subset(data,Year==1)
Y2 <-subset(data,Year==2)
Y3 <-subset(data,Year==3)
Y4 <-subset(data,Year==4)




M3<-lme(SIR~Treatment*Veg*Season*Year,random=~1|plot,data=data,na.action=na.exclude)
M4<-lme(DOC~Treatment*Veg*Season*Year,random=~1|plot,data=data,na.action=na.exclude)


GWC2=data$GWC*100

setEPS()
postscript("Figure1.eps",onefile = FALSE,paper = "special",width = 7,height = 10)
par(mfrow=c(4,1),mai=c(0.3,0.6, 0.1, 0.1))
lineplot.CI(Date4,GWC2,Veg2,data=data,x.cont=T,ylab='GWC (%)',fixed=T,
            xaxt = 'n', cex.axis = 1, cex=1.5,err.width=0.03,lty=3,lwd = 2, xlim=c(0,990),xlab = " ",legend=T,x.leg=-2,y.leg=18)
par(las = 1)
axis(1,at=c(0,90,180,270,360,450,540,630,720,810,900,990),
     labels=c('','','','','','','','','','','',''))
abline(v=225)
abline(v=495)
abline(v=765)
text(990,17,'(a)',cex=1.1)
lineplot.CI(Date4,MBC,Veg2,data=data,x.cont=T,ylab=expression(paste("MBC (",mu,"g C ",g^-1," soil)")),fixed=T,
            xaxt = 'n', cex.axis = 1, cex=1.5,err.width=0.03,lty=3,lwd = 2, xlim=c(0,990),xlab = " ",legend=F)
par(las = 1)
axis(1,at=c(0,90,180,270,360,450,540,630,720,810,900,990),
     labels=c('','','','','','','','','','','',''))
abline(v=225)
abline(v=495)
abline(v=765)
text(990,121,'(b)',cex=1.1)
lineplot.CI(Date4,SIR,Veg2,data=data,x.cont=T,ylab=expression(paste("SIR (",mu,"g C ",g^-1,"soil ",hr^-1,")")),fixed=T,
            xaxt = 'n', cex.axis = 1, cex=1.5,err.width=0.03,lty=3,lwd = 2, xlim=c(0,990),xlab = " ",legend=F)
par(las = 1)
axis(1,at=c(0,90,180,270,360,450,540,630,720,810,900,990),
     labels=c('','','','','','','','','','','',''))
abline(v=225)
abline(v=495)
abline(v=765)
text(990,10.5,'(c)',cex=1.1)
lineplot.CI(Date4,DOC,Veg2,data=data,x.cont=T,ylab=expression(paste("DOC (",mu,"g C ",g^-1," soil)")),fixed=T,
            xaxt = 'n', cex.axis = 1, cex=1.5,err.width=0.03,lty=3,lwd = 2, xlim=c(0,990),xlab = " ",legend=F)
par(las = 1)
axis(1,at=c(0,90,180,270,360,450,540,630,720,810,900,990),
     labels=c('Wi 13','Sp','Su','Wi 14','Sp','Su','Wi 15','Sp','Su','Wi 16','Sp','Su'))
abline(v=225)
abline(v=495)
abline(v=765)
text(990,100,'(d)',cex=1.1)
dev.off()

setEPS()
postscript("SuppFigure2.eps",onefile = FALSE,paper = "special",width = 7,height = 5)
par(mfrow=c(1,2),mai=c(0.5,0.9, 0.3, 0.1))
bargraph.CI(Veg2,MBC,Treatment,data=data,ylim=c(0,110),legend=T,col=c('red','orange','blue'),ylab=expression(paste("MBC (",mu,"g C ",g^-1," soil)")),x.leg = 1, y.leg = 110, cex.leg = 0.9,leg.lab=c("50%","100%","150%"))
bargraph.CI(Veg2,MBC,Treatment,data=Y3,ylim=c(0,110),legend=F,col=c('red','orange','blue'))
text(1.5,72.5,'a',cex=1.1)
text(2.5,74.5,'a',cex=1.1)
text(3.5,97,'b',cex=1.1)
text(5.5,72.5,'a',cex=1.1)
text(6.5,74.7,'a',cex=1.1)
text(7.5,91.7,'b',cex=1.1)
dev.off()

setEPS()
postscript("SuppFigure3.eps",onefile = FALSE,paper = "special",width = 7,height = 5)
par(mfrow=c(1,2),mai=c(0.5,0.8, 0.3, 0.1))
bargraph.CI(Year2,MBC,data=subset(W,Veg==1),ylim=c(0,140),legend=F,xaxt = 'n',ylab=expression(paste("MBC (",mu,"g C ",g^-1," soil)")),col=c('darkolivegreen'))
axis(1,at=c(0.7,1.9,3.1,4.3),las=1,labels=c('2013','2014','2015','2016'))
text(2.5,137,'Native',cex=1.1)
text(0.7,130,'a',cex=1.1)
text(1.9,91,'b',cex=1.1)
text(3.1,82,'b',cex=1.1)
text(4.3,54,'c',cex=1.1)
bargraph.CI(Year2,MBC,data=subset(W,Veg==0),ylim=c(0,140),legend=F,xaxt = 'n',yaxt = 'n',ylab='',col=c('gold'))
axis(1,at=c(0.7,1.9,3.1,4.3),las=1,labels=c('2013','2014','2015','2016'))
text(2.5,137,'Exotic',cex=1.1)
text(0.7,91,'a',cex=1.1)
text(1.9,82,'a',cex=1.1)
text(3.1,84,'a',cex=1.1)
text(4.3,48,'b',cex=1.1)
dev.off()


setEPS()
postscript("SuppFigure4.eps",onefile = FALSE,paper = "special",width = 7,height = 5)
par(mfrow=c(1,2),mai=c(0.5,0.8, 0.3, 0.1))
bargraph.CI(Year2,SIR,data=subset(data,Veg==1),ylim=c(0,12),legend=F,xaxt = 'n',ylab=expression(paste("SIR (",mu,"g C ",g^-1,"soil ",hr^-1,")")),col=c('darkolivegreen'))
axis(1,at=c(0.7,1.9,3.1,4.3),las=1,labels=c('2013','2014','2015','2016'))
text(0.7,10.5,'a',cex=1.1)
text(1.9,8.5,'b',cex=1.1)
text(3.1,7.7,'c',cex=1.1)
text(4.3,8.2,'bc',cex=1.1)
text(2.5,11.5,'Native',cex=1.1)
bargraph.CI(Year2,SIR,data=subset(data,Veg==0),ylim=c(0,12),legend=F,xaxt = 'n',yaxt = 'n',ylab='',col=c('gold'))
axis(1,at=c(0.7,1.9,3.1,4.3),las=1,labels=c('2013','2014','2015','2016'))
text(0.7,10.3,'a',cex=1.1)
text(1.9,9,'b',cex=1.1)
text(3.1,8.35,'b',cex=1.1)
text(4.3,7.8,'c',cex=1.1)
text(2.5,11.5,'Exotic',cex=1.1)
dev.off()
