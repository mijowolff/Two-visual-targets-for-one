###################################################
# 
# Analyses to "Two visual targets for the price of one? (2015)"
# Wolff, Scholz, Akyürek, van Rijn

##################################################

# Set working directory
setwd("")
library(data.table) # make sure packages are installed
library(lme4)

load("Data_all_subs.Rdat") # Blinks already interpolated and data downsampled (100 Hz)

## In the following lines wrong trials are removed. A behavioural analysis (e.g. computing the proportion of correct trials) 
## is afterwards therefore not possible anymore. 

## Remove those trials in which just one target was present, and they failed to report that target correctly (which 
## can be due to either reporting a single incorrect target, or to reporting a spurious second target).
dat <- dat[!(Cond == "One_Target" & BothCorrect == 0),]

## Remove those trials in which two targets were presented, but people did not report the correct targets (in any order), nor did they
## integrate the two targets into one (so they reported features that weren't presented in the targets).
dat <- dat[!(Cond == "Two_Targets" & BothCorrect == 0 & OrderErr == 0 & Integration == 0 & Blinked == 0),]
## This line above means: through any errors-in-reporting out, but do keep all other possible trials.

## Create some variables that we'll need for the analyses:
dat[,NumReported := as.factor(ifelse(Cond == "Two_Targets" & (BothCorrect == 1 | OrderErr == 1),2,1))]

# Make the variable "Subj" a factor
dat$Subj = as.factor(dat$Subj)

# Make the variables"BothCorrect", "Integration","OrderErr", and "Blinked" numeric variables
dat$BothCorrect = as.numeric(dat$BothCorrect)
dat$Integration = as.numeric(dat$Integration)
dat$OrderErr = as.numeric(dat$OrderErr)
dat$Blinked = as.numeric(dat$Blinked)
## Remove the participant who didn't follow instructions:
dat <- dat[Subj != "22",]


#################################################################
# Figure 2

# Two-target correct trials
plotdat <- dat[Cond=="Two_Targets" & BothCorrect == "1",list(Dil=mean(Dil,na.rm=TRUE)),by=Time]
setkey(plotdat,Time)
plotdat[,plot(Time,Dil,type="l",ylim=c(-0.0,.10),lwd = 3.5, lend = 1, ylab = "Proportional Pupil Dilation",  xlim=c(-0,2200), xlab="Time (ms)")]
legend("bottom", bty = "n",legend = c("2: Order Error","2: Correct Order", "2: T2-missed", "2: Integration", "1: Correct"), y.intersp=1.25, lty=c(2,1,2,1,1), lwd=c(3.5,3.5,3,3,1.75), col=c("black","black","darkgrey","darkgrey","black")) 

#blinks
plotdat <- dat[Blinked == "1",list(Dil=mean(Dil,na.rm=TRUE)),by=Time]
setkey(plotdat,Time)
plotdat[,lines(Time,Dil,type="l",lwd = 3, col="darkgrey", lty=2)] 

# One Target
plotdat <- dat[Cond=="One_Target" & BothCorrect == "1",list(Dil=mean(Dil,na.rm=TRUE)),by=Time]
setkey(plotdat,Time)
plotdat[,lines(Time,Dil,type="l",lwd = 1.75, ylab = "Proportional Pupil Dilation", col="black")]

# Order error trials
plotdat <- dat[Cond=="Two_Targets" & OrderErr == "1",list(Dil=mean(Dil,na.rm=TRUE)),by=Time]
setkey(plotdat,Time)
plotdat[,lines(Time,Dil,type="l",lwd = 3.5, col="black", lty=2)] 

# Integration trials 
plotdat <- dat[Cond=="Two_Targets" & Integration == "1",list(Dil=mean(Dil,na.rm=TRUE)),by=Time]
setkey(plotdat,Time)
plotdat[,lines(Time,Dil,type="l",lwd = 3.5, col="darkgrey")]



#################################################################
#Analyses
#################################################################

##LME

## Analysis to see if the number of targets presented affects the pupil
anadat <- dat[Time>=0 & Time<=2200,list(AveDil=mean(Dil,na.rm=TRUE)),by=list(Subj,Trial,Cond)]
lmeFull <- anadat[,lmer(AveDil ~ Cond + (Cond | Subj))]
lmeNoCond <- anadat[,lmer(AveDil ~ (Cond | Subj))]

summary(lmeFull)

anova(lmeFull, lmeNoCond, test="F")

## Analysis to see if Intregation, Order Error and Blinks is different from other Two Target conditions:
anadat <- dat[Time>=0 & Time<=2200,list(AveDil=mean(Dil,na.rm=TRUE)),by=list(Subj,Trial,Cond,OrderErr,Integration,Blinked)]
lmeFull <- anadat[,lmer(AveDil ~ Cond + OrderErr + Integration + Blinked + (Cond | Subj))]
lmeNoOrderErr <- anadat[,lmer(AveDil ~ Cond + Integration + Blinked + (Cond | Subj))]
lmeNoIntegration <- anadat[,lmer(AveDil ~ Cond + OrderErr + Blinked + (Cond | Subj))]
lmeNoBlinked <- anadat[,lmer(AveDil ~ Cond + OrderErr + Integration + (Cond | Subj))]
lmeReduced <- anadat[,lmer(AveDil ~ Cond + (Cond | Subj))]

summary(lmeFull)

#Order Error
anova(lmeFull, lmeNoOrderErr, test="F")
#Integration
anova(lmeFull,lmeNoIntegration,test="F") 
#Blinked
anova(lmeFull, lmeNoBlinked, test="F")

## Integration trials are clearly different from other two target trials. The plot shows that integration is 
## actually more similar to the one target trials. To test this, we also ran an analysis based on a post-hoc 
## categorization of how many targets were reported. In this analysis:

#Analysis to see if number of targets reported affects the pupil
anadat <- dat[Time>=0 & Time<=2200,list(AveDil=mean(Dil,na.rm=TRUE)),by=list(Subj,Trial,NumReported)]
lmeFull <- anadat[,lmer(AveDil ~ NumReported + (NumReported | Subj))]
lmeNoNumReported <- anadat[,lmer(AveDil ~ (NumReported | Subj))]

summary(lmeFull)
anova(lmeFull, lmeNoNumReported, test="F")

## Analysis to see if Integration and blinks are different from One Target
anadat <- dat[Time>=0 & Time<=2200,list(AveDil=mean(Dil,na.rm=TRUE)),by=list(Subj,Trial,Integration,OrderErr,Blinked,NumReported)]
lmeFull <- anadat[,lmer(AveDil ~ NumReported + OrderErr + Integration + Blinked + (NumReported | Subj))]
lmeNoOrderErr <- anadat[,lmer(AveDil ~ NumReported + Integration + Blinked + (NumReported | Subj))]
lmeNoIntegration <- anadat[,lmer(AveDil ~ NumReported + OrderErr + Blinked + (NumReported | Subj))]
lmeNoBlinked <- anadat[,lmer(AveDil ~ NumReported + OrderErr + Integration + (NumReported | Subj))]
lmeReduced <- anadat[,lmer(AveDil ~ NumReported + (NumReported | Subj))]

summary(lmeFull)

anova(lmeFull,lmeNoOrderErr,test="F")
anova(lmeFull,lmeNoIntegration,test="F") 
anova(lmeFull,lmeNoBlinked,test="F" )


#################################
## Bayes factors

library(BayesFactor)
##  two participants didn't have data in all cells of the design and are therefore excluded:
datBF <- dat[Subj != "01",]
datBF <- datBF[Subj != "10", ]

datBF$Subj <- factor(datBF$Subj)

anadatBF <- datBF[Time>=0 & Time<=2200,
                  list(AveDil=mean(Dil,na.rm=TRUE)),
                  by=list(Subj,Trial,OrderErr,Integration,Blinked,NumReported)]
bfFull <- lmBF(AveDil ~ NumReported + OrderErr + Integration + Blinked + Subj, whichRandom="Subj",data=anadatBF)
bfNoOrderErr <- lmBF(AveDil ~ NumReported  + Integration + Blinked + Subj, whichRandom="Subj",data=anadatBF)
bfNoBlinked <- lmBF(AveDil ~ NumReported + OrderErr + Integration + Subj, whichRandom="Subj",data=anadatBF)
bfNoIntegration <- lmBF(AveDil ~ NumReported  + OrderErr + Blinked + Subj, whichRandom="Subj",data=anadatBF)
bfNoIntNoOrderErr<- lmBF(AveDil ~ NumReported + Blinked + Subj, whichRandom="Subj",data=anadatBF)
bfJustNumReported <- lmBF(AveDil ~ NumReported  + Subj, whichRandom="Subj",data=anadatBF)

bfNoOrderErr / bfFull
bfNoIntegration/bfFull
#bfNoBlinked/bfFull #not doing this one since blinks were not null results before
bfNoIntNoOrderErr/bfFull
#bfJustNumReported/bfFull #not this one either