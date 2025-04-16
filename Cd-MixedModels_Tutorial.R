#_____________________________________________________________________________________________
#_____________________________________________________________________________________________
########## Linear Mixed Effects Models Tutorial ###########
########## UCSB R Seminar  ######################
#_____________________________________________________________________________________________
#_____________________________________________________________________________________________

# written by LDL Anderegg
# last updated 16 Apr 2025

# GOAL: Demonstrate how and why we use linear mixed effects models, as well as model selection techniques

#_____________________________________________________________________________________________
###### Load Packages ########
#_____________________________________________________________________________________________
# install.packages("nlme")
# install.packages("lme4")
# install.packages("lmerTest")
# install.packages("tidyverse")
# install.packages("ggplot2")
# install.packages("RColorBrewer")
# install.packages("car")
# install.packages("MuMIn")

library(nlme) # oldie but goodie, uses lme()
library(lme4) # newer workhorse, but doesn't have alternative variance formulations
library(lmerTest) # expands lme4 and gives you p-values in output (though beware these!)
library(ggplot2)
library(RColorBrewer)
library(car)
library(MuMIn) # package for performing multi-model inference, or doing automated model selection


# make two color palettes, a light and a dark one
mypal <- brewer.pal(n=8, "Dark2")
mypal.light <- paste0(mypal, "88")
#_____________________________________________________________________________________________
########## BACKGROUND: All (ish) Stats are linear models and lm example ###########
#_____________________________________________________________________________________________

# Fun dragon example cribbed from: https://github.com/ourcodingclub/CC-Linear-mixed-models

## load the data and have a look at it
load("data/dragons.RData") # grab the dragons RData file from the 'data' folder
head(dragons)

# so each row is a dragon, which has a testScore and a bodyLength, and comes from a mountainRange, and is nested in a site
dragons$bodyLength0 <- dragons$bodyLength - 155 # we're making dragon length start close to zero so we can see the intercept

######## Continuous Predictors ##################
# Hypothesis: Larger dragons do better on tests
### first we're going to visualize this relationship
quartz(width=3, height=3) # make a plotting device
par(mar=c(4,4,1,1)) # set the margins of the plotting device
plot(testScore~bodyLength0, dragons, ylab="Test Score %", xlab="Body Length (m)", pch=16, col="grey", xlim=c(0,84))
  # plot the relationship
abline(v=0, lty=2) # add in a vertical line at 0 to illustrate intercept
abline(lm(testScore~bodyLength0, dragons), lwd=3) # add in the linear model fit

# save our nice figure
quartz.save(file = "Figure_LinearRegressionExample.pdf", type = "pdf")
dev.off() # kill the plotting device so we're back to plotting in the viewer


### +++++++ now explore the linear model
mod1 <- lm(formula = testScore~bodyLength0, data = dragons) # fit a linear model to the data
mod1 # printing the model object spits out the call and the coefficients
str(mod1) # but there's a SHITTTON more information stored in the object
summary(mod1) # the summary function pulls out most of the things we care about
# and you can easily look at 'model criticism plots' to make sure model assumptions aren't violated

###  Assumptions? ALWAYS look at model criticism plots!

## Plot the residuals - the red line should be close to being flat, like the dashed grey line

plot(mod1, which = 1)  # not perfect, but look alright

## Have a quick look at the  qqplot too - point should ideally fall onto the diagonal dashed line

plot(mod1, which = 2)  # a bit off at the extremes, but that's often the case; again doesn't look too bad

## also make sure some outliers aren't driving things
plot(mod1, which=5) # things with high 'leverage' == bad

# you can also extract fun stuff from your model:
mod1$residuals[1:5] # residuals
mod1$fitted.values[1:5] # fitted values

# e.g. you can plot the fitted values over our figure
plot(testScore~bodyLength0, dragons, ylab="Test Score %", xlab="Body Length (m)", pch=16, col="grey", xlim=c(0,84))
points(mod1$fitted.values~dragons$bodyLength0)


#___________________________________________
############## lm can also deal with categorical variables and multiple predictors #####

# Hypothesis: Dragons from different Mountain ranges score different on tests
quartz(width=3, height=3) # make a plotting device
par(mar=c(5,4,1,1))
boxplot(testScore~mountainRange, dragons, ylab="Test Score %", xlab="", las=2)

# save our nice figure
quartz.save(file = "Figure_LinearRegressionCategoricalExample.pdf", type = "pdf")
dev.off() # kill quartz

####### ++++++++ Fit a linear model with categorical predictors
summary(lm(testScore~mountainRange, dragons))
# The trick is... these p-values and the coefficients may not always be what you're interested in
# They are RELATIVE TO THE INTERCEPT category
summary(aov(testScore~mountainRange, dragons))
  # this ANOVA p-value is the 'overall significance of mountain'

# This can get tough when you're mixing predictors
summary(lm(testScore~mountainRange + bodyLength, dragons))
# ADVANCED: but you can also get an ANOVA-style p-value from a 'likelihood ratio test'
mod2 <- lm(testScore~mountainRange + bodyLength, dragons)
mod1 <- lm(testScore~bodyLength, dragons)
anova(mod1, mod2) # why is it called anova()? *shrug*

########### END: lm tutorial ############################
#_________________________________________________________________









#_________________________________________________________________
#_________________________________________________________________
########## Mixed Models: Dealing with non-independence
#_________________________________________________________________
#_________________________________________________________________
xtabs(~mountainRange + site, dragons)
# looks like site is nested within mountain range



## It is good practice to  standardise your explanatory variables before proceeding - you can use scale() to do that:
  # note: this is for numerical/computational reasons
dragons$bodyLength2 <- scale(dragons$bodyLength)

## Back to our question: is test score affected by body length?

###---- Fit all data in one analysis -----###

## One way to analyse this data would be to try fitting a linear model to all our data, ignoring the sites and the mountain ranges for now.

basic.lm <- lm(testScore ~ bodyLength2, data = dragons)

summary(basic.lm)

## Let's plot the data with ggplot2


ggplot(dragons, aes(x = bodyLength, y = testScore)) +
  geom_point()+
  geom_smooth(method = "lm")

### BUT there is STRUCTURE (i.e. non-independence) in our data!!!

#e.g. We could also plot it colouring points by mountain range
#    dragons within a mountain range are probably more similar in a lot of things (genetics, diet, choice of extra-curriculars) than dragons from different mountains
#    -> this means dragons are not truly independent in our dataset!
quartz(width=3, height=3)
ggplot(dragons, aes(x = bodyLength0, y = testScore, colour = mountainRange))+
  geom_point(size = 1)+
  #geom_smooth(method="lm", se=F) +
  ylab("Test Score (%)")+
  xlab("Body Length (m)")+
  theme_classic()+
  theme(legend.position = "none")
# save our nice figure
quartz.save(file = "Figure_DragonLength_byMountain_noline.pdf", type = "pdf")
dev.off() # kill quartz

## From the above plots it looks like our mountain ranges vary both in the dragon body length and in their test scores. This confirms that our observations from within each of the ranges aren't independent. We can't ignore that.

## So what do we do?????


#_____________________________________________________________________________________________
#_____________________________________________________________________________________________
########## OPTION 1: We fit ALL THE MOUNTAINS ###########
#_____________________________________________________________________________________________
#_____________________________________________________________________________________________


## Refit the model, but add in mountainRange as an additional predictor

#***** You code here:
mod3 <- lm(testScore~bodyLength0 + mountainRange, dragons)


# now look at the output of that model with summary()
summary(mod3)



# let's plot the from your model on top of the data (sorry, going back to base plotting)
palette(mypal.light) # quick set the palette so our data are plotted in light points
plot(testScore~bodyLength0, dragons, ylab="Test Score %", xlab="Body Length (m)", pch=16, col=mountainRange, xlim=c(0,84))
  # you add our model predictions to this figure!
palette(mypal) # reset palette so predictions are plotted in dark points
points()



### But wait!!! this assumes that the slope is identical in all moutain ranges!
# we really need to fit an interaction term:
  # these can be codes as predictor1 * preidctor2 (main effects and interaction)
  # or as predictor1 + predictor2 (main effects) + predictor1:predictor2 (interaction)

mod4 <- lm(testScore~bodyLength0 + mountainRange + bodyLength0*mountainRange, dragons)

# and what does this look like?
summary(mod4)

palette(mypal.light)
plot(testScore~bodyLength0, dragons, ylab="Test Score %", xlab="Body Length (m)", pch=16, col=mountainRange, xlim=c(0,84))
pallete(mypal)
points(mod4$fitted.values~mod4$model$bodyLength0, col=mod4$model$mountainRange)
  # note: you could either use bodyLength0 and mountainRange as the color from the dragons dataset,
  #       or you can pull them from mod4$model, which stores all the data used in model fitting.
  #       the advantage of pulling directly from the mod4$model object is that it excludes NAs, which otherwise screw up the relationship between mod4$fitted.values and dragons


# so now we know...something?
# but DOES BODY LENGTH MATTER???




#_____________________________________________________________________________________________
#_____________________________________________________________________________________________
########## OPTION 2: Use Random Effects ###########
#_____________________________________________________________________________________________
#_____________________________________________________________________________________________


# using the lmer function in lme4, we can fit random effects with the + (random slope | random intercept) syntax

#### FIRST: Let's fit only the random intercepts model
mod3mm <- lmer(testScore~bodyLength + (1|mountainRange), dragons) # equivalent to mod3
summary(mod3mm)
str(mod3mm) # oh boy, 'merMod' objects have a LOT of stuff in them


palette(mypal.light)
plot(testScore~bodyLength0, dragons, ylab="Test Score %", xlab="Body Length (m)", pch=16, col=mountainRange, xlim=c(0,84))
mod3mm_preds <- predict(mod3mm) # pull out our fitted values
palette(mypal)
points(mod3mm_preds~dragons$bodyLength0, col=dragons$mountainRange)


#### SECOND: Let's fit the random slopes AND intercepts model
mod4mm <- lmer(testScore~bodyLength + (bodyLength|mountainRange), dragons) # equivalent to mod3
# uhoh, the fit is 'singular'
help('isSingular')

# we'll ignore this for now, but usually it means you should drop something from your model
summary(mod4mm)
  # This is actually pretty interpretable!

palette(mypal.light)
plot(testScore~bodyLength0, dragons, ylab="Test Score %", xlab="Body Length (m)", pch=16, col=mountainRange, xlim=c(0,84))
mod4mm_preds <- predict(mod4mm) # pull out our fitted values
points(mod4mm_preds~dragons$bodyLength0, col=dragons$mountainRange)


### Important: Criticize this model to make sure it's not violating assumptions!
# residuals 
qqp(resid(mod4mm))
# make sure random effects are normal-ish
mod4ranef <- ranef(mod4mm)
qqp(mod4ranef$mountainRange$bodyLength)
qqp(mod4ranef$mountainRange$`(Intercept)`)

# make sure data is pretty homoskedastic
boxplot(resid(mod4mm)~dragons$mountainRange)
plot(resid(mod4mm)~dragons$bodyLength); abline(h=0, col="red")



#_____________________________________________________________________________________________
#_____________________________________________________________________________________________
########## BEGIN: Ecological Example ###########
#_____________________________________________________________________________________________
#_____________________________________________________________________________________________

# This code takes the compiled output of the Basal Area Growth analysis 
# and Growth Synchrony Analysis for all species replicates, performs statistical tests
# for large-scale relationships between climatic and competitive growth constraints
# and mean growth rate, and plots the results.
# The Supplemental Dataset S3: Growth_Summaries_20180428.csv provides the full output derived from performing the example analyses
# above for all species-replicates.

Sumstatsall <- read.csv("data/SIData_S3_All_Summaries_20180428.csv")


######## Mean Growth v Synchrony Stats ##################
# since the SEs of synchrony estimates aren't symmetrical 
# (because of backtransforming from a beta regression)
# I'll use an inverse variance-weighting based on the lower SE only.

## raw BAI and bai reduction
# invCorrSD <- 1/Sumstatsall$SDcorr^2 / sum(1/Sumstatsall$SDcorr^2, na.rm=T) # inverse Variance weighting
invCorr.se <- 1/(Sumstatsall$corr.mod.mean-Sumstatsall$corr.mod.lse) / sum(1/(Sumstatsall$corr.mod.mean-Sumstatsall$corr.mod.lse), na.rm=T) # inverse SE weighting


baisynchglobal.int <- lmer(corr.mod.mean~scale(BAI.mod.mean.int) + (scale(BAI.mod.mean.int)|St_Sp), Sumstatsall)
baisynchglobalweighted.int <- lmer(corr.mod.mean~scale(BAI.mod.mean.int) + (scale(BAI.mod.mean.int)|St_Sp), Sumstatsall, weights=invCorr.se)
#baisynchglobalweighted.int <- lmer(corr.mod.mean~scale(BAI.mod.mean.int) + (scale(BAI.mod.mean.int)|St_Sp), Sumstatsall, weights=invCorrSD)

# examine residuals
qqp(resid(baisynchglobalweighted.int))
rfs <- ranef(baisynchglobalweighted.int)[[1]]
qqp(rfs[,2])

summary(baisynchglobal.int) # p = 0.066
summary(baisynchglobalweighted.int) # p=0.0446




######## Mean Growth v CompSens Stats ##################
## RAW BAI and RAW comp sens (bai reduction)
# create an inverse se metric with which to weight mixed effects model 
invCompSens.se.int <- 1/(Sumstatsall$CompSens.se.int) / sum(1/Sumstatsall$CompSens.se.int, na.rm=T)

baicompglobal.int <- lmer(CompSens.mean.int~BAI.mod.mean.sc.int + (BAI.mod.mean.sc.int|St_Sp), Sumstatsall)
baicompglobalweighted.int <- lmer(CompSens.mean.int~BAI.mod.mean.sc.int + (BAI.mod.mean.sc.int|St_Sp), Sumstatsall, weights = invCompSens.se.int)
#lmer(CompSens.mean.int~scale(BAI.mod.mean.int) + (scale(BAI.mod.mean.int)|St_Sp), Sumstatsall, weights= invCompSens.se.int)

# model criticism plots
qqp(resid(baicompglobal.int)) # innermost residuals look normal?
rfs <- ranef(baicompglobal.int)[[1]]
qqp(rfs[,2]) # random effects look normal?


summary(baicompglobal.int) # p=0.000434
summary(baicompglobalweighted.int) #p=0.00138 




#### Mean growth vs CompSens, everything as a proportion of max BAI ###
baicompglobal.prop.int <- lmer(CompSens.prop.int~BAI.prop.int + (BAI.prop.int|St_Sp), Sumstatsall)
invCompSens.se <- 1/(Sumstatsall$CompSens.se.int) / sum(1/Sumstatsall$CompSens.se.int, na.rm=T)
baicompglobal.propweighted.int <- lmer(CompSens.prop.int~BAI.prop.int + (BAI.prop.int|St_Sp), Sumstatsall , weights=invCompSens.se)

qqp(resid(baicompglobal.propweighted.int))
rfs <- ranef(baicompglobal.propweighted.int)[[1]]
qqp(rfs[,1]) # not the world's most normal

summary(baicompglobal.prop.int) # p =0.155
summary(baicompglobal.propweighted.int) # p=0.351




#======================================================
######## ** FIGURE 3: Large Scale Trade-off ################################
#======================================================
#. light St_Sp trend lines, emphasize points

# function for plotting upper and lower error bars

# make a function for drawing error bars
error_bars<- function(xvar, yvar, upper, lower=upper, errordata, color="black", length=0.1, lwd=1, plotlower=T, plotupper=T,angle=90,...){
  if(plotupper==T){
    # upper error bar
    arrows(x0 = errordata[,xvar], y0=errordata[,yvar], 
           x1=errordata[,xvar], y1=errordata[,yvar]+errordata[,upper], #add the sterr to the mean value
           angle = angle, col=color, length = length, lwd=lwd,...)
  }
  if(plotlower==T){
    # lower error bar
    arrows(x0 = errordata[,xvar], y0=errordata[,yvar], 
           x1=errordata[,xvar], y1=errordata[,yvar]-errordata[,lower], #subtract the sterr from the mean value
           angle=angle, col=color, length = length, lwd=lwd,...)
  }
}





# set color palet to be paired light and dark blue and green
prescols <- brewer.pal(n=4, "Paired")[c(2,4,1,3)]

# set lwd, lt for species-replicate lines
stsp_lwd <- 1
stsp_lty <- 2

quartz(width=6, height=4)
par(mar=c(4,4.4,2,0.2), oma=c(0,0,0,1), mfrow=c(1,2), mgp=c(2.5,.8,0))
#palette(paste0(colors.ord,"AA"))

plot(corr.mod.mean~BAI.mod.mean.int,Sumstatsall, col=prescols[1], pch=16, cex=1.3,
     ylab="Growth Synchrony",
     xlab=expression(paste("Mean Growth ("*mm^2/yr*")")),
     ylim=c(0, .9), xlim=c(200,2800), cex.lab=1.2, type="n")
# error_bars(xvar="BAI.mod.mean.int", yvar = "corr.mod.mean", upper = "SDcorr", errordata = Sumstatsall,length = 0, color = prescols[3])
#arrows(x0=Sumstatsall$BAI.mod.mean.int, y0=Sumstatsall$corr.mod.mean, y1=Sumstatsall$corr.mod.ci97.5,length=0, lwd=2, col=prescols[3] )
#arrows(x0=Sumstatsall$BAI.mod.mean.int, y0=Sumstatsall$corr.mod.mean, y1=Sumstatsall$corr.mod.ci2.5,length=0, lwd=2, col=prescols[3] )
arrows(x0=Sumstatsall$BAI.mod.mean.int, y0=Sumstatsall$corr.mod.mean, y1=Sumstatsall$corr.mod.use,length=0, lwd=2, col=prescols[3] )
arrows(x0=Sumstatsall$BAI.mod.mean.int, y0=Sumstatsall$corr.mod.mean, y1=Sumstatsall$corr.mod.lse,length=0, lwd=2, col=prescols[3] )



points(corr.mod.mean~BAI.mod.mean.int,Sumstatsall, col=prescols[1], pch=16, cex=1.3)
# St_Sp trend lines
for(i in unique(Sumstatsall$St_Sp)){
  d <- Sumstatsall[which(Sumstatsall$St_Sp==i),]
  lines(fitted(lm(corr.mod.mean~BAI.mod.mean.int, d))~na.omit(d$BAI.mod.mean.int), col=prescols[1],lty=stsp_lty,lwd=stsp_lwd) #lty=2)
}
## global trend line
bc <- 1298.616
bs <- 595.8935#615.95
bcoef <- coef(summary(baisynchglobal.int))[,1]
bcoefa <- (bcoef[1]*bs - bcoef[2]*bc)/bs
bcoefb <- bcoef[2]/bs
abline(b=bcoefb, a=bcoefa, lwd=2)
mtext(text="p=0.045", side = 3, line = -1, adj=0.1)

#legend('bottomright', legend=levels(Sumstatsall$State), pch=c(16,17,18), ncol=1, bty="n", cex=.8)

plot(CompSens.mean.int.rev~BAI.mod.mean.int,Sumstatsall, col=prescols[2], pch=16, cex=1.3,
     ylab="Sens to Comp (growth reduction)", #\n(mm2 BAI reduction)",
     xlab=expression(paste("Mean Growth ("*mm^2/yr*")")),
     xlim=c(200,2800), cex.lab=1.2 ,ylim=c(-350, 750))
error_bars(xvar="BAI.mod.mean.int", yvar = "CompSens.mean.int.rev", upper = "CompSens.se.int", errordata = Sumstatsall,length = 0, color = prescols[4])
points(CompSens.mean.int.rev~BAI.mod.mean.int,Sumstatsall, col=prescols[2], pch=16, cex=1.3)
# individual St_Sp trendlines
for(i in unique(Sumstatsall$St_Sp)){
  d <- Sumstatsall[which(Sumstatsall$St_Sp==i),]
  lines(fitted(lm(CompSens.mean.int.rev~BAI.mod.mean.int, d))~na.omit(d$BAI.mod.mean.int), col=prescols[2], lty=stsp_lty, lwd=stsp_lwd)
}
# adding global trend line
bc <- 1298.616 # BAI centering component
bs <- 595.8935#615.95 # BAI scaling component
baicompglobal.rev <- lmer(CompSens.mean.int.rev~BAI.mod.mean.sc.int + (BAI.mod.mean.sc.int|St_Sp), Sumstatsall)
bcoef <- coef(summary(baicompglobal.rev))[,1]
bcoefa <- (bcoef[1]*bs - bcoef[2]*bc)/bs
bcoefb <- bcoef[2]/bs
abline(b=bcoefb, a=bcoefa, lwd=2)
# abline(lm(CompSens.mean~BAI.mod.mean,Sumstatsall), lwd=2, col="blue")
mtext(text="p=0.001", side = 3, line=-1, adj=.1)
# legend in bottom-right corner
#legend("bottomright", legend=levels(Sumstatsall$Species), col=colors.ord, pch=16, xpd=T, cex=.8, ncol=2, bty="n")




####### END: Large Scale Trade-off analysis ############################
#_____________________________________________________________________________________________





#_______________________________________________________________________
############## ADVANCED: Model Selection ##############################
#______________________________________________________________________


# Steps from Zuur et al. 2009 Mixed effects models and extensions in ecology with R.

#1) Determine your random effects structure with 'over the top' fixed effects structure
#   - fit models with REML and compare with AIC or AICc
#2) Determine your most parsimonious fixed effects structure with a consistent random effects structure
#   - fit models with ML and compare with AIC or AICc
#3) Refit your best model with REML to get best parameter estimates
#4) (and throughout) Make model criticism plots to make sure your model isn't violating assumptions

# first make a UNIQUE site indicator
dragons$site.unique <- paste(dragons$site, dragons$mountainRange, sep=".")
dragons$bodyLength_scaled <- scale(dragons$bodyLength)
dragons$testScore_scaled <- scale(dragons$testScore)


###### 1) Select random effect structure
# no random effects (have to use gls() from nlme package rather than lm for comparison with lmer objects)
rand0 <- gls(testScore~bodyLength0*mountainRange, dragons,method = "REML")
# random intercepts model
rand1 <- lmer(testScore~bodyLength0*mountainRange + (1|site.unique), dragons, REML = T)
# random slope + intercept model
rand2 <- lmer(testScore~bodyLength*mountainRange + (bodyLength0|site.unique), dragons, REML = T)

AIC(rand0, rand1, rand2)

# or AICc from the MuMIn package for small sample sizes
#install.packages("MuMIn")
library(MuMIn)
AICc(rand0, rand1, rand2)

# Random Intercepts is best!


###### 2) Select fixed effects
mod0 <- lmer(testScore~1 + (1|site.unique), dragons, REML=F)
mod1 <- lmer(testScore~bodyLength0 + (1|site.unique), dragons, REML=F)
mod2 <- lmer(testScore~mountainRange + (1|site.unique), dragons, REML=F)
mod3 <- lmer(testScore~bodyLength0 + mountainRange + (1|site.unique), dragons, REML=F)
mod4 <- lmer(testScore~bodyLength0*mountainRange + (1|site.unique), dragons, REML=F)

# now use AIC to select the most parsimonious model
AIC(mod1, mod2, mod3, mod4)
# general rule: the most parsimonious model is the one with the fewest
# parameters that is within 2 AIC units of the lowest AIC model
# differences of <2 AIC units between models are 'not significant' difference
# (i.e. in a likelihood ratio test, your p-value is usually >0.05)

anova(mod2, mod3)
anova(mod1, mod2)


###### 3) refit model with REML
mod2best <- lmer(testScore~mountainRange + (1|site.unique), dragons, REML=T)


###### 4) model criticism
qqp(resid(mod2best))
ranefs <- ranef(mod2best)[1]
qqp(ranefs$site.unique$`(Intercept)`)
