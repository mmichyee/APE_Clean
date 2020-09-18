setwd("~/2018_NYU GPH/Applied Practice/GIS/TRIMMED MAP")

# Importing full join CSV from QGIS -------------------------------------------------
fulljoin <- read.csv("SVI_ACS_500Cities_TrimmedJoinShort.csv")
str(fulljoin)
fulljoin$ACS_agesex_CTLABEL <- as.character(fulljoin$ACS_agesex_CTLABEL)

fulljoinclean <- fulljoin[-c(88),] # DROP ctract 319 (no people)



# CORRELATION MATRIX for risk factors (minus CT319) -------------------------------------------------
#install.packages("moments") #assess normality
library("moments")
skewness(fulljoinclean) #assess by closeness to 0
kurtosis(fulljoinclean)


#install.packages("Hmisc")
library("Hmisc")
corr <- rcorr(as.matrix(fulljoinclean))


df.corr=data.frame(corr$r)
df.corrsig=data.frame(corr$P)

write.csv(df.corr, file = "fulljoincorrclean.csv")
write.csv(df.corrsig, file = "fulljoincorrsigclean.csv")

# Running correlation matrix with COVID outcome data
#covidmay <- read.csv("GIS/ArealInterp/areal_20200518.csv")
#covidaug <- read.csv("GIS/ArealInterp/areal_20200806.csv")

fulljoincovid <- read.csv("SVI_ACS_500Cities_TrimmedJoinCovid.csv") #dropped CT319
covidcorr <- rcorr(as.matrix(fulljoincovid))

df.covidcorr=data.frame(covidcorr$r)
df.covidcorrsig=data.frame(covidcorr$P)
df.covidcorrsigcode=data.frame(covidcorr$P)

write.csv(df.covidcorr, file = "fulljoincorr_covid.csv")
write.csv(df.covidcorrsig, file = "fulljoincorrsig_covid.csv")


# Factor analysis (elimination clean-up) -------------------------------------------------
cleandata <- fulljoin[-c(1)] # drop census tract column from dataset

#PRINCIPLE COMPONENT ANALYSIS
cleandata.pca <- prcomp(cleandata, center=TRUE, scale=TRUE)
summary(cleandata.pca)  
print(cleandata.pca) #eigenvalues, assess closeness to 1
biplot(cleandata.pca, scale=0)


#finding # of factors (HORN'S PARALLEL ANALYSIS)
#install.packages("paran")
library(paran) #calculate eigenvalues
paran(cleandata, iterations=0, centile=0, quietly=FALSE, 
      status=TRUE, all=FALSE, cfa=FALSE, graph=FALSE, 
      color=TRUE, col=c("black","red","blue"), 
      lty=c(1,2,3), lwd=1, legend=TRUE, file="", 
      width=640, height=640, grdevice="png", seed=0, mat=NA, n=NA)
##3 components


#also check scree plot with psych package
#install.packages('psych')
library(psych)
library(ggplot2)
factors <- fa.parallel(cleandata,
                       fm = 'ml',
                       fa = 'fa',
                       n.iter = 50,
                       SMC = TRUE,
                       quant = .95)
##3 factor groups

#drop land use variables for Industrial and Manu, Public Facilities, Parking, Vacant Land
cleandata <- subset(cleandata, select = -c(35, 37, 39,40))


# Factor analysis (elimination) -------------------------------------------------
#INITIAL MODEL
fit3vari <- factanal(cleandata, factors = 3, rotation ="varimax", lower = 0.99)
print(fit3vari$loadings,cutoff = 0.4)
#remove factors with no loadings at cutoff: EP_MUNIT, EP_MOBILE, EP_GROUPQ
                  #landusejoin_sum1, landusejoin_sum4, landusejoin_sum5, landusejoin_sum7

fit3pro <- factanal(cleandata, factors = 3, rotation ="promax", lower = 0.99) #fewer cross loadings
print(fit3pro$loadings,cutoff = 0.4)
#remove factors with no loadings at cutoff: SAME AS ABOVE
#visualization
#fitplot <- fit3pro$loadings[,1:2] 
#plot(fitplot,type="n") # set up plot 
#text(fitplot,labels=names(cleandata),cex=.7) # add variable names

cleandata2 <- subset(cleandata, select = -c(9, 10, 13, 35, 34, 33, 31))

factors <- fa.parallel(cleandata2,
                       fm = 'ml',
                       fa = 'fa',
                       n.iter = 50,
                       SMC = TRUE,
                       quant = .95)


#3 factors (REPLACES OLD)
fit3vari <- factanal(cleandata2, factors = 3, rotation ="varimax", lower = 0.2)
print(fit3vari,cutoff = 0.4)
fit3pro <- factanal(cleandata2, factors = 3, rotation ="promax", lower = 0.2)
print(fit3pro,cutoff = 0.4)
#remove factors with no loadings at cutoff: ACSST5Y2018.S0101_agesex_PERC_MALE

cleandata3 <- subset(cleandata2, select = -c(27))
fit3vari <- factanal(cleandata3, factors = 3, rotation ="varimax", lower = 0.2)
print(fit3vari,cutoff = 0.4)
fit3pro <- factanal(cleandata3, factors = 3, rotation ="promax", lower = 0.2)
print(fit3pro, cutoff = 0.4)

#remove natural resource occ
cleandata3 <- subset(cleandata3, select = -c(25))
fit3pro <- factanal(cleandata3, factors = 3, rotation ="promax", lower = 0.01)
print(fit3pro, cutoff = 0.4)

#remove sales occ
cleandata3 <- subset(cleandata3, select = -c(24))
fit3pro <- factanal(cleandata3, factors = 3, rotation ="promax", lower = 0.01)
print(fit3pro,cutoff = 0.4)

#remove uninsured
cleandata3 <- subset(cleandata3, select = -c(11))
fit3pro <- factanal(cleandata3, factors = 3, rotation ="promax", lower = 0.01)
print(fit3pro,cutoff = 0.4)

#remove unemployment
cleandata3 <- subset(cleandata3, select = -c(2))
fit3pro <- factanal(cleandata3, factors = 3, rotation ="promax", lower = 0.01)
print(fit3pro,cutoff = 0.4)

#remove multifam landuse
cleandata3 <- subset(cleandata3, select = -c(23))
fit3pro <- factanal(cleandata3, factors = 3, rotation ="promax", lower = 0.01)
print(fit3pro,cutoff = 0.4)

#remove open space landuse
cleandata3 <- subset(cleandata3, select = -c(23))
fit3pro <- factanal(cleandata3, factors = 3, rotation ="promax", lower = 0.01)
print(fit3pro,cutoff = 0.4)

#REASSESS FACTORS - 3 suggested
factors <- fa.parallel(cleandata3,
                       fm = 'ml',
                       fa = 'fa',
                       n.iter = 50,
                       SMC = TRUE,
                       quant = .95)

#remove no vehicle
cleandata4 <- subset(cleandata3, select = -c(9))
fit3pro <- factanal(cleandata4, factors = 3, rotation ="promax", lower = 0.01)
print(fit3pro,cutoff = 0.4)

#remove single parent
cleandata4 <- subset(cleandata4, select = -c(5))
fit3pro <- factanal(cleandata4, factors = 3, rotation ="promax", lower = 0.01)
print(fit3pro,cutoff = 0.4)

#remove crowding
cleandata4 <- subset(cleandata4, select = -c(7))
fit3pro <- factanal(cleandata4, factors = 3, rotation ="promax", lower = 0.01)
print(fit3pro,cutoff = 0.4)

#remove production
cleandata4 <- subset(cleandata4, select = -c(19))
fit3pro <- factanal(cleandata4, factors = 3, rotation ="promax", lower = 0.01)
print(fit3pro,cutoff = 0.4)

#remove disability
cleandata4 <- subset(cleandata4, select = -c(4))
fit3pro <- factanal(cleandata4, factors = 3, rotation ="promax", lower = 0.01)
print(fit3pro,cutoff = 0.4)

#REASSESS FACTORS - 3 suggested
factors <- fa.parallel(cleandata4,
                       fm = 'ml',
                       fa = 'fa',
                       n.iter = 50,
                       SMC = TRUE,
                       quant = .95)

#remove age65
cleandata5 <- subset(cleandata4, select = -c(3))
fit3pro <- factanal(cleandata5, factors = 3, rotation ="promax", lower = 0.01)
print(fit3pro,cutoff = 0.4)

#remove poverty
cleandata5 <- subset(cleandata5, select = -c(1))
fit3pro <- factanal(cleandata5, factors = 3, rotation ="promax", lower = 0.01)
print(fit3pro,cutoff = 0.4)

#remove BP
cleandata5 <- subset(cleandata5, select = -c(5))
fit3pro <- factanal(cleandata5, factors = 3, rotation ="promax", lower = 0.01)
print(fit3pro,cutoff = 0.4)

#REASSESS FACTORS - 3 suggested
factors <- fa.parallel(cleandata5,
                       fm = 'ml',
                       fa = 'fa',
                       n.iter = 50,
                       SMC = TRUE,
                       quant = .95)

#remove CHD
cleandata6 <- subset(cleandata5, select = -c(7))
fit3pro <- factanal(cleandata6, factors = 3, rotation ="promax", lower = 0.01)
print(fit3pro,cutoff = 0.4)

#remove asthma
cleandata6 <- subset(cleandata6, select = -c(6))
fit3pro <- factanal(cleandata6, factors = 3, rotation ="promax", lower = 0.01)
print(fit3pro,cutoff = 0.4)

#remove copd
cleandata6 <- subset(cleandata6, select = -c(6))
fit3pro <- factanal(cleandata6, factors = 3, rotation ="promax", lower = 0.01)
print(fit3pro,cutoff = 0.4)

#REASSESS FACTORS - 3 suggested
factors <- fa.parallel(cleandata6,
                       fm = 'ml',
                       fa = 'fa',
                       n.iter = 50,
                       SMC = TRUE,
                       quant = .95)


# Factor analysis (11 components) -------------------------------------------------
#remove no hs diploma only
#The p-value is 3.54e-30
cleandata7 <- subset(cleandata6, select = -c(1))
fit3pro <- factanal(cleandata7, factors = 3, rotation ="promax", lower = 0.01)
print(fit3pro,cutoff = 0.4)

#remove minority only
#The p-value is 7.62e-34 
cleandata7 <- subset(cleandata6, select = -c(2))
fit3pro <- factanal(cleandata7, factors = 3, rotation ="promax", lower = 0.01)
print(fit3pro,cutoff = 0.4)

#remove limited eng only
#The p-value is 4.56e-30
cleandata7 <- subset(cleandata6, select = -c(3))
fit3pro <- factanal(cleandata7, factors = 3, rotation ="promax", lower = 0.01)
print(fit3pro,cutoff = 0.4)

#remove arthritis only
#The p-value is 3.24e-17 
cleandata7 <- subset(cleandata6, select = -c(4))
fit3pro <- factanal(cleandata7, factors = 3, rotation ="promax", lower = 0.01)
print(fit3pro,cutoff = 0.4)

#remove cancer only
#The p-value is 1.13e-17 
cleandata7 <- subset(cleandata6, select = -c(5))
fit3pro <- factanal(cleandata7, factors = 3, rotation ="promax", lower = 0.01)
print(fit3pro,cutoff = 0.4)

#remove smoking only
#The p-value is 1.02e-26 
cleandata7 <- subset(cleandata6, select = -c(6))
fit3pro <- factanal(cleandata7, factors = 3, rotation ="promax", lower = 0.01)
print(fit3pro,cutoff = 0.4)

#remove diabetes only
#The p-value is 4.17e-19 
cleandata7 <- subset(cleandata6, select = -c(7))
fit3pro <- factanal(cleandata7, factors = 3, rotation ="promax", lower = 0.01)
print(fit3pro,cutoff = 0.4)

#remove obesity only
#The p-value is 1.19e-26  
cleandata7 <- subset(cleandata6, select = -c(8))
fit3pro <- factanal(cleandata7, factors = 3, rotation ="promax", lower = 0.01)
print(fit3pro,cutoff = 0.4)

#remove stroke only
#The p-value is 7.32e-23 
cleandata7 <- subset(cleandata6, select = -c(9))
fit3pro <- factanal(cleandata7, factors = 3, rotation ="promax", lower = 0.01)
print(fit3pro,cutoff = 0.4)

#remove management only
#The p-value is 3.98e-33 
cleandata7 <- subset(cleandata6, select = -c(10))
fit3pro <- factanal(cleandata7, factors = 3, rotation ="promax", lower = 0.01)
print(fit3pro,cutoff = 0.4)

#remove service only
#The p-value is 1.1e-33 
cleandata7 <- subset(cleandata6, select = -c(11))
fit3pro <- factanal(cleandata7, factors = 3, rotation ="promax", lower = 0.01)
print(fit3pro,cutoff = 0.4)

######remove cancer only
#The p-value is 1.13e-17 
cleandata7 <- subset(cleandata6, select = -c(5))
fit3pro <- factanal(cleandata7, factors = 3, rotation ="promax", lower = 0.01)
print(fit3pro,cutoff = 0.4)


#REASSESS FACTORS - 2 suggested
factors <- fa.parallel(cleandata7,
                       fm = 'ml',
                       fa = 'fa',
                       n.iter = 50,
                       SMC = TRUE,
                       quant = .95)

fit2pro <- factanal(cleandata7, factors = 2, rotation ="promax", lower = 0.01)
print(fit2pro,cutoff = 0.4)


# Factor analysis (<=10 components) -------------------------------------------------

#remove arthritis etc
cleandata8 <- subset(cleandata7, select = -c(4))
fit2pro <- factanal(cleandata8, factors = 2, rotation ="promax", lower = 0.01)
print(fit2pro,cutoff = 0.4)

#remove diabetes
#p-value is 5.14e-12  
cleandata9 <- subset(cleandata8, select = -c(5))
fit2pro <- factanal(cleandata9, factors = 2, rotation ="promax", lower = 0.01)
print(fit2pro,cutoff = 0.4)

#remove service 
#p-value is 1.91e-06 
cleandata9<- subset(cleandata9, select = -c(8))
fit2pro <- factanal(cleandata9, factors = 2, rotation ="promax", lower = 0.01)
print(fit2pro,cutoff = 0.4)

#remove stroke  ###FINAL MODEL
#p-value is 0.198 
cleandata10<- subset(cleandata9, select = -c(6))
fit2pro <- factanal(cleandata10, factors = 2, rotation ="promax", lower = 0.01)
print(fit2pro,cutoff = 0.4)


#with regression scores
fit2pro <- factanal(cleandata10, factors = 2, rotation ="promax", scores="regression", lower = 0.01) 
print(fit2pro$loadings,cutoff = 0.4)


fit2export <- fit2pro$loadings[,1:2]
write.csv(fit2export, file = "fulljoin_2factorloading.csv")


# Data cleaning for weighted regression -------------------------------------------------
# Calculate weighting by land area: % coverage of CT determined in QGIS
covid <- read.csv("WEIGHTS/weights.csv", stringsAsFactors=FALSE)

library("dplyr") 

# generating weighted outcome variables
aug_caserate <- covid %>% 
  group_by(ctlabel) %>% 
  summarise(weighted.mean(aug6_COVID_CASE_RATE, perc_ct))

aug_deathrate <- covid %>% 
  group_by(ctlabel) %>% 
  summarise(weighted.mean(aug6_COVID_DEATH_RATE, perc_ct))

aug_perpos <- covid %>% 
  group_by(ctlabel) %>% 
  summarise(weighted.mean(aug6_PERCENT_POSITIVE, perc_ct))

may_caserate <- covid %>% 
  group_by(ctlabel) %>% 
  summarise(weighted.mean(may18_COVID_CASE_RATE, perc_ct))

may_deathrate <- covid %>% 
  group_by(ctlabel) %>% 
  summarise(weighted.mean(may18_COVID_DEATH_RATE, perc_ct))

may_perpos <- covid %>% 
  group_by(ctlabel) %>% 
  summarise(weighted.mean(may18_PERCENT_POSITIVE, perc_ct))

# combining into single table
weighted <- aug_caserate %>% 
  left_join(aug_deathrate, by= "ctlabel") %>% 
  left_join(aug_perpos, by= "ctlabel") %>% 
  left_join(may_caserate, by= "ctlabel") %>% 
  left_join(may_deathrate, by= "ctlabel") %>% 
  left_join(may_perpos, by= "ctlabel") 

# shorten column names (new name = old name syntax)
weighted <- weighted %>% 
  rename(wt_aug6_CASE_RATE = `weighted.mean(aug6_COVID_CASE_RATE, perc_ct)`) %>% 
  rename(wt_aug6_DEATH_RATE = `weighted.mean(aug6_COVID_DEATH_RATE, perc_ct)`) %>% 
  rename(wt_aug6_POSITIVE = `weighted.mean(aug6_PERCENT_POSITIVE, perc_ct)`) %>% 
  rename(wt_may18_CASE_RATE = `weighted.mean(may18_COVID_CASE_RATE, perc_ct)`) %>% 
  rename(wt_may18_DEATH_RATE = `weighted.mean(may18_COVID_DEATH_RATE, perc_ct)`) %>% 
  rename(wt_may18_POSITIVE = `weighted.mean(may18_PERCENT_POSITIVE, perc_ct)`)


# WEIGHTED REGRESSION -------------------------------------------------
# IMPORT FACTOR SCORES
F1 <- read.csv("_Factor1.csv", stringsAsFactors=FALSE)
F2 <- read.csv("_Factor2.csv", stringsAsFactors=FALSE)

### Factor 1 Variables 
hist(F1$SUMF1) #right skew
library("Hmisc")

wtF1 <- weighted %>% 
  left_join(F1 %>% 
              select(ctlabel, SUMF1), by= "ctlabel")

corr_wtF1 <- rcorr(as.matrix(wtF1))

print(subset(corr_wtF1$r,select="SUMF1"))
print(subset(corr_wtF1$P,select="SUMF1"))

head(wtF1)

lm_F1augcase <- summary(lm(wt_aug6_CASE_RATE ~ SUMF1, data = wtF1))
lm_F1augdeath <- summary(lm(wt_aug6_DEATH_RATE ~ SUMF1, data = wtF1))
lm_F1augposi <- summary(lm(wt_aug6_POSITIVE ~ SUMF1, data = wtF1))
lm_F1maycase <- summary(lm(wt_may18_CASE_RATE ~ SUMF1, data = wtF1))
lm_F1maydeath <- summary(lm(wt_may18_DEATH_RATE ~ SUMF1, data = wtF1))
lm_F1mayposi <- summary(lm(wt_may18_POSITIVE ~ SUMF1, data = wtF1))

qqnorm(lm_F1augcase$residuals)
qqline(lm_F1augcase$residuals)
qqnorm(lm_F1augdeath$residuals)
qqline(lm_F1augdeath$residuals)
qqnorm(lm_F1augposi$residuals)
qqline(lm_F1augposi$residuals)

qqnorm(lm_F1maycase$residuals)
qqline(lm_F1maycase$residuals)
qqnorm(lm_F1maydeath$residuals)
qqline(lm_F1maydeath$residuals)
qqnorm(lm_F1mayposi$residuals)
qqline(lm_F1mayposi$residuals)

#plot(lm_F1augcase$fitted.values,lm_F1augcase$residuals) #no fitted values?
hist(lm_F1augcase$residuals)
hist(lm_F1augdeath$residuals)
hist(lm_F1augposi$residuals)
hist(lm_F1maycase$residuals)
hist(lm_F1maydeath$residuals)
hist(lm_F1mayposi$residuals)

# Factor 2 Variables
hist(F2$SUMF2)

wtF2 <- weighted %>% 
  left_join(F2 %>% 
              select(ctlabel, SUMF2), by= "ctlabel")

corr_wtF2 <- rcorr(as.matrix(wtF2))

print(subset(corr_wtF2$r,select="SUMF2"))
print(subset(corr_wtF2$P,select="SUMF2"))

head(wtF2)

lm_F2augcase <- summary(lm(wt_aug6_CASE_RATE ~ SUMF2, data = wtF2))
lm_F2augdeath <- summary(lm(wt_aug6_DEATH_RATE ~ SUMF2, data = wtF2))
lm_F2augposi <- summary(lm(wt_aug6_POSITIVE ~ SUMF2, data = wtF2))
lm_F2maycase <- summary(lm(wt_may18_CASE_RATE ~ SUMF2, data = wtF2))
lm_F2maydeath <- summary(lm(wt_may18_DEATH_RATE ~ SUMF2, data = wtF2))
lm_F2mayposi <- summary(lm(wt_may18_POSITIVE ~ SUMF2, data = wtF2))

qqnorm(lm_F2augcase$residuals)
qqline(lm_F2augcase$residuals)
qqnorm(lm_F2augdeath$residuals)
qqline(lm_F2augdeath$residuals)
qqnorm(lm_F2augposi$residuals)
qqline(lm_F2augposi$residuals)

qqnorm(lm_F2maycase$residuals)
qqline(lm_F2maycase$residuals)
qqnorm(lm_F2maydeath$residuals)
qqline(lm_F2maydeath$residuals)
qqnorm(lm_F2mayposi$residuals)
qqline(lm_F2mayposi$residuals)

hist(lm_F2augcase$residuals)
hist(lm_F2augdeath$residuals)
hist(lm_F2augposi$residuals)
hist(lm_F2maycase$residuals)
hist(lm_F2maydeath$residuals)
hist(lm_F2mayposi$residuals)

# Set into table
lm_F1augcase$r.squared <- round((lm_F1augcase$r.squared*100), digits = 2)
lm_F1augdeath$r.squared <- round((lm_F1augdeath$r.squared*100), digits = 2)
lm_F1augposi$r.squared <- round((lm_F1augposi$r.squared*100), digits = 2)
lm_F1maycase$r.squared <- round((lm_F1maycase$r.squared*100), digits = 2)
lm_F1maydeath$r.squared <- round((lm_F1maydeath$r.squared*100), digits = 2)
lm_F1mayposi$r.squared <- round((lm_F1mayposi$r.squared*100), digits = 2)

lm_F2augcase$r.squared <- round((lm_F2augcase$r.squared*100), digits = 2)
lm_F2augdeath$r.squared <- round((lm_F2augdeath$r.squared*100), digits = 2)
lm_F2augposi$r.squared <- round((lm_F2augposi$r.squared*100), digits = 2)
lm_F2maycase$r.squared <- round((lm_F2maycase$r.squared*100), digits = 2)
lm_F2maydeath$r.squared <- round((lm_F2maydeath$r.squared*100), digits = 2)
lm_F2mayposi$r.squared <- round((lm_F2mayposi$r.squared*100), digits = 2)


fit <- data.frame(Outcome = c("May Case Rate", "May Death Rate", "May Positive Tests", 
                  "August Case Rate", "August Death Rate", "August Positive Tests"),
                  F1 = c(lm_F1maycase$r.squared, lm_F1maydeath$r.squared, lm_F1mayposi$r.squared,
                         lm_F1augcase$r.squared, lm_F1augdeath$r.squared, lm_F1augposi$r.squared),
                  F2  = c(lm_F2maycase$r.squared, lm_F2maydeath$r.squared, lm_F2mayposi$r.squared,
                          lm_F2augcase$r.squared, lm_F2augdeath$r.squared, lm_F2augposi$r.squared))

write.csv(fit, file = "regression.csv")

