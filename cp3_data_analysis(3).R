# Scan in Workspace/load libraries
load("cp3_analysis_dat.Rdata")
library(mosaic)
library(survey)
library(lmtest)
library(sandwich)
library(stats)

###########
# Survey Corrections
###########
svy=svydesign(ids=~psu+survey,
              strata=~form.Demographiques_introduction.region+towns,
              data=cpdf)

############
# Functions
###########

thelm=lm(reformulate('gender',response='vio.index'),data=cpdf)
thecoeftest<-coeftest(thelm,vcov=vcovHC(thelm,type="HC2"))
opts<-names(coef(thelm)[2])
coefs<-coef(thelm)[opts]
pval<-thecoeftest[opts,4]
std<-thecoeftest[opts,2]
theci<-cbind(coefs-(std*2),coefs+(std*2))
return(cbind(coefs,pval,theci))

#lm function
lm.fun<-function(outcome,treatment,data=cpdf)
{
  thelm=lm(reformulate(treatment,response=outcome),data=data)
  thecoeftest<-coeftest(thelm,vcov=vcovHC(thelm,type="HC2"))
  opts<-names(coef(thelm)[2])
  coefs<-coef(thelm)[opts]
  pval<-thecoeftest[opts,4]
  std<-thecoeftest[opts,2]
  theci<-cbind(coefs-(std*2),coefs+(std*2))
  return(cbind(coefs,pval,theci))
}
lm.fun(outcome='vio.index',treatment='gender')
lm.fun(outcome='vio.index',treatment='gender+psu')

df<-cpdf[!is.na(cpdf['vio.index']),]
df2<-df[!is.na(df['gender']),]
df2$vioMD<- tol.df$tol.scale - ave(tol.df$tol.scale,tol.df$psuF, na.rm=TRUE)
summary(tol.df$tolscaleMD)

psublock.fun<-function(outcome,treatment,data=cpdf)
{
  df<-data[!is.na(data[outcome]),]
  df2<-df[!is.na(df[treatment]),]
  
}

## A function to get matched set adjusted effects
matchefffn<-function(outcome,thedesign="fm1",df){
  ## Some people have missing outcomes within sets leaving the set of size 1. Dump those sets.
  solomatches <- names(table(df[[thedesign]]))[table(df[[thedesign]])<2]
  dfm <- droplevels(df[!(df[[thedesign]] %in% solomatches),])
  ## Now matched set mean center the data
  dfm$outcomeMD <- align.by.block(dfm[[outcome]],dfm[[thedesign]])
  dfm$a24MD <- align.by.block(dfm[["a24"]],dfm[[thedesign]])
  ## And estimate an ATE plus associated stat inf.
  matchres <- eff.fun('outcomeMD','a24MD',dfm)
  return(matchres)
}




thelm=svyglm(reformulate('gender',response='vio.index'),design=svy)
opts<-names(coef(thelm)[2])
coefs<-coef(thelm)[opts]
pval<-summary(thelm)[opts,4]
std<-thecoeftest[opts,2]
theci<-cbind(coefs-(std*2),coefs+(std*2))
return(cbind(coefs,pval,theci))

#survey lm function
svylm.fun<-function(outcome,treatment,data=svy)
{
  thelm=svyglm(reformulate(treatment,response=outcome),design=data)
  thecoef<-coef(thelm)[2]
  pvals<-summary(thelm)$coefficients[2,4]
  theci<-confint(thelm)[2,]
  return(cbind(thecoef,pvals,theci[1],theci[2]))
}
svylm.fun(outcome='vio.index',treatment='gender')



#function
gen.fun<-function(outcome){
  lme=lmer(reformulate('(1|psuF)+state+a24*gender',response=outcome),data=wrk.df)
  return(lme)
}

##################
# STUFF
#################

svymean(~vio.index,svy, na.rm=T)
mean(cpdf$vio.index,na.rm=T)





svy_lm1=svyglm(vio.index~gender-1,design=svy)

summary(svy_lm1)
summary(lm1<-lm(vio.index~gender+(1+psu),cpdf))

require(lme4)
summary(fm<-lmer(vio.index~gender+(1|psu),data=cpdf))


