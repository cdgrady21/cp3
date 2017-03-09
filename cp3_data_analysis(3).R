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
test<-cpdf %>% dplyr::count(psu)
names(test)<-c("psu","num_respondents")
cpdf<-merge(cpdf,test,by="psu")
cpdf$weights<-15/cpdf$num_respondents

svy1=svydesign(ids=~psu+survey,
              strata=~region+towns,
              weights=~weights,
              data=cpdf)
svy <- as.svrepdesign(svy1,type="bootstrap", replicates=1000)

############
# Functions
###########
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
#lm.fun(outcome='vio.index',treatment='gender')
#lm.fun(outcome='vio.index',treatment='female+psu')

psublock.fun<-function(outcome,treatment,dat)
{
  df<-dat[!is.na(dat[outcome]),]
  df2<-df[!is.na(df[treatment]),]
  df2$outcomeMD<- df2[[outcome]] - ave(df2[[outcome]],df2$psu, na.rm=TRUE)
  df2$treatmentMD<- df2[[treatment]] - ave(df2[[treatment]],df2$psu, na.rm=T)
  done<-lm.fun(outcome='outcomeMD',treatment='treatmentMD',data=df2)
  return(done)
}
#psublock.fun(outcome='vio.index', treatment='female',dat=cpdf)

#survey lm function
svylm.fun<-function(outcome,treatment,data=svy)
{
  thelm=svyglm(reformulate(treatment,response=outcome),design=data)
  thecoef<-coef(thelm)[2]
  pvals<-summary(thelm)$coefficients[2,4]
  theci<-confint(thelm)[2,]
  return(cbind(thecoef,pvals,theci[1],theci[2]))
}
#svylm.fun(outcome='vio.index',treatment='gender')

##################
# Look at stuff sysematically
#################
