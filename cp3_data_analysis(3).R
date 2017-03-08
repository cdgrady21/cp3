# Scan in Workspace/load libraries
load("cp3_analysis_dat.Rdata")
library(mosaic)
library(survey)

###########
# Survey Corrections
###########
svy=svydesign(ids=~psu+survey,
              strata=~form.Demographiques_introduction.region+towns,
              data=cpdf)

############
# Functions
###########

#lm function
lm.fun<-function(outcome,treatment,data=cpdf)
{
  lm=lm(reformulate(treatment,response=outcome),data=data)
  return(lm)
}
#lm.fun(outcome='vio.index',treatment='gender')




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


