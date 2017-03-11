# Scan in Workspace/load libraries
load("cp3_analysis_dat.Rdata")
library(mosaic)
library(survey)
library(lmtest)
library(sandwich)
library(stats)
library(ggplot2)
library(reshape2)

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
#lm functions
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

####################################
# # Easily get mean & Standard Error of survey vars
## plot.vars will be fed into this.
plot.vars<-c("gender", 'adult', 'religion', 'region', 
             'form.End.survey_language','vio.index')
meanse.fun<-function(outcome,vars,df)
{
  df1<-svyby(reformulate(outcome),
             reformulate(vars[1]),
             df,svymean,na.rm=TRUE)
  df1$subset<-vars[1]
  names(df1)<-c('variable',outcome,'se','subset')
  
  df2<-svyby(reformulate(outcome),
             reformulate(vars[2]),
             df,svymean,na.rm=TRUE)
  df2$subset<-vars[2]
  names(df2)<-c('variable',outcome,'se','subset')
  
  df3<-svyby(reformulate(outcome),
             reformulate(vars[3]),
             df,svymean,na.rm=TRUE)
  df3$subset<-vars[3]
  names(df3)<-c('variable',outcome,'se','subset')
  
  df4<-svyby(reformulate(outcome),
             reformulate(vars[4]),
             df,svymean,na.rm=TRUE)
  df4$subset<-vars[4]
  names(df4)<-c('variable',outcome,'se','subset')
  
  df5<-svyby(reformulate(outcome),
             reformulate(vars[5]),
             df,svymean,na.rm=TRUE)
  df5$subset<-vars[5]
  names(df5)<-c('variable',outcome,'se','subset')
  
  final<-rbind(df1,df2,df3,df4,df5)
  return(final)
}
#mean.df<-meanse.fun('vio.index',plot.vars,svy)


########
# Table-making function
#######
plot.fun<-function(outcome,data)
{
  plot.vars<-c("gender", 'adult', 'religion', 'region', 
               'form.End.survey_language',outcome)
  df<-meanse.fun(outcome,plot.vars,data)
  return(df)
}
plot.fun(outcome='vio.index',data=svy)


# Gosh darnit
plot.vars<-c("gender", 'adult', 'religion', 'region', 'form.End.survey_language','vio.index')
df.test<-meanse.fun('vio.index',plot.vars,svy)
mean.df<-reshape2::melt(df.test,id.vars=c('vio.index','se','subset'))[,-4] # drop redundant "variable" column.
mean.df$subset<-as.factor(mean.df$subset)
levels(mean.df$subset)<-c('Age',"Language","Gender","Region","Religion")
mean.df$subset<-factor(mean.df$subset, levels(mean.df$subset)[c(3,1,4,5,2)])
tally(mean.df$value) # could re-order this?

# my survey exp paper
ggplot(mean.df, aes(x=subset, y=vio.index, fill=value)) + 
  geom_bar(position=position_dodge(0.9), stat="identity",
           colour="black", # Use black outlines,
           size=.5) +      # Thinner black lines
  geom_errorbar(aes(ymin=vio.index-se, ymax=vio.index+se),
                size=.3,    # Thinner lines
                width=.2,
                position=position_dodge(.9)) +
  xlab("Respondent Subsets") +
  ylab("Violence Index Score") +
  scale_fill_hue(name="Group", # Legend label, use darker colors
                 breaks=c('female','male','Adult','Youth','exno','no',
                          'christian','islam','other','survey_lang_fr',
                          'survey_lang_ful','survey_lang_other'),
                 labels=c('Female','Male','Adult','Youth','Ex North','North',
                          'Christian','Muslim','Other Rel','French',
                          'Fulfulde','Other Lang')) +
  ggtitle("Violence Index by Group") +
  scale_y_continuous(limit=c(0,4)) +
  theme_bw() +
  theme(panel.grid.major = element_blank()) +
  theme(plot.title = element_text(size=18))








#########################
# Lost Items --> didnt work or dismissed
df.plot<-svy[['variables']][plot.vars]
require(reshape2)
mean.df<-reshape2::melt(df.plot,id.vars='vio.index')
stopifnot(mean(cpdf[['vio.index']][cpdf$religion %in% "christian"],na.rm=T)==
            mean(mean.df$vio.index[mean.df$value %in% 'christian'],na.rm=T))
qplot(variable, vio.index, data=mean.df, geom=c("boxplot", "jitter"), 
      fill=variable, main="vio.index", ylim=c(3,4),
      xlab="", ylab="vio.index")




##################
# Look at stuff systematically
#################

