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


########
# Table-making function
#######
plot.fun<-function(outcome,data)
{
  plot.vars<-c("gender", 'adult', 'religion', 'region', 
               'form.End.survey_language',outcome)
  df.plot<-data[,plot.vars]
  require(reshape2)
  mean.df<-reshape2::melt(df.plot,id.vars=outcome)
  stopifnot(mean(cpdf[[outcome]][cpdf$religion %in% "christian"],na.rm=T)==
              mean(mean.df[[outcome]][mean.df$value %in% 'christian'],na.rm=T))
  return(head(mean.df))
}
plot.fun(outcome='vio.index',data=cpdf)

plot.vars<-c("gender", 'adult', 'religion', 'region', 'form.End.survey_language','vio.index')
df.plot<-cpdf[,plot.vars]
require(reshape2)
mean.df<-reshape2::melt(df.plot,id.vars="vio.index")
stopifnot(mean(cpdf[['vio.index']][cpdf$religion %in% "christian"],na.rm=T)==
            mean(mean.df$vio.index[mean.df$value %in% 'christian'],na.rm=T))

ggplot(mean.df, aes(variable, vio.index)) +   
  geom_bar(aes(fill = value), position = "dodge", stat="identity",
           colour="black", # Use black outlines,
           size=.3) +      # Thinner lines
  xlab("Subsets") +
  ylab("Vio index") +
  scale_fill_hue(name="Subset", # Legend label, use darker colors
                 breaks=c("female",'male',),
                 labels=c("Female", "Adult", "Youth",
                          "Christian", "Muslim", "Other Rel",
                          "Extreme North","North",
                          "French","Fulfulde","Other Lang") +
  scale_y_continuous(limit=c(0,5)) +
  theme_bw() +
  theme(panel.grid.major = element_blank()) +
  theme(plot.title = element_text(size=18))


ggplot(mean.df,aes(x=variable,y=vio.index,fill=factor(value)))+
  geom_bar(stat="identity",position="dodge")+
  scale_fill_discrete(name="Subsets",
                      breaks=c(1, 2,3,4,5,6,7,8,9,10,11,12),
                      labels=c("Male", "Female", "Adult", "Youth",
                               "Christian", "Muslim", "Other Rel",
                               "Extreme North","North",
                               "French","Fulfulde","Other Lang"))+
  xlab("Subset")+ylab("Mean Outcome Score")



qplot(gender, vio.index, data=cpdf, geom=c("boxplot", "jitter"), 
      fill=gender, main="vio.index by gender",
      xlab="", ylab="vio.index")




##################
# Look at stuff systematically
#################

