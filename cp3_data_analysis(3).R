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
  names(df1)<-c('variable',outcome,'se')
  
  df2<-svyby(reformulate(outcome),
             reformulate(vars[2]),
             df,svymean,na.rm=TRUE)
  names(df2)<-c('variable',outcome,'se')
  
  df3<-svyby(reformulate(outcome),
             reformulate(vars[3]),
             df,svymean,na.rm=TRUE)
  names(df3)<-c('variable',outcome,'se')
  
  df4<-svyby(reformulate(outcome),
             reformulate(vars[4]),
             df,svymean,na.rm=TRUE)
  names(df4)<-c('variable',outcome,'se')
  
  df5<-svyby(reformulate(outcome),
             reformulate(vars[5]),
             df,svymean,na.rm=TRUE)
  names(df5)<-c('variable',outcome,'se')
  
  final<-rbind(df1,df2,df3,df4,df5)
  return(final)
}
#meanse.fun('vio.index',plot.vars,svy)


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

plot.vars<-c("gender", 'adult', 'religion', 'region', 'form.End.survey_language','vio.index')

# Gosh darnit
ggplot()


# my survey exp paper
df_rand_exp <- summarySE(data, measurevar="rand_exp", groupvars=c("rand_tr","strata"), na.rm=T)
df_rand_exp$rand_tr=as.factor(df_rand_exp$rand_tr)
df_rand_exp$strata=revalue(df_rand_exp$strata, c("bf"="Ben Farmers", "bp"="Ben Pastoralists",
                                                 "nf"="Nas Farmers", "np"="Nas Pastoralists"))

ggplot(mean.df, aes(x=variable, y=vio.index, fill=value)) + 
  geom_bar(position=position_dodge(), stat="identity",
           colour="black", # Use black outlines,
           size=.3) +      # Thinner lines
  geom_errorbar(aes(ymin=rand_exp-se, ymax=rand_exp+se),
                size=.3,    # Thinner lines
                width=.2,
                position=position_dodge(.9)) +
  xlab("Strata") +
  ylab("Percentage Would Live in Intergroup Community") +
  scale_fill_hue(name="Condition", # Legend label, use darker colors
                 breaks=c("5","25","50","75"),
                 labels=c("5%", "25%","50%","75%")) +
  ggtitle("Figure 4: Randomization Experiment") +
  scale_y_continuous(limit=c(0,.8)) +
  theme_bw() +
  theme(panel.grid.major = element_blank()) +
  theme(plot.title = element_text(size=18))


#stackoverflow
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

