########################
#Setup: set working directory, set libraries, scan in data, make variables, etc...
###############################
#setwd("C:/Users/Christopher/Desktop/Africa/Cameroon/cp3")
#library(DataCombine)
library(mosaic)
library(plyr)
library(dplyr)
library(car)
library(reshape)
#library(data.table)
cp3=read.csv("cp3_final_data.csv")
cp3=cp3[,c(1:118,129:142)] #remove duplicate cols that are my named vars during survey monitor.
psu=read.csv("cp3_final_psus.csv")
psu=psu[,-c(3,4,5)]

############
# Merge PSUs
###########
cp3<-merge(cp3,psu,by=c("ï..number", "username"))


#########################################
# Functions
#########################################
df=cp3 # cp3 will be with descriptive responses, df will be with numbered responses
test=df # the test df to mess with
#yes/no qwusation
yn.fun=function(var)
{
  car::recode(as.character(var),
              "'yes'=1;
              'no'=0;
              'no_response'=NA")
}
test$newvar=yn.fun(test$form.Attitudes.people_other_rel_in_comm5)
stopifnot(table(test$newvar)['1']==table(test$form.Attitudes.people_other_rel_in_comm5)['yes'])

#frequency of things
medfreq.fun=function(var)
{
  car::recode(as.character(var),
         "'daily'=4;
         'at_least_once_a_week'=3;
         'at_least_once_a_month'=2;
         'at_least_once_a_year'=1;
         'rarely_never'=0;
         'no_response'=NA")
}
test$newvar=medfreq.fun(test$form.media_frequency_question_group_broadcast.listen_to_the_)
stopifnot(table(test$newvar)['4']==table(test$form.media_frequency_question_group_broadcast.listen_to_the_)['daily'])


#a24
a24freq.fun=function(var)
{
  car::recode(as.character(var),
         "'every_day'=4;
         'every_week'=3;
         'every_month'=2;
         'less_than_once_a_month'=1;
         '---'=0;
         'no_response'=NA")
}
test$newvar=a24freq.fun(test$form.watch_arewa24_frequency)
stopifnot(table(test$newvar)['4']==table(test$form.watch_arewa24_frequency)['every_day'])

#agree/disagree scale
agree.scale=function(var)
{
  car::recode(as.character(var),
         "'strongly_agree'=3;
         'somewhat_agree'=2;
         'somewhat_disagree'=1;
         'strongly_disagree'=0;
         'no_response'=NA")
}
test$newvar=agree.scale(test$form.Attitudes.women_money)
stopifnot(table(test$newvar)['3']==table(test$form.Attitudes.women_money)['strongly_agree'])


#amounts of things
trust.scale=function(var)
{
  car::recode(as.character(var),
              "'a_lot'=3;
              'some'=2;
              'not_much'=1;
              'none'=0;
              'no_response'=NA")
}
test$newvar=trust.scale(test$form.Confiance.local_radio)
stopifnot(table(test$newvar)['3']==table(test$form.Confiance.local_radio)['a_lot'])


# social contact freq
confreq.scale=function(var)
{
  car::recode(as.character(var),
              "'frequentlyl'=3;
              'sometimes'=2;
              'rarely'=1;
              'never'=0;
              'no_response'=NA")
}
test$newvar=confreq.scale(test$form.Attitudes.person_from_another_ethnicity)
stopifnot(table(test$newvar)['2']==table(test$form.Attitudes.person_from_another_ethnicity)['sometimes'])

#violence
vio.scale=function(var)
{
  car::recode(as.character(var),
              "'always'=3;
              'sometimes'=2;
              'rarely'=1;
              'never'=0;
              'no_response'=NA")
}
test$newvar=vio.scale(test$form.Attitudes.violence_justification_list.defend_religion)
stopifnot(table(test$newvar)['2']==table(test$form.Attitudes.violence_justification_list.defend_religion)['sometimes'])

# media ops
radio_op.scale=function(var)
{
  car::recode(as.character(var),
              "'very_positive'=2;
              'positive'=1;
              '---'=0;
              'no_response'=NA")
}
test$newvar=radio_op.scale(test$form.radio_listener_group.opinion_of_chabab)
stopifnot(table(test$newvar)['1']==table(test$form.radio_listener_group.opinion_of_chabab)['positive'])

# Combining the functions --> doesn't work
allcode.fun=function(var)
{
  if(length(grep("yes",levels(var)))>0){
    yn.fun
  }
  else if(length(grep("daily",levels(var)))>0){
    medfreq.fun
  }
  else if(length(grep("strongly_agree",levels(var)))>0){
    agree.scale
  }
  else if(length(grep("not_much",levels(var)))>0){
    trust.scale
  }
  else if(length(grep("frequentlyl",levels(var)))>0){
    confreq.scale
  }
  else if(length(grep("always",levels(var)))>0){
    vio.scale
  }
  else if(length(grep("every_day",levels(var)))>0){
    a24freq.fun
  }
  else if(length(grep("positive",levels(var)))>0){
    radio_op.scale
  }
}
test$newvar=agree.scale(test$form.Attitudes.good_pol_transparency)
stopifnot(table(test$newvar)['3']==table(test$form.Attitudes.good_pol_transparency)['strongly_agree'])

#test$newvar=allcode.fun(test$form.Attitudes.good_pol_transparency)
#stopifnot(table(test$newvar)['3']==table(test$form.Attitudes.good_pol_transparency)['strongly_agree'])

##########
# Recoding all the questions
#########
#test=as.data.frame(apply(df,2,allcode.fun)) #still cannot get the mass rescale function to work
cpdf<-cp3
cpdf<-as.data.frame(apply(cpdf,2,yn.fun))
cpdf<-as.data.frame(apply(cpdf,2,medfreq.fun))
cpdf<-as.data.frame(apply(cpdf,2,agree.scale))
cpdf<-as.data.frame(apply(cpdf,2,trust.scale))
cpdf<-as.data.frame(apply(cpdf,2,confreq.scale))
cpdf<-as.data.frame(apply(cpdf,2,vio.scale))
cpdf<-as.data.frame(apply(cpdf,2,radio_op.scale))

stopifnot(nrow(cpdf)==nrow(cp3))

#############
# Re-scale the data: Chris: write rescale function that skips characters & factors 
############
#test$newvar=agree.scale(test$form.Attitudes.good_pol_transparency)
#test$newvar<-rescaler(test$newvar,type="range")
cpdf<-as.data.frame(apply(cpdf,2,rescaler,type="range"))


###########
# Recode some vars for easy use
##########
cpdf$gender<-cpdf$form.Demographiques_introduction.respondent_sex
cpdf$religion<-car::recode(cpdf$form.demographics_question_group.religion,
                           "'islam'='islam';
                           'christian'='christian';
                           else='other'")
cpdf$region<-cpdf$form.Demographiques_introduction.region

############
# Add some new variables
############
cpdf$muslim<-ifelse(cpdf$religion=="islam", 1, 0)
cpdf$christian<-ifelse(cpdf$religion=="christian", 1, 0)
cpdf$female<-ifelse(cpdf$gender=="female", 1, 0)
cpdf$male<-ifelse(cpdf$gender=="male", 1, 0)



###########################
# One-off Questions (to look at)
###########################
table(cp3$form.Confiance.personal_view_positive_change_ability)
table(cp3$form.Attitudes.copy.1.of.dif_ppl_live_peace) # this is mis-labeled
table(cp3$form.Attitudes.ethnic_divide_or_no)
table(cp3$form.Attitudes.religious_divide_or_no)
table(cp3$form.Attitudes.youth_involvement)
# tech --> no recode functions probably.
table(cp3$form.Technologie.tech_access)
table(cp3$form.Technologie.mobile_service_provider)
table(cp3$form.Technologie.personal_smartphone_os)
table(cp3$form.Technologie.personal_tablet_os)
table(cp3$form.Technologie.internet_access_locations)
#form.Technologie.other_internet_access_point_specified # no responses
# media listening
table(cp3$form.radio_listener_group.dabalaye_opinion)
table(cp3$form.radio_listener_group.Douniarou_opinion)
table(cp3$form.watch_arewa24_frequency)
table(cp3$form.arewa24_programs_watched)
# Demographics
table(cp3$form.End.survey_language)




############
# Aggregate Things
###########
#aggregate at psu level
ag.df<-cpdf
ag.df<-aggregate(ag.df[,c("muslim","christian","female","male")],
                 by=list(name=ag.df$psu),mean,na.rm=T)


ag.df2 <- cpdf %>% group_by(psu) %>%  summarise_each(funs(mean(., na.rm = TRUE)))
stopifnot(mean(ag.df2$form.Attitudes.good_pol_transparency[ag.df2$name==1],na.rm=T)==
            mean(ag.df$form.Attitudes.good_pol_transparency[ag.df$name==1],na.rm=T))


table(good.df$exp_group,good.df$ethnic_marriage..multiple.choice.question.,exclude=c())

testtab4a<-table(good.df$w19.trust_rel2..multiple.choice.question.)
testtab4b<-table(ivr$w19.trust_rel2..multiple.choice.question.)
stopifnot(testtab4b["1. completely"]==testtab4a['1']) # check nothing changed

##############
# Save data/workspace
#############
save.image("cp3data.Rdata")
save(cpdf,file="cpdf.rda")