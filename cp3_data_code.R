########################
#Setup: set working directory, set libraries, scan in data, make variables, etc...
###############################
#setwd("C:/Users/Christopher/Desktop/Africa/Cameroon/Report")
#library(DataCombine)
library(mosaic)
library(plyr)
library(dplyr)
library(car)
#library(data.table)
cp3=read.csv("cp3_final_data.csv")
cp3=cp3[,c(1:118,129:142)] #remove duplicate cols that are my named vars.

#########################################
# Functions
#########################################
df=cp3
#yes/no
yn.fun=function(var)
{
  car::recode(as.character(var),
              "'yes'=1;
              'no'=0;
              'no_response'=NA")
}
#test$newvar=yn.fun(test$form.Attitudes.people_other_rel_in_comm5)

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
#test$newvar=medfreq.fun(test$form.media_frequency_question_group_broadcast.listen_to_the_)

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
#test$newvar=normscale(test$form.Attitudes.women_money)

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
#test$newvar=trust.scale(test$form.Confiance.local_radio)

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
#test$newvar=confreq.scale(test$form.Attitudes.person_from_another_ethnicity)

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

# media ops
radio_op.scale=function(var)
{
  car::recode(as.character(var),
              "'very_positive'=2;
              'positive'=1;
              '---'=0;
              'no_response'=NA")
}

# Combining the functions
allcode.fun=function(var)
{
  if(length(grep("yes",levels(var)))>0){
    yn.fun
  }
  else (length(grep("daily",levels(var)))>0){
    mediafreq.fun
  }
  else (length(grep("strongly_agree",levels(var)))>0){
    agree.scale
  }
  else (length(grep("not_much",levels(var)))>0){
    trust.scale
  }
  else (length(grep("frequentlyl",levels(var)))>0){
    confreq.scale
  }
  else (length(grep("always",levels(var)))>0){
    vio.scale
  }
  else (length(grep("every_day",levels(var)))>0){
    a24freq.fun
  }
  else (length(grep("positive",levels(var)))>0){
    radio_op.scale
  }
}
test=allcode.fun(df$form.Attitudes.good_pol_transparency)

##########
# Recoding all the questions
#########
test=as.data.frame(apply(df,2,allcode.fun))

stopifnot(nrow(test)==nrow(df))

###########################
# One-off Questions
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

#############
# Re-scale the data (still cannot get mass re-scale function to work, have to do individually).
############
cpdf<-cp3
cpdf<-as.data.frame(apply(cpdf,2,yn.fun))

