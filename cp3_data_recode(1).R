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
cp3=cp3[,c(2:118,130:142)] #remove duplicate cols that are my named vars during survey monitor.
psu=read.csv("cp3_final_psus.csv")
psu=psu[,-c(3,4,5)] # remove things don't need
psu<-lapply(psu,as.character)

# from cp3, remove region/town vars because "final_psus" has it fixed for typos, etc...
f.vars<-c('form.Demographiques_introduction.region',
          'form.Demographiques_introduction.region_no',
          'form.Demographiques_introduction.region_exno')
cp3<-subset(cp3, select=-c(form.Demographiques_introduction.region,
                            form.Demographiques_introduction.region_no,
                            form.Demographiques_introduction.region_exno))

############
# Merge PSUs
###########
cp3<-merge(cp3,psu,by=c("ï..number", "username"))
#rename(cp3, c("ï..number"="int_num")) # plyr method
names(cp3)[names(cp3)=="ï..number"]<-"int_num"
cp3$int_num<-as.character(cp3$int_num)
cp3$form.Demographiques_introduction.respondent_age<-as.character(cp3$form.Demographiques_introduction.respondent_age)

#########################################
# Functions
#########################################
test=cp3 # the test df to mess with
#yes/no qwusation
yn.fun=function(var)
{
  car::recode(as.character(var),as.factor.result=F,
              "'yes'=1;
              'no'=0;
              'no_response'=NA;
              else=NA")
}
test$newvar=yn.fun(test$form.Attitudes.people_other_rel_in_comm5)
stopifnot(table(test$newvar)['1']==table(test$form.Attitudes.people_other_rel_in_comm5)['yes'])

#frequency of things
medfreq.fun=function(var)
{
  car::recode(as.character(var),as.factor.result=F,
         "'daily'=4;
         'at_least_once_a_week'=3;
         'at_least_once_a_month'=2;
         'at_least_once_a_year'=1;
         'rarely_never'=0;
         'no_response'=NA;
              else=NA")
}
test$newvar=medfreq.fun(test$form.media_frequency_question_group_broadcast.listen_to_the_)
stopifnot(table(test$newvar)['4']==table(test$form.media_frequency_question_group_broadcast.listen_to_the_)['daily'])


#a24
a24freq.fun=function(var)
{
  car::recode(as.character(var),as.factor.result=F,
         "'every_day'=4;
         'every_week'=3;
         'every_month'=2;
         'less_than_once_a_month'=1;
         '---'=0;
         'no_response'=NA;
              else=NA")
}
test$newvar=a24freq.fun(test$form.watch_arewa24_frequency)
stopifnot(table(test$newvar)['4']==table(test$form.watch_arewa24_frequency)['every_day'])

#agree/disagree scale
agree.scale=function(var)
{
  car::recode(as.character(var),as.factor.result=F,
         "'strongly_agree'=3;
         'somewhat_agree'=2;
         'somewhat_disagree'=1;
         'strongly_disagree'=0;
         'no_response'=NA;
              else=NA")
}
test$newvar=agree.scale(test$form.Attitudes.women_money)
stopifnot(table(test$newvar)['3']==table(test$form.Attitudes.women_money)['strongly_agree'])


#amounts of things
trust.scale=function(var)
{
  car::recode(as.character(var),as.factor.result=F,
              "'a_lot'=3;
              'some'=2;
              'not_much'=1;
              'none'=0;
              'no_response'=NA;
              else=NA")
}
test$newvar=trust.scale(test$form.Confiance.local_radio)
stopifnot(table(test$newvar)['3']==table(test$form.Confiance.local_radio)['a_lot'])


# social contact freq
confreq.scale=function(var)
{
  car::recode(as.character(var),as.factor.result=F,
              "'frequentlyl'=3;
              'sometimes'=2;
              'rarely'=1;
              'never'=0;
              'no_response'=NA;
              else=NA")
}
test$newvar=confreq.scale(test$form.Attitudes.person_from_another_ethnicity)
stopifnot(table(test$newvar)['2']==table(test$form.Attitudes.person_from_another_ethnicity)['sometimes'])

#violence
vio.scale=function(var)
{
  car::recode(as.character(var),as.factor.result=F,
              "'always'=0;
              'sometimes'=1;
              'rarely'=2;
              'never'=3;
              'no_response'=NA;
              else=NA")
}
test$newvar=vio.scale(test$form.Attitudes.violence_justification_list.defend_religion)
stopifnot(table(test$newvar)['1']==table(test$form.Attitudes.violence_justification_list.defend_religion)['sometimes'])

# media ops
radio_op.scale=function(var)
{
  car::recode(as.character(var),as.factor.result=F,
              "'very_positive'=2;
              'positive'=1;
              '---'=0;
              'no_response'=NA;
              else=NA")
}
test$newvar=radio_op.scale(test$form.radio_listener_group.opinion_of_chabab)
stopifnot(table(test$newvar)['1']==table(test$form.radio_listener_group.opinion_of_chabab)['positive'])

# Combining the functions
allcode.fun=function(var)
{
  if(length(grep("yes",levels(var)))>0){
    yn.fun(var)
  }
  else if(length(grep("at_least_once_a_week",levels(var)))>0){
    medfreq.fun(var)
  }
  else if(length(grep("every_day",levels(var)))>0){
    a24freq.fun(var)
  }
  else if(length(grep("strongly_agree",levels(var)))>0){
    agree.scale(var)
  }
  else if(length(grep("not_much",levels(var)))>0){
    trust.scale(var)
  }
  else if(length(grep("frequentlyl",levels(var)))>0){
    confreq.scale(var)
  }
  else if(length(grep("always",levels(var)))>0){
    vio.scale(var)
  }
  else if(length(grep("very_positive",levels(var)))>0){
    radio_op.scale(var)
  }
  else
    var
}
# many tests
test$newvar=agree.scale(test$form.Attitudes.good_pol_transparency)
stopifnot(table(test$newvar)['3']==table(test$form.Attitudes.good_pol_transparency)['strongly_agree'])
stopifnot(class(test$newvar) %in% "numeric")

test$newvar2=allcode.fun(test$form.Attitudes.good_pol_transparency)
stopifnot(table(test$newvar2)['3']==table(test$form.Attitudes.good_pol_transparency)['strongly_agree'])
stopifnot(class(test$newvar2) %in% "numeric")

test$newvar2=allcode.fun(test$form.Technologie.internet_access_locations)
stopifnot(table(test$newvar2)==table(test$form.Technologie.internet_access_locations))

test$newvar2=allcode.fun(test$form.watched_arewa24)
stopifnot(table(test$newvar2)['1']==table(test$form.watched_arewa24)['yes'])
stopifnot(class(test$newvar2) %in% "numeric")


##########
# Recoding all the questions
#########
#test=as.data.frame(apply(df,2,allcode.fun)) #still cannot get the mass rescale function to work
cpdf<-cp3
# apply converts to matrix, resetting all values to character
#cpdf<-as.data.frame(apply(cpdf,2,allcode.fun))
#cpdf<-as.data.frame(apply(cpdf,2,yn.fun))
#cpdf<-as.data.frame(apply(cpdf,2,medfreq.fun))
#cpdf<-as.data.frame(apply(cpdf,2,agree.scale))
#cpdf<-as.data.frame(apply(cpdf,2,trust.scale))
#cpdf<-as.data.frame(apply(cpdf,2,confreq.scale))
#cpdf<-as.data.frame(apply(cpdf,2,vio.scale))
#cpdf<-as.data.frame(apply(cpdf,2,radio_op.scale))

cpdf<-data.frame(lapply(cpdf,allcode.fun))

stopifnot(nrow(cpdf)==nrow(cp3))
typetest=sapply(cpdf,is.numeric)
stopifnot(typetest[59]==TRUE) # women.money should be numeric


###############
# Reverse Scale Questions where Agreement is Bad/Negative
##############
test<-cpdf
#women: women_work_children_suffer, boy_edu_better, early_marriage_good --> agree is bad
# the vars
recode.women<-c('form.Attitudes.women_work_children_suffer',
                'form.Attitudes.boy_edu_better')

#agree/disagree scale
rev.scale=function(var)
{
  car::recode(as.character(var),as.factor.result=F,
              "0=3;
              1=2;
              2=1;
              3=0;
              else=NA")
}
test$newvar=rev.scale(test$form.Attitudes.boy_edu_better)
stopifnot(table(test$newvar)['3']==table(test$form.Attitudes.boy_edu_better)['0'])

#
test<-cpdf

# do the recode for women
test[,recode.women]<-lapply(cpdf[,recode.women],rev.scale)
# affects these 3 vars and no others?
stopifnot(table(test$form.Attitudes.boy_edu_better)['3']==table(cpdf$form.Attitudes.boy_edu_better)['0'])
stopifnot(table(test$form.Attitudes.women_money)==table(cpdf$form.Attitudes.women_money))

#
cpdf<-test

#other vars
recode.vars<-c("form.Attitudes.ppl_not_vote","form.Attitudes.corruption_problem",
               "form.Attitudes.violence_problem")
# do the recode for other vars
test[,recode.vars]<-lapply(cpdf[,recode.vars],rev.scale)
# affects these 3 vars and no others?
stopifnot(table(test$form.Attitudes.ppl_not_vote)['3']==table(cpdf$form.Attitudes.ppl_not_vote)['0'])
stopifnot(table(test$form.Attitudes.women_money)==table(cpdf$form.Attitudes.women_money))

#
cpdf<-test

#############
# Re-scale the data: Chris: write rescale function that skips characters & factors 
############
test=cpdf # test df
# the fun
scale.fun=function(var)
{
  if(is.numeric(var)==T){
    reshape::rescaler(var,type="range")
  }
  else if (is.numeric(var)==F){
    as.character(var)
  }
}
## test the fun on a numeric variable
#test$newvar=scale.fun(test$form.Attitudes.good_pol_transparency)
#tally(test$newvar); tally(test$form.Attitudes.good_pol_transparency)
# works

# skip non-numeric?
test$newvar2<-scale.fun(test$form.Technologie.internet_access_locations)
stopifnot(table(test$newvar2)==table(test$form.Technologie.internet_access_locations))
tally(test$newvar2)


# rescale all numerics to be 0-1 on test, confirm same as non-scaled cpdf, then make cpdf from test.
test<-as.data.frame(lapply(cpdf,scale.fun))
typetest2=sapply(test,is.numeric)
stopifnot(typetest2[59]==TRUE) # women.money should be numeric
stopifnot(typetest==typetest2)
stopifnot(table(test$form.Attitudes.women_money)['1']==table(cpdf$form.Attitudes.women_money)['3'])

# save cpdf
cpdf<-test

# get class of vars
classOf<-lapply(cpdf,class)
str(classOf)

##############
# Save data/workspace
#############
save.image("cp3data.Rdata")
save(cpdf,file="cpdf.rda")
