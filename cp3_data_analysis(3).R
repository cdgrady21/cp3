# Scan in Workspace/load libraries
load("cp3_analysis_dat.Rdata")
library(mosaic)
library(survey)
library(lmtest)
library(sandwich)
library(stats)
library(ggplot2)
library(reshape2)

###############
# Again re-scale everything to be 0-1
###############
cpdf<-as.data.frame(lapply(cpdf,scale.fun)) # already tested function in (1) script
#typetest2=sapply(test,is.numeric)
#stopifnot(table(test$vio.index)['1']==table(cpdf$vio.index)['4'])

###########
# Survey Corrections
###########
# fix mistaken town
## when psus not nested in towns
#test<-unique(cpdf[,c("psu","towns")])
#sort(table(test$psu))
cpdf$towns[cpdf$psu%in%'10']<-"Meskine"
cpdf$towns[cpdf$psu%in%'11']<-"Maroua"
cpdf$towns[cpdf$psu%in%'12']<-"Maroua"
cpdf$towns[cpdf$psu%in%'14']<-"Maroua"
cpdf$towns[cpdf$psu%in%'8']<-"Meskine"


test<-cpdf %>% dplyr::count(psu)
names(test)<-c("psu","num_respondents")
cpdf<-merge(cpdf,test,by="psu")
cpdf$weights<-15/cpdf$num_respondents
cpdf$fpc2<-80
# number of PSUs per town from cp3_list in the sampling folder
#Garoua    Gashiga     Guider      Kaele   Kousseri       Maga     Maroua    Meskine 
#26          9         34         12         47          5         26         26 
#Mokolo       Mora Tchatibali     Yagoua 
#26         19         31         16
cpdf$fpc1[cpdf$towns%in%'garoua']<-26
cpdf$fpc1[cpdf$towns%in%'gashiga']<-9
cpdf$fpc1[cpdf$towns%in%'Guider']<-34
cpdf$fpc1[cpdf$towns%in%'Kaele']<-12
cpdf$fpc1[cpdf$towns%in%'Kousseri']<-47
cpdf$fpc1[cpdf$towns%in%'Maga']<-5
cpdf$fpc1[cpdf$towns%in%'Maroua']<-26
cpdf$fpc1[cpdf$towns%in%'Meskine']<-26
cpdf$fpc1[cpdf$towns%in%'Mokolo']<-26
cpdf$fpc1[cpdf$towns%in%'Mora']<-19
cpdf$fpc1[cpdf$towns%in%'Tchati-bali']<-31
cpdf$fpc1[cpdf$towns%in%'Yagoua']<-16

svy1=svydesign(ids=~psu+survey,
              strata=~towns,
              weights=~weights,
              #pps="brewer",
              fpc=~fpc1+fpc2,
              data=cpdf)
#don't use weights & pps at same time.
svy <- as.svrepdesign(svy1,type="bootstrap", replicates=1000)

# svyby(~vio.index,~region,svy1,svymean,na.rm=TRUE)
#big<-cpdf[cpdf$psu%in%c("30","56","17","59","28"),]
#summary(big$vio.index[big$region%in%"exno"])
#summary(big$vio.index[big$region%in%"no"])
#small<-cpdf[cpdf$psu%in%c("26","27","55","20","24","52","16","21","23","29","46","49","50","54","18","19","44","62","41","53","58","61","64"),]
#summary(small$vio.index[cpdf$region%in%"exno"])
#summary(small$vio.index[cpdf$region%in%"no"])

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
  thecoef<-coef(thelm)
  pvals<-summary(thelm)$coefficients[,4]
  theci<-confint(thelm)
  return(cbind(thecoef,pvals,theci[,1],theci[,2]))
}
#svylm.fun(outcome='vio.index',treatment='gender*region')


####################################
# # Easily get mean & Standard Error of survey vars
## plot.vars will be fed into this.
#plot.vars<-c("gender", 'adult', 'religion', 'region', 
#             'form.End.survey_language','vio.index')
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
#mean.df<-meanse.fun('form.Attitudes.corruption_problem',plot.vars,svy)

########
# Table-making function
#######
plot.fun<-function(outcome,dat,title)
{
  # set the vars of the plot dataset
  plot.vars<-c("gender", 'adult', 'religion', 'region', 
               'form.End.survey_language',outcome)
  
  # from function above.
  # grab mean & se of those vars, accounting for survey parameters.
  df1<-meanse.fun(outcome,plot.vars,dat)
  
  # Format the data for ggplot
  mean.df<-reshape2::melt(df1,id.vars=c(outcome,'se','subset'))[,-4] # drop redundant "variable" column.
  mean.df$subset<-as.factor(mean.df$subset)
  levels(mean.df$subset)<-c('Age',"Language","Gender","Region","Religion")
  mean.df$subset<-factor(mean.df$subset, levels(mean.df$subset)[c(3,1,4,5,2)])
  
  # calculate ymin and ymax for error bars
  miner<-mean.df[[outcome]]-(mean.df$se)
  maxer<-mean.df[[outcome]]+(mean.df$se)

  # Make the plot
  ggplot(mean.df, aes(x=subset, y=mean.df[[outcome]], fill=value)) + 
    geom_bar(position=position_dodge(0.9), stat="identity",
             colour="black", # Use black outlines,
             size=.5) +      # Thinner black lines
    geom_errorbar(aes(ymin=miner, ymax=maxer),
                  size=.3,    # Thinner lines
                  width=.2,
                  position=position_dodge(.9)) +
    xlab("Respondent Subsets") +
    ylab("Outcome Score") +
    scale_fill_hue(name="Group", # Legend label, use darker colors
                   breaks=c('female','male','Adult','Youth','exno','no',
                            'christian','islam','other','survey_lang_fr',
                            'survey_lang_ful','survey_lang_other'),
                   labels=c('Female','Male','Adult','Youth','Ex North','North',
                            'Christian','Muslim','Other Rel','French',
                            'Fulfulde','Other Lang')) +
    ggtitle(title) +
    #scale_y_continuous(limit=c(0,1)) +
    theme_bw() +
    theme(panel.grid.major = element_blank()) +
    theme(plot.title = element_text(size=18))
  
}
#plot.fun(outcome="vio.index",
#         dat=svy,title="Violence Index by Group")
#plot.fun(outcome='form.Attitudes.corruption_problem',dat=svy,
#         title="Corruption Belief by Group")



######################
# ggplot saved because takes a long time to run.
######################
# social contact
sc.plot<-plot.fun(outcome='sc.index',dat=svy,title='Social Contact Index by Group')
sc.gen.lm<-svylm.fun(outcome='sc.index',treatment='gender',data=svy)

#cultural understanding
cult.plot<-plot.fun(outcome='cult.index',dat=svy,title='Cultural Understanding Index by Group')

# women
wom.plot<-plot.fun(outcome='wom.index',dat=svy,title="Women's Empowerment Index by Group")
wom.rel.lm<-svylm.fun(outcome='wom.index',treatment='region',data=svy)

wom_money.plot<-plot.fun(outcome='form.Attitudes.women_money',dat=svy,
                         title="Women Should Have a Say in How Her Household Spends Money")
wom_money.lm<-svylm.fun(outcome='form.Attitudes.women_money',treatment='region',data=svy)

wom_work.plot<-plot.fun(outcome='form.Attitudes.women_work_children_suffer',dat=svy,
                        title="Women Work Children Suffer")
wom_work.lm<-svylm.fun(outcome='form.Attitudes.women_work_children_suffer',treatment='gender')

eduboys.plot<-plot.fun(outcome='form.Attitudes.boy_edu_better',dat=svy,
                       title="Education Boys Better")
wom_money.lm<-svylm.fun(outcome='form.Attitudes.boy_edu_better',treatment='region',data=svy)

earlymarriage.plot<-plot.fun(outcome='form.Attitudes.early_marriage_good',dat=svy,
                             title="Early Marriage Good")

# Rel Tol
reltol.plot<-plot.fun(outcome='reltol.index',dat=svy,title='Religious Tolerance Index by Group')

valid.plot<-plot.fun(outcome='form.Attitudes.one_valid_interpret',dat=svy,
                     title='More than One Valid Interpretation of Religious Teaching')
myrel_peace.plot<-plot.fun(outcome='form.Attitudes.my_rel_promotes_peace',dat=svy,
                           title='My Religion Promotes Peace')
otherrel_peace.plot<-plot.fun(outcome='form.Attitudes.other_rel_promte_peace',dat=svy,
                              title='Other Religions Promote Peace')
diffrel_peace.plot<-plot.fun(outcome='form.Attitudes.dif_ppl_live_peace',dat=svy,
                             title='People of Different Religions Live Peacefully')

# Political/Civic
pol_trans.plot<-plot.fun(outcome='form.Attitudes.good_pol_transparency',
                         dat=svy,title='Good Political Transparency')
dont_vote.plot<-plot.fun(outcome='form.Attitudes.ppl_not_vote',
                         dat=svy,title="People Don't Vote")
corruption.plot<-plot.fun(outcome='form.Attitudes.corruption_problem',
                         dat=svy,title='Corruption a Problem')
comm_solve.plot<-plot.fun(outcome='form.Attitudes.community_solve_problems',
                          dat=svy,title='Community Solves Problems')



# Rando exp --> Just didn't work.  Virtually everyone said yes in all conditions.
tally(cpdf$form.Attitudes.people_other_rel_in_comm75)
tally(cpdf$form.Attitudes.people_other_rel_in_comm50)
tally(cpdf$form.Attitudes.people_other_rel_in_comm25)
tally(cpdf$form.Attitudes.people_other_rel_in_comm5)


# Rel/Ethnic Feelings
vote_ethnic.plot<-plot.fun(outcome='vote_ethnic',
                          dat=svy,title='Elect Same Ethnicity')
#tally(cpdf$form.Attitudes.copy.1.of.dif_ppl_live_peace)
ethnic_divide.plot<-plot.fun(outcome='ethnic_divide',
                           dat=svy,title='Overcome Ethnic Divide')
#tally(cp3$form.Attitudes.ethnic_divide_or_no)

religious_divide.plot<-plot.fun(outcome='religious_divide',
                             dat=svy,title='Overcome Religious Divide')
#tally(cp3$form.Attitudes.ethnic_divide_or_no)


# Anti-Violence Empowerment
pos_change.plot<-plot.fun(outcome='pos_change',
                                dat=svy,title='Can Affect Positive Change')
#tally(cp3$form.Confiance.personal_view_positive_change_ability)
violence_problem.plot<-plot.fun(outcome='form.Attitudes.violence_problem',
                          dat=svy,title='Violence Not a Problem')
violence_problem.plot<-plot.fun(outcome='form.Attitudes.pwr_against_violence',
                                dat=svy,title='Power Against Violence')

# Justice System
legal_recourse.plot<-plot.fun(outcome='form.Attitudes.legal_recourse',
                              dat=svy,title='Legal Recourse')

# Youth-Old
youth.plot<-plot.fun(outcome='youth.index',
                              dat=svy,title='Youth Index by Group')
old_respect.plot<-plot.fun(outcome='form.Attitudes.old_people_get_it',
                     dat=svy,title='Older People Respect Youth')
youth_respect.plot<-plot.fun(outcome='form.Attitudes.youth_listen',
                           dat=svy,title='Youth People Respect Elders')
elders_understand.plot<-plot.fun(outcome='form.Attitudes.old_people_get_world',
                             dat=svy,title='Older People Understand World')
elders_applicable.plot<-plot.fun(outcome='form.Attitudes.older_problem_solving_methods_best',
                                 dat=svy,title='Elders Problem-Solving Applicable')
youth_involved.plot<-plot.fun(outcome='form.Attitudes.youth_involvement',
                                 dat=svy,title="Elders' Problem-Solving Applicable")


# Violence
vio.plot<-plot.fun(outcome='vio.index',dat=svy,title='Violence Index by Group')
defend_rel.plot<-plot.fun(outcome='form.Attitudes.violence_justification_list.defend_religion',
                          dat=svy,title='Justified to Defend Religion')
maintain.culture.plot<-plot.fun(outcome='form.Attitudes.violence_justification_list.maintain_culture',
                                dat=svy,title='Justified to Maintain Culture')
criminals_justice.plot<-plot.fun(outcome='form.Attitudes.violence_justification_list.vigilatante',
                                 dat=svy,title='Justified to Bring Criminals to Justice')
gov_change.plot<-plot.fun(outcome='form.Attitudes.violence_justification_list.force_govt_change',
                          dat=svy,title='Justified to Force Gov to Change')

# Others
electricity.plot<-plot.fun(outcome='form.Technologie.regular_access_to_electricity',
                           dat=svy,title='Regular Access to Electricity')
# Tech Access
mobile.plot<-plot.fun(outcome='mobile',dat=svy,title='Mobile Phone Ownership by Group')
smartphone.plot<-plot.fun(outcome='smartphone',dat=svy,title='Smartphone Ownership by Group')
computer.plot<-plot.fun(outcome='computer',dat=svy,title='Computer Ownership by Group')
tablet.plot<-plot.fun(outcome='tablet',dat=svy,title='Tablet Ownership by Group')

# Mobile Service Providers
mtn.plot<-plot.fun(outcome='mtn',dat=svy,title='MTN Use by Group')
camtel.plot<-plot.fun(outcome='camtel',dat=svy,title='Camtel Use by Group')
orange.plot<-plot.fun(outcome='orange',dat=svy,title='Orange Use by Group')
nextel.plot<-plot.fun(outcome='nextel',dat=svy,title='Nextel Use by Group')
otherMSP.plot<-plot.fun(outcome='otherMSP',dat=svy,title='Other MSP Use by Group')

# OS
android.plot<-plot.fun(outcome='android',dat=svy,title='Android OS by Group')
blackberry.plot<-plot.fun(outcome='blackberry',dat=svy,title='Blackberry OS by Group')
ios.plot<-plot.fun(outcome='ios',dat=svy,title='iOS by Group')
windows.plot<-plot.fun(outcome='windows',dat=svy,title='Windows OS by Group')

# Internet Use
int.plot<-plot.fun(outcome='form.Technologie.access_to_internet',dat=svy,title="Internet Access")
int_friend_rel.plot<-plot.fun(outcome='int_friend_rel',dat=svy,title="Internet at Friend/Relative's House")
int_cafe.plot<-plot.fun(outcome='int_cafe',dat=svy,title='Internet at Internet Cafe')
int_mobile.plot<-plot.fun(outcome='int_mobile',dat=svy,title='Internet on Mobile Phone')
#int_tablet.plot<-plot.fun(outcome='int_tablet',dat=svy,title='Internet on Tablet') # 0 said this
int_other.plot<-plot.fun(outcome='int_other',dat=svy,title='Internet Some Other Way')

# Media/Radio Listening
don_derkeen.plot<-plot.fun(outcome='form.radio_listener_group.Douniarou_Derkeen',
                         dat=svy,title='Listen to Douniarou Derkeen')
dd_freq.plot<-plot.fun(outcome='form.radio_listener_group.frequecy_Douniarou',
                           dat=svy,title='Listen to Douniarou Derkeen Frequency')
dd_op.plot<-plot.fun(outcome='form.radio_listener_group.Douniarou_opinion',
                       dat=svy,title='Douniarou Derkeen Opinion')

# Arewa24 Plots
alawar.plot<-plot.fun(outcome='alawar',dat=svy,title="Watch Alawar Yara")
waiwaye.plot<-plot.fun(outcome='waiwaye',dat=svy,title="Watch Waiwaye")
dadin.plot<-plot.fun(outcome='dadin_kowa',dat=svy,title="Watch Dadin Kowa")
hhh.plot<-plot.fun(outcome='hhh',dat=svy,title="Watch Hausa Hip Hop")
kundin.plot<-plot.fun(outcome='kundin',dat=svy,title="Watch Kundin Kannywood")
tauraruwa.plot<-plot.fun(outcome='tauraruwa',dat=svy,title="Watch Tauraruwa")
gari.plot<-plot.fun(outcome='gari',dat=svy,title="Watch Gari Ya Waye")
matasa.plot<-plot.fun(outcome='matasa',dat=svy,title="Watch Matasa")
other.plot<-plot.fun(outcome='other',dat=svy,title="Watch Other A24 Program")

# Other demographics


#####################
# Save it!
####################
save.image('cp3_report_dat.Rdata')

#########################
# Lost Items --> didnt work or dismissed
#df.plot<-svy[['variables']][plot.vars]
#require(reshape2)
#mean.df<-reshape2::melt(df.plot,id.vars='vio.index')
#stopifnot(mean(cpdf[['vio.index']][cpdf$religion %in% "christian"],na.rm=T)==
#            mean(mean.df$vio.index[mean.df$value %in% 'christian'],na.rm=T))
#qplot(variable, vio.index, data=mean.df, geom=c("boxplot", "jitter"), 
#      fill=variable, main="vio.index", ylim=c(3,4),
#      xlab="", ylab="vio.index")

#qplot(towns,vio.index,data=cpdf[!is.na(cpdf$vio.index),],geom=c("boxplot","jitter"))

#ggplot(data=cpdf, aes(x=towns, y=vio.index)) +
#  geom_bar(stat="identity")