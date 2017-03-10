# Scan in Workspace/load libraries
load("cp3data.Rdata")
library(mosaic)
library(psych)

##########
# Make these "---" things NA like they should be
##########
cpdf[cpdf=="---"]<-NA
cpdf<-droplevels(cpdf)

###########
# Recode some vars for easy use
##########
cpdf$gender<-cpdf$form.Demographiques_introduction.respondent_sex
cpdf$religion<-car::recode(cpdf$form.demographics_question_group.religion,
                           "'islam'='islam';
                           'christian'='christian';
                           else='other'")
cpdf$region<-cpdf$form.Demographiques_introduction.region
cpdf$age<-as.numeric(as.character(cpdf$form.Demographiques_introduction.respondent_age))


############
# Add some new variables
############
cpdf$muslim<-ifelse(cpdf$religion=="islam", 1, 0)
cpdf$christian<-ifelse(cpdf$religion=="christian", 1, 0)
cpdf$female<-ifelse(cpdf$gender=="female", 1, 0)
cpdf$male<-ifelse(cpdf$gender=="male", 1, 0)
cpdf$french<-ifelse(cpdf$form.End.survey_language=="survey_lang_fr",1,0)
cpdf$fulfulde<-ifelse(cpdf$form.End.survey_language=="survey_lang_ful",1,0)
cpdf$other<-ifelse(cpdf$form.End.survey_language=="survey_lang_other",1,0)
cpdf$youth<-ifelse(cpdf$age<30,1,0)
cpdf$adult<-ifelse(cpdf$age<=30,"Adult","Youth")

region.vars<-c('int_num','form.Demographiques_introduction.region_no',
                  'form.Demographiques_introduction.region_exno')
cpdf[,region.vars]<-lapply(cpdf[,region.vars],as.character)
cpdf$towns<-ifelse(is.na(cpdf$form.Demographiques_introduction.region_exno),
                   cpdf$form.Demographiques_introduction.region_no,
                   cpdf$form.Demographiques_introduction.region_exno)
stopifnot(table(cpdf$form.Demographiques_introduction.region_exno[cpdf$towns=="Maga"])==30)
#table(cpdf$towns,cpdf$form.Demographiques_introduction.region_no)
#table(cpdf$towns,cpdf$form.Demographiques_introduction.region_exno)

#########################
# Make Outcome Indices
########################
# social contact (0.76)
cpdf$sc.index<-cpdf$form.Attitudes.person_from_another_ethnicity+
  cpdf$form.Attitudes.person_from_another_religion+
  cpdf$form.Attitudes.refugee_or_foreigner

#sc.index<-with(cpdf,data.frame(form.Attitudes.person_from_another_ethnicity,
#                                 form.Attitudes.person_from_another_religion,
#                                 form.Attitudes.refugee_or_foreigner))
#alpha(sc.index)
#summary(alpha(sc.index))


#################
# cultural understanding
cult.vars<-c("form.Attitudes.tolerant_living_with_others_value",
               "form.Attitudes.other_religions_respect",
               'form.Attitudes.other_ethnicities_respect',
               'form.Attitudes.other_regions_respect',
               'form.Attitudes.political_leaders_respect',
               'form.Attitudes.youth_proud_culture')

cpdf$cult.index<-with(cpdf,form.Attitudes.tolerant_living_with_others_value+
                        form.Attitudes.other_religions_respect+
                        form.Attitudes.other_ethnicities_respect+
                        form.Attitudes.other_regions_respect+
                        form.Attitudes.political_leaders_respect+
                        form.Attitudes.youth_proud_culture)

#cult.index<-with(cpdf, data.frame(form.Attitudes.tolerant_living_with_others_value,
#                   form.Attitudes.other_religions_respect,
#                   form.Attitudes.other_ethnicities_respect,
#                   form.Attitudes.other_regions_respect,
#                   form.Attitudes.political_leaders_respect,
#                   form.Attitudes.youth_proud_culture))
#alpha(cult.index)


##############
# Women index (0.59)
wom.vars<-c('form.Attitudes.women_money',
              'form.Attitudes.women_work_children_suffer',
              'form.Attitudes.boy_edu_better',
              'form.Attitudes.early_marriage_good')
#crappy way
#cpdf$wom.index3<-with(cpdf,form.Attitudes.women_money+
#                      form.Attitudes.women_work_children_suffer+
#                     form.Attitudes.boy_edu_better+
#                    form.Attitudes.early_marriage_good)
## much better way to make index
#cpdf$wom.index2<-with(cpdf,eval(parse(text=paste(wom.vars,collapse="+"))))
# OR even easier
cpdf$wom.index<-rowSums(cpdf[,wom.vars])

# check
#stopifnot(cpdf$wom.index==cpdf$wom.index2)
#stopifnot(cpdf$wom.index==cpdf$wom.index3)
#it failed?
#cpdf$int_num[cpdf$wom.index!=cpdf$wom.index2]
#cpdf[cpdf$wom.index!=cpdf$wom.index2,wom.vars]
#cpdf[cpdf$wom.index!=cpdf$wom.index2,c('wom.index',"wom.index2")]

#cpdf$int_num[cpdf$wom.index!=cpdf$wom.index3]
#cpdf[cpdf$wom.index!=cpdf$wom.index3,wom.vars]
#cpdf[cpdf$wom.index!=cpdf$wom.index3,c('wom.index',"wom.index3")]
##Nope, different rounding that is inconsequential.

# index alpha
#wom.index<-with(cpdf,data.frame(eval(parse(text=paste(wom.vars,collapse=",")))))
#wom.index<-data.frame(cpdf[,wom.vars])

#alpha(wom.index)


###############
# Religious Tolerance (0.74)
rel.vars<-c('form.Attitudes.one_valid_interpret',
            'form.Attitudes.my_rel_promotes_peace',
            'form.Attitudes.other_rel_promte_peace',
            'form.Attitudes.dif_ppl_live_peace')
cpdf$reltol.index<-rowSums(cpdf[,rel.vars])
#reltol.index<-data.frame(cpdf[,rel.vars])
#alpha(reltol.index)


##############
# Political/Civic Engagement (0.25)
pol.vars<-c('form.Attitudes.good_pol_transparency',
            'form.Attitudes.ppl_not_vote',
            'form.Attitudes.corruption_problem',
            'form.Attitudes.community_solve_problems')
cpdf$pol.index<-rowSums(cpdf[,pol.vars])
#pol.index<-data.frame(cpdf[,pol.vars])
#alpha(pol.index)


################
# Rando Exp
tally(cpdf$form.Attitudes.people_other_rel_in_comm75)
tally(cpdf$form.Attitudes.people_other_rel_in_comm50)
tally(cpdf$form.Attitudes.people_other_rel_in_comm25)
tally(cpdf$form.Attitudes.people_other_rel_in_comm5)


################
# Rel/Ethnic Feelings
tally(cpdf$form.Attitudes.copy.1.of.dif_ppl_live_peace)
tally(cpdf$form.Attitudes.ethnic_divide_or_no)
tally(cpdf$form.Attitudes.ethnic_divide_or_no)


###########
# Anti-Vio Empowerment
tally(cpdf$form.Confiance.personal_view_positive_change_ability)
tally(cpdf$form.Attitudes.violence_problem)
tally(cpdf$form.Attitudes.pwr_against_violence)


################
# Justice System
tally(cpdf$form.Attitudes.legal_recourse)
       

##############
# Youth-Old (0.62)
levels(cpdf$form.Attitudes.youth_involvement)<-c(NA,0,1)
cpdf$form.Attitudes.youth_involvement<-as.numeric(cpdf$form.Attitudes.youth_involvement)
youth.vars<-c('form.Attitudes.old_people_get_it',
            'form.Attitudes.youth_listen',
            'form.Attitudes.old_people_get_world',
            'form.Attitudes.older_problem_solving_methods_best',
            'form.Attitudes.youth_involvement')
cpdf$youth.index<-rowSums(cpdf[,youth.vars])
#youth.index<-data.frame(cpdf[,youth.vars])
#alpha(youth.index)


################
# Violence (0.72)
vio.vars<-c('form.Attitudes.violence_justification_list.defend_religion',
              'form.Attitudes.violence_justification_list.maintain_culture',
              'form.Attitudes.violence_justification_list.vigilatante',
              'form.Attitudes.violence_justification_list.force_govt_change')
cpdf$vio.index<-rowSums(cpdf[,vio.vars])
#vio.index<-data.frame(cpdf[,vio.vars])
#alpha(vio.index)


###########################
# One-off Questions (to look at)
###########################
# utilities
table(cp3$form.Technologie.regular_access_to_electricity)
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
# Aggregate Things?
###########
#aggregate at psu level
ag.vars<-c("muslim","christian","female","male","french","fulfulde","other")
town.df<-aggregate(cpdf[,ag.vars],
                 by=list(name=cpdf$towns),mean,na.rm=T)
region.df<-aggregate(cpdf[,ag.vars],
                   by=list(name=cpdf$form.Demographiques_introduction.region),mean,na.rm=T)


##############
# Save the workspace
##############
save.image('cp3_analysis_dat.Rdata')


################
# Look at some stuff
################
table(cpdf$form.demographics_question_group.primary_language_spoken,cpdf$form.End.survey_language)
 # many people who said their main language was fulfulde did the survey in french.
