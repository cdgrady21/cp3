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
cpdf$other_lang<-ifelse(cpdf$form.End.survey_language=="survey_lang_other",1,0)
cpdf$youth<-ifelse(cpdf$age<30,1,0)
cpdf$adult<-ifelse(cpdf$age>=30,"Adult","Youth")

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
# cultural understanding (0.81)
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
#psych::alpha(cult.index)


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
##wom.index<-with(cpdf,data.frame(eval(parse(text=paste(wom.vars,collapse=","))))) # bad way
#wom.index<-data.frame(cpdf[,wom.vars])

#psych::alpha(wom.index)


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
tally(cpdf$form.Attitudes.copy.1.of.dif_ppl_live_peace) # actually about voting for ethnic candidates
tally(cpdf$form.Attitudes.ethnic_divide_or_no)
tally(cpdf$form.Attitudes.religious_divide_or_no)

levels(cpdf$form.Attitudes.copy.1.of.dif_ppl_live_peace)
cpdf$vote_ethnic<-cpdf$form.Attitudes.copy.1.of.dif_ppl_live_peace
levels(cpdf$vote_ethnic)<-c(0,1,NA) # ethnicity not important as a 1
cpdf$vote_ethnic<-as.numeric(as.character(cpdf$vote_ethnic))
tally(cpdf$vote_ethnic)

levels(cpdf$form.Attitudes.ethnic_divide_or_no)
cpdf$ethnic_divide<-cpdf$form.Attitudes.ethnic_divide_or_no
levels(cpdf$ethnic_divide)<-c(0,NA,1)
cpdf$ethnic_divide<-as.numeric(as.character(cpdf$ethnic_divide))
tally(cpdf$ethnic_divide)

levels(cpdf$form.Attitudes.religious_divide_or_no)
cpdf$religious_divide<-cpdf$form.Attitudes.religious_divide_or_no
levels(cpdf$religious_divide)<-c(NA,1,0)
cpdf$religious_divide<-as.numeric(as.character(cpdf$religious_divide))
tally(cpdf$religious_divide)

###########
# Anti-Vio Empowerment
tally(cpdf$form.Confiance.personal_view_positive_change_ability)
cpdf$pos_change<-car::recode(cpdf$form.Confiance.personal_view_positive_change_ability,as.factor.result=F,
                             "'individual_agency_positive_change'=1;
                             'more_powerful_people_agency_positive_change'=0;
                             else=NA")
#tally(cpdf$pos_change)
#class(cpdf$pos_change)
tally(cpdf$form.Attitudes.violence_problem)
tally(cpdf$form.Attitudes.pwr_against_violence)
# Justice System
tally(cpdf$form.Attitudes.legal_recourse)

emp.vars<- c('pos_change',
             'form.Attitudes.violence_problem',
             'form.Attitudes.pwr_against_violence',
             'form.Attitudes.legal_recourse')
cpdf$emp.index<-rowSums(cpdf[,emp.vars])
#emp.index<-data.frame(cpdf[,emp.vars])
#alpha(emp.index)

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

#cpdf$youth.index1<-rowSums(cpdf[,c('form.Attitudes.old_people_get_it',
#                                   'form.Attitudes.old_people_get_world'
#                                   )])
#youth.index1<-data.frame(cpdf[,c('form.Attitudes.old_people_get_it',
#                                'form.Attitudes.old_people_get_world'
#)])
#psych::alpha(youth.index1)

#cpdf$youth.index2<-rowSums(cpdf[,c('form.Attitudes.youth_listen',
#                                   'form.Attitudes.older_problem_solving_methods_best',
#                                   'form.Attitudes.youth_involvement')])
#youth.index2<-data.frame(cpdf[,c('form.Attitudes.youth_listen',
#                                'form.Attitudes.older_problem_solving_methods_best',
#                                'form.Attitudes.youth_involvement')])
#psych::alpha(youth.index2)


################
# Violence (0.72)
vio.vars<-c('form.Attitudes.violence_justification_list.defend_religion',
              'form.Attitudes.violence_justification_list.maintain_culture',
              'form.Attitudes.violence_justification_list.vigilatante',
              'form.Attitudes.violence_justification_list.force_govt_change')
cpdf$vio.index<-rowSums(cpdf[,vio.vars])
#vio.index<-data.frame(cpdf[,vio.vars])
#psych::alpha(vio.index)

########################
# Confidence in Institutions (alpha = 0.77)
#grep("confiance", names(cp3),ignore.case=T) # cols 40:45, but not 40 because that's efficacy
conf.vars<-names(cpdf)[grep("confiance", names(cpdf),ignore.case=T)][-1] # remove the efficacy question
cpdf$conf.index<-rowSums(cpdf[,conf.vars])
cpdf[1:10,grep("conf.", names(cpdf),ignore.case=T)]

#conf.index<-data.frame(cpdf[,conf.vars])
#psych::alpha(conf.index)


###########################
# One-off Questions (to look at)
###########################
# utilities
table(cpdf$form.Technologie.regular_access_to_electricity)
# tech --> no recode functions probably.
#table(cp3$form.Technologie.tech_access)
#levels(cp3$form.Technologie.tech_access)
# phone access
cpdf$mobile<-ifelse(grepl("simple_mobile",cpdf$form.Technologie.tech_access)
                    ,1,0)
cpdf$smartphone<-ifelse(grepl("feature_or_smartphone",cpdf$form.Technologie.tech_access)
                    ,1,0)
cpdf$computer<-ifelse(grepl("computer",cpdf$form.Technologie.tech_access)
                        ,1,0)
cpdf$tablet<-ifelse(grepl("tablet",cpdf$form.Technologie.tech_access)
                      ,1,0)

# mobile service provider
#table(cp3$form.Technologie.mobile_service_provider)
#levels(cp3$form.Technologie.mobile_service_provider)
cpdf$mtn<-ifelse(grepl("mtn",cpdf$form.Technologie.mobile_service_provider)
                    ,1,0)
cpdf$camtel<-ifelse(grepl("Camtel",cpdf$form.Technologie.mobile_service_provider)
                    ,1,0)
cpdf$orange<-ifelse(grepl("orange",cpdf$form.Technologie.mobile_service_provider)
                    ,1,0)
cpdf$nextel<-ifelse(grepl("Nextel",cpdf$form.Technologie.mobile_service_provider)
                    ,1,0)
cpdf$otherMSP<-ifelse(grepl("other",cpdf$form.Technologie.mobile_service_provider)
                    ,1,0)

# OS
#table(cp3$form.Technologie.personal_smartphone_os)
#table(cp3$form.Technologie.personal_tablet_os)
cpdf$android<-ifelse(grepl("android", cpdf$form.Technologie.personal_smartphone_os),1,0)
cpdf$blackberry<-ifelse(grepl("blackberry", cpdf$form.Technologie.personal_smartphone_os),1,0)
cpdf$ios<-ifelse(grepl("iOS", cpdf$form.Technologie.personal_smartphone_os),1,0)
cpdf$windows<-ifelse(grepl("windows", cpdf$form.Technologie.personal_smartphone_os),1,0)

# Internet
tally(cpdf$form.Technologie.access_to_internet)
#table(cp3$form.Technologie.internet_access_locations)
#levels(cpdf$form.Technologie.internet_access_locations)
cpdf$int_home<- ifelse(grepl("home", cpdf$form.Technologie.internet_access_locations),1,0)
cpdf$int_work<- ifelse(grepl("work", cpdf$form.Technologie.internet_access_locations),1,0)
cpdf$int_friend_rel<-ifelse(grepl("friend_or_relative", cpdf$form.Technologie.internet_access_locations),1,0)
cpdf$int_cafe<-ifelse(grepl("internet_cafe", cpdf$form.Technologie.internet_access_locations),1,0)
cpdf$int_mobile<-ifelse(grepl("mobie", cpdf$form.Technologie.internet_access_locations),1,0)
cpdf$int_tablet<-ifelse(grepl("tablet", cpdf$form.Technologie.internet_access_locations),1,0)
cpdf$int_other<-ifelse(grepl("Other", cpdf$form.Technologie.internet_access_locations),1,0)

#cpdf$form.Technologie.other_internet_access_point_specified # no responses

# media listening
table(cpdf$form.radio_listener_group.Dabalaye) # only 9 people listened to Dabalaye
tally(cpdf$form.radio_listener_group.frequecy_dabalaye)
##table(cpdf$form.radio_listener_group.dabalaye_opinion) # only 9 people listened to Dabalaye

tally(cpdf$form.radio_listener_group.chabab_frequency) # only 3 people listened to Chabab
tally(cpdf$form.radio_listener_group.opinion_of_chabab) # only 3 people listened to Chabab

table(cpdf$form.radio_listener_group.Douniarou_Derkeen)
tally(cpdf$form.radio_listener_group.frequecy_Douniarou)
table(cpdf$form.radio_listener_group.Douniarou_opinion)

tally(cpdf$form.radio_listener_group.Dandal_Kura) # 0 people.

# A24
tally(cpdf$form.watched_arewa24)
table(cpdf$form.watch_arewa24_frequency)
levels(cp3$form.arewa24_programs_watched)
cpdf$alawar<-ifelse(grepl("alawar_yara", cpdf$form.arewa24_programs_watched),1,0)
cpdf$waiwaye<-ifelse(grepl("waiwaye", cpdf$form.arewa24_programs_watched),1,0)
cpdf$dadin_kowa<-ifelse(grepl("dadin_kowa", cpdf$form.arewa24_programs_watched),1,0)
cpdf$hhh<-ifelse(grepl("h_hip_hop", cpdf$form.arewa24_programs_watched),1,0)
cpdf$kundin<-ifelse(grepl("kundin", cpdf$form.arewa24_programs_watched),1,0)
cpdf$tauraruwa<-ifelse(grepl("tauraruwa", cpdf$form.arewa24_programs_watched),1,0)
cpdf$gari<-ifelse(grepl("gari_ya_waye", cpdf$form.arewa24_programs_watched),1,0)
cpdf$matasa<-ifelse(grepl("matasa", cpdf$form.arewa24_programs_watched),1,0)
cpdf$other<-ifelse(grepl("other", cpdf$form.arewa24_programs_watched),1,0)

# Demographics
table(cp3$form.End.survey_language)
table(cp3$form.demographics_question_group.respondent_highest_education_level)
cpdf$education<-car::recode(cpdf$form.demographics_question_group.respondent_highest_education_level,
                            as.factor.result=F,
                            "'no_formal_education'=0;
                            'primary_school'=1;
                            'quranic_school'=2;
                            'middle_school'=3;
                            'vocational_school'=4;
                            'high_school'=5;
                            'university_higher_edu'=6;
                            else=NA")
#prop.table(table(cpdf$education,cpdf$region),margin=2) # ex.north looks  lil poorer

# ethnic group
sort(table(cpdf$form.demographics_question_group.ethnic_background))
sara.ethnic<-c("sara", "guidar", "masa", "massa", "kirdi",
               "gula", "kara", "kreish", "nduka", "ngama",
               "kapsiki", "mada", "mafa", "matakam", "mofou", "m,oufou",
               "mora", "mousgoum", "muyang", "mouyang", "ouldeme", "guiziga",
               "podoko", "toupouri", "tupuri", "vame", "zulgo")
cpdf$sara_kirdi<- ifelse(grepl(paste(sara.ethnic, collapse="|"), cpdf$form.demographics_question_group.ethnic_background, ignore.case=T),1,0)
other.ethnic<-levels(cpdf$form.demographics_question_group.ethnic_background)[grepl(paste(sara.ethnic, collapse="|"), 
                                                                                    levels(cpdf$form.demographics_question_group.ethnic_background),
                                                                                    ignore.case=T) %in% 0]
# failing to get a sorted table of ethnicities that are in other.ethnic
#sort(table(tolower(cpdf$form.demographics_question_group.ethnic_background)))[names(table(tolower(cpdf$form.demographics_question_group.ethnic_background))) %in% tolower(other.ethnic)]
sort(table(tolower(cpdf$form.demographics_question_group.ethnic_background)))

cpdf$fulani<- ifelse(grepl("fulani", cpdf$form.demographics_question_group.ethnic_background, ignore.case=T), 1, 0)
cpdf$mundang<- ifelse(grepl("mundang", cpdf$form.demographics_question_group.ethnic_background, ignore.case=T), 1, 0)
cpdf$arab<- ifelse(grepl("arab", cpdf$form.demographics_question_group.ethnic_background, ignore.case=T), 1, 0)
cpdf$kanuri<- ifelse(grepl("kanur", cpdf$form.demographics_question_group.ethnic_background, ignore.case=T), 1, 0)
cpdf$hausa<- ifelse(grepl("haus", cpdf$form.demographics_question_group.ethnic_background, ignore.case=T), 1, 0)
cpdf$other_ethnic<- ifelse(rowSums(cpdf[c('sara_kirdi', 'fulani', 'mundang', 'arab', 'kanuri', 'hausa')])>0, 0,1)

#cpdf[1:10,c('sara_kirdi', 'fulani', 'mundang', 'arab', 'kanuri', 'hausa', 'other_ethnic')]


# employment
# languages


############
# Aggregate Things?
###########
#aggregate at psu level
ag.vars<-c("muslim","christian","female","male","french","fulfulde","other_lang")
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
