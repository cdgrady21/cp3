

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
