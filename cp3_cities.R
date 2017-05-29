###################
## Cities in Survey
##################
library(ggmap)
library(maps)
data(world.cities)

# http://worldpopulationreview.com/countries/cameroon-population/cities/
# has lots of cities.

#http://cameroon.opendataforafrica.org/PHCC2015/population-and-housing-census-of-cameroon-2015?region=1002610-maga
# has the official census of 2005
#http://cvuc.cm/national/index.php/fr/carte-communale/region-du-sud/130-association/carte-administrative/nord/benoue/511-gashiga
# has gashiga
# https://www.samaritanspurse.org/medical/mission-hospitals-hospital-de-meskine-cameroon/
# has meskine population and location. 12,000.
maga=85100
tchatibali=32063
mora=179777
mokolo=242274
yagoua=91979
kaele=105504
maroua=330410
guider=223503
#bibemi=133191
garoua=265583
kousseri=101246
#meskine=12000
gashiga=50000

thecities<-as.data.frame(rbind(maga, tchatibali, mora, mokolo, yagoua, kaele, maroua, guider, garoua, kousseri, gashiga))
colnames(thecities) <- "pop2005"
thecities$city<-rownames(thecities)
thecities$states<-c("Far", "Far", "Far", "Far", "Far", "Far", "Far", 
                    "North", "North", "Far", "North")
thecities$pop2017<-thecities$pop2005 + thecities$pop2005*(.025*(2017-2005))

#meskine and gashiga were already new info
#thecities$pop2017[thecities$city %in% 'meskine']<-thecities$pop2005[thecities$city %in% 'meskine']
thecities$pop2017[thecities$city %in% 'gashiga']<-thecities$pop2005[thecities$city %in% 'gashiga']

# save cities file/workspace
save(thecities,file="cp3_cities.rda")















####### OLD#
################
citynames<-c('Garoua', #'Gashiga', #'Bibemi', #'Meskine', #bibemi not included, meskine part of maroua, gashiga part of garoua
             'Guider','Maroua', 'Kousseri', 'Yagoua', 'Maga', 'Tchatibali',
             'Kaele', 'Mora', 'Mokolo')
names(citynames)<-c('North','North', #'North','North',
                    'Far North','Far North','Far North', 'Far North',
                    'Far North',#'Far North','Far North','Far North',
                    'Far North')
thecities<-data.frame(name=citynames,state=names(citynames))
row.names(thecities)<-thecities$name


# population of cities
#sort(table(world.cities$name[world.cities$name%in%thecities$name])) # 2 moras
#world.cities[world.cities$name%in% "Mora",] # One Mora in Spain
world.cities<-world.cities[world.cities$country.etc %in% 'Cameroon',]
world.cities<-world.cities[world.cities$name %in% thecities$name,]
world.cities.pop=world.cities[,c("name","pop")]
thecities=merge(thecities,world.cities.pop)
thecities$pop=as.numeric(thecities$pop)
thecities[with(thecities, order(pop)), ] #check pop numbers

