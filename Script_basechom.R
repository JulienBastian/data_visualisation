##base de données chomage 2020 
## chargement de la base de donnée 
chomage2020 <-read.csv("FD_csv_EEC20.csv",sep = ";")

#library(esquisse)

##Mettre en français 
#iset_i18n("fr")
#esquisser(chomage2020)
##lancement des librairies
library(questionr)
library(tidyverse)
#library(ggplot2)
library(FactoMineR)

## I- recodage de la base de données ----

## catégorie socio -professionnelle 
chomage2020
mode(chomage2020$CSTOTR)
chomage2020$CSP<-as.character(chomage2020$CSTOTR)
chomage2020$CSP<-fct_recode(chomage2020$CSP,
                            "Non renseigné"="0",
                            "Agriculteurs exploitants"="1",
                            "Artisans, commerçants, et chefs d'entreprise"="2",
                            "Cadres et professions intellectuelles supérieures"="3",
                            "Professions intermédiaires"="4",
                            "Employés"="5",
                            "Ouvriers"="6",
                            "Inactifs avec activité professionnelle antérieure"="7",
                            "Autres personnes sans activité professionnelle"="8" )



##vérification 
table(chomage2020$CSTOTR)
table(chomage2020$CSP)

## statut d 'activité 
mode(chomage2020$ACTEU6)
chomage2020$statutACT<-as.character(chomage2020$ACTEU6)
chomage2020$statutACT=fct_explicit_na(chomage2020$statutACT, "null")
chomage2020$statutACT<-fct_recode(chomage2020$statutACT,
                                  "Sans objet" = "null", ##non trouve par R - laisse la case vide - voir comment remplacer le vide par "sans objet" - mais n'empêche pas le pgm de tourner
                                  "Actif occupé" = "1",
                                  "Chômeur PSERE (Population sans Emploi à la Recherche d'un Emploi)"="3",
                                  "Autre chômeur BIT"="4",
                                  "Étudiant, élève, stagiaire en formation (inactifs)"="5",
                                  "Autres inactifs (dont retraités)"="6")

##verification
sum(is.na(as.character(chomage2020$ACTEU6)))
table(chomage2020$ACTEU6)
table(chomage2020$statutACT)


## tranche d'âge (AGE5)

mode(chomage2020$AGE5)
chomage2020$AGE<-as.character(chomage2020$AGE5)
#age au dernier jour de la semaine de référence
chomage2020$AGE<-fct_recode(chomage2020$AGE,
                            #"15 à 29 ans"="0", ## recodage inutile car CSP sans actif 
                            "15 à 29 ans"="15",
                            "30 à 39 ans"="30",
                            "40 à 49 ans"="40",
                            "50 à 59 ans "="50",
                            "60 ans ou +"="60",)


##verification

table(chomage2020$AGE5)
table(chomage2020$AGE)

## Actif ou non actif (ACTIF)

mode(chomage2020$ACTIF)
chomage2020$tauxdechômage<-as.character(chomage2020$ACTIF)
chomage2020$tauxdechômage<-fct_recode(chomage2020$tauxdechômage,
                                      "Actif au sens du BIT/Actif"="1",
                                      "Actif au sens du BIT/inactif"="2",
)

##verification
table(chomage2020$ACTIF)
table(chomage2020$tauxdechômage)


## raison changement d'emploi (CREACCP)
mode(chomage2020$CREACCP)
chomage2020$raisonchangemploi<-as.character(chomage2020$CREACCP)
chomage2020$raisonchangemploi=fct_explicit_na(chomage2020$raisonchangemploi, "null")
chomage2020$raisonchangemploi<-fct_recode(chomage2020$raisonchangemploi,
                                          "Sans objet (SOUA distinct de 1 et SOUB distinct de 1) ou non renseigné"="null",
                                          "Risque de perdre ou va perdre son emploi actuel (y compris fin de contrats courts)"="1",
                                          "Désire un emploi plus intéressant"="2",
                                          "Veut un emploi plus stable (CDI)"="3",
                                          "Veut travailler plus d'heures"="4",
                                          "Désire un travail avec un rythme horaire plus adapté ou plus modulable"="5",
                                          "Désire des conditions de travail moins pénibles ou plus adaptées à sa santé"="6",
                                          "Désire augmenter ses revenus"="7",
                                          "Désire diminuer son temps de transport"="8",
                                          "Doit ou veut déménager"="9",
                                          "Veut s'installer à son compte"="10",
                                          "Veut changer de métier ou de secteur"="11",
                                          "Trouve l'ambiance de travail mauvaise, les relations de travail conflictuelles"="12",
                                          "Autre raison"="13",
)

##vérification 

table(chomage2020$CREACCP)
table(chomage2020$raisonchangemploi)


##II-Analyse de la base----
## tableau CSP et statut d 'activité 

tmp<-table(chomage2020$CSP,chomage2020$statutACT)
tmprop<-lprop(tmp)
chisq.test(tmp) #p-value
#write.table(tmprop,file = "excel_provisoire.xls",sep = "\t")

## tableau Age et taux de chômage

tmp<-table(chomage2020$AGE,chomage2020$tauxdechômage)
tmprop<-lprop(tmp)
chisq.test(tmp) #p-value
#write.table(tmprop,file = "excel_tableau âge.xls",sep = "\t")

## tableau taux de chômage et statut d'activité 

tmp<-table(chomage2020$tauxdechômage,chomage2020$statutACT)
tmprop<-lprop(tmp)
chisq.test(tmp) #p-value
#write.table(tmprop,file = "excel_statutact-chômage.xls",sep = "\t")

## tableau catégorie socio-pro et raison de changement d 'emploi 
tmp<-table(chomage2020$CSP,chomage2020$raisonchangemploi)
tmprop<-lprop(tmp)
chisq.test(tmp) #p-value
#write.table(tmprop,file = "excel_tableauchangement d'emploi et csp .xls",sep = "\t")

##tableau tri-varié age , csp,, taux de chômage  : taux de chômage en fonction de l'âge et de la csp 
freq(chomage2020$AGE)
freq(chomage2020$CSP)
freq(chomage2020$tauxdechômage)

##tir croisé 
lprop(table(chomage2020$AGE5,chomage2020$tauxdechômage))
lprop(table(chomage2020$CSP,chomage2020$tauxdechômage))

chisq.test(table(chomage2020$AGE5,chomage2020$tauxdechômage))
chisq.test(table(chomage2020$CSP,chomage2020$tauxdechômage))
#write.table(tmprop,file = "excel_tableau taux de chomage age et csp .xls",sep = "\t")


##tableau tri-varié : sexe , csp et taux de chomâge : taux de chômage en fonction du sexe et de la csp 

freq(chomage2020$SEXE)
freq(chomage2020$CSP)
freq(chomage2020$tauxdechômage)

## tir croisé 
lprop(table(chomage2020$SEXE,chomage2020$tauxdechômage))
lprop(table(chomage2020$CSP,chomage2020$tauxdechômage))

chisq.test(table(chomage2020$SEXE,chomage2020$tauxdechômage))
chisq.test(table(chomage2020$CSP,chomage2020$tauxdechômage))
#write.table(tmprop,file = "excel_tableautaux de chomage sexe et csp .xls",sep = "\t")

##tableau tri-varié : sexe , age et taux de chomâge : taux de chômage en fonction du sexe et de l 'âge 

freq(chomage2020$SEXE)
freq(chomage2020$AGE5)
freq(chomage2020$tauxdechômage)

## tir croisé 
lprop(table(chomage2020$SEXE,chomage2020$tauxdechômage))
lprop(table(chomage2020$AGE5,chomage2020$tauxdechômage))

chisq.test(table(chomage2020$SEXE,chomage2020$tauxdechômage))
chisq.test(table(chomage2020$AGE5,chomage2020$tauxdechômage))
#write.table(tmprop,file = "excel_tableautaux de chomage sexe et age .xls",sep = "\t")


## tableau tri varié : statut d 'act,, taux de chômage et csp 
freq(chomage2020$CSP)
freq(chomage2020$statutACT)
freq(chomage2020$tauxdechômage)

## tir croisé 
lprop(table(chomage2020$statutACT,chomage2020$tauxdechômage))
lprop(table(chomage2020$CSP,chomage2020$tauxdechômage))

chisq.test(table(chomage2020$statutACT,chomage2020$tauxdechômage))
chisq.test(table(chomage2020$CSP,chomage2020$tauxdechômage))
#write.table(tmprop,file = "excel_tableautaux de chomage csp et statut act  .xls",sep = "\t")



