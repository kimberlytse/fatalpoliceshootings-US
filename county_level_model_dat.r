#County Level
cdat1 = shootings_killings%>%
  group_by(Year,State,County)%>%
  summarise(Count=n())

shootings_killings = left_join(shootings_killings,cdat1,by=c("Year","State","County"))
  
county_dat1 = left_join(shootings_killings,policedep_count_complete_15_16,by=c("Year","State","City","County"))

county_dat2 = left_join(county_dat1,offense_county_16_18,by=c("Year","State","County"))

county_dat3 = left_join(county_dat2,Pop_diversity_and_black_perc_pop_18,by=c("State","County"))

county_dat4 = left_join(county_dat3,Urbanization_PopDensity_2010,by=c("State","County"))

county_dat5 = left_join(county_dat4,Firearm_fatalities_17_20,by=c("Year","State","County"))

county_dat6 = left_join(county_dat5,county_male_percentages_15_19,by=c("Year","State","County"))

county_dat7 = left_join(county_dat6,AgeGroup_20_44_2015_2019,by=c("Year","State","County"))

county_dat8 = left_join(county_dat7,LandArea,by=c("State","County"))

county_complete = county_dat8%>%
  mutate(Crime_Index_10k = Total_Crime*10000/Population,
         Pop_Density = Population/LandArea)

write.csv(county_complete,'county_complete_dat.csv',row.names = F)

officer_mean = county_complete%>%
  group_by(State,County)%>%
  summarise(officer_mean = round(mean(officers_per_10k_pop,na.rm = T),2))

county_complete = left_join(county_complete,officer_mean,by=c("State","County"))

county_complete = county_complete%>%
  mutate(officers_per_10k_pop = ifelse(Year%in%c(2015,2016),
                                       officers_per_10k_pop,officer_mean))
crime_mean = county_complete%>%
  group_by(State,County)%>%
  summarise(crime_mean = round(mean(Crime_Index_10k,na.rm = T),2))

county_complete = left_join(county_complete,crime_mean,by=c("State","County"))

county_complete = county_complete%>%
  mutate(Crime_Index_10k = ifelse(Year%in%c(2016,2017,2018),
                                  Crime_Index_10k,crime_mean))

fire_state = Firearm_fatalities_17_20%>%
  group_by(Year,State)%>%
  summarise(fire_min_state=min(Numer_of_Firearm_Fatalities,na.rm = T))

#write.csv(fire_state,'firearm_fatality_mean_state.csv',row.names =F)

Firearm_fatalities_17_20 = left_join(Firearm_fatalities_17_20,fire_state,by=c("Year","State"))

Firearm_fatalities_17_20 = Firearm_fatalities_17_20%>%
   mutate(Numer_of_Firearm_Fatalities = round(ifelse(Numer_of_Firearm_Fatalities==0,
                                               fire_min_state,Numer_of_Firearm_Fatalities)))

firearm_mean = Firearm_fatalities_17_20%>%
   group_by(State,County)%>%
   summarise(firearm_mean = round(mean(Numer_of_Firearm_Fatalities,na.rm = T),2))

county_complete = left_join(county_complete,firearm_mean,by=c("State","County"))

county_complete = county_complete%>%
   mutate(Numer_of_Firearm_Fatalities = ifelse(Year%in%c(2017,2018,2019,2020),
                                               Numer_of_Firearm_Fatalities,
                                               firearm_mean))

county_level = county_complete%>%
  group_by(Year,State,County)%>%
  summarise(Count = round(mean(Count)),
            officer_10k = round(mean(officers_per_10k_pop,na.rm = T),2),
            crime_10k = round(mean(Crime_Index_10k,na.rm = T),2),
            pop_diversity = round(mean(Population_Diversity_Index,na.rm = T),2),
            black_pop_perc = round(mean(Black_pop_perc,na.rm = T),2),
            urbanization_index = round(mean(Urban_pop_perc,na.rm = T),2),
            firearm_fatalities_100k = round(mean(Numer_of_Firearm_Fatalities,na.rm = T),2),
            population = round(mean(Population,na.rm = T),2),
            pop_density = round(mean(Pop_Density,na.rm = T),2),
            male_perc = round(mean(male_percentage,na.rm = T),2),
            age_20_45_perc = round(mean(AgeGroup_20_45_percentage,na.rm = T),2))

county_level = county_level%>%
  mutate(Count = ifelse(Year==2020,2*Count,Count))

act_1 = shootings_killings%>%
  mutate(armed_simplified = ifelse(Shootings_armed%in%c("air pistol","Airsoft pistol",
                                                        "BB gun","BB gun and vehicle",
                                                        "bean-bag gun","gun","gun and car",
                                                        "gun and knife","gun and sword",
                                                        "gun and vehicle", "guns and explosives",
                                                        "hatchet and gun", "machete and gun",
                                                        "nail gun", "pellet gun"),1,0),
         official_action = ifelse(`Official disposition of death (justified or other)`%in%
                                    c("Charged", "Charged with 2nd degree murder",
                                      "Charged with a crime", "Charged with felony murder",
                                      "Charged with manslaughter", "Charged with manslaughter, acquitted",
                                      "Charged with murder", "Charged with murder, Acquitted",
                                      "Charged with negligent homicide, Acquitted",
                                      "Charged with reckless homicide", "Charged, Acquitted",
                                      "Charged, Convicted of 2nd degree manslaughter, Sentenced to 4 years",
                                      "Charged, convicted of manslaughter", "Charged, Convicted of manslaughter, Sentenced to 2.5 years in prison",
                                      "Charged, Convicted, Sentenced to 5 years in prison",
                                      "Charged, Mistrial declared", "Charged, Mistrial declared, Pled Guilty for Violating Scott's Civil Rights",
                                      "Criminal", "Unjustified, Officer fired"),"Charged",
                                  ifelse(`Official disposition of death (justified or other)`%in%c("Justified", "Justified by Attorney General",
                                                                                                   "Justified by County Attorney","Justified by County Prosecutor",
                                                                                                   "Justified by District Attorney", "Justified by Fifth Judicial Circuit Solicitor",
                                                                                                   "Justified by outside agency", "Justified by Prosecuting Attorney",
                                                                                                   "Justified by Prosecutor","Justified by Prosecutor's Office",
                                                                                                   "Justified by State's Attorney","Justified by State Attorney",
                                                                                                   "Justified; New York State Police investigation; Schwalm's brother offered condolences to the deputy because of his brother's illness."),"Justified", ifelse(`Official disposition of death (justified or other)`%in%c("Ongoing investigation", "Under investigation"),"Ongoing",ifelse(`Official disposition of death (justified or other)`%in%c("Grand jury/No bill or Cleared","No Known Charges","Unknown","Unreported"),"Unreported","Pending")))),
         Victim_Criminal_Charges_Simplified = ifelse(`Criminal Charges?`%in%c("Charged with a crime","Charged with manslaughter","Charged, Acquitted","Charged, Convicted, Sentenced to 2.5 years in prison","Charged, Convicted, Sentenced to 4 years","Charged, Convicted, Sentenced to 40 years in prison","Charged, Convicted, Sentenced to 5 years in prison","Charged, Mistrial","Charged, Mistrial, Plead Guilty to Civil Rights Charges"),"Charged","No Charges"))%>%
  group_by(Year,State,County,official_action)%>%
  tally()%>%
  na.omit()
act_2 = act_1%>%
  group_by(Year,State,County)%>%
  summarise(perc=round(n*100/sum(n),2))
act_1=cbind(act_1[,-5],"Percentage"=act_2$perc)
act_final = spread(act_1,official_action,Percentage)
names(act_final)[4:8] = paste(c("Official_action_Charged","Official_action_Justified","Official_action_Ongoing","Official_action_Pending","Official_action_Unreported"))

county_level = left_join(county_level,act_final,by=c("Year","State","County"))

write.csv(county_level,'county_level_model.csv',row.names = F) 