state_model = model_complete_dat%>%
  mutate(police_frac_total_expend = police_expenditure/total_expenditure,
         Race = factor(Race,levels = c("W","H","B","A","N","O")),
         Shootings_flee = factor(Shootings_flee,levels = c("Not fleeing","Car","Foot","Other")),
         density_p_mi = (density_p_mi2_2020+density_p_mi2_2020)/2,
         pop_diversity_index = 1-((White_Percent/100)^2+(Black_Percent/100)^2+
                                    (Hispanic_Percent/100)^2+(Asian_Percent/100)^2+
                                    (Other_Percent/100)^2))%>%
  group_by(Year,State)%>%
  summarise(Count = mean(ifelse(Year==2020,2*n,n)),
            Age= median(Age,na.rm = T),
            Population = round(mean(Population)),
            Black_pop_perc = round(mean(Black_Percent),2),
            Firearm_10k = round(mean(Firearm_per_10k,na.rm = T),2),
            Crime_10k = round(mean(Crime_Index_10k,na.rm = T),2),
            Pol_frac_total = round(mean(police_frac_total_expend,na.rm = T),5),
            pop_density = round(mean((density_p_mi2_2017+density_p_mi2_2020)/2),2),
            pop_diversity = round(mean(1-((White_Percent/100)^2+(Black_Percent/100)^2+
                                            (Hispanic_Percent/100)^2+(Asian_Percent/100)^2+
                                            (Other_Percent/100)^2)),2))

#Offical action
act_1 = model_complete_dat%>%
  group_by(Year,State,official_action)%>%
  tally()%>%
  na.omit()
act_2 = act_1%>%
  group_by(Year,State)%>%
  summarise(perc=round(n*100/sum(n),2))
act_1=cbind(act_1[,-4],"Percentage"=act_2$perc)
act_final = spread(act_1,official_action,Percentage)
names(act_final)[3:7] = paste(c("Official_action_Charged","Official_action_Justified","Official_action_Ongoing","Official_action_Pending","Official_action_Unreported"))

state_model = left_join(state_model,act_final,by=c("Year","State"))


#Urbanization_Index
state_model = left_join(state_model,urban_index,by=c("Year","State"))

#Male population percentage
state_model = left_join(state_model,male_pop_perc,by=c("Year","State"))

#Popuation median age
state_model = left_join(state_model,pop_median_age,by=c("Year","State"))
write.csv(state_model,'state_level_model.csv',row.names = F)