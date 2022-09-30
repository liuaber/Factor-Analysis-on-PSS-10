setwd("~/Desktop/covid19_survey")
covid = read.csv("covid19_data.csv")
sum(is.na(covid$Q1)) 
# There are 2 null responses for Q1, which indicates these 2 "participants" did 
# not click on anaything to the informed consent form at the beginning. Hence, it's reasonabale to exclude them as part of this study. 
covid = covid[-c(which(is.na(covid$Q1)==TRUE)),] 
dim(covid) #Check if the null responses have been removed. 
sum(covid$Q1 == 6) 
#6 indicates that these "participants"" answered no for the informed consent form, 
#Thus, we have 5 people did not agree with the information consent form. They
#should be excluded from the participants of this study. 
covid = covid[-c(which(covid$Q1 == 6)),] 
table(covid$Q1.1)
table(covid$Q4)
covid_mdi = covid[-c(which(is.na(covid$MDI_1)==TRUE|is.na(covid$MDI_2)==TRUE|is.na(covid$MDI_3)==TRUE|
                             is.na(covid$MDI_4)==TRUE|is.na(covid$MDI_5)==TRUE|is.na(covid$MDI_6)==TRUE|is.na(covid$MDI_7)==TRUE|is.na(covid$MDI_8)==TRUE|is.na(covid$MDI_9)==TRUE|is.na(covid$MDI_10)==TRUE|is.na(covid$MDI_11)==TRUE|is.na(covid$MDI_12)==TRUE)),]
dim(covid_mdi)###1850 responses left
attach(covid_mdi)
### The true coding is 5 to 0, while the default coding is 1 to 6. 
covid_mdi$MDI_total = (5*8-(MDI_1+MDI_2+MDI_3+ MDI_4+MDI_5+MDI_6 +MDI_7+MDI_10-8))+(pmax(6-MDI_8,6-MDI_9))+
  (pmax(6-MDI_11,6-MDI_12)) ##Since the actual labeling is 5-0 while our labeling is 1-6. 
#We calculate the score in this way since we didn't intend to use it as a diagnostic tool so instead of diagnosis, this is used as a rating scale like Hamilton scale. The threshold: 21-25: mild; 26-30: moderate; 31 or higher: severe
detach(covid_mdi)
mean(covid_mdi$MDI_total) #The mean value is in fact very high for all valid participants.
hist(covid_mdi$MDI_total)
dep_class = c(sum(MDI_total<=20),sum(MDI_total>20 & MDI_total<=25),sum(MDI_total> 25 & MDI_total<= 30),sum(MDI_total >30))
pie(dep_class,labels = c("No Depression \n (48.000%)","Mild Depression \n (10.162%)","Moderate Depression \n (10.595%) ","Severe Depression \n (31.243%) "),cex = 1.1,col = c('azure','darkorange','firebrick',"blue"),radius = 1)
barplot(dep_class,names.arg = c("No Depression","Mild","Moderate","Severe"),main = "Depression")
covid_mdi_gender = covid_mdi[-c(which(is.na(covid_mdi$Q1.1==TRUE))),]
### Now organize the data frame to test the difference in MDI score among different gender group
data_gender_mdi = data.frame(
  Sex = as.factor(covid_mdi_gender$Q1.1),
  MDI_Score = covid_mdi_gender$MDI_total
)
table(data_gender_mdi$Sex) #Notice this happens for the whole survey, our results are biased due to the fact that more than 60% of the participants are women

kruskal.test(MDI_Score~Sex,data = data_gender_mdi)
mean(subset(data_gender_mdi,Sex==1)$MDI_Score)
mean(subset(data_gender_mdi,Sex==2)$MDI_Score)
mean(subset(data_gender_mdi,Sex==3)$MDI_Score) #Present this part and explain it
sum(subset(data_gender_mdi,Sex==2)$MDI_Score>30)/sum(table(data_gender_mdi$Sex)) #20% of female were facing severe depression，this might be due to that female tend to be more empathetic than man in general during a pandemic. 
sum(subset(data_gender_mdi,Sex==1)$MDI_Score>30)/sum(table(data_gender_mdi$Sex))
sum(subset(data_gender_mdi,Sex==3)$MDI_Score>30)/sum(table(data_gender_mdi$Sex))
covid_mdi_race = covid_mdi[-c(which(is.na(covid_mdi$Q4==TRUE))),] 

data_race_mdi = data.frame(
  Ethnicity = as.factor(covid_mdi_race$Q4),
  MDI_Score = covid_mdi_race$MDI_total
)
kruskal.test(MDI_Score~Ethnicity,data = data_race_mdi)

#This is rather strange since the results since it does not suggest the Asians are facing more severe depression compared to other ethnicities
sum(subset(data_race_mdi,Ethnicity == 1)$MDI_Score>30)/sum(data_race_mdi$Ethnicity==1)
sum(subset(data_race_mdi,Ethnicity == 2)$MDI_Score>30)/sum(data_race_mdi$Ethnicity==2)
sum(subset(data_race_mdi,Ethnicity == 3)$MDI_Score>30)/sum(data_race_mdi$Ethnicity==3)
sum(subset(data_race_mdi,Ethnicity == 4)$MDI_Score>30)/sum(data_race_mdi$Ethnicity==4)
sum(subset(data_race_mdi,Ethnicity == 5)$MDI_Score>30)/sum(data_race_mdi$Ethnicity==5)
sum(subset(data_race_mdi,Ethnicity == 6)$MDI_Score>30)/sum(data_race_mdi$Ethnicity==6)
sum(subset(data_race_mdi,Ethnicity == 7)$MDI_Score>30)/sum(data_race_mdi$Ethnicity==7)
sum(subset(data_race_mdi,Ethnicity == 8)$MDI_Score>30)/sum(data_race_mdi$Ethnicity==8)
sum(subset(data_race_mdi,Ethnicity == 9)$MDI_Score>30)/sum(data_race_mdi$Ethnicity==9)


#Racism Question
covid_mdi_racism = covid_mdi[-c(which(is.na(covid_mdi$Q22==TRUE))),] #Delete the rows, 1758 responses left.
data_racism_mdi = data.frame(
  racism = as.factor(covid_mdi_racism$Q22),
  MDI_Score = covid_mdi_racism$MDI_total
)
wilcox.test(MDI_Score~racism,data = data_racism_mdi)
###
#### Q11 employment
covid_mdi_emp = covid_mdi[-c(which(is.na(covid_mdi$Q11==TRUE))),] #Delete the rows, 1758 responses left.
data_emp_mdi = data.frame(
  emp = as.factor(covid_mdi_emp$Q11),
  mdi_Score = covid_mdi_emp$MDI_total
)
kruskal.test(mdi_Score~emp,data = data_emp_mdi) #reject the null
#### Q25 Family member tested positive
covid_mdi_fam = covid_mdi[-c(which(is.na(covid_mdi$Q25==TRUE))),] #Delete the rows, 1758 responses left.
data_fam_mdi = data.frame(
  fam = as.factor(covid_mdi_fam$Q25),
  mdi_Score = covid_mdi_fam$MDI_total
)
kruskal.test(mdi_Score~fam,data = data_fam_mdi) #reject the null
#### Q26A tested positive
covid_mdi_pos = covid_mdi[-c(which(is.na(covid_mdi$Q26A==TRUE))),] #Delete the rows, 1758 responses left.
data_pos_mdi = data.frame(
  pos = as.factor(covid_mdi_pos$Q25),
  mdi_Score = covid_mdi_pos$MDI_total
)
kruskal.test(mdi_Score~pos,data = data_pos_mdi) #reject the null

### Q24 financial
sum(is.na(covid_mdi$Q24==TRUE)) #There're 178 students that didn't respond to this question. 
covid_mdi_fin = covid_mdi[-c(which(is.na(covid_mdi$Q24==TRUE))),] #Delete the rows, 1758 responses left.
data_fin_mdi = data.frame(
  fin = as.factor(covid_mdi_fin$Q24),
  mdi_Score = covid_mdi_fin$MDI_total
)
kruskal.test(mdi_Score~fin,data = data_fin_mdi)

###Q6
##### Classification of student status
### Now we want to investigate if there is a difference in mdi score between undergrads, masters and PhD students
###### Classification (Q6 coding:6: Master; 7: PhD; 8: Not seeking for degree; 9: Other)
sum(is.na(covid_mdi$Q6==TRUE)) #There're 156 students that didn't respond to this question. 
covid_mdi_class = covid_mdi[-c(which(is.na(covid_mdi$Q6==TRUE))),] #Delete the rows, 1755 responses left. 
### Now organize the data frame to test the difference in mdi score among different classifications.
data_class_mdi = data.frame(
  Classification = as.factor(covid_mdi_class$Q6),
  mdi_Score = covid_mdi_class$MDI_total
)


kruskal.test(mdi_Score~Classification,data = data_class_mdi)#Do not reject the null 


###Age
covid_mdi_age = covid_mdi[-c(which(is.na(covid_mdi$Q5==TRUE))),] #Delete the rows, 1755 responses left. 
### Now organize the data frame to test the difference in pss score among different classifications.
data_age_mdi = data.frame(
  age = as.factor(covid_mdi_age$Q5),
  mdi_Score = covid_mdi_age$MDI_total
)
kruskal.test(mdi_Score~age,data = data_age_mdi)#Do not reject the null 