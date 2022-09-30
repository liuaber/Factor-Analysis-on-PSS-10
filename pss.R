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
covid_pss = covid[-c(which(is.na(covid$PSS_1)==TRUE|is.na(covid$PSS_2)==TRUE|is.na(covid$PSS_3)==TRUE|
                             is.na(covid$PSS_4)==TRUE|is.na(covid$PSS_5)==TRUE|is.na(covid$PSS_6)==TRUE|is.na(covid$PSS_7)==TRUE|is.na(covid$PSS_8)==TRUE|is.na(covid$PSS_9)==TRUE|is.na(covid$PSS_10)==TRUE)),]
dim(covid_pss) #345 responses was deleted here. 1738 responses left for pss scale calculation


##### We shall only include complete responses for a scale. 
##### Adding a column for total score of PSS, notice in the qualtrics the responses were labelled as 1-5.
##### Instead the actual range for each response is 0-4. 
##### 1,2,3,6,9,10 are negative questions, so sum them first and -6 for the reasons stated above. 
##### For 4,5,7,8, the original range in the dataset is 1-5. Now we reverse it by using 5 minus the item. 
attach(covid_pss)
sex = covid_pss$Q1.1
race = covid_pss$Q4
fin = covid_pss$Q24
class = covid_pss$Q6
age = covid_pss$Q5
employ = covid_pss$Q11
marital = covid_pss$Q3
covid_pss$PSS_total = (PSS_1+PSS_2+PSS_3+ PSS_6+PSS_9+PSS_10 -6)+(5-PSS_4)+(5-PSS_5)+(5-PSS_7)+(5-PSS_8) 
detach(covid_pss)
covid_pss_biv = data.frame(class = class,
                               sex = sex,
                               race = race,
                               fin = fin,
                               age = age,
                               employ = employ,
                               marital = marital,
                               #child = as.factor(child),
                               PSS_Score = covid_pss$PSS_total
                        
)
covid_pss_biv  = na.omit(covid_pss_biv) #1699 left
kruskal.test(covid_pss_biv$PSS_Score~covid_pss_biv$sex)
kruskal.test(covid_pss_biv$PSS_Score~covid_pss_biv$class)
kruskal.test(covid_pss_biv$PSS_Score~covid_pss_biv$race)
kruskal.test(covid_pss_biv$PSS_Score~covid_pss_biv$fin)
kruskal.test(covid_pss_biv$PSS_Score~covid_pss_biv$employ)
kruskal.test(covid_pss_biv$PSS_Score~covid_pss_biv$marital)
kruskal.test(covid_pss_biv$PSS_Score~covid_pss_biv$age)


mean(covid_pss$PSS_total) #The mean value is in fact very high for all valid participants.
hist(covid_pss$PSS_total)
#### Suppose we want to use the reference from New Hampshire Dept. of Administrative Service
### 0-13: low stress
### 14-26: moderate stress
### 27-40: high stress
attach(covid_pss)

stress_class = c(sum(PSS_total<=13),sum(PSS_total>13 & PSS_total<27),sum(PSS_total>=27))
#stress_class_percent = c(sum(PSS_total<=13),sum(PSS_total>13 & PSS_total<27),sum(PSS_total>=27))/length(PSS_total)
pie(stress_class,labels = c("Low Perceived Stress \n (16.05%)","Moderate Perceived Stress \n (56.67%)","High Perceived Stress \n (27.27%) "),cex = 1.1,col = c('azure','darkorange','firebrick'),radius = 1)
pie3D(stress_class,main ="The Classification of student participants based on PSS",labels = c("Low Perceived Stress \n (16.05%)","Moderate Perceived Stress \n (56.67%)","High Perceived Stress \n (27.27%) "),explode = 0.05,labelcex = 0.8,col = c('azure','darkorange','firebrick'),theta = pi/5) #I think, sequential color schemes are proper
covid_pss_gender = covid_pss[-c(which(is.na(covid_pss$Q1.1==TRUE))),] #1730 responses are left.
### Now organize the data frame to test the difference in pss score among different gender group
data_gender_pss = data.frame(
  Sex = as.factor(covid_pss_gender$Q1.1),
  PSS_Score = covid_pss_gender$PSS_total
)
table(data_gender_pss$Sex)
sum(subset(data_gender_pss,Sex==1)$PSS_Score<=13)
sum(subset(data_gender_pss,Sex==1)$PSS_Score<27 & subset(data_gender_pss,Sex==1)$PSS_Score>13)
sum(subset(data_gender_pss,Sex==1)$PSS_Score>=27)
sum(subset(data_gender_pss,Sex==2)$PSS_Score<=13)
sum(subset(data_gender_pss,Sex==2)$PSS_Score<27 & subset(data_gender_pss,Sex==2)$PSS_Score>13)
sum(subset(data_gender_pss,Sex==2)$PSS_Score>=27)
sum(subset(data_gender_pss,Sex==3)$PSS_Score<=13)
sum(subset(data_gender_pss,Sex==3)$PSS_Score<27 & subset(data_gender_pss,Sex==3)$PSS_Score>13)
sum(subset(data_gender_pss,Sex==3)$PSS_Score>=27)
library(ggplot2)
ggplot(data_gender_pss)+aes(x = Sex,y = PSS_Score)+ geom_boxplot(fill="grey")
## We shall see a big difference between the distribution of Male, Female and "other", then check the histogram of each subgroup
par(mfrow = c(3,1))
hist(subset(data_gender_pss,Sex==1)$PSS_Score, main = "pss Score for Male Students")
hist(subset(data_gender_pss,Sex==2)$PSS_Score, main = "pss Score for Female Students")
hist(subset(data_gender_pss,Sex==3)$PSS_Score, main = "pss Score for Other Students") ### None of the above follows a normal distribution, hence we attempt to do Shapiro-Wilks test to check the normality
shapiro.test(subset(data_gender_pss,Sex==1)$PSS_Score)
shapiro.test(subset(data_gender_pss,Sex==2)$PSS_Score)
shapiro.test(subset(data_gender_pss,Sex==3)$PSS_Score)
###We shall reject the normality assumptions for all of these, we should go for Kruskal-Wallis test (Wilcox test is for 2 groups comparison)
kruskal.test(PSS_Score~Sex,data = data_gender_pss)
###### Ethnicity
sum(is.na(covid_pss$Q4==TRUE)) #There're 7 students that didn't respond to this question. 
covid_pss_race = covid_pss[-c(which(is.na(covid_pss$Q4==TRUE))),] #Delete the rows, 1731 responses left.
data_race_pss = data.frame(
  Ethnicity = as.factor(covid_pss_race$Q4),
  PSS_Score = covid_pss_race$PSS_total
)
### This part provides the mean PSS value by groups, no difference.
aggregate(data_race_pss[, 2], list(data_race_pss$Ethnicity), mean)
### Boxplot comparison
ggplot(data_race_pss)+aes(x = Ethnicity,y = PSS_Score)+ geom_boxplot(fill="grey")
### We see that group 5 and group 6 generally has lower 

kruskal.test(PSS_Score~Ethnicity,data = data_race_pss)#Do not reject the null
covid_pss_racism = covid_pss[-c(which(is.na(covid_pss$Q22==TRUE))),] #Delete the rows, 1758 responses left.
data_racism_pss = data.frame(
  racism = as.factor(covid_pss_racism$Q22),
  PSS_Score = covid_pss_racism$PSS_total
)
table(data_racism_pss$racism)
sum(subset(data_racism_pss,racism==1)$PSS_Score<=13)
sum(subset(data_racism_pss,racism==1)$PSS_Score<27 & subset(data_racism_pss,racism==1)$PSS_Score>13)
sum(subset(data_racism_pss,racism==1)$PSS_Score>=27)
sum(subset(data_racism_pss,racism==2)$PSS_Score<=13)
sum(subset(data_racism_pss,racism==2)$PSS_Score<27 & subset(data_racism_pss,racism==2)$PSS_Score>13)
sum(subset(data_racism_pss,racism==2)$PSS_Score>=27)
wilcox.test(PSS_Score~racism,data = data_racism_pss)


#### Q11 employment
covid_pss_emp = covid_pss[-c(which(is.na(covid_pss$Q11==TRUE))),] #Delete the rows, 1758 responses left.
data_emp_pss = data.frame(
  emp = as.factor(covid_pss_emp$Q11),
  pss_Score = covid_pss_emp$PSS_total
)
kruskal.test(pss_Score~emp,data = data_emp_pss) #reject the null
#### Q25 Family member tested positive
covid_pss_fam = covid_pss[-c(which(is.na(covid_pss$Q25==TRUE))),] #Delete the rows, 1758 responses left.
data_fam_pss = data.frame(
  fam = as.factor(covid_pss_fam$Q25),
  pss_Score = covid_pss_fam$PSS_total
)
kruskal.test(pss_Score~fam,data = data_fam_pss) #reject the null
#### Q26A tested positive
covid_pss_pos = covid_pss[-c(which(is.na(covid_pss$Q26A==TRUE))),] #Delete the rows, 1758 responses left.
data_pos_pss = data.frame(
  pos = as.factor(covid_pss_pos$Q26A),
  pss_Score = covid_pss_pos$PSS_total
)
kruskal.test(pss_Score~pos,data = data_pos_pss) #reject the null

### Q24 financial
sum(is.na(covid_pss$Q24==TRUE)) #There're 178 students that didn't respond to this question. 
covid_pss_fin = covid_pss[-c(which(is.na(covid_pss$Q24==TRUE))),] #Delete the rows, 1758 responses left.
data_fin_pss = data.frame(
  fin = as.factor(covid_pss_fin$Q24),
  pss_Score = covid_pss_fin$PSS_total
)
kruskal.test(pss_Score~fin,data = data_fin_pss)

###Q6
##### Classification of student status
### Now we want to investigate if there is a difference in pss score between undergrads, masters and PhD students
###### Classification (Q6 coding:6: Master; 7: PhD; 8: Not seeking for degree; 9: Other)
sum(is.na(covid_pss$Q6==TRUE)) #There're 156 students that didn't respond to this question. 
covid_pss_class = covid_pss[-c(which(is.na(covid_pss$Q6==TRUE))),] #Delete the rows, 1755 responses left. 
### Now organize the data frame to test the difference in pss score among different classifications.
data_class_pss = data.frame(
  Classification = as.factor(covid_pss_class$Q6),
  pss_Score = covid_pss_class$PSS_total
)
kruskal.test(pss_Score~Classification,data = data_class_pss)#Do not reject the null 
###Age
covid_pss_age = covid_pss[-c(which(is.na(covid_pss$Q5==TRUE))),] #Delete the rows, 1755 responses left. 
### Now organize the data frame to test the difference in pss score among different classifications.
data_age_pss = data.frame(
  age = as.factor(covid_pss_age$Q5),
  pss_Score = covid_pss_age$PSS_total
)
kruskal.test(pss_Score~age,data = data_age_pss)#Do not reject the null 

### kruskal test
covid_pss_biv = data.frame(class = as.factor(class),
                           sex = as.factor(sex),
                           race = as.factor(race),
                           fin = as.factor(fin),
                           age = as.factor(age),
                           employ = as.factor(employ),
                           marital = as.factor(marital),
                           #child = as.factor(child),
                           PSS_Score = covid_pss$PSS_total
                           
)
covid_pss_biv  = na.omit(covid_pss_biv) #1699 left
kruskal.test(covid_pss_biv$PSS_Score~covid_pss_biv$age)
kruskal.test(covid_pss_biv$PSS_Score~covid_pss_biv$sex)
kruskal.test(covid_pss_biv$PSS_Score~covid_pss_biv$marital)
kruskal.test(covid_pss_biv$PSS_Score~covid_pss_biv$race)
kruskal.test(covid_pss_biv$PSS_Score~covid_pss_biv$class)
kruskal.test(covid_pss_biv$PSS_Score~covid_pss_biv$employ)
kruskal.test(covid_pss_biv$PSS_Score~covid_pss_biv$fin)
###age
table(covid_pss_biv$age)
table(covid_pss_biv$sex)
table(covid_pss_biv$marital)
table(covid_pss_biv$race)
table(covid_pss_biv$class)
table(covid_pss_biv$employ)

#age
sum(subset(covid_pss_biv,age==1)$PSS_Score<=13)
sum(subset(covid_pss_biv,age==1)$PSS_Score<27 & subset(covid_pss_biv,age==1)$PSS_Score>13)
sum(subset(covid_pss_biv,age==1)$PSS_Score>=27)
sum(subset(covid_pss_biv,age==2)$PSS_Score<=13)
sum(subset(covid_pss_biv,age==2)$PSS_Score<27 & subset(covid_pss_biv,age==2)$PSS_Score>13)
sum(subset(covid_pss_biv,age==2)$PSS_Score>=27)
sum(subset(covid_pss_biv,age==3)$PSS_Score<=13)
sum(subset(covid_pss_biv,age==3)$PSS_Score<27 & subset(covid_pss_biv,age==3)$PSS_Score>13)
sum(subset(covid_pss_biv,age==3)$PSS_Score>=27)
sum(subset(covid_pss_biv,age==4)$PSS_Score<=13)
sum(subset(covid_pss_biv,age==4)$PSS_Score<27 & subset(covid_pss_biv,age==4)$PSS_Score>13)
sum(subset(covid_pss_biv,age==4)$PSS_Score>=27)
sum(subset(covid_pss_biv,age==5)$PSS_Score<=13)
sum(subset(covid_pss_biv,age==5)$PSS_Score<27 & subset(covid_pss_biv,age==5)$PSS_Score>13)
sum(subset(covid_pss_biv,age==5)$PSS_Score>=27)
sum(subset(covid_pss_biv,age==6)$PSS_Score<=13)
sum(subset(covid_pss_biv,age==6)$PSS_Score<27 & subset(covid_pss_biv,age==6)$PSS_Score>13)
sum(subset(covid_pss_biv,age==6)$PSS_Score>=27)

#sex
sum(subset(covid_pss_biv,sex==1)$PSS_Score<=13)
sum(subset(covid_pss_biv,sex==1)$PSS_Score<27 & subset(covid_pss_biv,sex==1)$PSS_Score>13)
sum(subset(covid_pss_biv,sex==1)$PSS_Score>=27)
sum(subset(covid_pss_biv,sex==2)$PSS_Score<=13)
sum(subset(covid_pss_biv,sex==2)$PSS_Score<27 & subset(covid_pss_biv,sex==2)$PSS_Score>13)
sum(subset(covid_pss_biv,sex==2)$PSS_Score>=27)
sum(subset(covid_pss_biv,sex==3)$PSS_Score<=13)
sum(subset(covid_pss_biv,sex==3)$PSS_Score<27 & subset(covid_pss_biv,sex==3)$PSS_Score>13)
sum(subset(covid_pss_biv,sex==3)$PSS_Score>=27)

#marital
sum(subset(covid_pss_biv,marital==1)$PSS_Score<=13)
sum(subset(covid_pss_biv,marital==1)$PSS_Score<27 & subset(covid_pss_biv,marital==1)$PSS_Score>13)
sum(subset(covid_pss_biv,marital==1)$PSS_Score>=27)
sum(subset(covid_pss_biv,marital==2)$PSS_Score<=13)
sum(subset(covid_pss_biv,marital==2)$PSS_Score<27 & subset(covid_pss_biv,marital==2)$PSS_Score>13)
sum(subset(covid_pss_biv,marital==2)$PSS_Score>=27)
sum(subset(covid_pss_biv,marital==3)$PSS_Score<=13)
sum(subset(covid_pss_biv,marital==3)$PSS_Score<27 & subset(covid_pss_biv,marital==3)$PSS_Score>13)
sum(subset(covid_pss_biv,marital==3)$PSS_Score>=27)
sum(subset(covid_pss_biv,marital==4)$PSS_Score<=13)
sum(subset(covid_pss_biv,marital==4)$PSS_Score<27 & subset(covid_pss_biv,marital==4)$PSS_Score>13)
sum(subset(covid_pss_biv,marital==4)$PSS_Score>=27)
sum(subset(covid_pss_biv,marital==5)$PSS_Score<=13)
sum(subset(covid_pss_biv,marital==5)$PSS_Score<27 & subset(covid_pss_biv,marital==5)$PSS_Score>13)
sum(subset(covid_pss_biv,marital==5)$PSS_Score>=27)
sum(subset(covid_pss_biv,marital==6)$PSS_Score<=13)
sum(subset(covid_pss_biv,marital==6)$PSS_Score<27 & subset(covid_pss_biv,marital==6)$PSS_Score>13)
sum(subset(covid_pss_biv,marital==6)$PSS_Score>=27)
sum(subset(covid_pss_biv,marital==7)$PSS_Score<=13)
sum(subset(covid_pss_biv,marital==7)$PSS_Score<27 & subset(covid_pss_biv,marital==7)$PSS_Score>13)
sum(subset(covid_pss_biv,marital==7)$PSS_Score>=27)

#ethnicity
sum(subset(covid_pss_biv,race==1)$PSS_Score<=13)
sum(subset(covid_pss_biv,race==1)$PSS_Score<27 & subset(covid_pss_biv,race==1)$PSS_Score>13)
sum(subset(covid_pss_biv,race==1)$PSS_Score>=27)
sum(subset(covid_pss_biv,race==2)$PSS_Score<=13)
sum(subset(covid_pss_biv,race==2)$PSS_Score<27 & subset(covid_pss_biv,race==2)$PSS_Score>13)
sum(subset(covid_pss_biv,race==2)$PSS_Score>=27)
sum(subset(covid_pss_biv,race==3)$PSS_Score<=13)
sum(subset(covid_pss_biv,race==3)$PSS_Score<27 & subset(covid_pss_biv,race==3)$PSS_Score>13)
sum(subset(covid_pss_biv,race==3)$PSS_Score>=27)
sum(subset(covid_pss_biv,race==4)$PSS_Score<=13)
sum(subset(covid_pss_biv,race==4)$PSS_Score<27 & subset(covid_pss_biv,race==4)$PSS_Score>13)
sum(subset(covid_pss_biv,race==4)$PSS_Score>=27)
sum(subset(covid_pss_biv,race==5)$PSS_Score<=13)
sum(subset(covid_pss_biv,race==5)$PSS_Score<27 & subset(covid_pss_biv,race==5)$PSS_Score>13)
sum(subset(covid_pss_biv,race==5)$PSS_Score>=27)
sum(subset(covid_pss_biv,race==6)$PSS_Score<=13)
sum(subset(covid_pss_biv,race==6)$PSS_Score<27 & subset(covid_pss_biv,race==6)$PSS_Score>13)
sum(subset(covid_pss_biv,race==6)$PSS_Score>=27)
sum(subset(covid_pss_biv,race==7)$PSS_Score<=13)
sum(subset(covid_pss_biv,race==7)$PSS_Score<27 & subset(covid_pss_biv,race==7)$PSS_Score>13)
sum(subset(covid_pss_biv,race==7)$PSS_Score>=27)
sum(subset(covid_pss_biv,race==8)$PSS_Score<=13)
sum(subset(covid_pss_biv,race==8)$PSS_Score<27 & subset(covid_pss_biv,race==8)$PSS_Score>13)
sum(subset(covid_pss_biv,race==8)$PSS_Score>=27)
sum(subset(covid_pss_biv,race==9)$PSS_Score<=13)
sum(subset(covid_pss_biv,race==9)$PSS_Score<27 & subset(covid_pss_biv,race==9)$PSS_Score>13)
sum(subset(covid_pss_biv,race==9)$PSS_Score>=27)

#class
sum(subset(covid_pss_biv,class==1)$PSS_Score<=13)
sum(subset(covid_pss_biv,class==1)$PSS_Score<27 & subset(covid_pss_biv,class==1)$PSS_Score>13)
sum(subset(covid_pss_biv,class==1)$PSS_Score>=27)
sum(subset(covid_pss_biv,class==2)$PSS_Score<=13)
sum(subset(covid_pss_biv,class==2)$PSS_Score<27 & subset(covid_pss_biv,class==2)$PSS_Score>13)
sum(subset(covid_pss_biv,class==2)$PSS_Score>=27)
sum(subset(covid_pss_biv,class==3)$PSS_Score<=13)
sum(subset(covid_pss_biv,class==3)$PSS_Score<27 & subset(covid_pss_biv,class==3)$PSS_Score>13)
sum(subset(covid_pss_biv,class==3)$PSS_Score>=27)
sum(subset(covid_pss_biv,class==4)$PSS_Score<=13)
sum(subset(covid_pss_biv,class==4)$PSS_Score<27 & subset(covid_pss_biv,class==4)$PSS_Score>13)
sum(subset(covid_pss_biv,class==4)$PSS_Score>=27)
sum(subset(covid_pss_biv,class==5)$PSS_Score<=13)
sum(subset(covid_pss_biv,class==5)$PSS_Score<27 & subset(covid_pss_biv,class==5)$PSS_Score>13)
sum(subset(covid_pss_biv,class==5)$PSS_Score>=27)
sum(subset(covid_pss_biv,class==6)$PSS_Score<=13)
sum(subset(covid_pss_biv,class==6)$PSS_Score<27 & subset(covid_pss_biv,class==6)$PSS_Score>13)
sum(subset(covid_pss_biv,class==6)$PSS_Score>=27)
sum(subset(covid_pss_biv,class==7)$PSS_Score<=13)
sum(subset(covid_pss_biv,class==7)$PSS_Score<27 & subset(covid_pss_biv,class==7)$PSS_Score>13)
sum(subset(covid_pss_biv,class==7)$PSS_Score>=27)
sum(subset(covid_pss_biv,class==8)$PSS_Score<=13)
sum(subset(covid_pss_biv,class==8)$PSS_Score<27 & subset(covid_pss_biv,class==8)$PSS_Score>13)
sum(subset(covid_pss_biv,class==8)$PSS_Score>=27)
sum(subset(covid_pss_biv,class==9)$PSS_Score<=13)
sum(subset(covid_pss_biv,class==9)$PSS_Score<27 & subset(covid_pss_biv,class==9)$PSS_Score>13)
sum(subset(covid_pss_biv,class==9)$PSS_Score>=27)

#employ
sum(subset(covid_pss_biv,employ==1)$PSS_Score<=13)
sum(subset(covid_pss_biv,employ==1)$PSS_Score<27 & subset(covid_pss_biv,employ==1)$PSS_Score>13)
sum(subset(covid_pss_biv,employ==1)$PSS_Score>=27)
sum(subset(covid_pss_biv,employ==2)$PSS_Score<=13)
sum(subset(covid_pss_biv,employ==2)$PSS_Score<27 & subset(covid_pss_biv,employ==2)$PSS_Score>13)
sum(subset(covid_pss_biv,employ==2)$PSS_Score>=27)
sum(subset(covid_pss_biv,employ==3)$PSS_Score<=13)
sum(subset(covid_pss_biv,employ==3)$PSS_Score<27 & subset(covid_pss_biv,employ==3)$PSS_Score>13)
sum(subset(covid_pss_biv,employ==3)$PSS_Score>=27)
sum(subset(covid_pss_biv,employ==4)$PSS_Score<=13)
sum(subset(covid_pss_biv,employ==4)$PSS_Score<27 & subset(covid_pss_biv,employ==4)$PSS_Score>13)
sum(subset(covid_pss_biv,employ==4)$PSS_Score>=27)
sum(subset(covid_pss_biv,employ==5)$PSS_Score<=13)
sum(subset(covid_pss_biv,employ==5)$PSS_Score<27 & subset(covid_pss_biv,employ==5)$PSS_Score>13)
sum(subset(covid_pss_biv,employ==5)$PSS_Score>=27)
sum(subset(covid_pss_biv,employ==6)$PSS_Score<=13)
sum(subset(covid_pss_biv,employ==6)$PSS_Score<27 & subset(covid_pss_biv,employ==6)$PSS_Score>13)
sum(subset(covid_pss_biv,employ==6)$PSS_Score>=27)
sum(subset(covid_pss_biv,employ==7)$PSS_Score<=13)
sum(subset(covid_pss_biv,employ==7)$PSS_Score<27 & subset(covid_pss_biv,employ==7)$PSS_Score>13)
sum(subset(covid_pss_biv,employ==7)$PSS_Score>=27)

#financial
sum(subset(covid_pss_biv,fin==1)$PSS_Score<=13)
sum(subset(covid_pss_biv,fin==1)$PSS_Score<27 & subset(covid_pss_biv,fin==1)$PSS_Score>13)
sum(subset(covid_pss_biv,fin==1)$PSS_Score>=27)
sum(subset(covid_pss_biv,fin==2)$PSS_Score<=13)
sum(subset(covid_pss_biv,fin==2)$PSS_Score<27 & subset(covid_pss_biv,fin==2)$PSS_Score>13)
sum(subset(covid_pss_biv,fin==2)$PSS_Score>=27)
sum(subset(covid_pss_biv,fin==3)$PSS_Score<=13)
sum(subset(covid_pss_biv,fin==3)$PSS_Score<27 & subset(covid_pss_biv,fin==3)$PSS_Score>13)
sum(subset(covid_pss_biv,fin==3)$PSS_Score>=27)
sum(subset(covid_pss_biv,fin==4)$PSS_Score<=13)
sum(subset(covid_pss_biv,fin==4)$PSS_Score<27 & subset(covid_pss_biv,fin==4)$PSS_Score>13)
sum(subset(covid_pss_biv,fin==4)$PSS_Score>=27)
sum(subset(covid_pss_biv,fin==5)$PSS_Score<=13)
sum(subset(covid_pss_biv,fin==5)$PSS_Score<27 & subset(covid_pss_biv,fin==5)$PSS_Score>13)
sum(subset(covid_pss_biv,fin==5)$PSS_Score>=27)

