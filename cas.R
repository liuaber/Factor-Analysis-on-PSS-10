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
#dim(covid) #Now this should leave us with 2083 responses in total from the start
covid_cas = covid[-c(which(is.na(covid$CAS_1)==TRUE|is.na(covid$CAS_2)==TRUE|is.na(covid$CAS_3)==TRUE|
                             is.na(covid$CAS_4)==TRUE|is.na(covid$CAS_5)==TRUE)),]
dim(covid_cas) #167 responses was deleted here. 1911 responses left for CAS scale calculation
##### We shall only include complete responses for a scale. 
##### Adding a column for total score of CAS, notice in the qualtrics the responses were labelled as 1-5.
##### Instead the actual range for each response is 0-4. 
attach(covid_cas)
covid_cas$CAS_total = CAS_1+CAS_2+CAS_3+CAS_4+ CAS_5 -5 #-5 for the reasons we stated above
#detach(covid_cas)

anxiety_class = c(sum(CAS_total<9),sum(CAS_total>=9))
anxiety_class_percent = c(sum(CAS_total<9),sum(CAS_total>=9))/length(CAS_total)

pie(anxiety_class,labels = c("Participants without \n Dysfunctional Anxiety \n (94.19%)","Participants with \n Dysfunctional Anxiety \n (5.81%)"),cex = 1.1,col = c('azure','firebrick'),radius = 1)

hist(covid_cas$CAS_total,breaks = seq(0,20,by=2),main = "The distribution of CAS score \n among student participants in UNLV", xlab = "CAS score",ylab = "No. of Students",col = seq(1,10))
#### Honestly we care about the group such that have >= 9 in a CAS scale. 
cas_sum = c(sum(covid_cas$CAS_total>=9),sum(covid_cas$CAS_total<9))
name = c("CAS score \n >=9","CAS score \n <9")
barplot(cas_sum,names.arg = name,main = "Distribution of CAS scores of \n 1911 participants from UNLV",ylim = c(0,2000),col = c('red','grey'),cex.names = 0.7,ylab = "Number of Participants")

###### Gender analysis (Q1.1 coding: Female: 2; Male: 1)
### First delete the rows such that gender is NA from covid_cas file
sum(is.na(covid_cas$Q1.1==TRUE)) #There're 154 students that didn't respond to this question. 
covid_cas_gender = covid_cas[-c(which(is.na(covid_cas$Q1.1==TRUE))),] #Delete the rows, 1757 responses left. 
### Now organize the data frame to test the difference in CAS score among different gender group
data_gender = data.frame(
  Sex = as.factor(covid_cas_gender$Q1.1),
  CAS_Score = covid_cas_gender$CAS_total
)
table(data_gender$Sex)
library(ggplot2)
ggplot(data_gender)+aes(x = Sex,y = CAS_Score)+ geom_boxplot(fill="grey")
## We shall see a big difference between the distribution of Male, Female and "other", then check the histogram of each subgroup
par(mfrow = c(3,1))
hist(subset(data_gender,Sex==1)$CAS_Score, main = "CAS Score for Male Students")
hist(subset(data_gender,Sex==2)$CAS_Score, main = "CAS Score for Female Students")
hist(subset(data_gender,Sex==3)$CAS_Score, main = "CAS Score for Other Students") ### None of the above follows a normal distribution, hence we attempt to do Shapiro-Wilks test to check the normality
shapiro.test(subset(data_gender,Sex==1)$CAS_Score)
shapiro.test(subset(data_gender,Sex==2)$CAS_Score)
shapiro.test(subset(data_gender,Sex==3)$CAS_Score)
###We shall reject the normality assumptions for all of these, we should go for Kruskal-Wallis test (Wilcox test is for 2 groups comparison)
kruskal.test(CAS_Score~Sex,data = data_gender)

### This suggest we reject the null since the p-value is 1.659e-13. We conclude there're significant difference between gender groups.
sum(subset(data_gender,Sex==1)$CAS_Score>=9)
sum(subset(data_gender,Sex==1)$CAS_Score<9)
sum(subset(data_gender,Sex==2)$CAS_Score>=9)
sum(subset(data_gender,Sex==2)$CAS_Score<9)
sum(subset(data_gender,Sex==3)$CAS_Score>=9)
sum(subset(data_gender,Sex==3)$CAS_Score<9)

### However, if we wanna justify such difference was genuinely made by the difference in sex orientation, we shall perform a permutation test will label exchange and see the location of test statistic within a whole distribution. (This shall be performed later.)
### One suggestion: perm.kruskal.test
#From RVAideMemoire v0.9-8-5
#by Maxime Herv
library(RVAideMemoire)

### Now we want to investigate if there is a difference in CAS score between transgenders and non-trangenders group
###### Transgender analysis (Q2 coding: Non-transgender: 2; Transgender: 1; Other: 3)
sum(is.na(covid_cas$Q2==TRUE)) #There're 153 students that didn't respond to this question. 
covid_cas_trans = covid_cas[-c(which(is.na(covid_cas$Q2==TRUE))),] #Delete the rows, 1758 responses left. 
### Now organize the data frame to test the difference in CAS score among different transgender group
data_trans = data.frame(
  Transgender = as.factor(covid_cas_trans$Q2),
  CAS_Score = covid_cas_trans$CAS_total
)
library(ggplot2)
ggplot(data_trans)+aes(x = Transgender,y = CAS_Score)+ geom_boxplot(fill="grey")
kruskal.test(CAS_Score~Transgender,data = data_trans)#Reject the null 
###Q6
##### Classification of student status
### Now we want to investigate if there is a difference in CAS score between undergrads, masters and PhD students
###### Classification (Q6 coding:6: Master; 7: PhD; 8: Not seeking for degree; 9: Other)
sum(is.na(covid_cas$Q6==TRUE)) #There're 156 students that didn't respond to this question. 
covid_cas_class = covid_cas[-c(which(is.na(covid_cas$Q6==TRUE))),] #Delete the rows, 1755 responses left. 
### Now organize the data frame to test the difference in CAS score among different classifications.
data_class = data.frame(
  Classification = as.factor(covid_cas_class$Q6),
  CAS_Score = covid_cas_class$CAS_total
)


kruskal.test(CAS_Score~Classification,data = data_class)#Do not reject the null 
###Q4
### We also wanna investigate if the race/ethnicity makes a difference in the analysis result. 
### Coding: 
### 1. Native Alaskan/ American Indian
### 2. Asian or Asian American
### 3. Black or African American
### 4. Hispanic/Latino/a/x
### 5. MENA/Middle Eastern Origin
### 6. Pacific Islander/Hawaiian
### 7. White/ Caucasian
### 8. Biracial/Multiracial
### 9. Other
### Demographics
sum(is.na(covid_cas$Q4==TRUE)) #There're 153 students that didn't respond to this question. 
covid_cas_race = covid_cas[-c(which(is.na(covid_cas$Q4==TRUE))),] #Delete the rows, 1758 responses left.
data_race = data.frame(
  Ethnicity = as.factor(covid_cas_race$Q4),
  CAS_Score = covid_cas_race$CAS_total
)
table(data_race$Ethnicity)
### This part provides the mean CAS value by groups
aggregate(data_race[, 2], list(data_race$Ethnicity), mean)
### Boxplot comparison
ggplot(data_race)+aes(x = Ethnicity,y = CAS_Score)+ geom_boxplot(fill="grey")
### We see that group 5 and group 6 generally has lower 
par(mfrow = c(3,3))
hist(subset(data_race,Ethnicity==1)$CAS_Score, main = "CAS Score for Native Alaskan/American Indian")
hist(subset(data_race,Ethnicity==2)$CAS_Score, main = "CAS Score for Asian/Asian American")
hist(subset(data_race,Ethnicity==3)$CAS_Score, main = "CAS Score for Black/African American")
hist(subset(data_race,Ethnicity==4)$CAS_Score, main = "CAS Score for Hispanic/Latino/a/x")
hist(subset(data_race,Ethnicity==5)$CAS_Score, main = "CAS Score for MENA/Arab Origin")
hist(subset(data_race,Ethnicity==6)$CAS_Score, main = "CAS Score for Pacific/Islander")
hist(subset(data_race,Ethnicity==7)$CAS_Score, main = "CAS Score for White/Caucasian")
hist(subset(data_race,Ethnicity==8)$CAS_Score, main = "CAS Score for Biracial/Multiracial")
hist(subset(data_race,Ethnicity==9)$CAS_Score, main = "CAS Score for Others")
kruskal.test(CAS_Score~Ethnicity,data = data_race)#Do not reject the null 
### Demographics
sum(subset(data_race,Ethnicity==1)$CAS_Score<9)
sum(subset(data_race,Ethnicity==1)$CAS_Score>=9)
sum(subset(data_race,Ethnicity==2)$CAS_Score<9)
sum(subset(data_race,Ethnicity==2)$CAS_Score>=9)
sum(subset(data_race,Ethnicity==3)$CAS_Score<9)
sum(subset(data_race,Ethnicity==3)$CAS_Score>=9)
sum(subset(data_race,Ethnicity==4)$CAS_Score<9)
sum(subset(data_race,Ethnicity==4)$CAS_Score>=9)
sum(subset(data_race,Ethnicity==5)$CAS_Score<9)
sum(subset(data_race,Ethnicity==5)$CAS_Score>=9)
sum(subset(data_race,Ethnicity==6)$CAS_Score<9)
sum(subset(data_race,Ethnicity==6)$CAS_Score>=9)
sum(subset(data_race,Ethnicity==7)$CAS_Score<9)
sum(subset(data_race,Ethnicity==7)$CAS_Score>=9)
sum(subset(data_race,Ethnicity==8)$CAS_Score<9)
sum(subset(data_race,Ethnicity==8)$CAS_Score>=9)
sum(subset(data_race,Ethnicity==9)$CAS_Score<9)
sum(subset(data_race,Ethnicity==9)$CAS_Score>=9)

### Q24 financial status
sum(is.na(covid_cas$Q24==TRUE)) #There're 178 students that didn't respond to this question. 
covid_cas_fin = covid_cas[-c(which(is.na(covid_cas$Q24==TRUE))),] #Delete the rows, 1758 responses left.
data_fin = data.frame(
  fin = as.factor(covid_cas_fin$Q24),
  CAS_Score = covid_cas_fin$CAS_total
)
table(data_fin$fin)
sum(subset(data_fin,fin==1)$CAS_Score<9)
sum(subset(data_fin,fin==1)$CAS_Score>=9)
sum(subset(data_fin,fin==2)$CAS_Score<9)
sum(subset(data_fin,fin==2)$CAS_Score>=9)
sum(subset(data_fin,fin==3)$CAS_Score<9)
sum(subset(data_fin,fin==3)$CAS_Score>=9)
sum(subset(data_fin,fin==4)$CAS_Score<9)
sum(subset(data_fin,fin==4)$CAS_Score>=9)
sum(subset(data_fin,fin==5)$CAS_Score<9)
sum(subset(data_fin,fin==5)$CAS_Score>=9)
kruskal.test(CAS_Score~fin,data = data_fin)

#Q22
#have you experienced any discriminatory or hostile behavior due to your race/ethnicity 
covid_cas_racism = covid_cas[-c(which(is.na(covid_cas$Q22==TRUE))),] #Delete the rows, 1758 responses left.
data_racism = data.frame(
  racism = as.factor(covid_cas_racism$Q22),
  CAS_Score = covid_cas_racism$CAS_total
)
table(data_racism$racism)
sum(subset(data_racism,racism==1)$CAS_Score<9)
sum(subset(data_racism,racism==1)$CAS_Score>=9)
sum(subset(data_racism,racism==2)$CAS_Score<9)
sum(subset(data_racism,racism==2)$CAS_Score>=9)
wilcox.test(CAS_Score~racism,data = data_racism)
detach(covid_cas)
#### Q11 employment
covid_cas_emp = covid_cas[-c(which(is.na(covid_cas$Q11==TRUE))),] #Delete the rows, 1758 responses left.
data_emp = data.frame(
  emp = as.factor(covid_cas_emp$Q11),
  CAS_Score = covid_cas_emp$CAS_total
)
kruskal.test(CAS_Score~emp,data = data_emp) #reject the null
#### Q25 Family member tested positive
covid_cas_fam = covid_cas[-c(which(is.na(covid_cas$Q25==TRUE))),] #Delete the rows, 1758 responses left.
data_fam = data.frame(
  fam = as.factor(covid_cas_fam$Q25),
  CAS_Score = covid_cas_fam$CAS_total
)
wilcox.test(CAS_Score~fam,data = data_fam) #reject the null
#### Q26A tested positive
covid_cas_pos = covid_cas[-c(which(is.na(covid_cas$Q26A==TRUE))),] #Delete the rows, 1758 responses left.
data_pos = data.frame(
  pos = as.factor(covid_cas_pos$Q25),
  CAS_Score = covid_cas_pos$CAS_total
)
kruskal.test(CAS_Score~pos,data = data_pos) #reject the null

covid_cas_age = covid_cas[-c(which(is.na(covid_cas$Q5==TRUE))),] #Delete the rows, 1757 responses left. 
### Now organize the data frame to test the difference in CAS score among different gender group
data_age = data.frame(
  age = as.factor(covid_cas_gender$Q5),
  CAS_Score = covid_cas_gender$CAS_total
)
kruskal.test(CAS_Score~age,data = data_age) #reject the null
