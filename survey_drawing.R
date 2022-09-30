#survey drawing
mail_list = read.csv("survey_drawing.csv")
str(mail_list)
mail_list = mail_list$email
length(mail_list)
null_res = which(mail_list == "")
length(null_res) #125 null responses
mail_list_clean = mail_list[-null_res]
length(mail_list_clean)
length(mail_list_clean)+length(null_res)==length(mail_list) #TRUE
which(mail_list_clean=="") #integer(0)
sample_size = length(mail_list_clean)
#set.seed(993)
lucky_dogs = sample(1:sample_size,10)
mail_list_clean[lucky_dogs]
#
#[1] "magalv1@unlv.nevada.edu"  "retumr1@unlv.nevada.edu"  "Kingt15@unlv.Nevada.edu" 
#[4] "foulk@unlv.nevada.edu"    "wooe1@unlv.nevada.edu"    "zamane1@unlv.nevada.edu" 
#[7] "parkel2@unlv.nevada.edu"  "Ortize13@unlv.nevada.edu" "kjer@unlv.nevada.edu"    
#[10] "mahmoz1@unlv.nevada.edu" 

### covid-19 data structure
covid = read.csv("covid19_data.csv")
head(covid)
sum(is.na(covid$Q1))
covid = covid[-c(which(is.na(covid$Q1)==TRUE)),] #Delete the null response for Q1 (2 null)
dim(covid) #check dimension of the data matrix
sum(is.na(covid$CAS_1))
sum(is.na(covid$CAS_2))
sum(is.na(covid$CAS_3))
sum(is.na(covid$CAS_4))
sum(is.na(covid$CAS_5))
##### For CAS scale
covid_cas = covid[-c(which(is.na(covid$CAS_1)==TRUE|is.na(covid$CAS_2)==TRUE|is.na(covid$CAS_3)==TRUE|
is.na(covid$CAS_4)==TRUE|is.na(covid$CAS_5)==TRUE)),]

sum(is.na(covid_cas$CAS_2))
dim(covid_cas) #167 responses was deleted here. 1911 responses left for CAS scale calculation
##### We shall only include complete responses for a scale. 
##### Adding a column for total score of CAS, notice in the qualtrics the responses were labelled as 1-5.
##### Instead the actual range for each response is 0-4. 
attach(covid_cas)
covid_cas$CAS_total = CAS_1+CAS_2+CAS_3+CAS_4+ CAS_5 -5 #-5 for the reasons we stated above
detach(covid_cas)
sum(covid_cas$CAS_total>=9)


##### We shall take the "Effects of COVID-19" on college students mental health in the US: An Interview-survey study"
##### As a reference (PSS scale)
### read covid now again
sum(is.na(covid$PSS_1))
sum(is.na(covid$PSS_2))
sum(is.na(covid$PSS_3))
sum(is.na(covid$PSS_4))
sum(is.na(covid$PSS_5))
sum(is.na(covid$PSS_6))
sum(is.na(covid$PSS_7))
sum(is.na(covid$PSS_8))
sum(is.na(covid$PSS_9))
sum(is.na(covid$PSS_10))
covid_pss = covid[-c(which(is.na(covid$PSS_1)==TRUE|is.na(covid$PSS_2)==TRUE|is.na(covid$PSS_3)==TRUE|
                             is.na(covid$PSS_4)==TRUE|is.na(covid$PSS_5)==TRUE|is.na(covid$PSS_6)==TRUE
                           |is.na(covid$PSS_7)==TRUE|is.na(covid$PSS_8)==TRUE|is.na(covid$PSS_9)==TRUE|
                             is.na(covid$PSS_10)==TRUE)),]
dim(covid_pss)
### 1738 responses left
attach(covid_pss)
#covid_pss$PSS_total = PSS_1+PSS_2+PSS_3+PSS_4+ PSS_5+PSS_6+PSS_7+PSS_8+PSS_9+PSS_10 -10 #-10 for the reasons we stated above
detach(covid_pss)

sum(is.na(covid$MDI_1))
sum(is.na(covid$MDI_2))
sum(is.na(covid$MDI_3))
sum(is.na(covid$MDI_4))
sum(is.na(covid$MDI_5))
sum(is.na(covid$MDI_6))
sum(is.na(covid$MDI_7))
sum(is.na(covid$MDI_8))
sum(is.na(covid$MDI_9))
sum(is.na(covid$MDI_10))
sum(is.na(covid$MDI_11))
sum(is.na(covid$MDI_12))
sum(is.na(covid$Q1.1))
sum(is.na(covid$Q2))
sum(is.na(covid$Q3))
sum(is.na(covid$Q4))
sum(is.na(covid$Q5))
sum(is.na(covid$Q6))
sum(is.na(covid$Q7))
sum(is.na(covid$Q8))
sum(is.na(covid$Q9))
sum(is.na(covid$Q10))
sum(is.na(covid$Q11))
sum(is.na(covid$Q12))
sum(is.na(covid$Q13))
sum(is.na(covid$Q14))
sum(is.na(covid$Q15))
sum(is.na(covid$Q16))
sum(is.na(covid$Q17A))
sum(is.na(covid$Q17B))
sum(is.na(covid$Q18))
sum(is.na(covid$Q19A))
sum(is.na(covid$Q19B))
sum(is.na(covid$Q20A))
sum(is.na(covid$Q20B))
sum(is.na(covid$Q21))
sum(is.na(covid$Q22))
sum(is.na(covid$Q23))
sum(is.na(covid$Q24))
sum(is.na(covid$Q25))
sum(is.na(covid$Q26A))
sum(is.na(covid$Q26B))
sum(is.na(covid$Q26C))
sum(is.na(covid$Q26D))


number_non_response = c(sum(is.na(covid$Q1)),
sum(is.na(covid$CAS_1)),
sum(is.na(covid$CAS_2)),
sum(is.na(covid$CAS_3)),
sum(is.na(covid$CAS_4)),
sum(is.na(covid$CAS_5)),
sum(is.na(covid$PSS_1)),
sum(is.na(covid$PSS_2)),
sum(is.na(covid$PSS_3)),
sum(is.na(covid$PSS_4)),
sum(is.na(covid$PSS_5)),
sum(is.na(covid$PSS_6)),
sum(is.na(covid$PSS_7)),
sum(is.na(covid$PSS_8)),
sum(is.na(covid$PSS_9)),
sum(is.na(covid$PSS_10)),
sum(is.na(covid$MDI_1)),
sum(is.na(covid$MDI_2)),
sum(is.na(covid$MDI_3)),
sum(is.na(covid$MDI_4)),
sum(is.na(covid$MDI_5)),
sum(is.na(covid$MDI_6)),
sum(is.na(covid$MDI_7)),
sum(is.na(covid$MDI_8)),
sum(is.na(covid$MDI_9)),
sum(is.na(covid$MDI_10)),
sum(is.na(covid$MDI_11)),
sum(is.na(covid$MDI_12)),
sum(is.na(covid$Q1.1)),
sum(is.na(covid$Q2)),
sum(is.na(covid$Q3)),
sum(is.na(covid$Q4)),
sum(is.na(covid$Q5)),
sum(is.na(covid$Q6)),
sum(is.na(covid$Q7)),
sum(is.na(covid$Q8)),
sum(is.na(covid$Q9)),
sum(is.na(covid$Q10)),
sum(is.na(covid$Q11)),
sum(is.na(covid$Q12)),
sum(is.na(covid$Q13)),
sum(is.na(covid$Q14)),
sum(is.na(covid$Q15)),
sum(is.na(covid$Q16)),
sum(is.na(covid$Q17A)),
sum(is.na(covid$Q17B)),
sum(is.na(covid$Q18)),
sum(is.na(covid$Q19A)),
sum(is.na(covid$Q19B)),
sum(is.na(covid$Q20A)),
sum(is.na(covid$Q20B)),
sum(is.na(covid$Q21)),
sum(is.na(covid$Q22)),
sum(is.na(covid$Q23)),
sum(is.na(covid$Q24)),
sum(is.na(covid$Q25)),
sum(is.na(covid$Q26A)),
sum(is.na(covid$Q26B)),
sum(is.na(covid$Q26C)),
sum(is.na(covid$Q26D))
)

question = c("Q1","CAS1","CAS2","CAS3","CAS4","CAS5",
             "PSS1","PSS2","PSS3","PSS4","PSS5","PSS6","PSS7","PSS8","PSS9","PSS10",
             "MDI_1","MDI_2","MDI_3","MDI_4","MDI_5","MDI_6","MDI_7","MDI_8","MDI_9","MDI_10","MDI_11","MDI_12",
             "Q1.1","Q2","Q3","Q4","Q5","Q6","Q7","Q8","Q9","Q10","Q11","Q12","Q13","Q14","Q15","Q16","Q17A",
             "Q17B", "Q18", "Q19A","Q19B","Q20A","Q20B","Q21","Q22","Q23","Q24","Q25",
             "Q26A","Q26B","Q26C","Q26D"
             )
covid_summary = data.frame(number_non_response,row.names = question)
covid_summary$response_rate = 1- covid_summary$number_non_response/2090

write.csv(covid_summary,file = "covid_summary.csv")

