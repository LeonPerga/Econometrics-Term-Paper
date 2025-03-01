library("ggplot2")
library("dplyr")
library("lmtest")
library("sandwich")
library("whitestrap")
library("car")
library("stargazer")
library("texreg")
library(broom)
library(lfe)
data <- read.csv("term_paper_data.csv")
#check commit
data[, c(4, 2)]
options(digits = 2, scipen = 999)

sfp = filter(data, sfp_offer == 1)
ssp = filter(data, ssp_offer == 1)
control = filter(data, control == 1)

#NOTE: first column is student id and is not a metric
sfp_means = colMeans(sfp)
names(sfp_means) = names(sfp)
ssp_means = colMeans(ssp)
names(ssp_means) = names(ssp)
# Create a vector with the column names
columns_to_select <- c("HS_GPA", "age", "female", "english", 
                       "dad_HS_grad", "dad_college_grad", "mom_HS_grad", 
                       "mom_college_grad", "uni_first_choice", "finish_in_4_yrs", 
                       "grad_degree", "live_home", "work_plans")
# Descriptions vector
descriptions <- c(
  "ממוצע ציונים בתיכון",        # HS_GPA
  "גיל",                        # age
  "משתנה דמי לאישה",            # female
  "שפת אם של האם היא אנגלית",   # english
  "אב בוגר תיכון",              # dad_HS_grad
  "אב בוגר אוניברסיטה",         # dad_college_grad
  "אם בוגרת תיכון",             # mom_HS_grad
  "אם בוגרת אוניברסיטה",        # mom_college_grad
  "לומד באוניברסיטה שנבחרה כעדיפות ראשונה", # uni_first_choice
  "מתכנן לסיים את התואר תוך 4 שנים", # finish_in_4_yrs
  "מעוניין בתואר מעבר לתואר ראשון", # grad_degree
  "גר בבית ההורים",             # live_home
  "מתכנן לעבוד בזמן הלימודים"   # work_plans
)
control_means = colMeans(control)
names(control_means) = names(control)
means_table = cbind(t(t(sfp_means)), t(t(ssp_means)), t(t(control_means)))
means_table = means_table[columns_to_select,]
means_table = cbind(descriptions, means_table)
colnames(means_table) = c("descriptions", "SFP", "SSP", "Control")



stargazer(means_table, type = "text", title = "means_table",  out = "Means_Table.html")

#P2Q3
data_P2Q3 = subset(data, data$ssp_offer == 1 | data$control==1)
model1 = lm(ssp_offer ~ HS_GPA + age + female + english + dad_HS_grad + dad_college_grad 
            + mom_HS_grad + mom_college_grad + uni_first_choice + finish_in_4_yrs + grad_degree + live_home + work_plans, data_P2Q3)
model1_fixed = coeftest(model1, vcov = vcovHC(model1, type = "HC1"))
stargazer(model1_fixed, type = "text", title = "model1", out = "model1.html")
htmlreg(model1_fixed, file = "table2.html", custom.columns = c("החותך", descriptions), digits = 2, stars = c(0.01, 0.05, 0.1), custom.model.names = c("Table 1— Contrasts by treatment of SSP"))
linearHypothesis(model1, c(
  "HS_GPA = 0",
  "age = 0",
  "female = 0",
  "english = 0",
  "dad_HS_grad = 0",
  "dad_college_grad = 0",
  "mom_HS_grad = 0",
  "mom_college_grad = 0",
  "uni_first_choice = 0",
  "finish_in_4_yrs = 0",
  "grad_degree = 0",
  "live_home = 0",
  "work_plans = 0"
))


### PART 3 - ANALYSIS OF SFP & SSP TREATMENT AFFECTS ###
########################################################

#P3Q4

#first we must filter out the students who were offered to take part in the program but didnt so we could properly isolate the control group

model2 = lm(first_sem_grade ~ ssp_signup + sfp_signup, data)
summary(model2)
#Homoscedasticity is preserved in the regression 
white_test(model2)
htmlreg(model2, file = "P3Q4.html", custom.columns = c("Intercept", "Dummy variable for belonging to SSP group", "Dummy variable for belonging to SFP group"),
        digits = 2, custom.model.names = c("Treatment Effects on First Year Outcomes in the Sample with Fall Grades") )
#P3Q5

model3 = lm(first_sem_grade ~ ssp_signup +sfp_signup + HS_GPA + age, data)
summary(model3)
cov(data$ssp_offer, data$HS_GPA)
linearHypothesis(model3, c("HS_GPA = 0", "age = 0"))
#Homoscedasticity is not preserved in the regression 
white_test(model3)
coeftest(model3, vcov = vcovHC(model3, type = "HC1"))



#P3Q6
# The null hypothesis H0: ssp_signup = sfp_signup
# The alternative hypothesis H1 : ssp_signup != sfp_signup
linearHypothesis(model3, c("ssp_signup = sfp_signup"))
# F statistic is 0.34, we will not reject the null hypothesis at a = 0.1, ssp_signup = sfp_signup

#P3Q7
model4 = lm(GPA_year1 ~ ssp_signup +sfp_signup + HS_GPA + age, data)
summary(model4)
model5 = lm(GPA_year2 ~ ssp_signup +sfp_signup + HS_GPA + age, data)
summary(model5)

affects_table = 
  data.frame(Treatment = c("SSP", "SFP"), Fall_Semester = 
               c(unname(model3$coefficients[2]), unname(model3$coefficients[3])),
                 Year_1 = c(unname(model4$coefficients[2]), unname(model4$coefficients[3])),
                 Year_2 = c(unname(model5$coefficients[2]), unname(model5$coefficients[3])))
affects_table
affects_table  = t(affects_table)
stargazer(affects_table, type = "text", title = "Affects_Table",  out = "Affects_Table.html")
#P3Q8
P3Q8_data = subset(data, data$sfp_offer == 1 | data$control == 1)

#Adding dummy variable - 1 if HS GPA is above the median, 0 otherwise
P3Q8_data["abv_med"] = ifelse(P3Q8_data$HS_GPA > median(P3Q8_data$HS_GPA), 1, 0)
model_q8 = lm(first_sem_grade ~ sfp_offer + abv_med  + abv_med*sfp_offer , P3Q8_data)
summary(model_q8)

# Adding interaction dummy variable
P3Q8_data["abv_med_sfp_offer"] = P3Q8_data$sfp_offer*P3Q8_data$abv_med
# We can see same model as q8
model_q82 = lm(first_sem_grade ~ sfp_offer + abv_med  + abv_med_sfp_offer + HS_GPA + age , P3Q8_data)
summary(model_q82)
# Output model to html
htmlreg(model_q82, file = "P3Q8.html", custom.columns = c("Intercept", "Dummy variable for belonging to SFP group", "Dummy variable for having HS GPA above median", "Interactio dummy variable", "HS GPA", "age"),
        digits = 2, custom.model.names = c("Differemces of SFP Treatment Effects on First Year Outcomes in the Sample with Fall Grades for students with GPA above median") )

# Checking hypothesis
linearHypothesis(model_q82, c("abv_med_sfp_offer = 0"))

### PART 4 - ANALYSIS OF INVOLVEMENT IN TREATMENT AFFECTS ###
#############################################################

#Q9
filtered_data = filter(data, sfp_offer == 1 )
mean_accepted = colMeans(filter(filtered_data, sfp_signup == 1 ))
mean_rejected = colMeans(filter(filtered_data, sfp_signup == 0))
names(mean_accepted) = names(data)
names(mean_rejected) = names(data)
means_table2 = cbind(mean_rejected, mean_accepted)
means_table2 = means_table2[columns_to_select,]
colnames(means_table2)=c("accpted the sfp program","rejected the sfp program")
summary(means_table2)
stargazer(means_table2, type = "html", title = "",  out = "means_table2.html")


#Q10
data_q10 = filter(data, ssp_offer == 0)
modelq10 = lm(first_sem_grade ~ sfp_signup,data_q10)
summary(modelq10)

#Q11
#H0 : cov(sfp_offer,sfp_signup) = 0 
stargazer(linearHypothesis(modelq11p1, "sfp_offer=0"),type = "html", title = " SFP בדיקת הקסוגניות של ההצעה להשתתף בתכנית  ",  out = "test.html")
modelq11p1 = lm(sfp_signup ~ sfp_offer, data_q10)
summary(modelq11p1)
#Pvalue is 0, we reject the null hypothesis
white_test(modelq11p1)
coeftest(modelq11p1, vcov = vcovHC(modelq11p1, type = "HC1"))
predicted_signup = fitted.values(modelq11p1)
modelq11p2 = lm(first_sem_grade ~ predicted_signup, data_q10)
summary(modelq11p2)
white_test(modelq11p2)
stargazer(modelq11p1, type = "html", title = "שלב ראשון",  out = "first step.html")
stargazer(modelq11p2, type = "html", title = "IV אמידת",  out = "modelq11p2.html")

