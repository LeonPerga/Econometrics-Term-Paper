library("ggplot2")
library("dplyr")
library("lmtest")
library("sandwich")
library("whitestrap")
library("car")
library("stargazer")
data <- read.csv("term_paper_data.csv")
#check commit
data[, c(4, 2)]
options(digits = 2)

data[, background_cols]
sfp = filter(data[,c(4, 3, )], sfp_offer == 1)
ssp = filter(data[,2:ncol(data)], ssp_offer == 1)
control = filter(data[,2:ncol(data)], control == 1)

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
round(means_table, 2)

stargazer(means_table, type = "text", title = "means_table",  out = "Means_Table.html")

#P2Q3
model1 = lm(ssp_offer ~ HS_GPA + age + female + english + dad_HS_grad + dad_college_grad 
            + mom_HS_grad + mom_college_grad + uni_first_choice + finish_in_4_yrs + grad_degree + live_home + work_plans, data)
model1_fixed = coeftest(model1, vcov = vcovHC(model1, type = "HC1"))
stargazer(model1_fixed, type = "text", title = "model1", out = "model1.html")
#P3Q4

#first we must filter out the students who were offered to take part in the program but didnt so we could properly isolate the control group

model2 = lm(first_sem_grade ~ ssp_signup + sfp_signup, data)
summary(model2)
#Homoscedasticity is preserved in the regression 
white_test(model2)

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
model4 = lm(GPA_year1 ~ ssp_signup +sfp_signup + HS_GPA + age, filtered_data)
summary(model4)
model5 = lm(GPA_year2 ~ ssp_signup +sfp_signup + HS_GPA + age, filtered_data)
summary(model5)

affects_table = 
  data.frame(Treatment = c("SSP", "SFP"), Fall_Semester = 
               c(unname(model3$coefficients[2]), unname(model3$coefficients[3])),
                 Year_1 = c(unname(model4$coefficients[2]), unname(model4$coefficients[3])),
                 Year_2 = c(unname(model5$coefficients[2]), unname(model5$coefficients[3])))
affects_table
