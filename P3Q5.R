#Testing control variables
library("ggplot2")
library("dplyr")
library("lmtest")
library("sandwich")
library("whitestrap")
library("car")
library("stargazer")
library("texreg")
data <- read.csv("term_paper_data.csv")
#check commit

#P3Q4

model2 = lm(first_sem_grade ~ ssp_offer + sfp_offer, data)
  summary(model2)
#Homoscedasticity is preserved in the regression 
white_test(model2)
htmlreg(model2, file = "P3Q4.html", custom.columns = c("Intercept", "Dummy variable for belonging to SSP group", "Dummy variable for belonging to SFP group"),
        digits = 2, custom.model.names = c("Treatment Effects on First Year Outcomes in the Sample with Fall Grades") )
#P3Q5

modelP3Q5_controls = lm(first_sem_grade ~ ssp_offer + sfp_offer + HS_GPA + age, data)

modelP3Q5_controls_2 = lm(first_sem_grade ~ ssp_offer + sfp_offer + HS_GPA + age + female + english + dad_HS_grad + dad_college_grad 
                          + mom_HS_grad + mom_college_grad + uni_first_choice + finish_in_4_yrs + grad_degree + live_home + work_plans, data)

modelP3Q5_test = lm(first_sem_grade ~  HS_GPA + age + female + english + dad_HS_grad + dad_college_grad 
                          + mom_HS_grad + mom_college_grad + uni_first_choice + finish_in_4_yrs + grad_degree + live_home + work_plans, data)
summary(modelP3Q5_test)

linearHypothesis(modelP3Q5_test, c(
  "HS_GPA = 0",
  "age = 0",
  "female = 0",
  "english = 0",
  "finish_in_4_yrs = 0"
))
cov(data$first_sem_grade, data$finish_in_4_yrs)
cov(data$first_sem_grade, data$HS_GPA)
cov(data$first_sem_grade, data$english)
cov(data$first_sem_grade, data$female)
cov(data$first_sem_grade, data$age)
plot(data$first_sem_grade, data$HS_GPA)
plot(data$first_sem_grade, data$female)
modelP3Q5_controls_3 = lm(first_sem_grade ~ ssp_offer + sfp_offer + HS_GPA, data)

summary(modelP3Q5_controls_3)


modelP3Q5_controls_4 = lm(first_sem_grade ~ ssp_offer + sfp_offer + female + HS_GPA +age +english, data)
summary(modelP3Q5_controls_4)

modelP3Q5_controls_5 = lm(first_sem_grade ~ ssp_offer + sfp_offer + female + HS_GPA +age +english , data)
summary(modelP3Q5_controls_5)



linearHypothesis(modelP3Q5_controls_4, c("ssp_offer = 0"))


#Homoscedasticity is not preserved in the regression 
white_test(modelP3Q5_controls_3)
coeftest(modelP3Q5_controls_3, vcov = vcovHC(model3, type = "HC1"))
