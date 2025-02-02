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
                          + mom_HS_grad + mom_college_grad + uni_first_choice + finish_in_4_yrs + grad_degree + live_home + work_plans + last_min, data)
summary(modelP3Q5_controls_2)

linearHypothesis(modelP3Q5_controls_2, c(
  "HS_GPA = 0",
  "age = 0",
  "female = 0",
  "english = 0",
  "finish_in_4_yrs = 0"
))


linearHypothesis(modelP3Q5_controls_2, c(
  "dad_HS_grad = 0",
  "dad_college_grad = 0",
  "mom_HS_grad = 0",
  "mom_college_grad = 0",
  "uni_first_choice = 0",
  "grad_degree = 0",
  "live_home = 0",
  "work_plans = 0"
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


modelP3Q5_controls_4 = lm(first_sem_grade ~ ssp_offer + sfp_offer + female + HS_GPA +age +english + finish_in_4_yrs, data)
summary(modelP3Q5_controls_4)

modelP3Q5_controls_5 = lm(first_sem_grade ~ ssp_offer + sfp_offer + female + HS_GPA +age +english , data)
summary(modelP3Q5_controls_5)
print(vcov(modelP3Q5_controls_5, data=data))

modelP3Q5_controls_6 = lm(first_sem_grade ~ ssp_offer + sfp_offer + uni_first_choice + work_plans +mom_college_grad +grad_degree + female + HS_GPA +age +english+ finish_in_4_yrs  , data)
summary(modelP3Q5_controls_6)

linearHypothesis(modelP3Q5_controls_6, c("uni_first_choice = 0", "work_plans = 0"))


#Homoscedasticity is not preserved in the regression 
white_test(modelP3Q5_controls_3)
coeftest(modelP3Q5_controls_3, vcov = vcovHC(model3, type = "HC1"))



##$########

columns_to_select <- c("HS_GPA", "age", "female", "english", 
                       "dad_HS_grad", "dad_college_grad", "mom_HS_grad", 
                       "mom_college_grad", "uni_first_choice", "finish_in_4_yrs", 
                       "grad_degree", "live_home", "work_plans")
model_1_29_2025_no_controls = lm(first_sem_grade ~ sfp_offer+ssp_offer, data)

model_1_29_2025 = lm(first_sem_grade ~ sfp_offer+ssp_offer+ HS_GPA + age + female + english + dad_HS_grad + dad_college_grad 
                    + mom_HS_grad + mom_college_grad + uni_first_choice + finish_in_4_yrs + grad_degree + live_home + work_plans + last_min, data)
summary(model_1_29_2025)
print(vcov(model_1_29_2025))

sum(model_1_29_2025$residuals^2)/((1006-17)*(1-0.019)*(var(data$sfp_offer))*(var(data$ssp_offer)))

sum(modelP3Q5_controls_5$residuals^2)/((1006-7)*(var(data$sfp_offer))*(var(data$ssp_offer)))

my_function <- function(x = names(model_1_29_2025$coefficients)) { # create a function with the name my_function
  x = paste(x, "= 0")
}
x = sapply(columns_to_select, my_function)
typeof(x)
y = c(1, 2)
typeof(y)
linearHypothesis(model_1_29_2025, c(
  "HS_GPA = 0",
  "age = 0",
  "female = 0",
  "english = 0",
  "finish_in_4_yrs = 0"
))

linearHypothesis(model_1_29_2025, x)

test_r2 = lm(sfp_offer ~ HS_GPA + age + female + english + dad_HS_grad + dad_college_grad 
             + mom_HS_grad + mom_college_grad + uni_first_choice + finish_in_4_yrs + grad_degree + live_home + work_plans + last_min, data = data)
summary(test_r2)

model_1_29_2025_2 = lm(first_sem_grade ~ sfp_offer+ssp_offer+ HS_GPA + age + female + english
                       + dad_college_grad+ uni_first_choice + finish_in_4_yrs + grad_degree + live_home + work_plans + last_min, data)
summary(model_1_29_2025_2)
print(vcov(model_1_29_2025_2))

test_r2_2 = lm(ssp_offer ~ HS_GPA + age + female + english + dad_HS_grad + dad_college_grad 
             + mom_HS_grad + mom_college_grad + uni_first_choice + finish_in_4_yrs + grad_degree + live_home + work_plans + last_min, data = data)
summary(test_r2_2)
