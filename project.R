library("ggplot2")
library("dplyr")
library("lmtest")
library("sandwich")
library("whitestrap")
data <- read.csv("term_paper_data.csv")
#check commit
sfp = filter(data, sfp_signup == 1)
ssp = filter(data, ssp_signup == 1)
control = filter(data, control == 1)

#NOTE: first column is student id and is not a metric
sfp_means = colMeans(sfp)
names(sfp_means) = names(sfp)
ssp_means = colMeans(ssp)
names(ssp_means) = names(ssp)
control_means = colMeans(control)
names(control_means) = names(control)
mean(sfp$GPA_year1)
sfp_means
ssp_means
control_means

#P2Q3
ssp_offer = filter(data, ssp_offer == 1)
model1 = lm(ssp_signup ~ HS_GPA + age + female + english + dad_HS_grad + dad_college_grad 
            + mom_HS_grad + mom_college_grad + uni_first_choice + finish_in_4_yrs + grad_degree + live_home + work_plans, ssp_offer)
coeftest(model1, vcov = vcovHC(model1, type = "HC1"))
ssp_offer_cor = filter(ssp_offer, age > 17 & age < 20)
model1_cor = lm(ssp_signup ~ HS_GPA + age + female + english + dad_HS_grad + dad_college_grad 
            + mom_HS_grad + mom_college_grad + uni_first_choice + finish_in_4_yrs + grad_degree + live_home + work_plans, ssp_offer_cor)
coeftest(model1_cor, vcov = vcovHC(model1_cor, type = "HC1"))

#Testing Push - 1/23/2025
