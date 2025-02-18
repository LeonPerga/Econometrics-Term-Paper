# Load required libraries
library("ggplot2")    # for data visualization
library("dplyr")      # for data manipulation
library("lmtest")     # for hypothesis testing
library("sandwich")   # for heteroskedasticity-robust standard errors
library("whitestrap") # for White's test
library("car")        # for linear hypothesis testing
library("stargazer")  # for creating regression tables
library("texreg")     # for creating regression tables in LaTeX
library("broom")      # for tidying up model outputs
library("lfe")        # for fixed-effects regression models

# Read the data from a CSV file
data <- read.csv("term_paper_data.csv")

# Set options for output formatting
options(digits = 4, scipen = 999)

### PART 2 - DESCRIPTION OF THE DATA & ALLOCATION BALANCE ###
########################################################
# --- PART 2 - QUESTION 3 (P2Q3) --------------------------------

# Filter the data based on treatment groups
sfp = filter(data, sfp_offer == 1)
ssp = filter(data, ssp_offer == 1)
control = filter(data, control == 1)

# NOTE: first column is student id and is not a metric

# Calculate column means for each treatment group
sfp_means = colMeans(sfp)
names(sfp_means) = names(sfp)
ssp_means = colMeans(ssp)
names(ssp_means) = names(ssp)
control_means = colMeans(control)
names(control_means) = names(control)

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

# Create a table combining means from different treatment group
means_table = cbind(t(t(sfp_means)), t(t(ssp_means)), t(t(control_means)))
means_table = means_table[columns_to_select,] # Select relevant columns
means_table = cbind(descriptions, means_table) # Add descriptions column
colnames(means_table) = c("descriptions", "SFP", "SSP", "Control") # Rename columns


# Output the table of means to an HTML file
stargazer(means_table, type = "text", title = "Table 1: means of groups",  out = "Table1.html")

# --- QUESTION 3 (P2Q3) --------------------------------
# Subset data for specific treatment groups (SSP and Control)
data_P2Q3 = subset(data, data$ssp_offer == 1 | data$control==1)

# Fit a linear model for the effect of various variables on SSP offer
model1 = lm(ssp_offer ~ HS_GPA + age + female + english +
              dad_HS_grad + dad_college_grad + mom_HS_grad + 
              mom_college_grad + uni_first_choice + 
              finish_in_4_yrs + grad_degree + live_home + 
              work_plans, data_P2Q3)

# Model fix for heteroskedasticity 
model1_fixed = coeftest(model1, vcov = vcovHC(model1, type = "HC1"))

# Output the results to an HTML table
htmlreg(model1_fixed, file = "Table2.html",
        custom.columns = c("החותך", descriptions), # Add descriptions column
        digits = 2, stars = c(0.01, 0.05, 0.1),    # Check significance for 0.01, 0.05, and 0.1
        custom.model.names = c("Table 2: Contrasts by treatment of SSP"), # Naming The table
        include.rsquared = TRUE) # Include in the table the R^2 

# Test the hypothesis that all coefficients are zero
# That way we prove ssp_offer has nothing to do with background variables
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
# --- QUESTION 4 (P3Q4) --------------------------------

# Fit a model for first semester grades based on treatment groups
model2 = lm(first_sem_grade ~ ssp_offer + sfp_offer, data)
summary(model2)

# Check for homoscedasticity using White's test - Homoscedasticity is preserved in the regression 
white_test(model2)

# Output model to html for the document.
htmlreg(model2, file = "Table3.html", 
        custom.columns = c( #Adding custom column of descriptions of the variables
          "Intercept",
          "Dummy variable for belonging to SSP group", 
          "Dummy variable for belonging to SFP group"),
        digits = 2, , stars = c(0.01, 0.05, 0.1), # Check significance for 0.01, 0.05, and 0.1
        custom.model.names = #Naming the table
          c("Table 3: Intent of Treatment Effects on First Year Outcomes in the Sample with Fall Grades"),
        include.rsquared = TRUE ) # Include in the table the R^2 

# --- QUESTION 5 (P3Q5) --------------------------------

# Fit a model with all variables as controls
model_test_controls = lm(first_sem_grade ~ sfp_offer + ssp_offer + HS_GPA
                         + age + female + english + dad_HS_grad + 
                           dad_college_grad + mom_HS_grad + mom_college_grad + 
                           uni_first_choice + finish_in_4_yrs + grad_degree + 
                           live_home + work_plans  + last_min +sfp_signup + ssp_signup , data)

# Output model to html for the document.
htmlreg(model_test_controls, file = "Table4.html", 
        digits = 2,, stars = c(0.01, 0.05, 0.1), # Check significance for 0.01, 0.05, and 0.1
        custom.model.names = c("Table 4 - Model 1"), #Naming the table
        include.rsquared = TRUE ) # Include in the table the R^2 

# Test for heteroskedasticity
white_test(model_test_controls)

# Fix heteroskedasticity using robust standard errors
model_test_controls_robust = coeftest(model_test_controls, vcov = vcovHC(model_test_controls, type = "HC1"))

# Robust standard errors in the model
robust_se = vcovHC(model_test_controls, type = "HC1")

# Output model to html with fixed heteroskedasticity
htmlreg(model_test_controls_robust, file = "Table5.html", digits = 2, stars = c(0.01, 0.05, 0.1), custom.model.names = c("Table 5 - Model 1 with fixed heteroskedasticity") , include.rsquared = TRUE )

# Check for multicollinearity in parental education variables
stargazer(vcov(model_test_controls)[c(8,9,10,11),c(8,9,10,11)], type = "text", title = "Table 6: vcov-matrix", out = "Table6.html")

# Test if parental education variables can be jointly zero
linearHypothesis(model_test_controls, c(
  "dad_HS_grad = 0",
  "dad_college_grad = 0",
  "mom_HS_grad = 0",
  "mom_college_grad = 0"),
  vcov. = robust_se)

# Only age, HS_GPA, female, english and finish_in_4_yrs are significant alone
# Are all the other at least significant together?

# Test the joint significance of non-significant variables
linearHypothesis(model_test_controls, c(
  "dad_HS_grad = 0",
  "dad_college_grad = 0",
  "mom_HS_grad = 0",
  "mom_college_grad = 0",
  "uni_first_choice = 0",
  "grad_degree = 0",
  "live_home = 0",
  "work_plans = 0",
  "last_min = 0",
  "sfp_signup = 0",
  "ssp_signup = 0"),
  vcov. = robust_se)

### They are not significant, so they are most likely irrelevant ###
###            Let's see if any variables are correlated         ###
print(vcov(model_test_controls))

### No visible covariance between variables ###

# --- New model significant variables only ---
model_test_controls_2 =  lm(first_sem_grade ~ sfp_offer + ssp_offer + HS_GPA +age + female + english + finish_in_4_yrs, data)

# Check for heteroskedasticity
white_test(model_test_controls_2)

# Fix heteroskedasticity using robust standard errors
model_test_controls_2_robust = coeftest(model_test_controls_2, vcov = vcovHC(model_test_controls_2, type = "HC1"))


# Define the covariance and summary functions
vcov = vcovHC(model_test_controls_2, type = "HC1") ###################!!!!!!!!!!!!!!!!!!!!!!

# Output the model to HTML for the document
htmlreg(model_test_controls_2, file = "Table7.html", 
        digits = 2,, stars = c(0.01, 0.05, 0.1), # Check significance for 0.01, 0.05, and 0.1
        custom.model.names = c("Table 7 - Model 2"), #Naming the table
        include.rsquared = TRUE )#Include in the table the R^2

# Output the fixed\robust model to HTML for the document
htmlreg(model_test_controls_2_robust, file = "Table8.html", 
        digits = 2,, stars = c(0.01, 0.05, 0.1), # Check significance for 0.01, 0.05, and 0.1
        custom.model.names = c("Table 8 - Model 2 Fixed heteroskedasticity"),  #Naming the table
        include.rsquared = TRUE ) # Include in the table the R^2

# Model without 'finish_in4_yrs'
model_test_controls_3 =  lm(first_sem_grade ~ sfp_offer + 
                              ssp_offer + HS_GPA + age + 
                              female + english , data)

# Output model to html for the document
htmlreg(model_test_controls_3, file = "Table9.html", 
        digits = 2, stars = c(0.01, 0.05, 0.1), # Check significance for 0.01, 0.05, and 0.1
        custom.model.names = c("Table 9 - Model 3"), # Naming the table
        include.rsquared = TRUE ) # Include in the table the R^2 

# Check for heteroskedasticity
white_test(model_test_controls_3)

# Robust standard errors in the model
model_test_controls_3_robust = coeftest(model_test_controls_3, 
                                        vcov = vcovHC(model_test_controls_2, type = "HC1"))

htmlreg(model_test_controls_3_robust, file = "Table10.html", digits = 2, stars = c(0.01, 0.05, 0.1), custom.model.names = c("Table 10 - Model 3 Fixed heteroskedasticity"), include.rsquared = TRUE )
summary(model_test_controls_3)

# We observe a bit lower st.d and higher R2, p-value close to 0.1
# Decision to keep 'finish_in4_yrs' based on statistical and theoretical rationale in document
 
# Check covariance of coefficients
print(vcov(model_test_controls_2))
stargazer(vcov(model_test_controls_2)[2:8, 2:8], type = "text", title = "Table 11: vcov-matrix 2_2", out = "Table11.html")

# Robust standard errors in the model
robust_se_2 = vcovHC(model_test_controls_2, type = "HC1")

# Test significance of joint significance of variables
linearHypothesis(model_test_controls_2, c(
  "HS_GPA = 0",
  "ssp_offer = 0",
  "sfp_offer = 0",
  "finish_in_4_yrs = 0",
  "female = 0",
  "english = 0",
  "age = 0"),
  vcov. = robust_se_2)

### The effects of all variables are significantly not 0 together, and so are relevant and can be kept in the model ###

mode_controls = coeftest(model_test_controls_2, vcov = vcovHC(model_test_controls_2, type = "HC1"))

# --- QUESTION 6 (P3Q6) --------------------------------

# The null hypothesis H0: ssp_signup = sfp_signup
# The alternative hypothesis H1 : ssp_signup != sfp_signup
linearHypothesis(model_test_controls_2, c("sfp_offer = ssp_offer"), vcov. = robust_se_2)
# F statistic is 0.077, we will not reject the null hypothesis at a = 0.1, sfp_offer is not ssp_offer

# --- QUESTION 7 (P3Q7) --------------------------------
# Define model for GPA over year1
model4 = lm(GPA_year1 ~ sfp_offer + ssp_offer + HS_GPA +age + female + english + finish_in_4_yrs, data)
# White test
white_test(model4)
# homoscedasticity is preserved


linearHypothesis(model4, c("sfp_offer =0", "ssp_offer = 0"))


summary(model4)

# Define model for GPA over year2
model5 = lm(GPA_year2 ~ sfp_offer + ssp_offer + HS_GPA +age + female + english + finish_in_4_yrs, data)

# White test
white_test(model5)
# homoscedasticity is NOT preserved
model5_robust = coeftest(model5, vcov = vcovHC(model5, type = "HC1"))

linearHypothesis(model5, c("sfp_offer =0", "ssp_offer = 0"), vcov. =  vcovHC(model5, type = "HC1"))

# Create summary table of treatment effects
affects_table = 
  rbind(Treatment = c("SFP", "std. Error" , "SSP",  "std. Error" ), 
        Fall_Semester = c(unname(model_test_controls_2$coefficients[2]),   mode_controls[2, "Std. Error"] , unname(model_test_controls_2$coefficients[3]), mode_controls[3, "Std. Error"]  ),
        Year_1 = c(unname(model4$coefficients[2]), summary(model4)$coefficients[2, "Std. Error"], unname(model4$coefficients[3]),  summary(model4)$coefficients[3, "Std. Error"]),
        Year_2 = c(unname(model5$coefficients[2]), model5_robust[1, "Std. Error"] , unname(model5$coefficients[3]), model5_robust[1, "Std. Error"]))

# Output table to html for the document
stargazer(affects_table, type = "text", title = "Table 12: Affects over time_Table",  out = "Table12.html")

# --- QUESTION 8 (P3Q8) --------------------------------
# subsetting the data to include only students allocated to SFP and Control groups
P3Q8_data = subset(data, data$sfp_offer == 1 | data$control == 1)

#Adding dummy variable - 1 if HS GPA is above the median, 0 otherwise
P3Q8_data["abv_med"] = ifelse(P3Q8_data$HS_GPA > median(P3Q8_data$HS_GPA), 1, 0)

# Interaction model for students above median HS GPA
model_q8 = lm(first_sem_grade ~ sfp_offer + abv_med  + abv_med*sfp_offer , P3Q8_data)

# Check for heteroskedasticity
white_test(model_q8)
# homoscedasticity is NOT preserved
model_q8_robust = coeftest(model_q8, vcov = vcovHC(model5, type = "HC1"))

# Hypothesis testing for interaction term
linearHypothesis(model_q8, c("sfp_offer:abv_med  = 0"), robust = TRUE)

# Output model to html
htmlreg(model_q8_robust, file = "Table13.html", custom.columns = c("Intercept", "Dummy variable for belonging to SFP group", "Dummy variable for having HS GPA above median", "Interaction dummy variable"),
        digits = 2, stars = c(0.001, 0.05,0.1), custom.model.names = c("Table 13: How SFP Effects grades for students with or without GPA above median"), include.rsquared = TRUE )



### PART 4 - ANALYSIS OF INVOLVEMENT IN TREATMENT AFFECTS ###
#############################################################

# --- QUESTION 9 (P4Q9) --------------------------------
# Summary statistics for students who accepted vs. rejected SFP
data["abv_med"] = ifelse(data$HS_GPA > median(data$HS_GPA), 1, 0)
filtered_data = filter(data, sfp_offer == 1 | control == 1 )
mean_accepted = colMeans(filter(filtered_data, sfp_signup == 1 & sfp_offer == 1))
mean_rejected = colMeans(filter(filtered_data, sfp_signup == 0 & sfp_offer == 1))
names(mean_accepted) = names(data)
names(mean_rejected) = names(data)
means_table2 = cbind(mean_rejected, mean_accepted)
columns_to_select = append(columns_to_select, "abv_med")
means_table2 = means_table2[columns_to_select,]
colnames(means_table2)=c("rejected the sfp program","accpted the sfp program")
summary(means_table2)
stargazer(means_table2, type = "html", title = "Table 14: Differances between backgrounds variables in acceptance and rejection subsets",  out = "Table14.html")

linearHypothesis(model_q8, c("sfp_offer + sfp_offer:abv_med = 0"), robust = TRUE)

# --- QUESTION 10 (P4Q10) --------------------------------
data_q10 = filter(data, ssp_offer == 0)

modelq10 = lm(first_sem_grade ~ sfp_signup,data_q10)

white_test(modelq10)
summary(modelq10)
stargazer(modelq10, type = "html", title = "Table 15: אמידת ההשפעה של השתתפות הטיפול על ממוצע הציונים",  out = "Table15.html")


# --- QUESTION 11 (P4Q11) --------------------------------

#H0 : cov(sfp_offer,sfp_signup) = 0 
stargazer(linearHypothesis(modelq11p1, "sfp_offer=0"),type = "html", title = "Table 16: SFP בדיקת מתאם בין הרשמה להשתתפות בתכנית  ",  out = "Table16.html")

# First-stage regression
modelq11p1 = lm(sfp_signup ~ sfp_offer, filtered_data)
summary(modelq11p1)
cov(filtered_data$sfp_offer, filtered_data$sfp_signup)
linearHypothesis(modelq11p1, "sfp_offer") #H0 : cov(sfp_offer,sfp_signup) = 0
coeftest(modelq11p1, vcov = vcovHC(modelq11p1, type = "HC1"))
predicted_signup = predict(modelq11p1)

# Second-stage regression
modelq11p2 = lm(first_sem_grade ~ predicted_signup, filtered_data)
summary(modelq11p2)

# Output first and second stage results
stargazer(modelq11p1, type = "html", title = "Table 17: שלב ראשון",  out = "Table17.html")
stargazer(coeftest(modelq11p1, vcov = vcovHC(modelq11p1, type = "HC1")), type = "html", title = "Table 18: שלב ראשון עם תיקון לשונות",  out = "Table18.html")
stargazer(modelq11p2, type = "html", title = "Table 19: IV אמידת",  out = "Table19.html")

print("Done!")

