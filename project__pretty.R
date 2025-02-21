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
options(digits = 3, scipen = 999)

### PART 2 - DESCRIPTION OF THE DATA & ALLOCATION BALANCE ###
########################################################
# --- PART 2 - QUESTION 3 (P2Q3) --------------------------------

# Filter the data based on treatment groups
sfp = filter(data, sfp_offer == 1)
ssp = filter(data, ssp_offer == 1)
control = filter(data, control == 1)

# NOTE: first column is student id and is not a metric

# Calculate column means for each treatment group
sfp_means = round(colMeans(sfp), 3)
names(sfp_means) = names(sfp)
ssp_means = round(colMeans(ssp), 3)
names(ssp_means) = names(ssp)
control_means =round(colMeans(control), 3)
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
stargazer(means_table,  type = "text", title = "Table 1: means of groups", # Naming the table
          out = "Table1.html") # Naming the file

# --- QUESTION 3 (P2Q3) --------------------------------
# Subset data for specific treatment groups (SSP and Control)
data_P2Q3 = subset(data, data$ssp_offer == 1 | data$control==1)

# Fit a linear model for the effect of various variables on SSP offer
model1 = lm(ssp_offer ~ HS_GPA + age + female + english +
              dad_HS_grad + dad_college_grad + mom_HS_grad + 
              mom_college_grad + uni_first_choice + 
              finish_in_4_yrs + grad_degree + live_home + 
              work_plans, data_P2Q3)


# Perform White's test
white_test_result <- white_test(model1)

# Extract values from white test
test_statistic <- white_test_result$w_stat
p_value <- white_test_result$p_value

# Bind values to a table
white_table = rbind(c("H0: Residuals are homoskedastic\t|", "H1: Heteroskedasticity of the residuals"), c("test_statistic", "p_value") , round(c(test_statistic, p_value), 3)) 

# Output the table to a html file for the document 
stargazer(white_table, summary = FALSE, type = "html", title = "White Test for Heteroskedasticity (model 1) ", out = "white_test_1.html")####################################

# Model fix for heteroskedasticity 
model1_fixed = coeftest(model1, vcov = vcovHC(model1, type = "HC1"))

# Output the results to an HTML table
htmlreg(model1_fixed, file = "Table2.html", # Naming the file
        custom.columns = c("החותך", descriptions), # Add descriptions column
        digits = 2, stars = c(0.01, 0.05, 0.1),    # Check significance for 0.01, 0.05, and 0.1
        custom.model.names = c("Table 2: Contrasts by treatment of SSP"), # Naming The table
        include.rsquared = TRUE) # Include in the table the R^2 

# Test the hypothesis that all coefficients are zero
# That way we prove ssp_offer has nothing to do with background variables
lh1 <- linearHypothesis(model1, c(
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


lh1_df <- as.data.frame(lh1)

# Rename rows to model names
rownames(lh1_df) = c("Restricted model: sfp ~ b0 + u"  , "Unrestricted model: ssp_offer ~ HS_GPA + age + female + english +..." )

# Use stargazer to output the hypothesis test table as HTML
stargazer(lh1_df, summary = FALSE, type = "html",
          title = "Linear Hypothesis (1) Test Results",
          out = "HypothesisTable1.html")


### PART 3 - ANALYSIS OF SFP & SSP TREATMENT AFFECTS ###
########################################################
# --- QUESTION 4 (P3Q4) --------------------------------

# Fit a model for first semester grades based on treatment groups
model2 = lm(first_sem_grade ~ ssp_offer + sfp_offer, data)

# Check for homoscedasticity using White's test - Homoscedasticity is preserved in the regression 
white_test(model2)

# Output model to html for the document.
htmlreg(model2, file = "Table3.html", # Naming the file
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
model_3 = lm(first_sem_grade ~ sfp_offer + ssp_offer + HS_GPA
                         + age + female + english + dad_HS_grad + dad_college_grad + mom_HS_grad + mom_college_grad +  uni_first_choice + finish_in_4_yrs + grad_degree + live_home + work_plans  + last_min +sfp_signup + ssp_signup , data)

# Output model to html for the document.
htmlreg(model_3, file = "Table4.html",  # Naming the file
        digits = 2,, stars = c(0.01, 0.05, 0.1), # Check significance for 0.01, 0.05, and 0.1
        custom.model.names = c("Table 4 - Model 1"), #Naming the table
        include.rsquared = TRUE ) # Include in the table the R^2 

# Perform White's test
white_test_result <- white_test(model_3)

# Extract values
test_statistic <- white_test_result$w_stat
p_value <- white_test_result$p_value

# Bind the values to a table
white_table = rbind(c("H0: Residuals are homoskedastic\t|", "H1: Heteroskedasticity of the residuals"), c("test_statistic", "p_value") , round(c(test_statistic, p_value), 3)) 

# Export test output to a html file for the document
stargazer(white_table, summary = FALSE, type = "html", title = "White Test for Heteroskedasticity (model 3) ", out = "white_test_3.html")####################################

# Fix heteroskedasticity using robust standard errors
model_3_robust = coeftest(model_3, vcov = vcovHC(model_3, type = "HC1"))

# Robust standard errors in the model
robust_se = vcovHC(model_3, type = "HC1")

# Output model to html with fixed heteroskedasticity
htmlreg(model_3_robust, file = "Table5.html", # Naming the file
        digits = 2, stars = c(0.01, 0.05, 0.1), # Check significance for 0.01, 0.05, and 0.1
        custom.model.names = c("Table 5 - Model 1 with fixed heteroskedasticity"), # Naming the table
        include.rsquared = TRUE ) # Include in the table the R^2 

# Check for multicollinearity in parental education variables
stargazer(vcov(model_3)[c(8,9,10,11),c(8,9,10,11)], type = "text", title = "Table 6: vcov-matrix", out = "Table6.html")

# Test if parental education variables can be jointly zero
lh2 <- linearHypothesis(model_3, c(
  "dad_HS_grad = 0",
  "dad_college_grad = 0",
  "mom_HS_grad = 0",
  "mom_college_grad = 0"),
  vcov. = robust_se)

lh2_df <- as.data.frame(lh2)

# Rename row names to model names
colnames(lh2_df) <- c("Res.Df", "Df", "F", "Pr(>F)")
rownames(lh2_df) = c("Restricted model"  , "Unrestricted model." )

# Use stargazer to output the hypothesis test table as HTML
stargazer(lh2_df, summary = FALSE, type = "html",
          title = "Linear Hypothesis (2) Test Results",
          out = "HypothesisTable2.html")

# Only age, HS_GPA, female, english and finish_in_4_yrs are significant alone
# Are all the other at least significant together?

# Test the joint significance of non-significant variables
lh3 <- linearHypothesis(model_3_robust, c(
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


lh3_df <- as.data.frame(lh3)
# Rename rows to model names
rownames(lh3_df) = c("Restricted model: first_sem_grade ~ sfp_offer + ssp_offer + HS_GPA +age + female + english + finish_in_4_yrs"  , "Unrestricted model." )


# Use stargazer to output the hypothesis test table as HTML
stargazer(lh3_df, summary = FALSE, type = "html",
          title = "Linear Hypothesis (3) Test Results",
          out = "HypothesisTable3.html")

### They are not significant, so they are most likely irrelevant ###
###            Let's see if any variables are correlated         ###
print(vcov(model_3))

### No visible covariance between variables ###

# --- New model significant variables only ---
model_4 =  lm(first_sem_grade ~ sfp_offer + ssp_offer + HS_GPA +age + female + english + finish_in_4_yrs, data)


# Perform White's test
white_test_result <- white_test(model_4)

# Extract values ########################
test_statistic <- white_test_result$w_stat
p_value <- white_test_result$p_value

# Bind values to a table
white_table = rbind(c("H0: Residuals are homoskedastic\t|", "H1: Heteroskedasticity of the residuals"), c("test_statistic", "p_value") , round(c(test_statistic, p_value), 3)) 

# Output the table to a html file for the document 
stargazer(white_table, summary = FALSE, type = "html", title = "White Test for Heteroskedasticity (model 4) ", out = "white_test_4.html")####################################

# Fix heteroskedasticity using robust standard errors
model_4_robust = coeftest(model_4, vcov = vcovHC(model_4, type = "HC1"))

# Define the covariance and summary functions
vcov = vcovHC(model_4, type = "HC1") ###################!!!!!!!!!!!!!!!!!!!!!!

# Model without 'finish_in_4_yrs'
model_4_without_finish_in_4_yrs =  lm(first_sem_grade ~ sfp_offer + 
                              ssp_offer + HS_GPA + age + 
                              female + english , data)

# Perform White's test
white_test_result <- white_test(model_4_without_finish_in_4_yrs)

# Extract values
test_statistic <- white_test_result$w_stat
p_value <- white_test_result$p_value

# Bind values to a table
white_table = rbind(c("H0: Residuals are homoskedastic\t|", "H1: Heteroskedasticity of the residuals"), c("test_statistic", "p_value") , round(c(test_statistic, p_value), 3)) 

# Output the table to a html file for the document 
stargazer(white_table, summary = FALSE, type = "html", title = "White Test for Heteroskedasticity (model 4 without finish_in_4_yrs) ", out = "white_test_4.5.html")

# Robust standard errors in the model
model_4_without_finish_in_4_yrs_robust = coeftest(model_4_without_finish_in_4_yrs, 
                                        vcov = vcovHC(model_4_without_finish_in_4_yrs, type = "HC1"))

# We observe a bit lower st.d and higher R2, p-value close to 0.1
# Decision to keep 'finish_in4_yrs' based on statistical and theoretical rationale in document

# Check covariance of coefficients
stargazer(vcov(model_4)[2:8, 2:8], # Choose covariance variables to observe
          type = "text", title = "Table 8: vcov-matrix 2_2", # Naming the table
          out = "Table8.html") # Naming the file

# Robust standard errors in the model
model_4_robust_se = vcovHC(model_4, type = "HC1")

# Test significance of joint significance of variables
lh4 <- linearHypothesis(model_4, c(
  "HS_GPA = 0",
  "ssp_offer = 0",
  "sfp_offer = 0",
  "finish_in_4_yrs = 0",
  "female = 0",
  "english = 0",
  "age = 0"),
  vcov. = model_4_robust_se)

lh4_df <- as.data.frame(lh4)
# Optionally, rename columns to more user‐friendly names
colnames(lh4_df) <- c("Res.Df", "Df", "F", "Pr(>F)")
rownames(lh4_df) = c("Restricted model"  , "Unrestricted model" )

# Use stargazer to output the hypothesis test table as HTML
stargazer(lh4_df, summary = FALSE, type = "html",
          title = "Linear Hypothesis (4) Test Results",
          out = "HypothesisTable4.html")

# Compute robust standard errors correctly for each model
se_model1_HC1 <- coeftest(model_3, vcov = vcovHC(model_3, type = "HC1"))[, 2]
se_model2_HC1 <- coeftest(model_4, vcov = vcovHC(model_4, type = "HC1"))[, 2]
se_model3_HC1 <- coeftest(model_4_without_finish_in_4_yrs, vcov = vcovHC(model_4_without_finish_in_4_yrs, type = "HC1"))[, 2]

# Nested table of all models.
htmlreg(list(model_3, model_3, 
             model_4, model_4, 
             model_4_without_finish_in_4_yrs, model_4_without_finish_in_4_yrs), file = "Table7.html", # Naming the file
        digits = 4, stars = c(0.001, 0.05,0.1), # Check significance for 0.01, 0.05, and 0.1,
        caption = "Table 7 models with and without insignificant variables",
        caption.above = TRUE,
        custom.header = list("Full model" = 1:2, "Only significant variables" = 3:4, "Without finish_in_4_yrs" = 5:6),
        custom.model.names = c("Normal Sd. Errors", "HC1", "Normal Sd. Errors", "HC1", "Normal Sd. Errors", "HC1"), # Naming the table
        override.se =  list(summary(model_3)$coefficients[, 2], se_model1_HC1, summary(model_4)$coefficients[, 2], se_model2_HC1, summary(model_4_without_finish_in_4_yrs)$coefficients[, 2], se_model3_HC1),
        include.rsquared = TRUE ) # Include in the table the R^2 




### The effects of all variables are significantly not 0 together, and so are relevant and can be kept in the model ###

model_controls = coeftest(model_4, vcov = vcovHC(model_4, type = "HC1")) # We will save the fixed model for Q7

# --- QUESTION 6 (P3Q6) --------------------------------

# The null hypothesis H0: ssp_signup = sfp_signup
# The alternative hypothesis H1 : ssp_signup != sfp_signup
lh5 <- linearHypothesis(model_4, c("sfp_offer = ssp_offer"), vcov. = model_4_robust_se)

lh5_df <- as.data.frame(lh5)
# Optionally, rename columns to more user‐friendly names
colnames(lh5_df) <- c("Res.Df", "Df", "F", "Pr(>F)")
rownames(lh5_df) = c("Restricted model"  , "Unrestricted model: first_sem_grade ~ sfp_offer + ssp_offer + HS_GPA +age + female + english + finish_in_4_yrs" )

# Use stargazer to output the hypothesis test table as HTML
stargazer(lh5_df, summary = FALSE, type = "html",
          title = "Linear Hypothesis (5) Test Results",
          out = "HypothesisTable5.html")

# F statistic is 0.077, we will not reject the null hypothesis at a = 0.1, sfp_offer is not ssp_offer

# --- QUESTION 7 (P3Q7) --------------------------------
# To save space in the document we will create 3 models
# And save the estimators of SFP and SSP to a table
# We already have model_controls for the first semester

# Define model for GPA over year1
model_5 = lm(GPA_year1 ~ sfp_offer + ssp_offer + HS_GPA +age + female + english + finish_in_4_yrs, data)

# Perform White's test
white_test_result <- white_test(model_5)

# Extract values
test_statistic <- white_test_result$w_stat
p_value <- white_test_result$p_value

# Bind values to a table
white_table = rbind(c("H0: Residuals are homoskedastic\t|", "H1: Heteroskedasticity of the residuals"), c("test_statistic", "p_value") , round(c(test_statistic, p_value), 3)) 

# Output the table to a html file for the document 
stargazer(white_table, summary = FALSE, type = "html", title = "White Test for Heteroskedasticity (model 6) ", out = "white_test_6.html")####################################

# Define model for GPA over year2
model_6 = lm(GPA_year2 ~ sfp_offer + ssp_offer + HS_GPA +age + female + english + finish_in_4_yrs, data)

# Perform White's test
white_test_result <- white_test(model_6)

# Extract values
test_statistic <- white_test_result$w_stat
p_value <- white_test_result$p_value

# Bind values to a table
white_table = rbind(c("H0: Residuals are homoskedastic\t|", "H1: Heteroskedasticity of the residuals"), c("test_statistic", "p_value") , round(c(test_statistic, p_value), 3)) 

# Output the table to a html file for the document 
stargazer(white_table, summary = FALSE, type = "html", title = "White Test for Heteroskedasticity (model 7) ", out = "white_test_7.html")####################################

model_6_robust_se = coeftest(model_6, vcov = vcovHC(model_6, type = "HC1"))

# Output all 3 models to a html file for the document 
stargazer(model_4, model_5, model_6,
          se = list(coeftest(model_4, vcov = vcovHC(model_4, type = "HC1"))[, 2] ,summary(model_5)$coefficients, coeftest(model_6, vcov = vcovHC(model_6, type = "HC1"))[, 2]),
          type = "html", 
          column.labels = c("Semester", "year1", "year 2"), # Model names
          title = "Table 9: Affects over time_Table",  # Title in Hebrew for IV estimation results
          out = "Table9.html") # Saves the output to an HTML file

#The variables are not significant after the first semester, but are they significant together?

# Are the variables significant together after year 2?
lh6_1 <- linearHypothesis(model_4, c("sfp_offer =0", "ssp_offer = 0"), vcov. =  vcovHC(model_4, type = "HC1"))
# No

# Rename rows to model names
lh6_1_df <- as.data.frame(lh6_1)
rownames(lh6_1_df) = c("Restricted model: first_sem_grade ~ HS_GPA +age + female + english + finish_in_4_yrs,"  , "Unrestricted model: first_sem_grade ~ sfp_offer + ssp_offer + HS_GPA +age + female + english + finish_in_4_yrs," )

# Use stargazer to output the hypothesis test table as HTML
stargazer(lh6_1_df,  summary = FALSE, type = "html",
          title = "Linear Hypothesis (6A) Test Results",
          out = "HypothesisTable6_1.html")

# Are the variables significant together after year 1?
lh6_2 <- linearHypothesis(model_5, c("sfp_offer =0", "ssp_offer = 0"))
# No

lh6_2_df <- as.data.frame(lh6_2)
# Rename rows to model names
rownames(lh6_2_df) = c("Restricted model: GPA_year1 ~ HS_GPA +age + female + english + finish_in_4_yrs,"  , "Unrestricted model:GPA_year1 ~ sfp_offer + ssp_offer + HS_GPA +age + female + english + finish_in_4_yrs," )

# Use stargazer to output the hypothesis test table as HTML
stargazer(lh6_2_df,  summary = FALSE, type = "html",
          title = "Linear Hypothesis (6B) Test Results",
          out = "HypothesisTable6_2.html")


# Are the variables significant together after year 2?
lh6_3 <- linearHypothesis(model_6, c("sfp_offer =0", "ssp_offer = 0"), vcov. =  vcovHC(model_6, type = "HC1"))
# No

lh6_3_df <- as.data.frame(lh6_3)
# Rename rows to model names
rownames(lh6_3_df) = c("Restricted model: GPA_year2 ~ HS_GPA +age + female + english + finish_in_4_yrs,"  , "Unrestricted model:GPA_year2 ~ sfp_offer + ssp_offer + HS_GPA +age + female + english + finish_in_4_yrs," )

# Use stargazer to output the hypothesis test table as HTML
stargazer(lh6_3_df,  summary = FALSE, type = "html",
          title = "Linear Hypothesis (6C) Test Results",
          out = "HypothesisTable6_3.html")


# Implications discussed in the document

# --- QUESTION 8 (P3Q8) --------------------------------

#Adding dummy variable - 1 if HS GPA is above the median, 0 otherwise
data["abv_med"] = ifelse(data$HS_GPA > median(data$HS_GPA), 1, 0)

# sub-setting the data to include only students allocated to SFP and Control groups
P3Q8_data = subset(data, data$sfp_offer == 1 | data$control == 1)

# Interaction model for students above median HS GPA
mdoel_7 = lm(first_sem_grade ~ sfp_offer + abv_med  + abv_med*sfp_offer , P3Q8_data)

# Check for heteroskedasticity
white_test(mdoel_7)
# homoscedasticity is NOT preserved when rounding down

# Hypothesis testing for interaction term
lh7 <- linearHypothesis(mdoel_7, c("sfp_offer:abv_med  = 0"))

lh7_df <- as.data.frame(lh7)
# Optionally, rename columns to more user‐friendly names
rownames(lh7_df) = c("Restricted model: first_sem_grade ~ sfp_offer + abv_med"  , "Unrestricted model:first_sem_grade ~ sfp_offer + abv_med  + sfp_offer:abv_med" )

# Use stargazer to output the hypothesis test table as HTML
stargazer(lh7_df,  summary = FALSE, type = "html",
          title = "Linear Hypothesis (7) Test Results",
          out = "HypothesisTable7.html")


# Output model to html
htmlreg(mdoel_7, file = "Table10.html", # Naming the file
        custom.columns = #Adding custom column of descriptions of the variables
          c("Intercept", "Dummy variable for belonging to SFP group", 
            "Dummy variable for having HS GPA above median", "Interaction dummy variable"),
        digits = 2, stars = c(0.001, 0.05,0.1), # Check significance for 0.01, 0.05, and 0.1
        custom.model.names = c("Table 10: How SFP Effects grades for students with or without GPA above median"), # Naming the table
        include.rsquared = TRUE ) # Include in the table the R^2 



### PART 4 - ANALYSIS OF INVOLVEMENT IN TREATMENT AFFECTS ###
#############################################################

# --- QUESTION 9 (P4Q9) --------------------------------

# Summary statistics for students who accepted vs. rejected SFP
filtered_data = filter(data, sfp_offer == 1 | control == 1 )

# Filter the data to include only rows where either sfp_offer or control is 1

# Calculate mean for students who accepted the SFP (sfp_signup = 1)
mean_accepted = colMeans(filter(filtered_data, sfp_signup == 1 & sfp_offer == 1))

# Calculate mean for students who rejected the SFP (sfp_signup = 0)
mean_rejected = colMeans(filter(filtered_data, sfp_signup == 0 & sfp_offer == 1))

# Set column names to match the original data for both mean_accepted and mean_rejected
names(mean_accepted) = names(data)
names(mean_rejected) = names(data)

# Combine the mean values for rejected and accepted students into a table
means_table2 = cbind(mean_rejected, mean_accepted)

# Add "abv_med" column to the selection of columns to include in the final table
columns_to_select = append(columns_to_select, "abv_med")

# Select only the columns specified in 'columns_to_select'
means_table2 = means_table2[columns_to_select,]

# Rename the columns to clearly indicate which group (rejected or accepted) the means represent
colnames(means_table2)=c("rejected the sfp program","accpted the sfp program")

# Use stargazer to generate a formatted table in HTML format
stargazer(means_table2, type = "html", 
          title = "Table 11: Differances between backgrounds variables in acceptance and rejection subsets",  # Naming the table
          out = "Table11.html") # Naming the file

# --- QUESTION 10 (P4Q10) --------------------------------
# Filter the data for students who received SFP offer or no offer at all, in other words
# filter out students who got offered to participate in the SSP treatment
data_q10 = filter(data, ssp_offer == 0)

# Fit a linear regression model to estimate the effect of "sfp_signup" on "first_sem_grade"
model_8 = lm(first_sem_grade ~ sfp_signup,data_q10)

# Perform the White test for heteroscedasticity
white_test(model_8)

# Perform White's test
white_test_result <- white_test(model_8)

# Extract values
test_statistic <- white_test_result$w_stat
p_value <- white_test_result$p_value

# Bind values to a table
testable = rbind(c("H0: Residuals are homoskedastic\t|", "H1: Heteroskedasticity of the residuals"), c("test_statistic", "p_value") , round(c(test_statistic, p_value), 3)) 

# Output the table to a html file for the document 
stargazer(testable, summary = FALSE, type = "html", title = "White Test for Heteroskedasticity, model (8)", out = "white_test_8.html")

# Create a table summarizing the results of the regression in HTML format using stargazer
stargazer(model_8, 
          type = "html", 
          title = "Table 12: אמידת ההשפעה של השתתפות הטיפול על ממוצע הציונים",  # Naming the table
          out = "Table12.html") # Naming the file


# --- QUESTION 11 (P4Q11) --------------------------------

# First-stage regression: Estimate the effect of 'sfp_offer' on 'sfp_signup
model_9 = lm(sfp_signup ~ sfp_offer, filtered_data)

# We expect heteroskedasticity because this model is a LPM model
# Therefore, we use the White test to check for heteroscedasticity
# Perform White's test
white_test_result <- white_test(model_9)

# Extract values
test_statistic <- white_test_result$w_stat
p_value <- white_test_result$p_value

# Bind values to a table
white_table = rbind(c("H0: Residuals are homoskedastic\t|", "H1: Heteroskedasticity of the residuals"), c("test_statistic", "p_value") , round(c(test_statistic, p_value), 3)) 

# Output the table to a html file for the document 
stargazer(white_table, summary = FALSE, type = "html", title = "White Test for Heteroskedasticity (model 10) ", out = "white_test_10.html")####################################
# As expected we can reject H0

# Now we test the estimator for 'sfp_offer' to check if the covariance between 'sfp_offer' and 'sfp_signup' exists
# Null hypothesis (H0): sfp_offer coefficient = 0 (no relationship)
# Alternative hypothesis (H1): sfp_offer coefficient != 0 (there is a relationship)
lh8 <- linearHypothesis(model_9, "sfp_offer", vcov = vcovHC(model_9, type = "HC1")) #H0 : sfp_offer = 0 | H1 : sfp_offer != 0
# Create a custom line to add to the stargazer table
lh8_df <- as.data.frame(lh8)


#rename rows to more user‐friendly names
rownames(lh8_df) = c("Restricted model: sfp_offer = 0"  , "Unrestricted model: sfp_signup ~ sfp_offer" )

# Use stargazer to output the hypothesis test table as HTML
stargazer(lh8_df, summary = FALSE, type = "html",
          title = "Linear Hypothesis (8) Test Results",
          out = "HypothesisTable8.html")


# Use the first-stage model to predict the values of 'sfp_signup'
predicted_signup = predict(model_9)

# Second-stage regression: Estimate the effect of predicted 'sfp_signup' on 'first_sem_grade'
# This is part of the 2SLS (two-stage least squares) met

# The second-stage regression model uses the predicted values of 'sfp_signup' (from the first stage) as the independent variable
model_10 = lm(first_sem_grade ~ predicted_signup, filtered_data)

coeftest(model_9, vcov = vcovHC(model_9, type = "HC1"))
# Calculate HC1 robust standard errors for model_9
robust_se <- list(sqrt(diag(vcovHC(model_9, type = "HC1"))))


# Output the 2sls first stage and second stage models to a html file for the document 

stargazer(model_10, model_9, model_9,
          se = list(summary(model_10)$coefficients[, 2],summary(model_9)$coefficients[, 2], coeftest(model_9, vcov = vcovHC(model_9, type = "HC1"))[, 2]),
          type = "html", 
          column.labels = c("2nd Stage", "1st Stage", "1st Stage (HC1)"), # Model names
          title = "Table 12: IV אמידת",  # Title in Hebrew for IV estimation results
          out = "Table12.html") # Saves the output to an HTML file
