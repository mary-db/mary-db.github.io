# ------------------------------------------------------------------
# Analysis code for:
# "Dietary diversity as a modifier of the effect of supplementation
# with multiple micronutrients during pregnancy on low birth weight
# in a randomized controlled trial in Bangladesh"
# Mary de Boer et al., American Journal of Clinical Nutrition, 2025
#
# NOTE: This script is provided for transparency of statistical
# methods only. The underlying trial data (JiVitA-3, NCT00860470)
# are governed by a Data Use Agreement and are not publicly available.
# Data files referenced here (mddw.csv, aim3.csv, foods.csv) are
# NOT included in this repository.
# ------------------------------------------------------------------
rm(list=ls())

# Load necessary packages
library(ggplot2)
library(haven)
library(tidyverse)
library(stringr)
library(jtools)
library(multcomp)
library(xtable)
library(sandwich)
library(lmtest)
library(glm2)
library(foreign)
library(msm)
library(patchwork)
library(gee)
library(geepack)
library(sf)
library(here)
library(table1)


mddw <- read.csv(here("data", "mddw.csv"))
data <- read.csv(here("data", "aim3.csv"))
foods <- read.csv(here("data", "foods.csv"))
foods <- foods[, c(1:10)]
colnames(foods) <- c("Grains_Roots_Tubers", "Pulses", "Nuts", "Dairy", "Meat_Poultry_Fish", "Egg",
                     "DGLV", "Other_VitA_Rich", "Other_Veg", "Other_Fruit")


histograms <- lapply(names(foods[,1:10]), function(col_name) {
  # Generate histogram
  ggplot(foods, aes_string(x=col_name)) +
    geom_histogram(binwidth = , fill = "steelblue", color = "black") +
    ggtitle(paste("Servings of", col_name)) +
    theme_minimal() +
    labs(title = paste("Servings of", col_name), x = "Weekly Servings", y = "Frequency")
})


# Number of rows and columns for the layout
rows <- 2  # Number of rows
columns <- 5  # Number of columns

# Ensure there are at least as many plots as spaces in the grid
if (length(histograms) < rows * columns) {
  # Fill in with blank plots if not enough plots for the grid
  blank_plots <- lapply(1:(rows * columns - length(histograms)), function(x) {
    ggplot() + theme_void()  # create an empty plot
  })
  histograms <- c(histograms, blank_plots)  # append blank plots to the list
}

# Arrange plots in a 5x2 grid
histogram_grid <- wrap_plots(histograms, nrow = rows, ncol = columns)

# Display the grid
print(histogram_grid)


mddw %>% group_by(MDDW) %>% summarise(ave = mean(WDDS), sd = sd(WDDS), med = median(WDDS),
                                      Q1 = quantile(WDDS, (0.25)), Q3 = quantile(WDDS, 0.75))

sum(mddw$MDDW)/nrow(mddw)

mddw %>% group_by(MMS) %>% summarise (ave = mean(WDDS), sd = sd(WDDS), med = median(WDDS),
Q1 = quantile(WDDS, (0.25)), Q3 = quantile(WDDS, 0.75))

mddw2<-mddw

mddw2$sex <-
  factor(mddw2$sex, levels=c(1,0),
         labels=c("Female",
                  "Male"))

mddw2$bmicat <-
  factor(mddw2$bmicat, levels=c(0,1, 2, 3, 4, 5),
         labels=c("BMI < 18.5",
                  "18.5 <= BMI < 20",
                  "20 <- BMI < 22.5",
                  "22.5 <= BMI < 25",
                  "25 <= BMI < 27.5",
                  "BMI >= 27.5"))

mddw2$agecat <-
  factor(mddw2$agecat, levels=c(0,1, 2),
         labels=c("< 18 years old",
                  "18 - 35 years old",
                  "> 35 years old"))

mddw2$parcat <-
  factor(mddw2$parcat, levels=c(0,1, 2),
         labels=c("Nulliparous",
                  "One previous child",
                  "More than one child"))

mddw2$edcat <-
  factor(mddw2$edcat, levels=c(0,1, 2, 3),
         labels=c("No Education",
                  "Primary Education",
                  "9th Grade Education",
                  "More than 9th Grade"))

mddw2$MMS <-
  factor(mddw2$MMS, levels=c(0,1),
         labels=c("IFA",
                  "MMS"))


mddw2$ramadan <-
  factor(mddw2$ramadan, levels=c(0,1),
         labels=c("No",
                  "Yes"))

mddw2$MDDW <-
  factor(mddw2$MDDW, levels=c(0,1),
         labels=c("Ate < 5 Food Groups",
                  "Ate >= 5 Food Groups"))

head(mddw2)
names(mddw2)[names(mddw2) == 'ses'] <- "LSI"
names(mddw2)[names(mddw2) == 'bmicat'] <- "BMI"
names(mddw2)[names(mddw2) == 'parcat'] <- "Parity"
names(mddw2)[names(mddw2) == 'edcat'] <- "Education"
names(mddw2)[names(mddw2) == 'agecat'] <- "Age"
names(mddw2)[names(mddw2) == 'sex'] <- "Sex"
names(mddw2)[names(mddw2) == 'ramadan'] <- "Ramadan"

footnote <- "BMI = Body Mass Index (kg/m2),IFA = Iron Folic Acid Supplements,
LSI = Living Standards Index, MDD-W =  Minimum Dietary Diveristy - Women,
MMS = Multiple Micronutrient Supplements, WDDS = Women's Dietary Diversity Score"
custom_render <- function(x) {
  with(stats.default(x),
       c("", "Mean (SD)" = sprintf("%s (%s)",
                               round_pad(MEAN, 2),
                               round_pad(SD, 2)),

         "Median (Q1, Q3)" = sprintf("%s (%s, %s)",
                                       round_pad(MEDIAN, 2),
                                     round_pad(Q1, 2),
                                     round_pad(Q3, 2)))
  )
}


caption  <- "Participant Characteristics by Level of MDD-W; Weekly Consumption >= 4"
table1(~ Parity + Education+ BMI+Age + LSI + Sex + WDDS + MMS| MDDW, data=mddw2,
       overall="Total", caption=caption, footnote=footnote,
       render.continuous= custom_render,
       topclass="Rtable1-zebra")

class(mddw2$LSI)
caption2  <- "Table 1: Participant Characteristics by Level of WDDS"
table1(~ Parity + Education+ BMI+Age + LSI + Sex + Ramadan + MMS| WDDS, data=mddw2,
       overall="Total", caption=caption2, footnote=footnote,
       render.continuous=c(.="Mean (SD)", .="Median [Q1, Q3]"),
       topclass="Rtable1-zebra")

mddw %>% group_by(WDDS) %>% summarize(
  prop = n()/nrow(mddw)) %>%
  filter(WDDS >=7) %>% summarize(prop_high = sum(prop))

mddw %>% group_by(MDDW) %>% summarize(
  prop = n()/nrow(mddw))


#run the WDDS against the outcome logistically
#only with diet
# Fit the model
WDDS_Only <- geeglm(lbw ~ WDDS, id=sectorid, data=mddw,
                   corstr="exchangeable", family = binomial(link="log"))

# Summary of the model
summary(WDDS_Only)

# Extract coefficients and robust covariance matrix
coef_WDDS <- coef(WDDS_Only)
vcov_WDDS <- vcov(WDDS_Only)
se_WDDS <- sqrt(diag(vcov_WDDS))

# Compute exponentiated coefficients and their 95% confidence intervals
exp_coef_WDDS <- exp(coef_WDDS)
alpha95 <- 0.05  # 95% confidence level
z_score95 <- qnorm(1 - alpha95/2)

lower_bound_WDDS <- exp(coef_WDDS - z_score95 * se_WDDS)
upper_bound_WDDS <- exp(coef_WDDS + z_score95 * se_WDDS)

# Create a results table
results_table_WDDS <- data.frame(
  Term = names(coef_WDDS),
  Estimate = exp_coef_WDDS,
  `Lower 95% CI` = lower_bound_WDDS,
  `Upper 95% CI` = upper_bound_WDDS
)

# Print the results table
print(results_table_WDDS)


#add in the treatment
# Fit the model
WDDS_MMS <- geeglm(lbw ~ WDDS+MMS, id=sectorid, data=mddw,
                   corstr="exchangeable", family = binomial(link="log"))

# Summary of the model
summary(WDDS_MMS)

# Extract coefficients and robust covariance matrix
coef_WDDS_MMS <- coef(WDDS_MMS)
vcov_WDDS_MMS <- vcov(WDDS_MMS)
se_WDDS_MMS <- sqrt(diag(vcov_WDDS_MMS))

# Compute exponentiated coefficients and their 95% confidence intervals
exp_coef_WDDS_MMS <- exp(coef_WDDS_MMS)
lower_bound_WDDS_MMS <- exp(coef_WDDS_MMS - z_score95 * se_WDDS_MMS)
upper_bound_WDDS_MMS <- exp(coef_WDDS_MMS + z_score95 * se_WDDS_MMS)

# Create a results table
results_table_WDDS_MMS <- data.frame(
  Term = names(coef_WDDS_MMS),
  Estimate = exp_coef_WDDS_MMS,
  `Lower 95% CI` = lower_bound_WDDS_MMS,
  `Upper 95% CI` = upper_bound_WDDS_MMS
)

# Print the results table
print(results_table_WDDS_MMS)

#Adjusted model no interaction
#Fit the model

start1 <- coef(gee(formula=lbw~WDDS+MMS +bmicat + edcat, id=sectorid,
                   data=mddw,corstr="exchangeable", family = binomial(link="log")))

start1 <- c(start1, parcat=0)

start2 <- coef(gee(formula=lbw~WDDS+MMS +bmicat + edcat + parcat,
                   id=sectorid, data=mddw,corstr="exchangeable", family = binomial(link="log"),
                   b = start1))
start2 <- c(start2, ses=0)

start3 <- coef(gee(formula=lbw~WDDS+MMS +bmicat + edcat + parcat +ses,
                   id=sectorid, data=mddw,corstr="exchangeable", family = binomial(link="log"),
                   b = start2))
start3 <- c(start3, sex=0)

start4 <- coef(gee(formula=lbw~WDDS+MMS +bmicat + edcat + parcat +ses + sex,
                   id=sectorid, data=mddw,corstr="exchangeable", family = binomial(link="log"),
                   b = start3))
start4 <- c(start4, agecat=0)

WDDS_MMS_Adjust <- gee(formula=lbw~WDDS+MMS +bmicat + edcat+parcat + ses + sex +
                         agecat, id=sectorid, data=mddw,
                       corstr="exchangeable", family = binomial(link="log"),
                       b = start4)
# Extract coefficients and robust covariance matrix
coef_WDDS_MMS_Adjust <- coef(WDDS_MMS_Adjust)
vcov_WDDS_MMS_Adjust <- WDDS_MMS_Adjust$robust.variance
se_WDDS_MMS_Adjust <- sqrt(diag(vcov_WDDS_MMS_Adjust))

# Compute exponentiated coefficients and their 95% confidence intervals
exp_coef_WDDS_MMS_Adjust <- exp(coef_WDDS_MMS_Adjust)
lower_bound_WDDS_MMS_Adjust <- exp(coef_WDDS_MMS_Adjust - z_score95 * se_WDDS_MMS_Adjust)
upper_bound_WDDS_MMS_Adjust <- exp(coef_WDDS_MMS_Adjust + z_score95 * se_WDDS_MMS_Adjust)

# Create a results table
results_table_WDDS_MMS_Adjust <- data.frame(
  Term = names(coef_WDDS_MMS_Adjust),
  Estimate = exp_coef_WDDS_MMS_Adjust,
  `Lower 95% CI` = lower_bound_WDDS_MMS_Adjust,
  `Upper 95% CI` = upper_bound_WDDS_MMS_Adjust
)

# Print the results table
print(results_table_WDDS_MMS_Adjust)

#interaction model
# Fit the interaction model
Interact_NoAdjust <- geeglm(formula =lbw~WDDS*MMS, id=sectorid, data=mddw,
                   corstr="exchangeable", family = binomial(link="log"))

#Summary of the model
summary(Interact_NoAdjust)
waldtest(Interact_NoAdjust, terms="WDDS:MMS")


# Extract coefficients and robust covariance matrix
coef_interaction_NoA <- coef(Interact_NoAdjust)
vcov_interaction_NoA <-vcov(Interact_NoAdjust)
se_interaction_NoA <- sqrt(diag(vcov_interaction_NoA))

# Compute exponentiated coefficients and their 95% confidence intervals
exp_coef_NoA <- exp(coef_interaction_NoA)
lower_bound_NoA <- exp(coef_interaction_NoA - z_score95 * se_interaction_NoA)
upper_bound_NoA <- exp(coef_interaction_NoA + z_score95 * se_interaction_NoA)

# Create a results table
results_table_NoA <- data.frame(
  Term = names(coef_interaction_NoA),
  Estimate = exp_coef_NoA,
  `Lower 95% CI` = lower_bound_NoA,
  `Upper 95% CI` = upper_bound_NoA
)

# Print the results table
print(results_table_NoA)


# Fit the Adjusted interaction model
start1 <- coef(gee(formula=lbw~WDDS*MMS +bmicat + edcat, id=sectorid,
                   data=mddw,corstr="exchangeable", family = binomial(link="log")))

start1 <- c(start1, parcat=0)

start2 <- coef(gee(formula=lbw~WDDS*MMS +bmicat + edcat + parcat,
                   id=sectorid, data=mddw,corstr="exchangeable", family = binomial(link="log"),
                   b = start1))
start2 <- c(start2, ses=0)

start3 <- coef(gee(formula=lbw~WDDS*MMS +bmicat + edcat + parcat +ses,
                   id=sectorid, data=mddw,corstr="exchangeable", family = binomial(link="log"),
                   b = start2))
start3 <- c(start3, sex=0)

start4 <- coef(gee(formula=lbw~WDDS*MMS +bmicat + edcat + parcat +ses + sex,
                   id=sectorid, data=mddw,corstr="exchangeable", family = binomial(link="log"),
                   b = start3))
start4 <- c(start4, agecat=0)

Interact_Adjust <- gee(formula=lbw~WDDS*MMS +bmicat + edcat+parcat + ses +
                         sex + agecat,
                       id=sectorid, data=mddw,corstr="exchangeable", family = binomial(link="log"),
                       b = start4)

# Summary of the adjusted model with interaction
summary(Interact_Adjust)

# Extract coefficients and robust covariance matrix
coef_interaction <- coef(Interact_Adjust)
vcov_interaction <- Interact_Adjust$robust.variance
se_interaction <- sqrt(diag(vcov_interaction))

# Compute exponentiated coefficients and their 95% confidence intervals
exp_coef <- exp(coef_interaction)
lower_bound <- exp(coef_interaction - z_score95 * se_interaction)
upper_bound <- exp(coef_interaction + z_score95 * se_interaction)

# Create a results table
results_table <- data.frame(
  Term = names(coef_interaction),
  Estimate = exp_coef,
  `Lower 95% CI` = lower_bound,
  `Upper 95% CI` = upper_bound
)

# Print the results table
print(results_table)

# Manual Wald Test for interaction terms
# Find the indices for the interaction terms
interaction_term <- "WDDS:MMS"
interaction_index <- grep("WDDS:MMS", names(coef_interaction))

# Extract the interaction coefficients and their covariance matrix
interaction_coef <- coef_interaction[interaction_index]
interaction_vcov <- vcov_interaction[interaction_index, interaction_index]
interaction_se <- sqrt(interaction_vcov)

# Compute the confidence intervals for the interaction terms with 90% CI
lower_bound_interaction_95 <- exp(interaction_coef - z_score95 * interaction_se)
upper_bound_interaction_95 <- exp(interaction_coef + z_score95 * interaction_se)

# Compute Wald Test statistic
wald_statistic <- (interaction_coef / interaction_se)^2
p_value <- 1 - pchisq(wald_statistic, df = 1)

# Create a results table for the interaction term
interaction_results_table <- data.frame(
  Term = interaction_term,
  Estimate = exp(interaction_coef),
  `Lower 95% CI` = lower_bound_interaction_95,
  `Upper 95% CI` = upper_bound_interaction_95,
  `Wald Test Statistic` = wald_statistic,
  `P-value` = p_value
)

# Print the interaction results table
print(interaction_results_table)


# Fit the Adjusted interaction model with the MDDW term instead
start1 <- coef(gee(formula=lbw~MDDW*MMS +bmicat + edcat, id=sectorid,
                   data=mddw,corstr="exchangeable", family = binomial(link="log")))

start1 <- c(start1, parcat=0)

start2 <- coef(gee(formula=lbw~MDDW*MMS +bmicat + edcat + parcat,
                   id=sectorid, data=mddw,corstr="exchangeable", family = binomial(link="log"),
                   b = start1))
start2 <- c(start2, ses=0)

start3 <- coef(gee(formula=lbw~MDDW*MMS +bmicat + edcat + parcat +ses,
                   id=sectorid, data=mddw,corstr="exchangeable", family = binomial(link="log"),
                   b = start2))
start3 <- c(start3, sex=0)

start4 <- coef(gee(formula=lbw~MDDW*MMS +bmicat + edcat + parcat +ses + sex,
                   id=sectorid, data=mddw,corstr="exchangeable", family = binomial(link="log"),
                   b = start3))
start4 <- c(start4, agecat=0)

MDDW_Interact_Adjust <- gee(lbw ~ MDDW*MMS +bmicat + edcat+parcat + ses + sex + agecat,
                               id=sectorid, data=mddw,corstr="exchangeable", family = binomial(link="log"),
                               b = start4)


# Extract coefficients and robust covariance matrix
coef_interaction <- coef(MDDW_Interact_Adjust)
vcov_interaction <- MDDW_Interact_Adjust$robust.variance
se_interaction <- sqrt(diag(vcov_interaction))

# Compute exponentiated coefficients and their 95% confidence intervals
exp_coef <- exp(coef_interaction)
lower_bound <- exp(coef_interaction - z_score95 * se_interaction)
upper_bound <- exp(coef_interaction + z_score95 * se_interaction)

# Create a results table
results_table <- data.frame(
  Term = names(coef_interaction),
  Estimate = exp_coef,
  `Lower 95% CI` = lower_bound,
  `Upper 95% CI` = upper_bound
)

# Print the results table
print(results_table)

# Find the indices for the interaction terms
interaction_term <- "MDDW:MMS"
interaction_index <- grep("MDDW:MMS", names(coef_interaction))

# Extract the interaction coefficients and their covariance matrix
interaction_coef <- coef_interaction[interaction_index]
interaction_vcov <- vcov_interaction[interaction_index, interaction_index]
interaction_se <- sqrt(interaction_vcov)

# Compute the confidence intervals for the interaction terms with 90% CI
lower_bound_interaction_95 <- exp(interaction_coef - z_score95 * interaction_se)
upper_bound_interaction_95 <- exp(interaction_coef + z_score95 * interaction_se)

# Compute Wald Test statistic
wald_statistic <- (interaction_coef / interaction_se)^2
p_value <- 1 - pchisq(wald_statistic, df = 1)

# Create a results table for the interaction term
interaction_results_table <- data.frame(
  Term = interaction_term,
  Estimate = exp(interaction_coef),
  `Lower 95% CI` = lower_bound_interaction_95,
  `Upper 95% CI` = upper_bound_interaction_95,
  `Wald Test Statistic` = wald_statistic,
  `P-value` = p_value
)

# Print the interaction results table
print(interaction_results_table)


mddw <- mddw %>% mutate(MDDW4 = if_else(WDDS >=4, 1, 0),
                        MDDW6 = if_else(WDDS >=6, 1, 0))


# Fit the Adjusted interaction model with the MDDW term instead
start1 <- coef(gee(formula=lbw~MDDW6*MMS +bmicat + edcat, id=sectorid,
                   data=mddw,corstr="exchangeable", family = binomial(link="log")))

start1 <- c(start1, parcat=0)

start2 <- coef(gee(formula=lbw~MDDW6*MMS +bmicat + edcat + parcat,
                   id=sectorid, data=mddw,corstr="exchangeable", family = binomial(link="log"),
                   b = start1))
start2 <- c(start2, ses=0)

start3 <- coef(gee(formula=lbw~MDDW6*MMS +bmicat + edcat + parcat +ses,
                   id=sectorid, data=mddw,corstr="exchangeable", family = binomial(link="log"),
                   b = start2))
start3 <- c(start3, sex=0)

start4 <- coef(gee(formula=lbw~MDDW6*MMS +bmicat + edcat + parcat +ses + sex,
                   id=sectorid, data=mddw,corstr="exchangeable", family = binomial(link="log"),
                   b = start3))
start4 <- c(start4, agecat=0)

MDDW6_Interact_Adjust <- gee(lbw ~ MDDW6*MMS +bmicat + edcat+parcat + ses + sex + agecat,
                            id=sectorid, data=mddw,corstr="exchangeable", family = binomial(link="log"),
                            b = start4)


# Extract coefficients and robust covariance matrix
coef_interaction <- coef(MDDW6_Interact_Adjust)
vcov_interaction <- MDDW6_Interact_Adjust$robust.variance
se_interaction <- sqrt(diag(vcov_interaction))

# Compute exponentiated coefficients and their 95% confidence intervals
exp_coef <- exp(coef_interaction)
lower_bound <- exp(coef_interaction - z_score95 * se_interaction)
upper_bound <- exp(coef_interaction + z_score95 * se_interaction)

# Create a results table
results_table <- data.frame(
  Term = names(coef_interaction),
  Estimate = exp_coef,
  `Lower 95% CI` = lower_bound,
  `Upper 95% CI` = upper_bound
)

# Print the results table
print(results_table)

# Find the indices for the interaction terms
interaction_term <- "MDDW6:MMS"
interaction_index <- grep("MDDW6:MMS", names(coef_interaction))

# Extract the interaction coefficients and their covariance matrix
interaction_coef <- coef_interaction[interaction_index]
interaction_vcov <- vcov_interaction[interaction_index, interaction_index]
interaction_se <- sqrt(interaction_vcov)

# Compute the confidence intervals for the interaction terms with 90% CI
lower_bound_interaction_95 <- exp(interaction_coef - z_score95 * interaction_se)
upper_bound_interaction_95 <- exp(interaction_coef + z_score95 * interaction_se)

# Compute Wald Test statistic
wald_statistic <- (interaction_coef / interaction_se)^2
p_value <- 1 - pchisq(wald_statistic, df = 1)

# Create a results table for the interaction term
interaction_results_table <- data.frame(
  Term = interaction_term,
  Estimate = exp(interaction_coef),
  `Lower 95% CI` = lower_bound_interaction_95,
  `Upper 95% CI` = upper_bound_interaction_95,
  `Wald Test Statistic` = wald_statistic,
  `P-value` = p_value
)

# Print the interaction results table
print(interaction_results_table)

#MMS Only in this subsample
# Fit the model
MMS_Only <- geeglm(lbw ~ MMS, id=sectorid, data=mddw,
                    corstr="exchangeable", family = binomial(link="log"))

# Summary of the model
summary(MMS_Only)

# Extract coefficients and robust covariance matrix
coef_MMS <- coef(MMS_Only)
vcov_MMS <- vcov(MMS_Only)
se_MMS <- sqrt(diag(vcov_MMS))

# Compute exponentiated coefficients and their 95% confidence intervals
exp_coef_MMS <- exp(coef_MMS)
alpha95 <- 0.05  # 95% confidence level
z_score95 <- qnorm(1 - alpha95/2)

lower_bound_MMS <- exp(coef_MMS - z_score95 * se_MMS)
upper_bound_MMS <- exp(coef_MMS + z_score95 * se_MMS)

# Create a results table
results_table_MMS <- data.frame(
  Term = names(coef_MMS),
  Estimate = exp_coef_MMS,
  `Lower 95% CI` = lower_bound_MMS,
  `Upper 95% CI` = upper_bound_MMS
)

# Print the results table
print(results_table_MMS)

#Interpret WDDS
# https://stats.oarc.ucla.edu/r/faq/how-can-i-explain-a-continuous-by-continuous-interaction/
at.WDDS <- c(0,1,2,3,4,5,6,7,8,9,10)
names(coef_interaction)
length(coef_interaction)
SEs <- coef_interaction_NoASEs <- rep(NA, length(at.WDDS))

for (i in 1:length(at.WDDS)){
  j <- at.WDDS[i]
  SEs[i] <- deltamethod (~ (x3) + (x10)*j, coef_interaction, vcov_interaction)
}

slopes <- coef_interaction[["MMS"]]+coef_interaction[["WDDS:MMS"]]*at.WDDS
upper <- slopes + 1.96*SEs
upper <- exp(upper)
lower <- slopes - 1.96*SEs
lower <- exp(lower)
slopes <- exp(slopes)
levels <- cbind(at.WDDS, slopes, lower, upper)
levels

#make plot of WDDS vs MMS RR
plot(at.WDDS, slopes, ylim = c(0.7, 1.2), xlim = c(0, 10), type = "l", lty = 1, xlab = "Dietary Diversity Score",
     ylab = "Marginal Effect of MMS on LBW", cex.axis = 1.5, cex.lab = 1.5)
axis(1, at = seq(0, 10, by = 1), labels = TRUE, cex.axis = 1.5)
points(at.WDDS, upper, type = "l", lty = 2, col = "steelblue")
points(at.WDDS, lower, type = "l", lty = 2, col = "steelblue")
points(at.WDDS, rep(1, length(at.WDDS)), type = "l", col = "gray")
title("Relative Risk of LBW in Women Taking MMS vs IFA \n by Dietary Diversity Score", cex.main = 1.5)

legend("bottomright", c("RR LBW", "95% CI", "Null Effect"),
       lty =c(1, 2, 1), col =c("black", "steelblue", "gray"))


#other descriptions of WDDS
quantile(mddw$WDDS)
hist(mddw$WDDS)

# To check multicollinearity
library('car')
library("quantmod")
library("MASS")
library("corrplot")
vif_values <- vif(adjustWDDS)
barplot(vif_values, main = "VIF Values", horiz = TRUE, col = "steelblue") #create horizontal bar chart to display each VIF value

abline(v = 5, lwd = 3, lty = 2)    #add vertical line at 5 as after 5 there is severe correlation

##descriptive stats regarding scores

# Define the columns
levels <- c(1:10)

# Calculate the proportions and round them to three decimal places
prevalence <- sapply(levels, function(col) {
  round(length(which(mddw$WDDS== levels[[col]])) / length(mddw$WDDS), 3)
})

# Create the table
prevalence_table <- data.frame(
  DDS = levels,
  Prevalence = prevalence
)

# Print the table
print(prevalence_table)

# Minimum detectable difference calculation
# Calculate proportions for each level
proportions <- prevalence * 0.5

# Calculate variances for each level
variances <- proportions * (1 - proportions)

# Calculate the total variance
total_variance <- sum(variances)

# Define the constants
Z_alpha_half <- 1.96
Z_beta <- 0.84
n <- length(mddw$WDDS)

# Calculate the log of the minimum detectable relative risk of interaction
log_RR_interaction <- sqrt(((Z_alpha_half + Z_beta)^2 * total_variance) / n)

# Calculate the relative risk of interaction
RR_interaction <- exp(log_RR_interaction)

# Print the results
cat("Log of the RR interaction:", log_RR_interaction, "\n")
cat("RR interaction:", RR_interaction, "\n")


# Define the columns
columns <- c("GRT", "Pulses", "Nuts", "Dairy", "MPF", "Egg", "DGLV", "OVitA", "OVeg", "OFruit")

# Calculate the proportions and round them to three decimal places
props <- sapply(columns, function(col) {
  round(length(which(mddw[[col]] == 1)) / length(mddw$WDDS), 3)
})

# Create the table
proportions_table <- data.frame(
  #Category = columns,
  Proportion = props
)

# Print the table
print(proportions_table)

