# ============================================================
#   NHANES 2017–2018 DATA MERGE + CLEANING SCRIPT (R)
#   Loads DEMO, DIQ, BMX, GHB
#   Merges all files, creates diabetes variable (cleaned),
#   and builds final analysis dataset
# ============================================================

# Install packages if needed:
# install.packages(c("haven", "dplyr"))

library(haven)
library(dplyr)

# ------------------------------------------------------------
# 1. LOAD ALL 4 NHANES FILES
# ------------------------------------------------------------

# Adjust this folder path to where your XPT files are located
data_path <- "~/Desktop/nhanes_2017_2018"

demo <- read_xpt(file.path(data_path, "DEMO_J.XPT"))
diq  <- read_xpt(file.path(data_path, "DIQ_J.XPT"))
bmx  <- read_xpt(file.path(data_path, "BMX_J.XPT"))
ghb  <- read_xpt(file.path(data_path, "GHB_J.XPT"))

# ------------------------------------------------------------
# 2. MERGE ALL DATASETS BY SEQN
# ------------------------------------------------------------

nhanes <- demo %>%
  left_join(diq, by = "SEQN") %>%
  left_join(bmx, by = "SEQN") %>%
  left_join(ghb, by = "SEQN")

# ------------------------------------------------------------
# 3. FILTER TO ADULTS (>= 20 YEARS)
# ------------------------------------------------------------

nhanes <- nhanes %>%
  filter(RIDAGEYR >= 20)

# ------------------------------------------------------------
# 4. CREATE CLEAN DIABETES OUTCOME VARIABLE (Self-Reported)
# ------------------------------------------------------------

# DIQ010 codes:
# 1 = Yes
# 2 = No
# 3 = Borderline
# 7 = Don't know
# 9 = Refused

nhanes <- nhanes %>%
  mutate(
    diabetes = case_when(
      DIQ010 == 1 ~ 1,                 # Yes
      DIQ010 == 2 ~ 0,                 # No
      DIQ010 %in% c(3, 7, 9) ~ NA_real_  # Borderline / DK / Refused -> NA
    )
  )

# Check diabetes counts
print(table(nhanes$diabetes, useNA = "ifany"))

# ------------------------------------------------------------
# 5. BUILD FINAL ANALYSIS DATASET
# ------------------------------------------------------------

analysis_dat <- nhanes %>%
  transmute(
    diabetes = factor(diabetes),
    age      = RIDAGEYR,
    sex      = factor(RIAGENDR, labels = c("Male", "Female")),
    race     = factor(RIDRETH3),
    educ     = factor(DMDEDUC2),
    bmi      = BMXBMI,
    waist    = BMXWAIST,
    hba1c    = LBXGH
  ) %>%
  filter(!is.na(diabetes))

# ------------------------------------------------------------
# 6. REMOVE ANY REMAINING MISSING VALUES
# ------------------------------------------------------------

analysis_dat <- analysis_dat %>% na.omit()

# Final sample size
cat("Final sample size: ", nrow(analysis_dat), "participants\n")

# Peek at the final dataset
print(head(analysis_dat))

# ============================================================
#  FIGURES + MODELS FOR NHANES DIABETES PROJECT
#  Uses: analysis_dat (already created)
# ============================================================

# Install if needed:
# install.packages(c("ggplot2", "pROC", "randomForest"))

library(ggplot2)
library(pROC)
library(randomForest)
library(dplyr)

# Quick check
str(analysis_dat)
table(analysis_dat$diabetes)

# ------------------------------------------------------------
# 0. TRAIN / TEST SPLIT
# ------------------------------------------------------------

set.seed(123)
train_idx <- sample(seq_len(nrow(analysis_dat)),
                    size = 0.7 * nrow(analysis_dat))

train_dat <- analysis_dat[train_idx, ]
test_dat  <- analysis_dat[-train_idx, ]

# create numeric outcome for ROC (0/1)
test_dat$diab_num  <- as.numeric(test_dat$diabetes) - 1
train_dat$diab_num <- as.numeric(train_dat$diabetes) - 1

# ------------------------------------------------------------
# 1. FIGURE 1: Histogram of HbA1c by diabetes status
# ------------------------------------------------------------

ggplot(analysis_dat, aes(x = hba1c, fill = diabetes)) +
  geom_histogram(position = "identity", alpha = 0.5, bins = 30) +
  labs(title = "Distribution of HbA1c by Diabetes Status",
       x = "HbA1c (%)",
       y = "Count",
       fill = "Diabetes") +
  theme_minimal()

# ------------------------------------------------------------
# 2. FIGURE 2: Diabetes prevalence by age group
# ------------------------------------------------------------

analysis_dat <- analysis_dat %>%
  mutate(age_group = cut(age,
                         breaks = c(20, 39, 59, 79, Inf),
                         labels = c("20–39", "40–59", "60–79", "80+"),
                         right = FALSE))

prev_age <- analysis_dat %>%
  group_by(age_group) %>%
  summarize(prevalence = mean(as.numeric(diabetes) - 1),
            n = n())

ggplot(prev_age, aes(x = age_group, y = prevalence)) +
  geom_col() +
  scale_y_continuous(labels = scales::percent) +
  labs(title = "Prevalence of Self-Reported Diabetes by Age Group",
       x = "Age group",
       y = "Diabetes prevalence") +
  theme_minimal()

# ------------------------------------------------------------
# 3. FIGURE 3: Boxplot of BMI by diabetes status
# ------------------------------------------------------------

ggplot(analysis_dat, aes(x = diabetes, y = bmi, fill = diabetes)) +
  geom_boxplot(alpha = 0.7) +
  labs(title = "BMI by Diabetes Status",
       x = "Diabetes",
       y = "BMI (kg/m²)") +
  theme_minimal() +
  theme(legend.position = "none")

# ------------------------------------------------------------
# 4. FIGURE 4: Scatterplot of BMI vs HbA1c, colored by diabetes
# ------------------------------------------------------------

ggplot(analysis_dat, aes(x = bmi, y = hba1c, color = diabetes)) +
  geom_point(alpha = 0.4) +
  labs(title = "Relationship Between BMI and HbA1c",
       x = "BMI (kg/m²)",
       y = "HbA1c (%)",
       color = "Diabetes") +
  theme_minimal()

# ------------------------------------------------------------
# 5. METHOD 1: Logistic Regression
# ------------------------------------------------------------

logit_fit <- glm(diabetes ~ age + sex + race + educ + bmi + waist + hba1c,
                 data   = train_dat,
                 family = binomial)

summary(logit_fit)

# predicted probabilities on test data
test_dat$logit_prob <- predict(logit_fit,
                               newdata = test_dat,
                               type = "response")

# ROC for logistic regression
roc_logit <- roc(response = test_dat$diab_num,
                 predictor = test_dat$logit_prob)

auc_logit <- auc(roc_logit)
auc_logit

# ------------------------------------------------------------
# 6. METHOD 2: Random Forest
# ------------------------------------------------------------

set.seed(123)
rf_fit <- randomForest(diabetes ~ age + sex + race + educ + bmi + waist + hba1c,
                       data = train_dat,
                       ntree = 500,
                       mtry  = 3,
                       importance = TRUE)

print(rf_fit)

# predicted probabilities on test data
test_dat$rf_prob <- predict(rf_fit,
                            newdata = test_dat,
                            type = "prob")[, "1"]

# ROC for random forest
roc_rf <- roc(response = test_dat$diab_num,
              predictor = test_dat$rf_prob)

auc_rf <- auc(roc_rf)
auc_rf

# ------------------------------------------------------------
# 7. FIGURE 5: ROC Curves (Logistic vs Random Forest)
# ------------------------------------------------------------

plot(roc_logit, col = "black", lwd = 2,
     main = "ROC Curves: Logistic Regression vs Random Forest")
plot(roc_rf, add = TRUE, col = "red", lwd = 2)
legend("bottomright",
       legend = c(
         paste0("Logistic (AUC = ", round(auc_logit, 3), ")"),
         paste0("Random Forest (AUC = ", round(auc_rf, 3), ")")
       ),
       col = c("black", "red"),
       lwd = 2)

# ------------------------------------------------------------
# 8. FIGURE 6: Random Forest Variable Importance
# ------------------------------------------------------------

varImpPlot(rf_fit,
           main = "Random Forest Variable Importance")

# ============================================================
# END OF SCRIPT
# ============================================================

