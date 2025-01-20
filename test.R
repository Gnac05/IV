library(readr)
credit_risk_dataset = read_csv("~/Applis/GMM3/Stat. Multi. Varié/project/credit_risk_dataset.csv")

head(credit_risk_dataset)


# Tester l'endogénéité dans le dataset Credit Risk
# Installer les packages nécessaires
if (!require("AER")) install.packages("AER")  # Pour les modèles IV
library(AER)

# Charger les données
data <- read.csv("~/Applis/GMM3/Stat. Multi. Varié/project/credit_risk_dataset.csv")

# Aperçu des données
head(data)
summary(data)

# Modèle de base sans variable instrumentale
basic_model <- lm(loan_status ~ loan_percent_income + person_income + person_age, data = data)
summary(basic_model)

# Modèle avec variables instrumentales
iv_model <- ivreg(loan_status ~ loan_percent_income + person_income + person_age | 
                    person_income + person_age + cb_person_cred_hist_length, data = data)
summary(iv_model)

# Effectuer le test de Durbin-Wu-Hausman
hausman_test <- summary(iv_model, diagnostics = TRUE)
print(hausman_test$diagnostics)
