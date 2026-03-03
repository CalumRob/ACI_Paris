library(ranger)
library(tidyverse)
library(data.table)
library(randomForest)
library(caret)
#library(e1071)
library("missForest")

setwd("D:/Users/Calum/Documents/La_Rochelle/ACI_2025/Amenities-Paris")

utrecht_dir <- "D:/Users/Calum/Documents/La_Rochelle/Second_paper/Fixe/Fixe/Utrecht"


ACI_data <- fread("ACI_all_rolling.csv")

ACI_data <- fread("ACI_all_rolling_RCA.csv")

ACI_data$rank2 = rank(ACI_data$Complexity)

#MOR_together_long <- fread("aci_df_cleanamen_15min.csv")

tapply(ACI_data$rank, ACI_data$Year, summary)


origins <- fread("R5/building_as_origins.csv")


Paris_newpop <- read.csv("Detailed_P16_POP.csv",
                         colClasses = c("IRIS" = "character")) %>% rename(CODE_IRIS = IRIS)




all_data <- fread("all_cov_data.csv")

origins <- left_join(origins,
                     fread(paste0(utrecht_dir,"/Airbnb_within_500m_origins_all_years.csv")) %>%rename(id=origin_id))


ACI_joined <- left_join(left_join(ACI_data %>% mutate(id = as.character(id)),
                                  origins %>% mutate(id = as.character(id))) %>% 
                          mutate(code_iris = as.character(code_iris)), 
                        all_data %>%mutate(CODE_IRIS=as.character(CODE_IRIS)), by = c("code_iris"="CODE_IRIS"))


names(ACI_joined) <- gsub(x = names(ACI_joined), pattern = "-", replacement = "_")

Revenus_21 <- fread("Revenus/BASE_TD_FILO_IRIS_2021_DISP.csv") %>% select(IRIS, DISP_MED21, DISP_GI21)

ACI_joined <- left_join(ACI_joined, Revenus_21, by = c("code_iris" = "IRIS"))%>%
  mutate(DISP_MED13 = as.numeric(DISP_MED13),
         DISP_MED16 = as.numeric(DISP_MED16),
         DISP_MED19 = as.numeric(DISP_MED19),
         DISP_MED21 = as.numeric(DISP_MED21),
         DISP_GI21 = as.numeric(gsub(",",".", DISP_GI21))) %>% 
           filter(!is.na(DISP_MED13) & !is.na(DISP_MED16) & !is.na(DISP_MED19) & !is.na(DISP_MED21) & !(DISP_MED21 == "ns"))

inf_1316 = mean(ACI_joined$DISP_MED16) / mean(ACI_joined$DISP_MED13)
inf_1319 = mean(ACI_joined$DISP_MED19) / mean(ACI_joined$DISP_MED13)
inf_1321 = mean(ACI_joined$DISP_MED21) / mean(ACI_joined$DISP_MED13)

ACI_joined <- ACI_joined %>% group_by(id, code_iris) %>%
  mutate(Income = ifelse(Year == 2014, DISP_MED13,
                         ifelse(Year == 2017, DISP_MED16 / inf_1316,
                                ifelse(Year == 2020, DISP_MED19 / inf_1319,
                                       ifelse(Year == 2023, DISP_MED21 / inf_1321, NA))))) %>%
  ungroup()

ACI_joined$Income = as.numeric(ifelse(ACI_joined$Income == "ns",NA,ACI_joined$Income))
nrow(ACI_joined[is.na(ACI_joined$Income),])/nrow(ACI_joined)


ACI_joined <- ACI_joined %>% group_by(id, code_iris) %>%
  mutate(Gini = ifelse(Year == 2014, DISP_GI13,
                         ifelse(Year == 2017, DISP_GI16,
                                ifelse(Year == 2020, DISP_GI19,
                                       ifelse(Year == 2023, DISP_GI21, NA))))) %>%
  ungroup()

ACI_joined$Gini = gsub(",",".", ACI_joined$Gini)
ACI_joined$Gini = as.numeric(ifelse(ACI_joined$Gini == "ns",NA,ACI_joined$Gini))
nrow(ACI_joined[is.na(ACI_joined$Gini),])/nrow(ACI_joined)


infSQM_1 = mean(ACI_joined$median_sqmp_2017_2019, na.rm = T) / mean(ACI_joined$median_sqmp_2014_2016, na.rm = T)
infSQM_2 = mean(ACI_joined$median_sqmp_2020_2022, na.rm = T) / mean(ACI_joined$median_sqmp_2014_2016, na.rm = T)


ACI_joined <- ACI_joined %>% group_by(id, code_iris) %>%
  mutate(SQM_Price_Mean = ifelse(Year == 2014, median_sqmp_2014_2016,
                       ifelse(Year == 2017, median_sqmp_2014_2016,
                              ifelse(Year == 2020, median_sqmp_2017_2019 / infSQM_1,
                                     ifelse(Year == 2023, median_sqmp_2020_2022 / infSQM_2, NA))))) %>%
  ungroup()

nrow(ACI_joined[is.na(ACI_joined$SQM_Price_Mean),])/nrow(ACI_joined)


tapply(ACI_joined$SQM_Price_Mean, ACI_joined$Year, summary)

#ACI_joined$Income = (ACI_joined$DISP_MED13 + ACI_joined$DISP_MED16 + ACI_joined$DISP_MED19) / 3
#ACI_joined$Gini = (ACI_joined$DISP_GI13 + ACI_joined$DISP_GI16 + ACI_joined$DISP_GI19) / 3

# ACI_joined = ACI_joined %>% 
#   mutate(SQM_Sum = median_sqmp_2014_2016 + median_sqmp_2017_2019 + median_sqmp_2020_2022,
#          noNA1 = ifelse(!is.na(median_sqmp_2014_2016),1,0),
#          noNA2 = ifelse(!is.na(median_sqmp_2017_2019),1,0),
#          noNA3 = ifelse(!is.na(median_sqmp_2020_2022),1,0),
#          SQM_Price_Mean = SQM_Sum / (noNA1 + noNA2 + noNA3))


summary(ACI_joined$SQM_Price_Mean)

ACI_joined <- ACI_joined %>%
  mutate(sumRes = Res_14 + Res_15 + Res_16 + Res_17 + Res_18 + Res_19 + Res_20,
         sumNb = Nb_14 + Nb_15 + Nb_16 + Nb_17 + Nb_18 + Nb_19 + Nb_20)


ACI_wide <- ACI_joined %>% 
  select(-c(rank, yearly_rank, Complexity_unscaled, diversity, ubiquity, rank2)) %>% 
  pivot_wider(., names_from = "Year", values_from = "Complexity", names_prefix = "Year")


ACI_forest <- ACI_wide %>% select(id,code_iris, Year2023,
                                    Year2020, Year2017, Year2014,Income, Gini, SQM_Price_Mean,
                                    P13_RP_ACH19, P13_RP,
                                    P19_CHOM1564,
                                    P19_RP, P19_LOG, P19_RP_LOCHLMV, 
                                    Res_14,Res_15,Res_16,Res_17,
                                    Res_18,Res_19,Res_20,Res_21,
                                    Nb_14,Nb_15,Nb_16,Nb_17,
                                    Nb_18,Nb_19,Nb_20,Nb_21,
                                    DISP_MED13, DISP_MED16, DISP_MED19,
                                    DISP_GI13, DISP_GI16, DISP_GI19,
                                    P14_POP, P17_POP, P19_POP,
                                    P19_MEN_ANEM10P, P13_MEN_ANEM10P,
                                    median_sqmp_2014_2016,median_sqmp_2017_2019,
                                    median_sqmp_2020_2022, C19_ACT1564_CS3,C19_ACT1564_CS4,
                                    P19_POP2539)

ACI_forest$CS_34 = (ACI_forest$C19_ACT1564_CS4 + ACI_forest$C19_ACT1564_CS3) /
  (ACI_forest$P19_POP - ACI_forest$P19_CHOM1564)

ACI_forest$ACH19_on_RP = ACI_forest$P13_RP_ACH19 / ACI_forest$P13_RP

ACI_forest_static <- ACI_forest %>%
  pivot_longer(.,cols = c("Year2014","Year2017","Year2020","Year2023"),names_to = "Year",
               values_to = "Complexity") %>%
  group_by(id, Year) %>%
  mutate(Income_2 = ifelse(Year == "Year2014",DISP_MED13,
                           ifelse(Year=="Year2017",DISP_MED16,DISP_MED19)),
         Gini = ifelse(is.na(DISP_GI13)==T,Gini,
                       ifelse(Year == "Year2014",DISP_GI13,
                              ifelse(Year=="Year2017",DISP_GI16,DISP_GI19))),
         Res = ifelse(Year == "Year2014", Res_14 + Res_15,
                      ifelse(Year == "Year2017", Res_16 + Res_17 + Res_18,
                             Res_19 + Res_20 + Res_21)),
         Nb = ifelse(Year == "Year2014", Nb_14 + Nb_15,
                     ifelse(Year == "Year2017", Nb_16 + Nb_17 + Nb_18,
                            Nb_19 + Nb_20 + Nb_21)),
         Population =ifelse(Year == "Year2014", P14_POP,
                            ifelse(Year == "Year2017", P17_POP,
                                   P19_POP)),
         AMEN_10P = ifelse(Year == "Year2014", P13_MEN_ANEM10P,P19_MEN_ANEM10P),
         Income = ifelse(is.na(Income_2) == T, Income, Income_2),
         SQM_Price = ifelse(Year == "Year2014", median_sqmp_2014_2016,
                            ifelse(Year == "Year2017", median_sqmp_2017_2019,
                                   median_sqmp_2020_2022)),
         SQM_Price = ifelse(!is.na(SQM_Price), SQM_Price,
                            SQM_Price_Mean),
         Inc_per_pop = Income/Population,
         Inc_times_pop = Income*Population) %>%
  ungroup()

ACI_forest_static[is.na(ACI_forest_static$Res),]$Res = 0
ACI_forest_static[is.na(ACI_forest_static$Nb),]$Nb = 0

View(cor(ACI_forest_static %>% select_if(is.numeric),
         ACI_forest_static$Complexity,
         use = "pairwise.complete.obs"))



library("margins")

margins <- margins_summary(lm)
(m <- margins(lm))
cplot(lm,"P13_RP_ACH19", what = "prediction")
# Splitting into train and test
# --- 1. Your train_test_split function (looks good!) ---
train_test_split <- function(data, percentage) {
  data_with_row_id <- data %>%
    mutate(id = row_number())
  
  # set.seed outside the function call is usually preferred for full script reproducibility
  # but having it here ensures this specific function call is reproducible if called independently.
  # For this script, we'll set it globally before calling.
  # set.seed(1234)
  training_data <- data_with_row_id %>%
    sample_frac(percentage)
  test_data <- anti_join(
    data_with_row_id,
    training_data,
    by = "id"
  )
  
  training_data$id <- NULL
  test_data$id <- NULL
  
  return(list(training_data, test_data))
}

# --- 2. Data Preprocessing (Do this ONCE) ---
set.seed(1234) # Set seed for reproducibility of the entire process

ACI_processed <- ACI_forest_static %>%
  mutate(
    # ACH19_ON_RP = P13_RP_ACH19 / P19_RP, # Make sure P19_RP is not zero
    Young = P19_POP2539 / Population    # Make sure Population is not zero
  ) %>%
  filter(!is.na(Income) & !is.na(Gini) & !is.na(Young)) %>% # Add other NAs you want to filter
  select(
    Complexity, Income, Gini, Res, Year, SQM_Price_Mean, Young
      #, ACH19_on_RP # Add this back if you use it
  ) %>%
  # It's good practice to remove any remaining NAs or impute them
  # ranger can handle NAs, but explicit handling is often better.
  na.omit() # Or use an imputation strategy

# Check if any data remains after filtering
if(nrow(ACI_processed) == 0) {
  stop("No data remaining after preprocessing and NA filtering. Check your filters and NA values.")
}
if(nrow(ACI_processed) < 20) { # Arbitrary small number
  warning("Very few data points remaining after preprocessing. Model might not be reliable.")
}

split_data <- train_test_split(ACI_processed, 0.8)
training_data <- split_data[[1]]
test_data <- split_data[[2]]

print(paste("Training data dimensions:", dim(training_data)))
print(paste("Test data dimensions:", dim(test_data)))

numeric_cols_train <- ACI_processed %>% select_if(is.numeric)
if ("Complexity" %in% names(numeric_cols_train) && ncol(numeric_cols_train) > 1) {
  cor_matrix <- cor(numeric_cols_train %>% select(-Complexity), # Predictors
                    numeric_cols_train$Complexity,             # Target
                    use = "pairwise.complete.obs", method = "spearman")
  print("Correlation with Complexity:")
  print(cor_matrix)
} else {
  warning("Could not compute correlations. Ensure 'Complexity' and other numeric predictors are present.")
}

# Make sure there are enough predictors if mtry is hardcoded
num_predictors <- ncol(training_data) - 1 # -1 for the outcome variable
chosen_mtry <- 3

rf_ranger <- ranger(
  formula = Complexity ~ .,
  data = training_data,
  mtry = chosen_mtry, # Number of variables to possibly split at in each node.
  # Default is floor(sqrt(number of predictors)) for classification,
  # floor(number of predictors / 3) for regression.
  # You can tune this parameter.
  num.trees = 500,
  importance = "permutation", # "impurity" is faster but can be biased. "permutation" is generally better.
  oob.error = TRUE,          # Calculate OOB error
  # keep.inbag = TRUE,        # Only if you need in-bag information later, adds memory overhead
  seed = 1234# For reproducibility of the ranger model itself
)

as.data.table(rf_ranger$variable.importance.local)[,Year := training_data$Year][,lapply(.SD,mean),by=Year]

# OOB Prediction Error (MSE for regression)
print(paste("OOB MSE:", rf_ranger$prediction.error))
# R-squared (OOB)
print(paste("OOB R-squared:", rf_ranger$r.squared)) # This is 1 - (OOB MSE / Variance of Y)


# --- 7. Feature Importance ---
importance_scores <- rf_ranger$variable.importance # Gets the named vector of importance scores
print("Feature Importance Scores:")
print(importance_scores)

importance_df <- data.frame(
  Variable = names(importance_scores),
  Importance = importance_scores
) %>%
  arrange(desc(Importance))



ggplot(importance_df, aes(x = reorder(Variable, Importance), y = Importance)) +
    geom_bar(stat = "identity", fill = "steelblue") +
    coord_flip() + # Horizontal bars
    labs(title = "Feature Importance (Permutation)",
         x = "Variable",
         y = "Mean Decrease in OOB MSE") +
    theme_minimal()


# --- 8. Evaluate on Test Data (Important!) ---
predictions <- predict(rf_ranger, data = test_data)
test_actuals <- test_data$Complexity


# Calculate test set metrics
# Make sure no NAs in test_actuals or predictions$predictions
valid_preds <- !is.na(predictions$predictions) & !is.na(test_actuals)


test_mse <- mean((predictions$predictions[valid_preds] - test_actuals[valid_preds])^2)
print(paste("Test Set MSE:", test_mse))
  
  # Test Set R-squared
test_r_squared <- 1 - (test_mse / var(test_actuals[valid_preds], na.rm=TRUE))
print(paste("Test Set R-squared:", test_r_squared))
  
  # Plot actual vs predicted
plot_data <- data.frame(Actual = test_actuals[valid_preds], Predicted = predictions$predictions[valid_preds],
                        Income = test_data$Income, SQM = test_data$SQM_Price_Mean, Young = test_data$Young,
                        Gini = test_data$Gini)

cor = cor(plot_data$Actual, plot_data$Predicted, method = "spearman")
cor(plot_data$Actual, plot_data$Income, method = "spearman")

test <- plot_data[sample(nrow(plot_data), size = 5000, replace = FALSE), ]

plot <- ggplot( plot_data, aes(x = Actual, y = Predicted)) +
    hrbrthemes::theme_ipsum_rc(axis_title_size = 12, grid = "XY")+
      geom_point(alpha = 0.5, aes(color = round(SQM/1000))) +
      geom_smooth(color = "black", linetype = "dashed", linewidth = 1.2) +
      scale_color_viridis_c(option = "mako", end = 0.9)+
      labs(#title = "Actual vs. Predicted on Test Set",
           x = "Actual ACI",
           y = "Predicted ACI",
           color = "Price per \nSQM (K€)") +
  annotate(x = -1, y = 2.3,geom = "text",label = paste0("ρ = ",round(cor,3)),
           size = 4)
  
plot
ggsave("Figures/Random_Forest_SQM.png", plot, dpi = 300, width = 2000, height = 1350, units = "px")

# training_data <- train_test_split(MOR_diff %>% 
#                                     filter(!is.na(Income) & !is.na(Gini)) %>%
#                                     select(rankdiff, Income, Gini, Res,ACH19_ON_RP,
#                                            Population, CS_34, AMEN_10P), 1)[[1]]
rf_ranger <- ranger(
  formula = Complexity ~ ., 
  data = training_data,mtry=3, num.trees = 500,
  importance = "permutation",oob.error = T, keep.inbag = T)

lm <- lm(data = MOR_iris, formula = Complexity ~ Year + Income + Gini + Res+
           Population + AMEN_10P + ACH19_ON_RP + P19_RP_LOCHLMV + CS_34 + Population/obs)
sum = summary(lm)
sum

prediction <- predict(rf_ranger, test_data)

#mean(prediction$predictions - test_data$Complexity)
#[1] -0.0004228572


mean(prediction$predictions - test_data$Complexity)

plot_data <- left_join(test_data %>% mutate(rowid=rownames(.)),
                       as.data.frame(prediction$predictions) %>%
                         mutate(rowid = rownames(.)))
names(plot_data)[length(plot_data)] = "Predictions"
library(ggpubr)

scatter_1 <- ggscatter(plot_data,x="Complexity", y="Predictions")

scatter_2 <- ggscatter(plot_data,x="Complexity", y="Predictions",
                       color = abs(y - x))

plot_data$Comp_rank = rank(plot_data$Complexity)
plot_data$Pred_rank = rank(plot_data$Predictions)

scatter_2 <- ggplot(plot_data,mapping=aes(x=Complexity, y=Predictions,
  color = Year))+
  geom_point(alpha = 0.3)+
  #scale_color_viridis_c()+
  geom_smooth(inherit.aes = T,level = 0.999,se=F, color = "#34a0a4", method = "loess")+
  theme_bw()+
  labs(x="Observed ACI", y="Predicted ACI",color="Gap")
scatter_2

# ggsave("Gdoc_plots/Complexity_predictions.png",scatter_2, width = 800, height = 600,
#        units = "px",dpi = 200)

density <- ggplot(plot_data,mapping=aes(x=Complexity-Predictions))+
  geom_density(aes(y=after_stat(density)),bw=3,
               position="identity", alpha = 0.1,linewidth=1,color = "#a7c957",
               fill = "#a7c957")+
  theme_bw()+
  labs(y="density",x="")
density

scatter_3 <- ggplot()+
  geom_point(plot_data, mapping=aes(x=rank, y=Predictions, 
                                    color = Year))




scatter_3
library(patchwork)
scatter_4 <- scatt

ggsave("rf_predictions.png",scatter_2/scatter_3)

ggplot(as.data.frame(rf_ranger$variable.importance) %>% setNames(.,"importance") %>%
         mutate(variable = rownames(.)), 
       aes(x=reorder(variable,importance), y=importance/max(rf_ranger$variable.importance),fill=importance))+ 
  geom_bar(stat="identity", position="dodge")+ coord_flip()+
  ylab("Variable Importance")+
  xlab("")+
  ggtitle("Information Value Summary")+
  guides(fill=F)+
  scale_fill_gradient(low="red", high="blue")

library("iml")
library("pdp")

predictor <- Predictor$new(rf_ranger, data = test_data%>%select(-Complexity), 
                           y = test_data$Complexity)

partial <- pdp::partial(rf_ranger, train = training_data,
                        plot = T, plot.engine = "ggplot2", parallel = T,
                        approx = T, pred.var = c("Res","P13_RP_ACH19","Income"))


imp <- FeatureImp$new(predictor, loss = "mae")

plot(imp)
imp$results

ale <- FeatureEffect$new(predictor, feature = "P13_RP_ACH19")
ale$plot()

interact <- Interaction$new(predictor)
plot(interact)

interact <- Interaction$new(predictor, feature = "P13_RP_ACH19")
plot(interact)

effs <- FeatureEffects$new(predictor)
plot(effs)

# rf_ranger2 <- ranger(
#   formula = Complexity ~ ., 
#   data = training_data %>% select(-Year,-Nb),mtry=6, num.trees = 300,
#   importance = "permutation")


