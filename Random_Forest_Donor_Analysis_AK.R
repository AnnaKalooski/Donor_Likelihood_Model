## Author: Anna Kalooski
## Description: EDA on MBB File & Donor likelihood modeling using Random Forest

install.packages("dplyr")
installed.packages("janitor")
install.packages("readr")
install.packages("lubridate")
install.packages("ggplot2")
install.packages("skimr")
install.packages("readxl")
install.packages("tidyr")
library(dplyr)
library(janitor)
library(readr)
library(lubridate)
library(ggplot2)
library(skimr)
library(readxl)
library(tidyr)

appends <- read_xlsx("Providence Appends 2024 - Class.xlsx")
seat_manifest_MBS <- read.csv("2024-25 Seat Manifest - MBS.csv")
scan_logs <- read.csv("2024-25 Seat Manifest - Scan Logs.csv")
str(appends)
str(seat_manifest_MBS)

# Convert dates to POSIXct
seat_manifest_MBS <- seat_manifest_MBS |>
  mutate(
    `Event.Date.Time` = mdy_hm(`Event.Date.Time`)
  )
scan_logs <- scan_logs |>
  mutate(
    `Event.Date.Time` = mdy_hm(`Event.Date.Time`),
    `Scan.Date.Time` = mdy_hm(`Scan.Date.Time`),
    scanned = !is.na(`Scan.Date.Time`),
  )
# Remove rows with missing account ID or event code
MBSmanifest_clean <- seat_manifest_MBS |> 
  filter(!is.na(`Account.ID`), !is.na(`Event.Code`))
scans_clean <- scan_logs |> 
  filter(!is.na(`Account.ID`), !is.na(`Event.Code`))

## Join MBB seat manifest and scan logs on Account.ID 
# This data set ended up being huge because it was a many-to-many join
combined_data <- left_join(
  MBSmanifest_clean,
  scans_clean,
  by = c("Account.ID","Event.Code")
)
# Create a data set that has all first scans
scans_first <- scans_clean |>
  group_by(Account.ID, Event.Code) |>
  slice_min(`Scan.Date.Time`, with_ties = FALSE)

# Join scans_first with seat manifest
firstscans_combines <- left_join(
  MBSmanifest_clean,
  scans_first,
  by = c("Account.ID","Event.Code")
)

# Add column that calculates how much time before tipoff a fan scanned in
firstscans_combines <- firstscans_combines |>
  mutate(minutes_before_tipoff = as.numeric(difftime(`Event.Date.Time.x`, `Scan.Date.Time`, units = "mins")))


colnames(firstscans_combines)

# Remove duplicate columns
firstscans_combines <- firstscans_combines |> 
  select(
    Level = Level.x,
    Section = Section.x,
    Row = Row.x,
    Seat = Seat.x,
    Seat.Status.Code = Seat.Status.Code.x,
    Account.ID,
    Item.Code = Item.Code.x,
    Price.Level.Code = Price.Level.Code.x,
    Price.Type.Code = Price.Type.Code.x,
    Event.Code,
    Date = Date.x,
    Cust.Type.Code = Cust.Type.Code.x,
    Bar.Code = Bar.Code.x,
    Scanned = Scanned..x,
    Item.Price = Item.Price.x,
    Event.Price = Event.Price.x,
    Disposition.Code = Disposition.Code.x,
    Event.Date.Time = Event.Date.Time.x,
    Event.Full.Name = Event.Full.Name.x,
    Season.Code,
    GRMID,
    
    # Add a few useful fields from scans
    Scan.Date.Time,
    Scan.Entry.Status,
    Scan.Gate,
    scanned,
    minutes_before_tipoff
  )

# Join seat manifest and appends on GRMID
appends_MBS <- firstscans_combines |>
  left_join(appends, by = c("GRMID" = "GRMCONTACTID"))

# Filter for not NA in GRMID field
appends_MBS <- appends_MBS |>
  filter(GRMID != "" & !is.na(GRMID))

# Remove columns where all rows are NA
appends_MBS <- appends_MBS |>
  select(where(~ !all(is.na(.))))

######################### Donor Analysis ###################################

# PC Donor?
appends_MBS <- appends_MBS |>
  mutate(
    pc_donor = coalesce(ACTIVITY24MO_DONATION_BY_MAIL, FALSE) |
      coalesce(ACTIVITY24MO_DONOR_OR_CONTRIBUTOR, FALSE) |
      (`Priority Points 040124` > 0)
  )

appends_MBS <- appends_MBS |>
  mutate(
    pc_donor = ifelse(is.na(pc_donor), FALSE, pc_donor)
  )

## Avg arrival time donor and no donor
appends_MBS |>
  filter(scanned == TRUE) |>
  group_by(pc_donor) |>
  summarise(
    avg_minutes_before = round(mean(minutes_before_tipoff, na.rm = TRUE), 1),
    median_minutes_before = round(median(minutes_before_tipoff, na.rm = TRUE), 1),
    count = n()
  )

# donor rates by fan base profile
appends_MBS |>
  group_by(`Fanbase profile`) |>
  summarise(
    count = n(),
    donor_count = sum(pc_donor, na.rm = TRUE),
    donor_rate = round(donor_count / count * 100, 1)
  ) |>
  arrange(desc(donor_rate))
## Invested professionals has the most donors at 11031. Suburban blue collar
## in second with 9049 and Empty nesters in third with 7611

# donor rates by wealth
appends_MBS |>
  group_by(WEALTH_HOUSEHOLD_INCOME) |>
  summarise(
    count = n(),
    donor_count = sum(pc_donor, na.rm = TRUE),
    donor_rate = round(donor_count / count * 100, 1)
  ) |>
  arrange(desc(donor_rate))
## $100,000 - $149,999 has the most with 10749,  $75,000 - $99,999 second most with 8316
## $65,000 - $74,999 third most with 5428

# Average stats by fanbase profile
fan_profile_summary <- appends_MBS |>
  group_by(`Fanbase profile`) |>
  summarise(
    avg_income = mean(WEALTH_HOUSEHOLD_INCOME_SINGLE_NUM, na.rm = TRUE),
    avg_net_worth = mean(WEALTH_NET_WORTH_SINGLE_NUM, na.rm = TRUE),
    avg_capacity = mean(ESTIMATED_CAPACITY, na.rm = TRUE),
    avg_rfm = mean(`RFM Score 041524`, na.rm = TRUE),
    avg_priority_points = mean(`Priority Points 040124`, na.rm = TRUE),
    avg_fanatics_value = mean(`Fanatics Lifetime Value`, na.rm = TRUE),
    count = n()
  ) |>
  arrange(desc(avg_income)) 

### Donor average stats ###
donor_summary <- appends_MBS |>
  filter(pc_donor == TRUE) |>
  summarise(
    avg_income = mean(WEALTH_HOUSEHOLD_INCOME_SINGLE_NUM, na.rm = TRUE),
    avg_net_worth = mean(WEALTH_NET_WORTH_SINGLE_NUM, na.rm = TRUE),
    avg_capacity = mean(ESTIMATED_CAPACITY, na.rm = TRUE),
    avg_rfm = mean(`RFM Score 041524`, na.rm = TRUE),
    avg_priority_points = mean(`Priority Points 040124`, na.rm = TRUE),
    avg_fanatics_value = mean(`Fanatics Lifetime Value`, na.rm = TRUE),
    avg_mins_before_tipoff = mean(`minutes_before_tipoff`, na.rm = TRUE),
    donor_count = n()
  )

# donor vs non donor average stats
donor_vs_nondonor <- appends_MBS |>
  mutate(donor_flag = ifelse(pc_donor, "Donor", "Non-Donor")) |>
  group_by(donor_flag) |>
  summarise(
    avg_income = mean(WEALTH_HOUSEHOLD_INCOME_SINGLE_NUM, na.rm = TRUE),
    avg_net_worth = mean(WEALTH_NET_WORTH_SINGLE_NUM, na.rm = TRUE),
    avg_capacity = mean(ESTIMATED_CAPACITY, na.rm = TRUE),
    avg_rfm = mean(`RFM Score 041524`, na.rm = TRUE),
    avg_priority_points = mean(`Priority Points 040124`, na.rm = TRUE),
    avg_fanatics_value = mean(`Fanatics Lifetime Value`, na.rm = TRUE),
    avg_mins_before_tipoff = mean(`minutes_before_tipoff`, na.rm = TRUE),
    count = n()
  )
############# Going to pivot to a random forest analysis

install.packages("randomForest")
library(randomForest)

names(appends_MBS)[names(appends_MBS) == "Fanatics Lifetime Value"] <- "Fanatics.Lifetime.Value"
names(appends_MBS)[names(appends_MBS) == "RFM Score 041524"] <- "RFM.Score"
names(appends_MBS)[names(appends_MBS) == "Priority Points 040124"] <- "Priority.Points"
names(appends_MBS)[names(appends_MBS) == "Fanbase profile"] <- "Fanbase.Profile"

rf_model <- randomForest(as.factor(pc_donor) ~ 
                           WEALTH_HOUSEHOLD_INCOME_SINGLE_NUM +
                           WEALTH_NET_WORTH_SINGLE_NUM +
                           ESTIMATED_CAPACITY +
                           Fanatics.Lifetime.Value +
                           RFM.Score +
                           minutes_before_tipoff +
                           Fanbase.Profile,
                         data = appends_MBS,
                         na.action = na.omit,
                         importance = TRUE)

importance(rf_model)

# scorable_data with all required predictors present
scorable_data <- appends_MBS |>
  filter(
    !is.na(WEALTH_HOUSEHOLD_INCOME_SINGLE_NUM),
    !is.na(WEALTH_NET_WORTH_SINGLE_NUM),
    !is.na(ESTIMATED_CAPACITY),
    !is.na(`Fanatics.Lifetime.Value`),     
    !is.na(`RFM.Score`),
    !is.na(minutes_before_tipoff),
    !is.na(`Fanbase.Profile`)
  )

# Predict donor probabilities using the random forest model
scorable_data$donor_score_rf <- predict(rf_model, newdata = scorable_data, type = "prob")[, 2]


# Mean donor score for actual donors
mean_donor_score <- mean(scorable_data$donor_score_rf[scorable_data$pc_donor == TRUE], na.rm = TRUE)

# Mean donor score for non-donors
mean_nondonor_score <- mean(scorable_data$donor_score_rf[scorable_data$pc_donor == FALSE], na.rm = TRUE)

print(mean_donor_score)
print(mean_nondonor_score)

high_potential_targets <- scorable_data_unique |>
  filter(pc_donor == FALSE, donor_score_rf > 0.5) |>
  arrange(desc(donor_score_rf))|>
  distinct(GRMID, .keep_all = TRUE)

## how good is this model?
install.packages("pROC")
library(pROC)

roc_obj <- roc(scorable_data$pc_donor, scorable_data$donor_score_rf)
auc(roc_obj)
# Area under the curve: 0.9998 - very high, good! test for overfitting next

#### Cross validation test
install.packages("caret")
library(caret)

# Filter data for relevant fields
donor_data <- appends_MBS |>
  filter(
    !is.na(WEALTH_HOUSEHOLD_INCOME_SINGLE_NUM),
    !is.na(WEALTH_NET_WORTH_SINGLE_NUM),
    !is.na(ESTIMATED_CAPACITY),
    !is.na(Fanatics.Lifetime.Value),
    !is.na(RFM.Score),
    !is.na(minutes_before_tipoff),
    !is.na(Fanbase.Profile)
  )

# Convert target to factor for classification
donor_data$pc_donor <- factor(donor_data$pc_donor, levels = c(FALSE, TRUE), labels = c("NonDonor", "Donor"))

# Step 3: Define 5-fold cross-validation with AUC
cv_control <- trainControl(
  method = "cv",
  number = 5,
  classProbs = TRUE,
  summaryFunction = twoClassSummary,
  verboseIter = TRUE
)

# Step 4: Train the model
set.seed(42)  # for reproducibility
cv_model <- train(
  pc_donor ~ WEALTH_HOUSEHOLD_INCOME_SINGLE_NUM +
    WEALTH_NET_WORTH_SINGLE_NUM +
    ESTIMATED_CAPACITY +
    Fanatics.Lifetime.Value +
    RFM.Score +
    minutes_before_tipoff +
    Fanbase.Profile,
  data = donor_data,
  method = "rf",
  trControl = cv_control,
  metric = "ROC"  # we want to maximize AUC
)

# Step 5: View results
print(cv_model)

# Create unique data set so there arent duplicate GRMID's
scorable_data_unique <- scorable_data |>
  filter(
    !is.na(WEALTH_HOUSEHOLD_INCOME_SINGLE_NUM),
    !is.na(WEALTH_NET_WORTH_SINGLE_NUM),
    !is.na(ESTIMATED_CAPACITY),
    !is.na(`Fanatics.Lifetime.Value`),     
    !is.na(`RFM.Score`),
    !is.na(minutes_before_tipoff),
    !is.na(`Fanbase.Profile`)
  ) |>
  distinct(GRMID, .keep_all = TRUE)


# histogram for donor score distribution

ggplot(scorable_data_unique, aes(x = donor_score_rf, fill = as.factor(pc_donor))) +
  geom_histogram(position = "identity", alpha = 0.6, bins = 50) +
  scale_fill_manual(values = c("steelblue", "darkorange"), 
                    name = "Donor Status",
                    labels = c("Non-Donor", "Donor")) +
  labs(
    title = "Distribution of Donor Scores",
    x = "Predicted Donor Score",
    y = "Count"
  ) +
  theme_minimal()

# Histogram of Donor Scores (Non-Donors Only, Threshold = 0.5)
# Calculate max y-position for annotation
max_count <- scorable_data_unique |>
  filter(pc_donor == FALSE) |>
  count(cut(donor_score_rf, breaks = 50)) |>
  pull(n) |>
  max()
# Count of high-potential non-donors
high_potential_count <- scorable_data_unique |>
  filter(pc_donor == FALSE, donor_score_rf > 0.5) |>
  nrow()

# Plot with adjusted annotation position
ggplot(filter(scorable_data_unique, pc_donor == FALSE), aes(x = donor_score_rf)) +
  geom_histogram(bins = 50, fill = "steelblue", alpha = 0.7) +
  geom_vline(xintercept = 0.5, color = "red", linetype = "dashed", size = 1) +
  annotate("text", x = 0.51, y = 100, label = "Threshold = 0.5",
           color = "red", hjust = 0, size = 4, fontface = "bold") +
  annotate("text", x = 0.55, y = 50, 
           label = paste(high_potential_count, "High-Potential Non-Donors"),
           color = "black", size = 3.5, hjust = 0) +
  labs(
    title = "High-Potential Non-Donors (Score > 0.5)",
    x = "Predicted Donor Score",
    y = "Count"
  ) +
  xlim(0, 0.9) +
  theme_minimal()




# Plot ROC curve
# Create ROC curve object
roc_obj <- roc(scorable_data$pc_donor, scorable_data$donor_score_rf)

# Plot
plot(roc_obj, col = "#1c61b6", lwd = 3, main = "ROC Curve for Donor Prediction")
text(0.6, 0.4, paste("AUC =", round(auc(roc_obj), 4)), cex = 1.2, col = "black")
abline(a = 0, b = 1, lty = 2, col = "gray")  # Diagonal reference line

### Bar chart for high potential non donors by fanbase profile
# Filter for non-donors with high predicted scores
high_potential <- scorable_data_unique |>
  filter(pc_donor == FALSE, donor_score_rf > 0.5)

# Count by fanbase profile
fanbase_summary <- high_potential |>
  count(Fanbase.Profile) |>
  arrange(desc(n))

# Plot
ggplot(fanbase_summary, aes(x = reorder(Fanbase.Profile, n), y = n)) +
  geom_col(fill = "steelblue") +
  coord_flip() +
  labs(
    title = "High-Potential Non-Donors by Fanbase Profile",
    x = "Fanbase Profile",
    y = "Count"
  ) +
  theme_minimal()

## Comparison btwn donors, non-donors, and high potential non donors
## FIRST TRYING MEAN
# Donor summary
donor_summary <- scorable_data_unique |>
  filter(pc_donor == TRUE) |>
  summarise(
    Avg_Income = mean(WEALTH_HOUSEHOLD_INCOME_SINGLE_NUM, na.rm = TRUE),
    Avg_Net_Worth = mean(WEALTH_NET_WORTH_SINGLE_NUM, na.rm = TRUE),
    Avg_Capacity = mean(ESTIMATED_CAPACITY, na.rm = TRUE),
    Avg_RFM = mean(RFM.Score, na.rm = TRUE),
    Avg_Priority_Points = mean(Priority.Points, na.rm = TRUE),
    Avg_Fanatics_Value = mean(Fanatics.Lifetime.Value, na.rm = TRUE),
    Avg_Arrival_Before_Tipoff = mean(minutes_before_tipoff, na.rm = TRUE),
    Count = n(),
    Group = "Donor"
  )

# Non-Donor summary
non_donor_summary <- scorable_data_unique |>
  filter(pc_donor == FALSE) |>
  summarise(
    Avg_Income = mean(WEALTH_HOUSEHOLD_INCOME_SINGLE_NUM, na.rm = TRUE),
    Avg_Net_Worth = mean(WEALTH_NET_WORTH_SINGLE_NUM, na.rm = TRUE),
    Avg_Capacity = mean(ESTIMATED_CAPACITY, na.rm = TRUE),
    Avg_RFM = mean(RFM.Score, na.rm = TRUE),
    Avg_Priority_Points = mean(Priority.Points, na.rm = TRUE),
    Avg_Fanatics_Value = mean(Fanatics.Lifetime.Value, na.rm = TRUE),
    Avg_Arrival_Before_Tipoff = mean(minutes_before_tipoff, na.rm = TRUE),
    Count = n(),
    Group = "Non-Donor"
  )

# High-Potential Non-Donor summary
high_potential_summary <- high_potential |>
  summarise(
    Avg_Income = mean(WEALTH_HOUSEHOLD_INCOME_SINGLE_NUM, na.rm = TRUE),
    Avg_Net_Worth = mean(WEALTH_NET_WORTH_SINGLE_NUM, na.rm = TRUE),
    Avg_Capacity = mean(ESTIMATED_CAPACITY, na.rm = TRUE),
    Avg_RFM = mean(RFM.Score, na.rm = TRUE),
    Avg_Priority_Points = mean(Priority.Points, na.rm = TRUE),
    Avg_Fanatics_Value = mean(Fanatics.Lifetime.Value, na.rm = TRUE),
    Avg_Arrival_Before_Tipoff = mean(minutes_before_tipoff, na.rm = TRUE),
    Count = n(),
    Group = "High-Potential Non-Donor"
  )
comparison_table <- bind_rows(donor_summary, non_donor_summary, high_potential_summary) |>
  select(Group, everything())  # Move Group to the first column

print(comparison_table)

## NOW TRYING MEDIAN
# Donor group
donor_median <- scorable_data_unique |>
  filter(pc_donor == TRUE) |>
  summarise(
    Median_Income = median(WEALTH_HOUSEHOLD_INCOME_SINGLE_NUM, na.rm = TRUE),
    Median_Net_Worth = median(WEALTH_NET_WORTH_SINGLE_NUM, na.rm = TRUE),
    Median_Capacity = median(ESTIMATED_CAPACITY, na.rm = TRUE),
    Median_RFM = median(RFM.Score, na.rm = TRUE),
    Median_Priority_Points = median(replace_na(Priority.Points, 0), na.rm = TRUE),
    Median_Fanatics_Value = median(Fanatics.Lifetime.Value, na.rm = TRUE),
    Median_Arrival_Before_Tipoff = median(minutes_before_tipoff, na.rm = TRUE),
    Count = n(),
    Group = "Donor"
  )

# Non-Donor group
nondonor_median <- scorable_data_unique |>
  filter(pc_donor == FALSE) |>
  summarise(
    Median_Income = median(WEALTH_HOUSEHOLD_INCOME_SINGLE_NUM, na.rm = TRUE),
    Median_Net_Worth = median(WEALTH_NET_WORTH_SINGLE_NUM, na.rm = TRUE),
    Median_Capacity = median(ESTIMATED_CAPACITY, na.rm = TRUE),
    Median_RFM = median(RFM.Score, na.rm = TRUE),
    Median_Priority_Points = median(replace_na(Priority.Points, 0), na.rm = TRUE),
    Median_Fanatics_Value = median(Fanatics.Lifetime.Value, na.rm = TRUE),
    Median_Arrival_Before_Tipoff = median(minutes_before_tipoff, na.rm = TRUE),
    Count = n(),
    Group = "Non-Donor"
  )

# High-Potential Non-Donor group
high_potential_median <- high_potential |>
  summarise(
    Median_Income = median(WEALTH_HOUSEHOLD_INCOME_SINGLE_NUM, na.rm = TRUE),
    Median_Net_Worth = median(WEALTH_NET_WORTH_SINGLE_NUM, na.rm = TRUE),
    Median_Capacity = median(ESTIMATED_CAPACITY, na.rm = TRUE),
    Median_RFM = median(RFM.Score, na.rm = TRUE),
    Median_Priority_Points = median(replace_na(Priority.Points, 0), na.rm = TRUE),
    Median_Fanatics_Value = median(Fanatics.Lifetime.Value, na.rm = TRUE),
    Median_Arrival_Before_Tipoff = median(minutes_before_tipoff, na.rm = TRUE),
    Count = n(),
    Group = "High-Potential Non-Donor"
  )
comparison_median_table <- bind_rows(donor_median, nondonor_median, high_potential_median) |>
  select(Group, everything()) 

## Author: Anna Kalooski
## Description: EDA on MBB File & Donor likelihood modeling using Random Forest

