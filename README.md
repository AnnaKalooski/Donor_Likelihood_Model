# Donor Likelihood Model – Random Forest (RStudio)

## Overview
This project was completed as part of a consulting course for my Master’s in Business Analytics program, with the Providence College Athletic Department as the client. I used real college basketball fan data from ticketing, scan, and CRM append sources to predict the likelihood that a fan would become a donor.  

The goal was to help the ticketing and marketing teams focus outreach on high-potential non-donors, improving efficiency and return on investment/effort. The model was developed in R using a Random Forest classifier and trained on features representing fan demographics, behavioral engagement metrics, and financial capacity indicators. It achieved an AUC of 0.9998, suggesting strong predictive performance.  

## Key Business Insights

#### Data-Driven Donor Targeting
Used a Random Forest model to assign donor likelihood scores to fans, enabling the athletic department to prioritize outreach to non-donors who resemble current donors based on behavioral and financial patterns.
#### Optimized Fundraising Strategy
The model surfaced a small group of “high-potential non-donors” (fans who haven’t donated but share similar traits to those who have). These individuals represent an opportunity for targeted engagement.
#### Segment-Specific Outreach Potential
Certain fanbase profile segments were significantly overrepresented among high-potential non-donors. Targeting these segments allows for more efficient and personalized fundraising campaigns, improving ROI on outreach efforts.

## Recommendations to Client
- Prioritize outreach to high-scoring non-donors identified by the model.  
- Combine predictive scores with relationship data so the marketing team can personalize outreach.  
- Update the model annually with new season data to maintain accuracy and adapt to changes in fan behavior.  
- Expand features to include digital engagement metrics (e.g., email opens and clicks) for potentially stronger predictions.  

## Tech Stack
- **Programming Language**: R  
- **Packages**: dplyr, lubridate, ggplot2, readxl, randomForest, pROC, caret  
- **Techniques**: Data cleaning, feature engineering, exploratory data analysis (EDA), random forest classification, cross-validation, ROC/AUC analysis, data visualization  
- **Data Sources**: Providence college men's basketball ticketing seat manifest, scan logs, CRM append data  

## Confidentiality Note
This project was conducted using proprietary data provided by the Providence College Athletic Ticket Office. Due to confidentiality agreements, the raw dataset and certain sensitive details are not included in this repository. All code and documentation are shared for demonstration of methods and workflow only.  
