# olist-regression-analysis
📦 Project Overview

This project presents an end-to-end exploratory data analysis (EDA) and regression modeling study using the Olist e-commerce dataset (97,000+ orders). The objective is to understand the key drivers of delivery delays and freight costs in an e-commerce logistics context.

🎯 Objectives

Identify how product attributes (weight, dimensions, photos, price) influence logistics outcomes

Model and explain freight cost and delivery delay days using regression techniques

Assess multicollinearity and model reliability using diagnostic tests

📊 Methodology

Data cleaning and feature engineering performed in R

Extensive EDA to analyze distributions, correlations, and city-level patterns

Built two OLS regression models:

Freight value prediction

Delivery delay (in days) prediction

Evaluated multicollinearity using Variance Inflation Factor (VIF)

🔍 Key Findings

Product attributes showed limited influence on delivery delays, indicating external logistics factors play a larger role

Price and delivery time were significant predictors of freight cost

VIF < 2 across predictors confirmed low multicollinearity and stable models

Data exhibited right-skewness, city-level inefficiencies, and weak correlation between customer reviews and delays

📈 Business Insights

The results suggest that improving delivery performance requires a stronger focus on logistics coordination and carrier efficiency, rather than solely optimizing product features.

🚚 Recommendations

Implement a centralized delivery management system

Improve coordination with third-party logistics providers

Monitor city-level performance metrics to reduce systemic delays

🔮 Future Enhancements

Incorporate geolocation, distance, and weather data

Apply text mining on customer reviews for satisfaction insights

Explore advanced machine learning models to improve predictive accuracy

🛠️ Tools & Skills

R Programming | Data Cleaning | Feature Engineering | Exploratory Data Analysis | Regression Analysis | Multicollinearity Diagnostics (VIF) | Data Visualization | Business Interpretation
