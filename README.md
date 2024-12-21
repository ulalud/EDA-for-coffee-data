# Exploratory Data Analysis of Coffee Data

## **Overview**
This document presents an exploratory data analysis (EDA) of Coffee data. The goal is to understand the factors that impacted the coffee beans ratings and the data structure. The data comprises **29 covariates and 945 records**. The final grade has been recorded under the `Total Cup Point` variable, and it is related to 13 other review covariates.

---

## **Analysis of Missing Data**
The analysis of missing data has been carried out to identify the types of missingness present in the coffee data. Key findings include:

- There are **157 observations with missing data**. 
- All missing records are in the altitude covariates: `Altitude Low Meters`, `Altitude Mean Meters`, and `Altitude High Meters`.
- Most records with missing altitude data follow **MAR** or **MNAR** patterns:
  - **90%** of records from Hawaii and **75%** from Peru lack altitude data (Figure 1).

### **Missing Data Handling**
Several techniques were used to handle missing data:
- **Listwise Deletion:** Minimal impact due to insignificant missing values.
- **Mean Imputation:** Applied to the single missing value in `Quakers`.
- **Predictive Mean Matching (PMM):** Used for imputation in `Altitude Mean Meters` after removing multicollinearity.

---

## **Outlier Detection**
Outlier analysis was conducted to ensure data quality and make the right modeling assumptions:

- **Modified Z-Score Analysis** identified outliers across most numerical covariates.
- Clusters in the data may explain the high number of outliers.

---

## **Multivariate Analysis Including Spatial Data**
### **Spatial Analysis**
- `Cupper Points` showed higher aftertaste ratings closer to the Equator (Figure 2).
- All data originates from North and South America.

### **Correlation Analysis**
- Strong positive correlations observed:
  - `Flavor` and `Aftertaste`: **0.85**
  - `Cupper Points` with `Flavor` and `Aftertaste`: **0.81, 0.82**
  - `Total Cup Points` with `Flavor`, `Aftertaste`, and `Cupper Points`: **0.82, 0.80, 0.80**
- Multicollinearity was resolved by removing altitude covariates.

---

## **Clustering Behavior in Numeric Review Data**
### **Initial Clustering Analysis**
- Simple tools like `ggpairs` and contour plots indicated **two clusters** with the `Moisture` covariate.

### **Advanced Clustering with t-SNE and K-Means**
- t-SNE embedding preserved local and global structure, aiding cluster identification.
- **Elbow Method** and **silhouette analysis** confirmed the presence of **four clusters** (Figures 4 and 5).

---

## **Conclusions**
- **Missing Data:** Addressed using various imputation techniques.
- **Correlation Insights:** Strong predictors identified for `Total Cup Points`.
- **Clustering:** Four distinct clusters detected, providing insights into data subgrouping.
- **Recommendations:**
  - Perform further EDA to refine insights.
  - Incorporate cross-validation to optimize hyperparameters like perplexity.

---
