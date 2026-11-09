# Customer Analytics and Campaign Response Modeling

This project analyzes customer purchasing behavior and marketing campaign response using R. It combines exploratory analysis, customer segmentation, and predictive modeling to identify high-value customer groups and the factors associated with campaign response.

## Project overview

The analysis includes:

- Data cleaning, missing-value handling, and outlier detection
- Descriptive analysis of customer income, age, and total spending
- Linear regression and residual diagnostics
- K-means customer segmentation
- Logistic regression for campaign-response prediction
- ROC and AUC evaluation
- Decision-tree training, cross-validation, pruning, and test-set evaluation
- Heterogeneity analysis across customer clusters, education levels, and age groups

The logistic regression model achieved an AUC of approximately 0.824 in the original analysis.

## Technologies

- R
- `skimr`
- `car`
- `pROC`
- `tree`

## Files

- `Customer Analytics and Campaign Response Modeling.R`: Complete analysis and modeling workflow

## Running the analysis

1. Place `marketing_campaign.csv` in the project directory.
2. Install the required R packages:

```r
install.packages(c("skimr", "car", "pROC", "tree"))
```

3. Open the R script and run it from the project directory.

## Data

The analysis expects a file named `marketing_campaign.csv`. The dataset is not included in this repository.

