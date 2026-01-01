
# Gallstone Prediction Using R

The goal is to achieve both:
- Strong predictive performance (best possible predictions)
- Model interpretability, so that important risk factors can be clearly understood using clinical and health data and compare multiple predictive models.

## Data Split
The dataset was split into training and testing data:
- Training data: 60%
- Testing data: 40%
- Random seed: 123

## Models Used
- Logistic Regression
- Stepwise Logistic Regression
- LASSO Logistic Regression
- Decision Tree
- Random Forest
- XGBoost with SHAP

## Model Evaluation
Models were evaluated using:
- Confusion Matrix
- ROC Curve
- AUC (Area Under the Curve)

ROC-based threshold tuning was applied using Youden’s J statistic.

## Data Source
Dataset obtained from Kaggle.
The dataset is not included in this repository due to licensing restrictions.

## How to Run
1. Download the dataset from Kaggle
2. Place it in the `data/` folder
3. Run `scripts/00_full_analysis.R`

