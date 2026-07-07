# election-classification-clustering

Two analyses on the 2016 US primary elections, using county-level data (about 3,100 counties). Written in R.

## Classification

Predicting whether Trump won more than 50% of the vote in a county, from its socioeconomic features. I compared logistic regression, SVM and random forest with cross-validation. The random forest did best, reaching about 78% accuracy against a 63% majority-class baseline.

## Clustering

Grouping counties by their demographics with model-based clustering, then describing the resulting groups using their economic features.

## Files

- `election_analysis.R` — the full R code for both parts
- `report.pdf` — the written report with the methods, tables and figures
