
# Methods

We perform inference on the impact of mining sites and activity on forest loss. Information is available on a fine-scale grid level, leading to an abundance of observations. In order to reduce imbalance and combat model dependence, we apply coarsened exact matching \citep{iacus2009cem} to this dataset. Our causal effect estimates are derived from a linear regression model. We specify and check robustness of models based theory and model selection procedures. Additionally, we employ a causal random forest \citep{wagner2018estimation} to estimate effects. This non-parametric approach captures more flexible processes and provides a valuable indicator of robustness.

# Results

- Mines significantly drive deforestation in thirteen of 21 countries
- For three countries we find mines as a deterrent to deforestation
- In five countries we find no significant effects from mining
(Interactions do not affect this too much, need to look closer though)

In Brazil a one percent increase in distance from mining sites is associated with a decrease in forest loss of -0.62 percent. This effect is magnified at higher distances to -0.87 for a five kilometer boundary and -0.94 for a 25 kilometer boundary.
(Indonesia: -0.38, -0.44, -0.50)

- Matching achieves balanced data, still plenty of statistical power
- Coefficients are robust to the omission of regressors (roads, protected areas, cropland)
