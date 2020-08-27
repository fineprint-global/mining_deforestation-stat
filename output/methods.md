
# Methods

We perform inference on the impact of mining sites and activity on forest loss. Information is available on a fine-scale grid level, leading to an abundance of observations. To control for national heterogeneity, we separate our dataset and operate on country-level data.
In order to reduce imbalance and combat model dependence, we use a coarsened exact matching approach \citep{iacus2009}.
We use a Tobit regression model, considering the fact that the dependent variable, forest loss, is truncated at zero. We control for a range of other variables that are known to affect forest loss to derive a causal estimate. These include population, distance to infrastructure, distance to agriculture, biomes, initial forest cover, and others. The dependent and explanatory distances are log-transformed. This allows an elasticity interpretation of coefficients and better reflect relevant quantities.
Several interaction terms are considered in order to capture non-linearities. Model selection builds upon our theoretical model; LASSO model selection techniques \citep{tibshirani} are used to explore potential permutations thereof.
<!-- Additionally, we employ a causal random forest \citep{wagner2018estimation} to estimate effects. This non-parametric approach captures more flexible processes and provides a valuable indicator of robustness. -->

# Results

- Mines significantly drive deforestation in thirteen of 21 countries
- For three countries we find mines as a deterrent to deforestation
- In five countries we find no significant effects from mining
(Interactions do not affect this too much, need to look closer though)

In Brazil a one percent increase in distance from mining sites is associated with a decrease in forest loss of -0.62 percent. This effect is magnified at higher distances to -0.87 for a five kilometer boundary and -0.94 for a 25 kilometer boundary.
(Indonesia: -0.38, -0.44, -0.50)

- Matching achieves balanced data, still plenty of statistical power
- Coefficients are robust to the omission of regressors (roads, protected areas, cropland)
