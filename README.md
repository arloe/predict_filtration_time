### predict filtration time
Predict filtration time & interpretate the model

### Introduction

AMPAC is a consigned pharmaceutical developer and manufacturer, located in Rancho Cordoba, California,
USA. In the case of pharmaceutical production, yield, quality, and the reduction of production time are
important issues as a batch process. Among these, in this project, the production time was progressed as a target. The process that can be reduced during production is filtration time, and the purpose of this task is to estimate the influencing factors affecting filtration time and their influence.

That is, finding an operation method that reduces the filtration Time rather than accurately predicting the filtration Time is the most important part from the user’s point of view. For this reason, models such as regression, which have good explanatory power, are the first to be considered. However, the assumption of a linear relationship between variables was determined to be too strong an assumption to be applied in a chemical process. Also, from the standpoint of the field, the reliability of models with good predictive power is inevitably high.

Therefore, after estimating a model with good predictive power, the analysis was conducted by estimating
the relationship between variables in the model

### Data
Process data (from MIS) and production management data (from MES) were used. However, data cannot be uploaded due to security issues.

### Modeling & Interpretaion

1. Prediction Performance

 + Quantitative evaluation is below.

![quantitative_evaluation.PNG](https://github.com/arloe/predict_filtration_time/tree/main/img/quantitative_evaluation.PNG)

 + Qualitative evaluation is below.

![qualitative_evaluation](https://github.com/arloe/predict_filtration_time/tree/main/img/qualitative_evaluation.PNG)

2. Interpretation using pdp

![partial_dependence_plot](https://github.com/arloe/predict_filtration_time/tree/main/img/partial_dependence_plot.PNG)

### Reference
 + Q. Zhao and T. Hastie. Causal interpretations of black-box models. Journal of Business & Economic Statistics, pages 1–10, 2019.

 + J. H. Friedman. Greedy function approximation: a gradient boosting machine. Annals of statistics, pages 1189–1232, 2001.