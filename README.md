# Statistics-with-R
A public library that will allow for automatic hypothesis testing on predictors.

To install the package in `R` copy and paste the following:
```
install.packages("https://github.com/lime-n/Statistics-with-R/blob/40272fa2e4fdc54e5acccb284aa9961b5644ca89/Hypothesis%20Testing/PerModel_0.1.0.tar.gz",repos = NULL, type="source") 

```

The following will be achieved:
1. ~~Get all combinations of a model, grab the t-statistic and p-value for the t-statistic.~~
2. ~~Next step, set an additional parameter to test for the null hypothesis.~~
3. ~~From this, get the entire models F-statistic and P-value at the F-statistic.~~
4. ~~Develop a function to calculate various hypotheses based on the current linear model coefficients~~
5. ~~Additionally, develop a function that can make additional predictions and hypotheses on this plus a confidence interval.~~

All the achievements have been reached with an additional package.

Limitations:  

**perModel**:
1. It can only take names of predictors and response without any whitespaces, symbols or numbers.
2. We can only generate additive models and not multiplicative (yet).

**perPlot**:
1. Only testing at the 95th Confidence interval, in the near future this will be amended for any region on the interval.

