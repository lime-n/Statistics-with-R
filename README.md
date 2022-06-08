# Statistics-with-R
A public library that will allow for automatic hypothesis testing on predictors.

To install the package in `R` copy and paste the following (Unfortunately direct download is currently not working):
```
install.packages("https://github.com/lime-n/Statistics-with-R/blob/40272fa2e4fdc54e5acccb284aa9961b5644ca89/Hypothesis%20Testing/PerModel_0.1.0.tar.gz",repos = NULL, type="source") 

```

How to use the functions:
**perModel:**
```
require(faraway)
dara(teengamb)

#This gets a list of linear model combinations that involves either 'sex' or 'status'
perModel(data=teengamb,response='gamble', predictor=c('sex','status'))

#Get the confidence interval from the median input of the data
perModel(teengamb,response='gamble', predictor=c('sex','status'),pred=list('confidence', 'median'))

#get predictions on specific input of the data. The input must match the length of independent variables.
perModel(teengamb,response='gamble', predictor=c('sex','status'),pred=list('prediction', c(9, 10, 6, 7)))

#Log transformations on the data; getting the confidence interval based on a set interval of data.
perModel(teengamb,response='gamble', predictor=c('log(status)', 'income'), pred=list('confidence',c(5, 8, 7, 8)))

#a polynomial transformation on the predictor and a confidence interval given the average value across all predictors.
perModel(teengamb,response='gamble', predictor=c('I(income^2)'), pred=list('confidence','median'))
```
**perPlot:**
```
#perPlot and how to use it

#in this case I used the dataset: teengamb
perPlot(models=lmod, predictors=c('status','verbal'), type='l')

```


Limitations:  

**perModel**:
1. It can only take names of predictors and response without any whitespaces, symbols or numbers.
2. We can only generate additive models and not multiplicative (yet).

**perPlot**:
1. Only testing at the 95th Confidence interval, in the near future this will be amended for any region on the interval.

