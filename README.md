# Statistics-with-R
A public library that will allow for automatic hypothesis testing on predictors.

To install the package in `R` copy and paste the following (WORKING!):
```
devtools::install_github("https://github.com/lime-n/Statistics-with-R/blob/57bd97585f4af367d1e2545c62f005a2c96653cb/Hypothesis%20Testing/PerModel_0.1.0.tar.gz")

```

How to use the functions:

**perModel:**
```
require(faraway)
data(teengamb)

#This gets a list of all linear model combinations for the predictors
perModel(data=teengamb,response='gamble', predictors='all')

#This gets a list of linear model combinations that involves either 'sex' or 'status'
perModel(data=teengamb,response='gamble', predictors=c('sex','status'))

#Get the confidence interval from the median input of the data
perModel(teengamb,response='gamble', predictors=c('sex','status'),pred=list('confidence', 'median'))

#get predictions on specific input of the data. The input must match the length of independent variables.
perModel(teengamb,response='gamble', predictors=c('sex','status'),pred=list('prediction', c(9, 10, 6, 7)))

#Log transformations on the data; getting the confidence interval based on a set interval of data.
perModel(teengamb,response='gamble', predictors=c('log(status)', 'income'), pred=list('confidence',c(5, 8, 7, 8)))

#a polynomial transformation on the predictor and a confidence interval given the average value across all predictors.
perModel(teengamb,response='gamble', predictors=c('I(income^2)'), pred=list('confidence','median'))
```
**perPlot:**
```
#perPlot and how to use it

#lmod is a list of linear models - in this case I used the dataset: teengamb
lmod <- perModel(data=teengamb,  response='gamble', predictors='all')
perPlot(models=lmod, predictors=c('status','verbal'), type='l')

```

<img width="780" alt="Screenshot 2022-06-08 at 15 28 41" src="https://user-images.githubusercontent.com/68914515/172642413-1b540ca9-c413-42cc-9c0c-b82f71786d2a.png">


Limitations:  

**perModel**:
1. It can only take names of predictors and response without any whitespaces, symbols or numbers.
2. We can only generate additive models and not multiplicative (yet).
3. You cannot subset on the linear model (yet)

**perPlot**:
1. Only testing at the 95th Confidence interval, in the near future this will be amended for any region on the interval.

----
New models:

**perDiag**: Interactive menu/input console for teaching or guiding linear model diagnostics when working with your dataset. (Complete)

Taster:

<img width="388" alt="Screenshot 2022-06-11 at 22 52 05" src="https://user-images.githubusercontent.com/68914515/173206100-86516575-26d6-4aa2-9fa6-5d084c57f065.png">


----

*Issues:*

08/06/2022
1. some issues involving perModel, such that not all combinations were retrieved. This is now fixed, and all combinations are available.

13/06/2022

1. perDiag needs updating for partial regressions on the following:
   - Generalise the number of predictors from the models so the regression runs on all model types.
     - Update: Using only the model with the full number of predictors as of now.

14/06/2022
1. perDiag needs working on for the following: splines, additive models and complex models. There is no easy way to make the interaction for these in a simple way because it strongly depends on what you're after. However, plans to develop this are underway.

