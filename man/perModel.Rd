\name{perModel}
\alias{EM}
\title{perModel}
\usage{
perModel(data=data, pred=NULL,...)

}

\arguments{
  \item{data}{the data to input.}
  \item{response}{The target response variable}
  \item{predictor}{the predictors that you are interested in.'all' selects all the predictors for you, although you can select single or multiple predictors with c('pred1','pred2'...)}
  \item{pred}{To make predictions on the data or grab the confidence interval. }
}
\description{
perModel takes all number of combinations of chosen predictors for a linear model. It can either take all predictors or a few selected. Additionally, you can perform transformations on the data just as a linear model. Although, no treatment for multiplicate arguments have been implemented. So, you can only perform additive treatments on the formula. Additionally, it has the option of mkaing predictions or getting the confidence interval based on a dataset.
}
\examples{
#This gets a list of linear model combinations that involves either 'disp' or 'hp'
data(mtcars)
lmod <- perModel(data=mtcars,response='mpg', predictor=c('disp','hp'))

#Get the confidence interval from the median input of the data
perModel(mtcars,response='mpg', predictor=c('disp','hp'),pred=list('confidence', 'median'))

#get predictions on specific input of the data. The input must match the length of independent variables.
perModel(mtcars,response='mpg', predictor=c('disp','hp'),pred=list('prediction', c(9, 10, 6, 7)))

#Log transformations on the data; getting the confidence interval based on a set interval of data.:
perModel(mtcars,response='mpg', predictor=c('log(disp)', 'hp'), pred=list('confidence',c(5, 8, 7, 8)))

#a polynomial transformation on the predictor and a confidence interval given the average value across all predictors.
perModel(mtcars,response='mpg', predictor=c('I(hp^2)'), pred=list('confidence','median'))

}
