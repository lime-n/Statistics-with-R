\name{perPlot}
\alias{EM}
\title{perPlot}
\usage{
perPlot(models, predictors, type=NULL)

}

\arguments{
  \item{models}{A list of indpendent linear models}
  \item{predictors}{Two predictors of interest to compare amongst these linear models}
  \item{type}{The type of plot}
}
\description{
perPlot takes a list of linear models and based on the predictors it selects only those models with the predictors included. Then it makes an ellipise plot of each model displaying the results from the null-hypothesis test for one-sided and two-sided tests.
}
\examples{
#lmod is a list of linear models created from perModel

data(mtcars)
lmod <- perModel(data=mtcars,response='mpg', predictor=c('disp','hp'))

perPlot(models=lmod, predictors=c('disp','hp'), type='l')
}
