
The script currently tests for the following:

1. perModel: When given the parameter 'predictor' we select for the predictor of main interest from the model. This will grab all combinations of this predictor. Additionally, we can test for predictions, confidence intervals based on a variety of new data.

2. perPlot: plots a list of linear models for hypothesis testing. These models are plotted as an ellipse which is calculated at the 95th % confidence interval. A result on both the one-sided and two-sided tests are produced either giving the result 'Accept', or 'Reject'.
