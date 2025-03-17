---
title: "MTH6139 Time Series" 
subtitle: "Coursework 1 -- Template" 
author: "HING FUNG CHAN" 
date: "Spring term 2025" 
output: 
  html_document:
    toc: true
    toc_float: true
    theme: spacelab 
    highlight: tango
editor_options: 
  markdown: 
    wrap: 72
---

```{r, echo=FALSE}
# This code will display the QMUL logo at the top right of the page
# Do not change this code
htmltools::img(src = knitr::image_uri("images/QMlogo.png"),
               alt = 'logo',
               style = 'position:absolute; top:0; right:0; padding:10px; width:20%;')
```
```{r include=FALSE}
library(prophet)
library(astsa)
library(dygraphs)
library(mosaic)
library(Rssa)
library(feasts)
```
# Section 1: Creating a dataframe and plots for our time series

In this section we will be creating a dataframe and plots for out time series - salmon, which is found in our library "astsa".

## 1.1: Creating the dataframe
For some equation: 
$x_t = \beta_0+ \beta_1t + s_1Q_1(t)+s_2Q_2(t)+...+s_{12}Q(t) +y_t$ 

, where $\beta_0 + \beta_1$ is our trend, $s_1Q_1(t)+s_2Q_2(t)+...+s_{12}Q(t)$ is our seasonal component and $y_t$ is our noise.
The function: $$ Q_i(t) = 
\begin{cases}
1\hspace{1cm} \text{if t is in quarter i}\\
0\hspace{1cm} \text{otherwise }
\end{cases} $$

Here is the code used to create our dataframe for the time series "salmon"

```{r}
salmon.df = data.frame(
    ds=zoo::as.yearmon(time(salmon)), 
    y=salmon)
#salmon.df is a data frame, which uses salmon -a time series, as its input, resulting in a list of tuples 
#(ds,y). In the line of coding, "ds" uses the time function and the function "as.yearmon" called from the 
#library "zoo", resulting in ds being in the form of Month/year, y is the monthly export price of salmon
#in US dollars.
```

## 1.2: Plots

We shall now create an interactive plot

```{r}
dygraph(salmon.df, main= "salmon export prices")%>%    
    dyRangeSelector(dateWindow =c("2003-09-01","2017-06-01"))
# this is an interactive plot which uses the library dygraphs and utilises the dataframe "salmon.df" as 
#its data. The "main" names the title of the plot as "salmon export prices" and "dyRangeSelector" adds 
#a range selector at the bottom of the plot; the date range starts at September 1 2003 and ends 
#at June 1 2017

```

# Section 2: Fitting a Model

We shall first decompose the time series salmon and plot our results

```{r}
model0 =stats::decompose(salmon)
#a variable model0 that calls the function "decompose" from the library "stats" and uses salmon ( a time series) as its input. This outputs the plots: observed data, trend, seasonal and random

plot(model0)
#creates a plot using "model0"

```

we now use linear regression models to try fit our time series
```{r}
model1 = lm(salmon~time(salmon),na.action=NULL)
#creates a variable "model1" which is a linear regression model, where salmon is the price of salmon and time(salmon) is the time in which the salmon was that price. "na.action=NULL" ensures that all NA values are not erased.
plot(salmon)
#plots salmon
lines(fitted(model1))
#creates a trend line on the fitted model1 plot

```
We see that a linear trend may not fit our time series well. Therefore we can try fitting a logarithmic version of salmon
```{r}
log_salmon = log(salmon)
#saves the logarithm of salmon into log_salmon
plot(log_salmon,type="l")
#plots log_salmon as a line plot
model2 = lm(log_salmon~time(log_salmon), na.action=NULL)
#creates a variable "model2" which is a linear regression model, where log_salmon is the logarithm of the price of salmon and time(log_salmon) is the logarithm of the time in which the salmon was that price. "na.action=NULL" ensures that all NA values are not erased.
lines(fitted(model2), col="red")
#creates a red trend line on the fitted model2 plot
mosaic::msummary(model2)
#calls the msummary function from the mosaic library and takes in model2 as its input

```
It is marginally better. but still horrible. We see that $\beta_0 =-96.814548$ and $\beta_1=0.048974$. By computing $\beta_1$ we deduce that log_salmon increases on average $\beta_1=0.048974$ monthly, this implies that salmon is increasing by, $\exp(\beta_1)=1.050193045$, every month, an average of $ 5\% $ a month

## 2.1: Seasonality

We shall now implement the seasonality component into our model. To turn Months into numerical values we use the cycle function.To find $s_1,...,s_{12}$ we use factor
```{r}

cycle(salmon)
#extracts position of a data point in its seasonal cycle of salmon
seasonal_labels = factor(cycle(salmon),labels = c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"))
# converts the months into its respective number i.e. Jan=1 , Feb=2 ,...,Dec=12
model3 = lm(log_salmon~0+seasonal_labels,na.action=NULL)
#a linear regression model that maps the logarithm of salmon to a month
mosaic::msummary(model3)
#calls the msummary function from the mosaic library and takes in model3 as its input
plot(log_salmon, type="l")
#creates a line plot using log_salmon as its input
lines(fitted(model3), col="red")
#creates a red trend line using the fitted values of model3 as its input


```
Another way to find $s_1,...,s_{12}$. $s_1$ is just the average all month 1 data values and then do the same for all the other $s_n$ with respect to their own numerical month value.

```{r}

cycle(salmon)==1
#checks for the number 1 (Jan) in salmon and outputs TRUE for 1, FALSE otherwise

log_salmon[cycle(salmon)==1]
#computes the logarithm of salmon if TRUE and returns the logarithm salmon price

for (i in 1:12){
    print(mean(log_salmon[cycle(salmon)==i]))
}
#for loop indexed from 1 to 12 and prints the mean of the logarithm of salmon for cycle(salmon)==i is TRUE

model3$coeff
#finds the coefficient of model3

```
Note that mode3$coeff and the mean(log_salmon[cycle(salmon)==i]) are the same values

###2.1.5: Seasonanility and trend
We note that what we have computed in section 2.1 did not produce a trend for our plot, but instead were just lines across our plot.

We now implement a trend for our seasonality component to better fit out data.
```{r}
##TREND AND SEASONALITY
model4 = lm(log_salmon~0+time(salmon)+seasonal_labels, na.action=NULL)
#a linear regression model that maps the logarithm of salmon to a month and the time of salmon price at a point in time

mosaic::msummary(model4)
#calls the msummary function from the mosaic library and takes in model4 as its input

plot(log_salmon,type="l")
#creates a line plot using log_salmon as its input

lines(fitted(model4), col="red")
#creates a red trend line using the fitted values of model4 as its input


```
It is a better fit that before, but still not perfect. This is because we are missing our noise component.

# Section 3: Eliminating Heteroscedacity
Heteroscedacity is variance is not at a constant throughout the time series, which can cause issues when trying to predict future observations. Hence we try to remove Heteroscedacity

## 3.1:  Finding λ
We first must find the optimal parameter $\lambda$, in the Box-Cox transformation by using the function B0xCox.lambda which is called from thr library forecast.

```{r}

(lambda = forecast::BoxCox.lambda(salmon))
#calls the BoxCox.lambda function from the forecast library and applies the BoxCox.lambda function to salmon, the sign of lambda indicates whether variance is increasing or decreasing


```
We obtain $\lambda =0.4746382$, which indicates that variance increases at a decreasing rate.

Now we compare the time series, transformed by the Box Cox, salmon
```{r}

plot(BoxCox(salmon,lambda))
#plots the time series, transformed by Box Cox, salmon

plot(log_salmon)
#plots the logarithm of the time series salmon 
```
we see that they are pretty much identical, or equally bad.

## 3.2: Breusch-Pagan Test
The Breusch-Pagan Test for heteroscedacity is based on hypothesis testing. We have the null hypothesis:   H0: the time series is homoscedastic 
and the alternative hypothesis: H1: the times series is heteroscedastic

For hypothesis testing we need to compute the p-value to see whether we can reject or fail to reject the null hypothesis. This can be done using the function bptest from the library lmtest.

```{r}
lmtest::bptest(salmon~time(salmon))
#calls the bptest function from the library "lmtest", where bp stands for Breusch Pagan test, and creates a linear regression model using salmon and time(salmon). This checks for heteroscedasticity. Let the null hypothesis H0: time series is homoscedastic
# and the alternative H1: time series is heteroskedastic

lmtest::bptest(log_salmon~time(log_salmon))
#calls the bptest function from the library "lmtest", where bp stands for Breusch Pagan test and creates a linear regression model using the logarithm of salmon and time(of the logarithm of salmon). This checks for heteroscedasticity.
#Let the null hypothesis H0: time series is homoscedastic and the alternative H1: time series is heteroskedastic


```
For the salmon bptest we obtained the p-value = 0.0007822 and for the log_salmon bptest we obtained the p-value as 0.6387.

For p-value <0.05, we reject null hypothesis , and for p-value >0.05, we fail to reject the null hypothesis. This implies that we should reject the null hypothesis for the original model and we fail to reject the null hypothesis for the logarithmic model.

In conclusion, we would prefer using the logarithmic model over the normal model as the normal model is heteroskedastic.

# Section 4: Autocorrelation
Autocorrelation measures how correlated our variables are at two time intervals.

## 4.1: Brockwell-Davies decomposition:
We perform Brockwell_Davies decomposition using the decompose function from the stats library. We assign the variable res as the residual noise 

```{r}

res = stats::decompose(astsa::salmon)$random
#calls the decompose function from the library "stats" using the time series salmon as its input. "$random" looks at the random column in salmon. "random" is our residual noise
plot(res)
#creates a plot of the residual noise


```
### 4.1.5: Cleaning the residual
The Brockwell-Davies algorithm results in a lot of missing values (NA), which can be seen at the beginning and the end of our time series salmon
```{r}
res
```

Therefore, we must eliminate the NA using the window function

```{r}
res = window(res, start = c(2004, 3), end = c(2016,12))
#a variable "res" that uses the function window, which uses res as its input. "start=c(2004,3)" makes the dataset start at March 2004, and "end = c(2016,12)" indicates that it ends at December 2016
res
```

## 4.2: Correolograms
Now that we have cleaned our residuals we are ready to plot the autocovariance.

```{r}
acf(res,type="covariance")
#creates a correlogram by computing the covariance of the residuals of salmon (res)
acf(res)
#creates a correlogram by computing the autocorrelation of the residuals of salmon (res)

```
We use a correlogram to see the relationship between each pair of variables in our dataset. Since the autocorrelations are not near zero, it suggests that it is not random 

## 4.3: Test for stationarity
A stationary time series means that the time series does not depend on time. A time series with trend or seasonality are not stationary, as trend and seasonality affects value of time series over different times.

We use Augmented Dickey-Fuller test and a Kwiatkowski-Phillips-Schmidt-Shin test to test for stationarity in our time series.

```{r}
##Augmented Dickey-FUller Test 
tseries::adf.test(res)  
#applies the Augmented Dickey-Fuller Test to the residuals of salmon (res) by calling the adf.test function from the tseries library. For the null hypothesis H0: series not stationary
#and the alternative H1: series is stationary. We see that we can reject the null hypothesis as the p-value is less than 0.05, so the series is stationary

##Kwiatkowski-Phillips-Schmidt-Shin Test
feasts::unitroot_kpss(res)
#applies the Kwiatkowski-Phillips-Schmidt-Shin Test by calling the unitroot_kpss function from the library feasts and uses the residual of salmon prices as its input. For the null hypothesis
#H0:series is stationary and the alternative H1: series is not stationary. We obtained the p-value as 0.1, which is greater than 0.05, so we fail to reject the null hypothesis, implying
# the series is stationary
```
For the Dickey-FUller Test,we use hypothesis tesing. For the null hypothesis H0:series not stationary, for the alternative H1:series is stationary. Since our p-value = 0.01 <0.05, we reject the null hypothesis meaning that the series is stationary.

For the Kwiatkowski-Phillips-Schmidt-Shin Test, we use another hypothesis testing. For the null hypothesis H0:series is stationary and the alternative H1: series is not stationary. We obtain the p-value = 0.1 > 0.05, so we fail to reject the null hypothesis,implying that the series is stationary.

Both tests have implied that the series is stationary. 

## 4.4: Is the residual noise (res) white noise?
White noise is a form of time series with no predictable trend or seasonality. White noise is composed of iid random values, meaning theyhave same mean, variance and distribution
```{r}
Box.test(res, type="Ljung-Box")

#applies the Ljung-Box test to the residuals of the salmon prices by using the function Box.test. For the null hypothesis H0:there is no autocorrelation in the residuals (white noise), and the alternative
#H1: there is autocorrelation in the residuals. We conclude that there is autocorrelation in the residuals, so the residuals are not random and are correlated at lag 1.


```
The Box-Ljung test utilises a hypothesis test. For the null hypothesis H0:there is no autocorrelation in the residuals (white noise), and the alternative H1: there is autocorrelation in the residuals. From the Box-Ljung test, we obtain the p-value<2.2e-16<0.05, so we reject the null hypothesis, which implies it is not white noise.

#Section 5: Prediction/Forecasting

## 5.1: Using the dataframe

Now that we have tried to model our time series, we can forecast it. We first use our dataframe from earlier to predict our future observations.
```{r}
m = prophet::prophet(salmon.df)
#m calls the function prophet from the library prophet, which requires a dataset (salmon.df) to fit a Prophet forecasting model

f = prophet::make_future_dataframe(m, periods=6, freq="quarter")
#the variable f is a dataframe containing 6 quartely periods in the future, by calling the make_future_dataframe from the library prophet

p = predict(m, f)
# p is a variable that contains predictions of salmon.df by using the predict function and "m" and "f" as its variables

plot(m,p)
#a plot of the salmon.df and the predictions/forecasting made. From the plot we also see black dots, a blue trend line and light blue shading. The black dots indicate the empirical data i.e. the salmon prices at a point in time. The blue trend line indicates the trend of the empirical data
#We observe that the trend goes beyond the actual observations, this is because the blue trend line represents the model's predicted values after the last empirical piece of data. The light blue shading indicates the confidence interval of a data point.


```
We can break this dataframe down into a trend and predicted salmon price plot 
```{r}
prophet_plot_components(m,p) 
#displays the trend and seasonality plots

```

## 5.2: Using Holt Winters model
We use Holt Winters function to predict some observations

```{r}
hw0 =HoltWinters(salmon)
#applies Holt Winters exponential smoothing to the salmon time series and stores it in the variable "hw0"

plot(hw0)
#creates a plot of hw0

p2 =predict(hw0,8)
#predicts 8 period of hw0 and stores it in p2

plot(hw0,p2)
#creates a plot of hw0 and p2
```
Here we forecasted 8 periods of time for the time series salmon.

Now we use Holt Winters function again to predict some observations
```{r}
hw1 = HoltWinters(log_salmon)
#applies Holt Winters exponential smoothing to the logarithic salmon time series and stores it in the variable "hw1"
plot(hw1)
#creates a plot of hw1
p3 = predict(hw1,8)
#predicts 8 period of hw0 and stores it in p3
plot(hw1,p3)
#creates a plot of hw0 and p3
```
This is much better than the Holt Winters prediction for the original dataset.

Alernatively, we can also plot the multiplicative variant of the model above
```{r}
model5=decompose(salmon, type="multiplicative")
#creates a variable "model5" which decomposes salmon (time series) using the multiplicative model

hw2= HoltWinters(salmon, seasonal="multiplicative")
#applies the Holt Winters model with multiplicative seasonal component and stores it in hw2

plot(model5)
#plots the contents of model5, which contains plots: observed, trend, seasonal and random

plot(hw2)
#plots hw2

p4 = predict(hw2,8)
#predicts 8 intervals of hw2 and stores it in p4

plot(hw2,p4)
# plots hw2 and the predictions p4


```
This is better than both the Holt winters prediction for the original and logarithmic version of the dataset.

## 5.3: Forecast Comparison
We can now compare all 4 forecasting to see which one seems the best
```{r}
plot(m,p)
plot(hw0,p2)
plot(hw1,p3)
plot(hw2,p4)


```
I believe that the multiplicative variant has the best forecasting out of all of them.

### 5.3.5: Actual data comparison
Using [Salmon export price dataset](https://www.indexmundi.com/commodities/?commodity=fish&months=120)
we see that in 2017 July fish prices dropped to $8.04, then in August $7.46 and so on.
```{r}
ds = c(0,1,2,3,4,5,6,7)
y = c(8.04,7.46,7.03,6.97,6.24,6.32,7.19,7.28)
plot(ds,y, type ="l")
```
The actual data provides a "V" shape, which is similar to the shape from the forecasted observations of all four models.



# 6 Hyperlink
To access this on my github page press "Click here".

[Click here](https://github.com/hingg123/time-series-cw.git)


# 7 References

-   Meta’s Prophet forecasting system:
    <https://facebook.github.io/prophet/docs/quick_start.html#r-api>
-   salmon export price data:
    <https://www.indexmundi.com/commodities/?commodity=fish&months=120>
