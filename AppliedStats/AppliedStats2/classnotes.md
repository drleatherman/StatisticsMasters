# Applied Statistics 2 Class Notes

## 2019/01/07

### Linear Regression Review

#### Simple Linear Regression (SLR)

Def: A single response as a linear function of a single explanatory variable

**Various forms of a Simple Linear Regression model**

```
E(Y) = b_0 + b_1 * x
E(Y|X) = b_0 + b_1 * x
mu_y|x = b_0 + b_1 * x
```

#### Mulitiple Linear Regression (MLR)

Def: A single response as a linear function of many explanatory variables (plus some assumptions)

If you understand MLR, you can extend it to other models:
    * Mixed/Hierarchical models - B's aren't fixed numbers but have distributions
    * Quantile Regression: modeling for quantile (0-1) instead of mean
    * Generalize Linear Models (GLM): Poisson, Logistic Regression: subpopulations aren't normal, you model a parameer of their distribution (that may not be the mean)
    * Lasso & Ridge Regression: estimate the B's in a way that penalizes them for being big. Is this useful for mitigating outliers?
    * Generalized Additive Models: non-linear left hand side of the equations in the previous slide plus penalization
        - Smooths response along locations

Slope: the change in the mean response for a 1 unit change in the explanatory variable
'associated' == observational study. Cannot establish causation

Interesting Tidbit: *GLM Models use Poisson, then Negative Binomial if Poisson doesn't work instead of the Normal Distribution for a subpopulation*

epsilon (error) = Y - (b_0 + b_1 * x)
If the scatterplot funnels, then a transformation is in order to make it linear (log?)
Transform the X if you are having Linearity issues

F-statistic p-value = How different is the Full model from the equal means model?
H_0: B_1 = 0from the equal means model?
H_A: B_1 != 0

H_0: Reduced model (equal means model)
H_A: Full Model 

In regression, "treatments" are specific values of the explanatory variables. Think ANOVA 

If a randomized experiment is repeated, do you treat each experiment separately or as each experiment as a random sample?

Indicators in Regression
x = 0 or 1

b_0 + b_1 * x
b_1 is the difference between x=1 and x=0

Regression with a Single Indicator variable == 2 Sample T-Test

```r
# where X is an indicator variable
t.test(Y ~ factor(X), var.equal=T, data=data)

lm(Y ~ X, data=data) %>% summary
```

After applying a log transformation, the effect is now a multiplicative effect on the median instead of an additive on the mean (when interpreting the result)

## 2019/01/14

### Multiple Linear Regression

Coefficients are all linear. A model is linear if it can be written as a sum of terms like: b_1 * f(x) where f(x) does not involve b's

**examples**
b_0 + b_1 * x_1 + b_2 * x_2 + b3 * x_3
b_0 + b_1 * x_1 + b_2 * x_1^2
b_0 + b_1 * x_1 + b_2 * x_2 + b3 * x_1 * x_2
b_0 + b_1 * x_1 + b_2 * log(x_2)

**bad examples**
b_0 + b_1 * x_1 + b_2 * x_2^b_3
( b_0 + b_1 * x_1 ) / ( b_2 * x_2 )

The effect of an explanatory variable is the change in mean response when the explanatory variable is increased by 1 unit, holding all other vars constant. 
This is not the case for models like: 
b_0 + b_1 * x_1 + b_2 * x_1^2

a parallel lines model has two variables that do not interact with each other but still have an effect on the response

effect of light intensity doesn't depend on timing
b0 + b1 * light + b2 * early

When toggling indicator variables in a model, the slope and intercept changes 
analysis of covariance = model with one continuous variable and a catagorical variable
parallel lines == anacova model

interaction 
  - effect of one variable varies across different levels of other variables 
  - Two variables are said to interact if the effect of one variable on the mean response depends on the other variable
  - b_3 * (light * early) == interaction
separate lines model is like a parallel lines model except the slopes are not the same.

effect of light intensity depends on timing
b0 + b1 * light + b2 * early + b3 * (light * early)

if my research question was true, how does my model look?

```r
# Variable:Variable indicates an interaction in R. It essentially means Intensity * early on the linear model
fit.sep <- lm(Flowers~Intensity + early + Intensity:early, data = case0901)
```
### Indicator Variables
When doing indicator variables instead of continuous vars, each indicator variable represents the difference in mean between the indicator variable and the baseline variable.

With K categories, you need K - 1 indicator variables. 
The category without an indicator var becomes the baseline category
Indicator = CAPITAL
Continuous = CamelCase

If data is right skewed and positive, then a log transformation will help

The effect of log gestation = b2
For a fixed body size, a 1 unit increase in the log(gestation) is associated with a b2 increase in mean log(brain) size

### Interpretation
Depends on what else is in the model

## 2019/01/28

Linear Regression are vectors where 
x_vector(n x 3) = [
    1 X_11 X_12,]
    1 X_21 X_22, ...
    ]
y_vector(n x 1) = [Y_1 Y_2 ... Y_n]

b_hat_vector(3 x 1) = ((X^T * X)^-1) * (X^T)Y

Confidence Interval = mean
Prediction Interval = single response

Full Model: (parallel lines model)
$\beta_0$ + $\beta_1$log(Mass) + $\beta_2$bird + $\beta_3$ebat

Reduced Model: (equal means model)
$\beta_0$ + $\beta_1$log(Mass)

Most F-tests in class are some form of an Extra Sum of Squares which compares and Full and Reduced model.

Extra Sum of Sqyares = SS resid from reduced model - SS resid from full model

F-stat = (ESS/E deg freedom)/full_model_sigma_hat^2
Extra degrees of fredom = # of params

Overall Regression F-Test
Known as the shotgun test to see whether or not we have an intercept-only model
Null: constant mean
Alternative: at least one $\beta$ is non-zero

a model with a continuous variable is a constrained case of a model

Assumptions for F-test:
* Constant Spread
* Response variables are normally distributed around mean
* Observations are independent

small p-value is evidence against the reduced model, assuming the full model is true.

R^2 (coefficient of determination) = proportion of variance in the response explained by explanatory variables. Adding variables always increase R^2, regardless of whether or not they are important.
Adjusted R^2 increases the additional bariable explained more variance than expected by chance. It may penalize you for adding in a bad field.

R^2 = 1 - RSS/TotalSS
Adj R^2 = 1 - (RSS/n - num_betas)/(TotalSS/n-1)

Interaction chart will crisscross if there are interactions.

Sometimes the simplest explanation is the best. Not everyone agrees with this.

# 02/04/2019

Can remove the intercept to get both categories using -1 in the model. Don't do that 99% of the time. Unless there is a physical expectation that the intercept is truly 0, don't remove the intercept.
Often multiple parameterizations of the same model will be used to answer all the question of interest.

```r
lm(Flowers ~ Intensity * factor(Time), ...)

# shorthand notation for above
lm(Flowers ~ Intensity + factor(Time) + Intensity:factor(Time))
```

## Modeling Strategy
0. Define questions of interest.
1. Explore the data
2. Formulate an inferential model
3. Check the model
  a. if appropriate, fit a richer model i.e. with interactions or curvature
  b. examine residuals
  c. see if extra terms can be dropped
  d. if model not okay, GOTO 1
4. Infer the answers to the questions of interest using appropriate inferential tools
5. Presentation - communicate results

Generally want to start with a model
  * can answer questions of interest
  * includes confounding variables
  * captures important relationships
and be willing to make adjustments as you go

n - estimated parameters => number of variables should be less than n

When combining qualitative and quantitative variables, that is considered doing analysis of covariance.

When in doubt, fit the full model. We can always throw them away if proven insignificant.

Can we use a log transformation so that we are talking in the context of the median?

Least squares estimators are not robust to outliers. Identify outliers early on so you don't end up tailoring a model to fit a few unusual observations.

An observation is said to be influential if the fitted model depends unduly on its value. For example, removing it changes the estimate of parameters greatly, changes conclusions, or changes which terms are indluded in the model.

Leverage $\h_i$
  * Measures the distance of the observaition from the average explanatory values (taking correlation in account)
  * High leverage = unusual combination of explanatory values = possibility to be influential
  * Typically on the extreme of X's for SLR - indicates a case occupies a position in the X-space that is not densely populated
  * Diagonal of the Hat Matrix
$\h_i$ > 2p / n

p = # of parameters / coefficients in the model
Can we penalize High leverage like adjusted R-Squared?

Studentized residuals ($\studres_i$)
  * Residual divided by its expected variation 
    * Variation is a mixed of MSE and leverage
  * High residual = observation far from the fitted line
  * abs(studres_i) > 2 == potential outlier (some people say 3 per the empirical rule)

Cook's Distance ($\D_i$)
  * Effect on estimated parameters when the observation is dropped out
  * What is the effect on your regression model when you move the ith case
  * High Cook's distance = influential on parameter estimates = changes regression estimates
  * D_i > 1 == influential on parameter estimates

outlier tend to be influential but not always

If you think a variable is important enough, leave it in whether it is significant or not.

Be careful about interpreting individual t-tests, especially if it involves a term that is elsewhere in the model

t-test == type 3 sum of squares test

You can test whether terms are necessary with the F-Test

delta method allows you to come up with standard errors for complex parameters like $\beta_1$ / ($\beta_1$ + $\beta_2$)

Partial Residuals
  * Sometimes you want to look at the relatinoship between an explanatory var and the response, after taking account the other vars
  * Always relative to an explanatory var
Residual = Obs - mu_hat
Partial = Obs - (rest of parameters. i.e b_0 + b_1 * explanatoryVar) 

## 2019/02/11

What has been learned so far
* Understand what a multiple regression model is
* Know how to do inference on a single and multiple parameters
* Some extra tools for checking models
* Our general strategy

What is left
* Some special cases
  - Two way ANOVA
  - multi-factor studies
  - no replication - studies with only one value per explanatory response?
* Model selection
* Serial Correlation (Independence not met. i.e Time Series analysis)
* Multivariate responses - what happens when comparing a variable of vector of means against a vector of means?

### Two Way ANOVA

#### One way ANOVA
* One response variable
* One grouping variable with many levels

Null: All means are the same
Alt: At least one is different

Full Model: separate means (all means are different)

Reduced Model: equal means (all have the same mean)

Compare with an extra sum of squares F-test. Afterwards answer particular questions about means

#### One Way ANOVA (Multiple Regression)

$\mu${% women | Judge} = JUDGE = $\beta_0$ + $\beta_1$A + $\beta_2$B + $\beta_3$C + $\beta_4$D + $\beta_5$E + $\beta_6$F

Reduced Model:

$\mu${% women | Judge} = JUDGE = $\beta_0$


#### Two Way ANOVA
One response variable
Two grouping variables with many levels
A Multiple regression model with two categorical variables
Use a **low** dimension of data

**Interesting tidbit** - Pygmalion Effect = High expectations translate to better performance

Balanced Design = each row/column or factor combinations have the same number of observations
  * treatment combination means indicate something different when the data is balanced/unbalanced

Saturated Model = Most complicated model we can possibly fit

parallel lines / additive model
$\mu${} = FACTOR1 + FACTOR2 
(I + J - 1) parameters

non-additive model
$\mu${} = FACTOR1 + FACTOR2 + FACTOR1 * FACTOR2
(I * J) parameters
* There are a variet of ways the means can behave in a non-additive way

Fit the model with the interaction term first and see if its significant.
F-test with (I - 1)(J - 1) and n - (I * J) degrees of freedom

Numerator DF = Number of parameters in full model - # of params in reduced model = (I - 1)(J - 1)
Denominator DF = n - # of B's in full model = n - IJ

Type 1 SS = sequential SS's (variables enter model in order listed);
Type 3 SS = mariginal SS's (what happens if variable in question is last variable entering the model)

If balanced, Type 1 and Type 3 SS are same.

Most of our discussions have been marginal in nature.

block = factor going in that we know will affect the response

**tidbit** - limpet = aquatic snail

**If proportion is in the response, then a transformation is needed**

BALANCED DATA ONLY

.5(mu_fF - mu_f) + .5(mu_LfF - mu_Lf)
* (Effect of large fish with little fish present) + (effect of Large fish with small fish and Limpets present)

# 2019/02/18

## Multifactor Studies w/o Replication

**Replicates**: Multiple measurements at a specific combination of explanatory variable values.
  * Allow a "model free" estimate of variation. i.e. Lack of Fit F-Tests for any model
  * Allows us to try and avoid overfitting a model

What do you do when you don't have replication?
  * Assume some interactions don't exist
  * Treat numerical factors as continuous, not categorical

Without replicates we rely on our model being adequate & using the residuals to estimate variance
  * If saturated model is fitted, there are no degrees of freedom left for estimation (its considered a perfect fit)

the deviation from each observation is used to estimate the variance of a group

Strategy:
  * Two Way ANOVA
  * Fit tentative Model, check for transformations & outliers, refine model, interpret.

If we fit a full model and there are no degrees of freedom left over, then we can't fit the model. 
There will be nothing left to estimate the variance so none of our statistical tools will work.

If we want to make all pairwise comparisons between signs we should adjust for multiple comparisons
Tukey-Kramer for pairwise comparisons
```r
library(agricolae)
HSD.test(lm.add.log, "Sign", console=T)
```

### Categorical vs Continuous

Continuous assumes that you have some rate of change between the variables. Categorical does not make that assumption

If interactions are of interest, then replicate!
If experimental units are expensive, you can sometimes gain more by reducing variability than increasing replicates

Think of important sources of variation when designing experiments

Pseudo Replication: replication needs to be at the level of experimental unit (items randomly assigned to treatment).
Replicates need ot be independent applications of the same treatment.

Read Pseudo Replication xpaper

## Variable Selection

Process of taking a large number of explanatory variables and selecting only a few to be in the regression model.

Big Concepts
  * There are diffeent approaches
  * Compare models with model selection criteria
    * AIC, BIC
  * Generally we consider a few good models, not just the one "best model"

Big Problems
  * Can't trust inference after variable selection
    - Why?
      - Because we only include significant variables
  * Model selection criteria are subject to variability too

Legitimate uses
  * Adjust for a large set of explanatory variables
    - Large # of vras to account for but not of direct interest. Do variable selection on just these vars
  * Prediction
    - want a simple model purely to predeict mean response, you will not interpret p-values or estimates

Illegitimates Uses
  * Fishing for explanations
    - which vars are important? Variable selection will not uncover some "true" model. The best model in one sample wont often be the best in another
  * Interpretation of included variables is dangerous
    - Inclusino depends on what other variables are being considered (particularly if they are correlated)

Multi-colinearity: multiple predictors are highly correlated with each other. Tends to inflate standard errors which drives t-ratios down and p-values up. Does not affect predicting but kills inference.

### Stepwise Methods

Historically popular
  * add or remove a variable one at a time
Only looks at a subset of all possible models

Things that can be controlled
  * Starting point, path through the model
  * choice of next "best" step and stopping point
    * Sleuth uses F-Test
Computationally quick but no guarantee ...

Forward Selection:
Start with an intercept term. Test each term for inclusion, include the "best" (smallest p-value from F-test) one. Repeat until no term passes our threshold

```r
# Pick the smallest p-value to start with
lm1a <- lm(SAT ~ log(Takers), data = case1201) # smallest p-value. select this one!
lm1b <- lm(SAT ~ Income, data = case1201)
lm1c <- lm(SAT ~ Years, data = case1201)
lm1d <- lm(SAT ~ Expend, data = case1201)
lm1e <- lm(SAT ~ Rank, data = case1201)
lm1f <- lm(SAT ~ Public, data = case1201)

# check to see the next smallest p-value
lm2a <- lm(SAT ~ log(Takers) + Income, data=case1201)
lm2b <- lm(SAT ~ log(Takers) + Years, data=case1201)
lm2c <- lm(SAT ~ log(Takers) + Expend, data=case1201) # smallest p-value. select this one!


# Get the next smallest p-value
lm3a <- lm(SAT ~ log(Takers) + Expend + Income, data = case1201)
...
```

#### Backward Selection:
Start with a Full Model. Test each term for deletion, delete the "worst" one (Biggest Value from F-Test). Repeat until no term fails our criteria.

```r
lm.back <- lm(SAT ~ log(Takers) + Expend + Years + Income + Public + Rank, data = case1201)
lm.back1 <- lm(SAT ~ log(Takers) + Expend + Years + Income + Rank, data = case1201)
lm.back2 <- lm(SAT ~ log(Takers) + Expend + Years + Rank, data = case1201)
lm.back2 <- lm(SAT ~ log(Takers) + Expend + Years, data = case1201)
```

Stepwise-selection:
Start with constant mean model

### All subsets
Look at all possible models
Judge them on some measure


#### Measures of fit

If number of params are the same, we prefer the model with small RSS
If different, we want to balance smaller RSS with fewer parameters
  * RSS always gets smaller if you add another parameter

### Common model selection criteria

#### Mallows' C_p stat

C_p = (RSS / Var_full) - n + 2p

if C_p < P, then model potentially will have no problems with bias as long as the full model has none.

#### Bayesian Information Criterion (BIC)

BIC = n * log(RSS / n) + log(n) * (p + 1)
* smaller the value, the better the fit (includes negative value. we want larger negative values)
* R's BIC function uses a different formula

#### Akaike Information Criterion (AIC)

AIC = n * log(RSS / n) + 2 * (p + 1)

* smaller the better
* R's AIC function also uses a different formula

#### Other Useful Methods

Principal Component based methods 
* large dataset with lots of variables
* Reduce size of dataset without reducing the variance
* New dataset with almost all the variables that are not correlated
* Can be tough to draw inference

Penalized Methods
  * Ridge, Lassos, Lars
Best Linear Unbiased Estimators (BLUEs)

# 2019/02/25

How correlated do two variables need to be in order to remove them from the model?

Quadratic terms allow for curvature. The interaction variables allow for interaction.
* Models shouldn't include quadratic terms if they don't include the linear one
* Models shouldn't include interaction terms if they don't include the main effects

Strategy: find a subset of good models, then restrict attention to those that follow good practice.

**What does center mean?**
  * polynomial - s^2 => (s - mean(s))^2
  * interaction - a*s => (a - mean(a))(s - mean(s))
When you include quadratic terms in the model, center quadratic terms to remove correlation with linear terms.
When you have two vars that are correlated, it is redundant information which causes multi-collinearity.

Multi-collinearity: when strongly correlated predictors occur in the same model
  * Does not impact regression coefficients
  * Often causes Standard Errors of $\beta_hat$ become larger than normal. Causes us to falsely fail to reject the null hypothesis

Lowest number of parameters for Mallow C_p and lowest BIC ideal

AIC/BIC some measure of variation in the model which assigns a penalty to worthless variables

Frequentist (Classical Statistics) vs Bayesian
  Frequentist - Given we have a parameter **theta**, what is the probability that the **data** fits
  Bayesian - Given we have the **data**, what is the probability that **theta** fits

## Time Series Analysis

Linear
A model for the mean: mu = b_0 + b_1 * X_1 + ... + b_p * X_p

### Serial Correlation a.k.a Autocorrelation

The multiple regression tools rely on the observations being independent (after accounting for the effects of expl vars)

Often when measurements are made at a adjacent points, there is a correlation

Can adjust for skewness by centering
Run: Number of consecutive observations above or below the mean

Positive Serial Correlation
  * an observation on one side of the mean tends to be followed by another observation on the same side of the mean
  * makes actual SE much larger

Negative serial correlation:
  * an observation on one side of the mean tends to be followed by another observation on the opposide side of the mean

#### Two Solutions
1. Adjust SE to be more appropriate
2. Filter variables to remove correlation

For both, you need to estimate the extent of the correlation (and make an assumption about its structure)

More advanced methods explicitly model the correlation
  * Time series analysis
  * Longitudinal data (Panel data in economics)

Adjusted SE on the sample average where r1 is the first serial correlation coeffcient. Appropriate under the autoregressive model of order 1, denoted as AR(1)
  * series is measured at equally spaced times
  * let v be the long run series mean, then mean{Y_t - v | past history} =...

SE_ybar = sqrt((1 + r1)/(1 - r1)) * s / sqrt(n)

** Used for two-sample T's or ANOVA where we need to account for serial correlation

#### Filter Variables

**Review the slides. Paid attention in class**

examine for serial correlation in the residuals, not the raw response

AR(1) => Autoregression Model with ORder 1 (i.e. Lag 1)

##### Testing for Serial Correlation

Large Sample Test

Z = r1 * sqrt(n)

If there is no serial correlation, Z has a normal distr.
* only appropriate when n > 100

Runs Test
* Count how many runs there are and compare to how many we would expect by chance alone with no serial correlation
* Simple non-parametric test

Is AR(1) Model adequate? - Primary tool is the PACF (Partial Autocorrelation function) plot


# 2019/03/04 - Multivariate Analysis

Measurement Unit: What is the object I am taking a measurement on?

Sampling Unit: A unit that is randomly selected from a population that I am taking a measurement on.

Experimental Unit: A unit that is being experimented on.

Multivariate response 
* Instead of a single value, the outcome (or response) is a vector of values

Repeated Measure:
* A special kind of multivariate response where the same variable is measured several times on each sampling or experimental unit
* Unlike Replicates which are multiple experimental at each combo of treatments

Longitudinal Studies
* response measured on each unit at multiple times. Units randomized to treatmeents at start

Crossover Experiments
* Response measure on each unit after each treatment

Fixed vs variable effects

Split-plot with Repeated measures
* 2 Factors involved. Units are randomized to levels of the first factor at the starts, then response is measured after each level of the second factor (in a random order)

Split-plot with Repeated measures at several locations
* Like above but over time

## Strategies

1. Single univariate analysis on a summary of the multivariate response
  * average, min, max, slope, etc
2. Separate univariate analyses on several summaries
  * only if uncorrelated and don't need to adjust for making many comparisons (i.e. Bonferroni)
3. Multivariate analysis on several summaries
  * Hotelling's T^2 - multivariate T-Test. mean vectoer of mu1 == mean vector of mu2
4. Treat subject (units) as a factor
  * If multiple measurements on one individual are independent (i.e. chimpanzee & signs study)

Profile Plot - See how a response changes over time for each subject

**Who validates an experiment's design?**

For correlated response variables, the confidence region may be an ellipse which is hard to compute and present

Hotelling's T^2 Adjustment adjusts the univariate confidence intervals to conservatively approximate the ellipse
Hotelling's T^2 statistic provides a joint test for both parameters at once

**For Hotelling's T, does the ratio of Hotelling's area and the area of the ellipse represent the confidence percentage?**

Ideal Model for Monkeys
1. The mean of response i in population j is mu_ij
2. Both pops have the same std devs of response, sigma_i
3. Both pops have the same pop. correlation between responses 
4. Responses normally distr. around means
5. 

pooled variation: grouping by responses
pooled covariance: grouping by groups

**Tidbit: Principal components - research this. how to interpret?**

T^2 can be transformed into an F distribution

Trace: a sum of the diagonal of a matrices

Pillai's trace - multivariate test statistic - multiplying eigen values of var-covar matrices

```r

model <- lm(cbind(col1,col2)~Group, data=data)
anova(model)

OR

library(Hotelling)
fit1 <- hotelling.test(cbind(col1,col2)~Group, data = data)
fit1
```

Joint Confidence Intervals
An ellipse is the best description of our join confidence, but hotelling's adjusted conf int. guarantee at least 95% conf.

estimate +- multiplier * SE

multiplier = sqrt(s * sum(n) - 2 / sum(n) - 3) * qf(.95, ...)

**Review some of week 10's slides. Missed content due to phone call with Mark. End at HighFiber experiment**

**When is a density curve better than a histogram?**

Tukey-Kramer compares independent groups
Paired T-Test with multivariate compares datapoints within a subject

## Single Bivariate Sample

T^2 = (t_1^2 + t_2^2 - 2 * r * t_1 * t_2 ) / 1 - r^2

where r is the sample correlation between the two responses, and t_n is the sample t-statistic

multiplier = sqrt((2 * (n - 1) / (n - 2)) * qf(.95, 2, n- 2)

# 2019/03/11

## Multivariate Tests

**Extending univariate tests to multivariate tests**

univariate looks at each variable by itself
multivariate takes into consideration how variables may be correlated

Two-Sample T^2-Test
* Two vectors of p predictors
* Same variances and same covariance matrices

(pooled covariance matrix) S_pl = 1 / (n_1 + n_2 - 2)[(n_1 - 1)S_1 + (n_2 - 1)S_2]
  where S1 and S2 are p x p estimated covariance matrices from each sample

(pooled variance) S^2_pl = ((n1 - 1)S1^2 + (n2 - 1)S2^2) / n1 + n2 - 2

If reject H_0 for Hotelling's T^2,
  1. One Univariate
  2. One multivariate

1. Run univariate t-test, one for each variable. However, for the Confidence Intervals, use
  * sqrt((n1 + n2 - 2)p/(n1 + n2 - p -1) * F{p,n1 + n2 - p - 1}(1 - alpha))

2. Look at abs val of coefficients from standardized discriminant function
a = S^-1_pl(ybar1 - ybar2) (unstandardized - use if vars are similar in scale & variance)
and
a* = (diag S_pl)^1/2 * a (standardized - use if vars have different scale or variance)

```r
%*% => matrix multiplication operator in R
anova(lm(cbind(col1,col2,col3,col4) ~ Group, data = myData))
```

Next steps? Run T-tests on each value or discriminant analysis

* If Sample sizes n1 and n2 are large, Hotellings T^2 is approx. correct
* if n1 = n2, Hotelling's T^2 is robust to departures from sigma1 = sigma2
  * else, Hotelling's T^2 becomes less robust

## MANOVA

Hotelling's T^2 is a special case of MANOVA with 2 groups

**k** independent random samples of size **n** from p-variate normal populations with equal covariance matrices

**Between** group sums of squares. p x p matrix withh between group SS for each p vars on the diagonal

H = n sum(ybar_.i - ybar..)(ybar_.i - ybar_..)`  (\` == transpose).

**Within** group SS if balanced

E =

E^-1 * H mimics between SS and within SS so drives the following tests

Four possible Test statistics to test H0
* Wilks' Test Statistic aka Wilks lambda

Lamda = det(E) / det(E + H)

Liklihood Ratio Test. Analagous to an F-test

* Roy's Test

theta = lambda1 / 1 + lambda1

lambda1 = largest eigenvalue of E^-1 * H


* Pillai's Statistic

sum(1-s,lambdai / 1 + lambdai)
s = min(k - 1, p)

* Lawley-Hotelling Statistic

### Comparing them

* Wilks' Lambda - most widely used
* if k > 2, each test stat can take on different values & one test is not usually superior than the others in all circumstances
* Roy's theta is not recommended in any situation unless all mus are collinear under standard MANOVA assumptions
* all tests robust to nonnormal pops exhibiting skewness or positive kurtosis.
  * Pillai's stat is superior to other when there is a heterogeneity of covariance matrices
  * Wilks lambda can be used except when there is severe heterogeneity of covariance matrices
* Most MANOVA software programs calculate all 3 and reach the same conclusions
  * When they don't, dig deeper

intraclass correlation = MSB - MSE / (MSB + (n + 1) * MSE)
n = # of obs per group
if intraclass correlatino < 0.5, then obs are not strongly correlated with each other