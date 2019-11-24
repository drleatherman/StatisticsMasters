# Applied Statistics *9/24/2018*
Paired T-Test looks for Mean difference 
Two Sample T-Test looks for Differenc in Means (slightly different)

Pooled Standard Deviation weights standard deviations for two populations. This is based on the fact that the distribution is a combined chi-squared distribution.
```r
# gives the probability based on a t-statistic and a degrees of freedom.
pt(t, df)
```
Randomization Distribution - If the null is true, it is the null distribution

If you have vague hypothesis, then you will not be able to estimate a parameter and thus confidence intervals.

The t-test is a generalization of a Randomization test when the additive treatment model is assumed

# Applied Statistics *10/1/2018*

A test is robust if the test is still valid even if the assumptions are not valid. For example, a test is valid if 5% of the time, the test rejects the null hypothesis.

Check if a procedure is valid under some violation of the assumptions
* Use mathematical theory
* Use simulation

How to determine whether or not a Confidence Interval is valid using Simulation
1. Decide the type of violation 
2. Simulate data according to violation in #1
3. Calculate a confidence interval for simulated data
4. Repeat steps #2 and #3 for how often it captures the true parameter

As long as sample sizes and standard deviations are the same, then t procedures work very well.

## Tests for Normality

* Shapiro-Wilk Test

H-naught: Data are normally distributed
H-alt: Data are not normally distributed
The shapiro test gives a p-value which indicates whether or not we should reject the null hypthosis. It is the equivalent of having the check engine light come on. If you have a very large sample and the data is slightly off of Normality, then the Shapiro-Wilk test will be way off.

```R
shapiro.test(value)
```

log == natural log to statisticians

right-skewed data should have a log transformation applied to it. Economic data is usually right-skewed. i.e. household income data

If the boxplot shows one median higher than the other, than a natural log transformation is good. 

### Procedure for Randomized Experiments
1. Take the log of the data
2. Perform t-test using the log values. If p-value is small, we have evidence the additive treatment effect on the log outcome is not zero.
3. We estimate multiplicative treatment on the untransformed outcome. Apply the exp to the values again so we can give the results in the original scale.

### Procedure for Observation Studies
1. Take the log of the data
2. Perform t-test using log data.
3. We estimate the median response for population. We can do this because medians and means are the same for normal data and we are assuming that this is normalized data for the t-test

# Applied Statistics *10/8/2018*

Alternatives to t-test -> Non-Parametric tests

Two Independent Samples0
* Two sample t-test
* Randomization test
* Wilcoxon Rank Sum / Mann-Whitney Test / Rank Sum Test
  - resistant to outliers
  - doesnt assume normality
  - null hypothesis: population distributions are the same
            OR     : addtitive treatment effect is zero 
  - Treat it as a hypergeometric distribution when determining the p-value
  - should not use the asymptotic technique when there are a lot of ties. Technically, it should be used when there are no ties
  - attach the dataframe first. Acts strange without it
  - Assumptions: Two populations have the same shape. Equal standard deviations. Independence of subjects within and between group
  - Robust to assumptions?: for and and 2 above - still valid but tests a different null hypothesis (null: two populations are identical)
  - Resistant to outliers?: Resistant
- Test statistic: Sum of Ranks
* Levene's Test
  - Test for equal population standard deviations
  - Some say you shouldn't use Levene's test to choose between two-sample t-test and Welcha's t-test.
  - "Check engine" test similar to Shapiro-Wilk test
  - Checking to see how observerations deviate from the median? "absolute deviation from the median" not commonly used in statistics
  - failing to reject doesn't tell you anything about the standard deviation
  ** How is this different than ANOVA? **
* Welch's t-test
  - t-test without assumption of equal standard deviations
  - approximation to the t-distribution
  - Uses Satterthwaite (sp) estimation for degrees of freedom. Essentially weights it by variance similar to the pooled variance.
  - Almost as good as the usual two sample t-test when the equal variance assumption is met and much better when the assumption is violated. Some researchers recommend to always use Welch's t-test. More complicated models tend to make the equal variance assumption
  - Null Hypothesis: difference in populations means is zero
  - Assumptions: Normal pops of abs deviations from median
  - Resistant to outliers?: Less Resistant. Still could have outliers that screw things up
* Paired t-test
* Sign Test
  - Null Hypothesis: Median difference is 0
  - Assumptions: Independence of subjects within groups
  - Robust? No
* Wilcoxon Signed Rank Test
  - more efficient
  - no assumption of normality
  - Null Hypothesis: Differences are symmmetric about 0
  - Test statistic: Sum of the ranks of positive differences

  1. Using abs val of differeneces, order from smallest to largest
  2. Drop zeros
  3. Rank abs values from smallest to largest. Ties are averaged.
  3. sum ranks where differences are positive
    - Under null we expect the test stiastic to be about n(n+1)/4

The power of a test is the probability that we will reject the null if the null is false. Typically we fix the significant level (alpa) and do what we can to maximize the power.

Using fewer assumptions often come with a decrease in power. 

### Resistant test with paired data.

Either or a two sampled t-test is appropriate or a paired t-test is appropriate, never both.


# Applied Statistics 10/15/2018

ANOVA F-test, two group t comparisons, linear combinations

Assumptions
1. Equal Variance 
2. Normally distributed
3. Independent samples between and within groups

Each sample provides an estimate of sigma with its sample std dev. We get the best estimate of sigma by making use of all the different information we have available and pooling all the sample standard devs. Don't necessarily want to do that if the std devs are too different.

Testing for Equal Variances. Very sensitive to departures from Normality
1. Breusch-Pagan (bptest)
2. Bartlett Test (bartlett.test)

Testing for Normality
1. Shapiro-Wilk (shapiro.test).
2. Lilliefors Kolmogorov-Smirnov Test (lille.test, nortest library)
3. Anderson-Darling (nortest)
3. Normality Probability Plots ()

General rule of thumb: If max(sd) / min(sd) > 2, then you probably dont have equal spread. If yes, then you shouldnt be pooling.

### ANOVA

Hope to see that the variance between the groups is significantly different than variance within the groups.
Comparing different sources of variation.

#### 1-way ANOVA 

Null: Assume all population means are the same
Alt: At least one is different

Full Model: a model that fully describes the set of alternatives
* One way ANOVA: all means could be different, aka separate means model.

Restricted model: a restriction of the full model imposed by the null hypothesis.
* One way ANOVA: all the means are the same aka equal means model

Extra Sum of Squares F-test compares how well models fit
Residual: Distance between the observation and the estimate for its mean

Sum of squared residuals: Residual Sum of Squares (RSS): For each model, square each residual and add them up.

Single mean model residuals (null/restricted model):
Obs - best guess for single mean (ybar)
Combine groups of means into single mean

Separate means model residuals (full model):
obs - best guess for group mean (ybar-i => average for group i)

Extra Sum of Squares = RSS_restricted - RSS_full
Extra df = df_restricted - df_full

Within group MS == MSE (Mean Squared Error)

#### ANOVA Table
Source      | SS          | df    | MS = (SS/df) | F                      | p-value (always upper tail)
Between Grp | BG_S        | J - 1 | BG_S / n - 1 | between.MS / within.MS | 1 - pf(F, J - 1, n - J)
Within Grp  | WG_S        | n - J | WG_S / n - J
Total       | BG_S + WG_S | n - 1 | SS / n - 1

Square Root of Within Grp MS = pooled standard deviation (s.p)
anova(lm(Response ~ Group))

When you have one numerator degree of freedom in an F test, you can use the t-test.
Book to read: *Weapons of Math Destruction*

# Applied Statistics 10/22/2018

## ANOVA

One way ANOVA is a common starting point for multiple group analysis but rarely directly answers the question of interest. If we know at least one will be different, then One Way ANOVA is not necessary.

* Robust
 - non-normal populations with large enough sample sizes.
* Not Robust
 - equal standard deviation assumption
 - indepedence between and within groups

Plot
* Residuals by group
* Res. against fitted means
* Res. Against other vars (like time)

Q: Why wouldn't we want to plot the distance from the squares as done in linear regression models?
A: Because this output is coming out **after** running a model. We need to run a model in order to get the residuals in the first place so we cannot plot a regression against residuals without running a model.

Normality plot. If near the line, then its normal. Can use a Shapiro-Wilk test to confirm. Can stray a little from the fitted line but if we have small sample sizes, we should be wary.

```R
ggplot(model1, aes(sample = .resid)) + stat_qq() + stat_qq_line()
```

Can do summary statistics on Residuals as well. Boxplots and Residual points are helpful in understanding whether or not the data is normal.

## Kruskal-Wallis ANOVA 

* Non-parametric version of the ANOVA test
* Removes the assumption of normal populations and is resistant to outliers.

Null: All groups have the same median
Procedure: Convert response to ranks (ignoring groups like Wilcoxon Rank Sum), then do ANOVA on ranks (almost)

## Linear Combinations

When using linear combinations of means, we can substitute sample averages.

g = C1mu1 + C2mu2 + ... + CImuI
g = C1ybar1 + C2ybar2 + ... + CIybarI

When you want to fit a model in R without the intercept, subtract 1 from the group.
```R
lm(Quant ~ Qual - 1)
```

Individual Error Rate: the probability of incorrectly rejecting the null hypothesis in a single test, alpha.

Familywise (or experimentwise) error rate: the probability of incorrectly rejecting at least one null hypothesis in a family of tests.

General strategy is to reduce individual alphas so that when conducting familywise tests, the overall alpha will be 0.05.

# Applied Statistics 10/29/2018

glht framework

only subtract 1 to remove the intercept when using this package

LSD.test in package 'agricolae'
* Least Significant Difference
* If they have the same letter, they are statistically similar.
* Just another way to tell if familywise comparisons are similar

Multiple Comparison Procedures
* Attempt to control the familywise error rate and familywise confidence level.
For Tests, increase the p-value the more tests we do, "adjusted p-values" OR decrease significance level, the more tests we do.

For confidence intervals: make the intervals wider, the more comparisons we make.
```
estimtate +- multiplier * SE
```

```r
attach(case0601)
model1 <- lm(Score~Handicap, data = case0601)
LSD.test(model1, "Handicap", p.adj = "none", console = T)

# Plot it out! If the interval does not contain 0, it is significant
qplot(lhs, estimate, data= confint(comparisons, calpha = univariate_calpha()), geom = "pointrange", ymin = lwr, ymax = upr) + coord_flip() + geom_hline(yintercept = 0) + xlab("Comparison")
```

Don't rely on the graphs overlapping to tell if they are different. Must look at the actual samples.

controls the familywise error rate when making...

Dunnett - difference in mean comparisons between the one group and all the other groups
* gives most narrow intervals
* J - 1 intervals
```r
comparisons <- glht(model2, linfct=mcp(Handicap="Dunnett"))
summary(comparisons)
confint(comparisons)
qplot(lhs, estimate, data= confint(comparisons)), geom = "pointrange", ymin = lwr, ymax = upr) + coord_flip() + geom_hline(yintercept = 0) + xlab("Comparison")
```
Tukey-Kramer - all pairwise difference in mean comparisons
* J choose 2 intervals
```r
# multcomp. note model2 is like model1 but without the intercept
comparisons <- glht(model2, linfct=mcp(Handicap="Tukey"))
summary(comparisons)
confint(comparisons)
# agricolae
HSD.test(model1, "Handicap", console = T)

qplot(lhs, estimate, data= confint(comparisons)), geom = "pointrange", ymin = lwr, ymax = upr) + coord_flip() + geom_hline(yintercept = 0) + xlab("Comparison")
```
Scheffe - all possible linear contrasts of means
* widest ()
* if you examine your data for linear combinations that give the smallest p-value, Scheffe would be the most appropriate
* basically if you aren't doing pairwise combinations
Bonferroni 
* for any set of k comparisons
* if you have too many comparisons, your intervals will be wide and you may not find anything significant
* guarantees familywise error rate is at most 5%
```r
# k is the number of comparisons that you are making
qt(1 - .05 / (k * 2), df) 

# change the confidence level manually to reflect the adjustment
qplot(lhs, estimate, data= confint(comparisons, level = 1-0.05/10, calpha = univariate_calpha()), geom = "pointrange", ymin = lwr, ymax = upr) + coord_flip() + geom_hline(yintercept = 0) + xlab("Comparison")
```
Tukey and Dunnett would be decided ahead of time based on the research question (planned)

Controlling familywise error rates are controversial - What is an experiment?

Always 
* figure out your comparisons prior to collecting data
* report how many comparisons you planned to do


## Simple Linear Regression (SLR)

```r
qplot(Velocity, Distance, data = case0701) + geom_smooth(method = "lm", se = F) + xlab("Recession Velocity") + ylab("Distance")
```
X = Explanatory Var
Y = Response Var

y_hat = Intercept + Slope*x
Slope gives associated change in **mean** response for 1 unit increase in explanatory var

#### Scope
* Scope of a model is min - max.
* If the intercept is beyond the scope, then don't interpret the scope
* Generally don't care about the Intercept
* Interpolation = making predictions within the scope of the model
* Extrapolation = making predications outside the scope of the model

Typically don't force an intercept of 0

Q: For a SLR model with a clear outlier that is within scope, can we trust it? 
A: Outliers on the X tend to be influential and we would note it. Ideally, one observation by itself shouldnt be swaying the model.

# Applied Statistics 11/5/2018

## Simple Regression

### Assumptions
* Normal subpopulation distribution of each value of X
* Means fall on a straight line - linear relationship
* Each subpopulation has the same standard deviation
* Independent observations

**Are models based a function of x still considered linear models?**

residual is difference between the observed response and its fitted value

Least Squares = Lines that gives the least possible sum of squared residuals = MVUE (Minimum Variance Unbiased Estimator) of line

consistent estimator = lim n-> inf (var(theta) = 0)
residuals provide an estimate of variation:

sigma_hat = sqrt(sum of squared res / degrees of freedom) 
    = sqrt(SSE / n - 2)
    = pooled std dev
    = Residual standard error when summary(model) is run

degrees of freedom = number of obs - number of estimated parameters

stderr_b1_hat = sigma_hat * sqrt( 1 / sum((x_i - x_bar)^2))

less certain about our estimate of the slope when we have a larger standard error

Three types of inference
1. Inference on the slope or intercept
 * uncertainty comes from sampling error in a single parameter
2. Inference about the mean response (at a given value of X)
 * uncertainty comes from sampling error in both params
3. Prediction of a new response (at a given value of X)
 * uncertainty comes from sampling error in both parameters and variability in subpopulations

Would not apply a t-test to a predicted value. Would do it for a mean value though

```r
# confidence interval for b0_hat and b1_hat for a model
confint(model.distance)
```

predicted value = b0_hat + b1_hat * X + (t * SE)
closer to the mean indicates a smaller confidence interval

```r
# check which value has the smaller standard error
df <- data.frame(Velocity = c(200,1000))
predict(model.distance, df, se=T)

# Confidence intervals
qplot(Velocity, Distance, data = case0701) + geom_smooth(method = "lm")

# Prediction Intervals
t_var <- predict(model.distance, interval = "prediction")
new_df <- cbind(case0701, t_var)
qplot(Velocity, Distance, data = new_df) + 
    geom_smooth(method = "lm") + 
    geom_line(aes(y=lwr), color = "red", linetype="dashed") + 
    geom_line(aes(y=upr), color = "red", linetype = "dashed")
```

# Applied Statistics 11/8/2018

### Assumptions
* Normal subpopulation distribution of each value of X
* Means fall on a straight line - linear relationship
* Each subpopulation has the same standard deviation
* Independent observations

Steps
1. Plot response against explanatory - create a scatterplot
 * are there problems that make linear regression look inappropriate? Would a transformation help?
2. Fit linear regression model
3. Use residuals to re-examine assumptions

When we did t-tests and ANOVA, we validated assumptions and then ran the model
When we do linear models, we run the model than validate assumptions.

T-tests and ANOVA could be done through modeling and force the issue though we didn't. If you have a model then you get residuals so you can run models first.

Can use standardized residuals to determine whether residuals are expected or unexpected.

Three residuals to check
1. Residuals against fitted values
* Linearity and constant variance
* **Not Robust** to Linearity
  * If not Linear, consider a more complicated model for the mean, or transform response, or explanatory, or both.
* **Not Robust** to constant spread. Least Squares Estimates will be unbiased by the standard errors will be wrong.
2. Res against explanatory variable
* Linearity and constant variance
3. Normal prob plot of residuals
* Residuals should be approximately normally distributed.
  * Theoretical is a z-score and attempts to match it up against a normal distribution
* **Robust with large samples**
  * If not normal, prediction intervals are misleading because prediction intervals rely on an underlying normal distribution
* Remedy: Transformation of response (or generalized linear models (GLM))
4. Independence
* **Not Robust** Least squares estimates will still be unbiased, but standard errors will be wrong
* Remedy: More complicated models

If linear assumption is true, the residuals should be equally spread either side of the zero, not systematically above or below zero
```r
# loess (Locally weighted sums of squares) curve
# bands are the confidence bands
model <- lm(Y ~ X, data = data)
qplot(X, .resid, data = model) + geom_smooth()
```

To do Levene's Test with Linear Regression, look at explanatory variable of X and find the median. Anything above is group 1, anything below is group 2. Compare those two groups. Could also do the Breusch-Pagan test (sensitive to normality)


### Log Transformations - Interpreting the Slope
Log Transform Y
* Spread is larger with larger mean_y and mean_y_given_x doesn't look linear in X
* 1 unit increase in X associated with a multiplicative increase of exp(B1) in the meadian
* assuming mean Y equals median Y on a transformed scale

Log Transform X
* Spread is constant but mean_y_given_x doesnt look linear in X
* doubling of X is assoicated with an additive increase of B1 * log(2) units in the mean response

Log transform X and Y
* Spread is larger with larger mean_y and mean_y_given_x doesn't look linear in X
* doubling of X associated with a multiplicative increase of 2^B1 times in the median response
* assuming mean Y equals median Y on the transformed scale

Comparing Models with One Way ANOVA
Equal Means model -> Regression Model -> Separate means model
Only works if there is at least 1 observation per group

## Regression ANOVA

H0: B1 = 0  <= reduced model = equal means model
HA: B1 != 0 <= full model = regression model

#### Regression ANOVA Table
Source                   | SS          | df    | MS = (SS/df) | F                      | p-value (always upper tail)
Between Grp (Regression) | BG_S        | p     | BG_S / p | between.MS / within.MS | 1 - pf(F, J - 2, n - J)
Within Grp (Residual)    | WG_S        | n - 2 | WG_S / n - 2
Total                    | BG_S + WG_S | n - 1 | SS / n - 1

p predictors

```r
# p-value of slope
summary(model) # F-statistic is the Regression ANOVA F statistic
anova(model)
```

If you have more than one observation at a given X, you can treat it like a class.
If you have Replication, you can treat it like a class and use ANOVA

Lack of Fit F-Statistic 
(
    (RSS_reduced - RSS_full) / (df_reduced - df_full) [J - 2]
) / 
(
    RSS_full / df_full
)

```r
model.reg <- lm(log(Time) ~ Voltage, data = case0802)
model.sep <- lm(log(Time) ~ Group, data = case0802)
anova(model.reg, model.sep)
```

R-squared: Measures the proportion of variation in the response that is explained by the regression model for the mean
Between 0 (implies slope = 0) and 1 (perfect fit)

### Correlation
Measures the strength of a linear relationship between X and Y
Correlation only makes sense if the data are pairs drawn from a population.

**The only way to tell if linear regression is appropriate is to examine the data (or the residuals)**