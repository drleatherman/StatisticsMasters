# Applied Statistics 1 Summary Guide

# Definitions

**Robust**: A test is robust if the test is still valid even if the assumptions are not valid. For example, a test is valid if 5% of the time, the test rejects the null hypothesis.

**Test Power**: The power of a test is the probability that we will reject the null if the null is false. Typically we fix the significant level (alpha) and do what we can to maximize the power. Using fewer assumptions often come with a decrease in power. 

**Causal Inference**: Using data to make inferences about cause and effect relationships. This can be done when experimental units are randomly assigned to treatments of interest.

**Randomized Experiment**: A study in which experimental units are randomly assigned.

**Full/Separate Means Model**: a model that fully describes the set of alternatives. Best guess for group means (ybar_i => average for group i)
  * One way ANOVA: all means could be different, aka separate means model.

**Restricted/Null/Single Mean model**: a restriction of the full model imposed by the null hypothesis. Best guess for a single mean across groups (ybar).
  * One way ANOVA: all the means are the same aka equal means model

**Residual**: Distance between the observation and the estimate for its mean.

**Residual Sum of Squares (RSS)**: For each model, square each residual and add them up.

**Individual Error Rate**: the probability of incorrectly rejecting the null hypothesis in a single test, alpha.

**Familywise (or experimentwise) error rate**: the probability of incorrectly rejecting at least one null hypothesis in a family of tests. General strategy is to reduce individual alphas so that when conducting familywise tests, the overall alpha will be 0.05.


# **Tests**

# Parametric Tests

## T-test
* generalization of a Randomization test when the additive treatment model is assumed
* Either a two sampled t-test is appropriate or a paired t-test is appropriate, never both.

#### General Procedure
1. Analyze data and produce summary statistics
  - Box plots, histograms, dot plots, 
1. Define null and alternative hypothesis
2. Determine appropriate test
3. Validate assumptions of chosen test are met. Usually by running more tests
4. Conduct test
5. Interpret and Summarize results

#### General Assumptions
1. Normally Distributed
2. Independent Samples
3. Equal Standard Deviations

### Paired
* find Mean difference between two samples
* Use when the data is paired

### Two Sample T-Test
* find Difference in Means (slightly different than Paired T-Test)

#### Robustness to Assumptions
1. Normal Distr: When large sample size
 
### Welch's
* Estimates DF by weighting individual DF by variance
* Less Resistant to outliers than other T-tests but not fully

Null: difference in pop means is zero

#### Robustness to Assumptions
1. Normal Distr: When large sample size
3. Equal Std Devs: Not an assumption
4. Abs Deviations from Median are Normal

### Randomization Test
* Used for experiments to randomize the subjects since experiments are **not** random samples
* Usually sample averages but could be variance, skewness (kurtosis), or an induced fixed additive change in response

Null: no difference between treatments 

**Additive Treatment Model**: response on treatment 2 is their response on treatment 1 plus some fixed number X that is the same for everyone

#### Assumptions
1. Random Allocations to groups

## ANOVA

Hope to see that the variance between the groups is significantly different than variance within the groups.
Comparing different sources of variation.

### 1-way ANOVA

Common starting point for multiple group analysis but rarely directly answers the question of interest. If we know at least one will be different, then One Way ANOVA is not necessary.

Null: Assume all population means are the same

#### Assumptions
1. Normality 
2. Equal Std Deviations
3. Independence between and within groups

#### Robustness to Assumptions
1. With Large Sample Sizes

#### ANOVA Table
Source      | SS          | df    | MS = (SS/df) | F                      | p-value (always upper tail)
Between Grp | BG_S        | J - 1 | BG_S / n - 1 | between.MS / within.MS | 1 - pf(F, J - 1, n - J)
Within Grp  | WG_S        | n - J | WG_S / n - J (MSE)
Total       | BG_S + WG_S | n - 1 | SS / n - 1

Square Root of MSE: Pooled Std Dev.

#### Procedure
1. Calculate Summary Statistics
2. Determine Hypothesis
3. Execute ANOVA model
4. Plot and Analyze Residuals
  * Residuals by group
  * Res. against fitted means
  * Res. Against other vars (like time)
  * Summary statistics. Boxplots and Residual points are helpful in understanding whether or not the data is normal.
5. Test for Assumptions based on Residuals

#### Multiple Comparisons
* Control the familywise error rate and confidence level when running multiple tests

When number of Tests Increase 
  * increase p-value (adjusted p-values) OR decrease significance level
  * Make confidence intervals wider. i.e. estimate +- multiplier * SE

When comparing two groups that are comprised of multiple groups, ensure Coeffecients for the two groups add up to 1 and -1 respectively. 

SE_multComp = pool.stdev * sqrt(sum(C_i^2/n_i)) where i is the group


##### Familywise comparisons

Always 
  * figure out your comparisons prior to collecting data
  * report how many comparisons you planned to do

**Dunnett**: difference in mean comparisons between the one group and all the other groups
* J - 1 intervals (narrowest)
* Decided ahead of time based on research question

**Tukey-Kramer**: all pairwise differences in mean comparisons
* J Choose 2 intervals
* Mostly used since most comparisons are pairwise
* Decided ahead of time based on research question

**Scheffe**: all possible linear contrasts of means
* widest
* Best when examining for linear combos that give the smallest p-value and not doing pairwise comparisons

**Bonferroni**: alpha = .05 / k * 2 where k is the number of comparisons
* guarantees familywise error rate is at most 5%

### Regression ANOVA

H0: B1 = 0  <= reduced model = equal means model
HA: B1 != 0 <= full model = regression model

#### Regression ANOVA Table
Source                   | SS          | df    | MS = (SS/df) | F                      | p-value (always upper tail)
Between Grp (Regression) | BG_S        | p     | BG_S / p     | between.MS / within.MS | 1 - pf(F, J - 2, n - J)
Within Grp (Residual)    | WG_S        | n - 2 | WG_S / n - 2
Total                    | BG_S + WG_S | n - 1 | SS / n - 1

Lack of Fit F-Statistic 
(
    (RSS_reduced - RSS_full) / (df_reduced - df_full) [J - 2]
) / 
(
    RSS_full / df_full
)

# Non Parametric Tests
  * Resistant to outliers
  * No assumptions of normality

## Wilcoxon Rank Sum / Mann-Whitney / Rank Sum

Null: population distributions are the same
  OR: additive treatment effect is zero

* do not use when lots of ties

#### General Assumptions
1. Equal standard deviations
2. Independence within and between groups

#### Robustness to Assumptions
1. Yes but tests a different null hypothesis (Two populations are identical)

## Wilcoxon Signed Rank Test

Null: differences are symmetric about 0

Test statistic: Sum of Ranks of Positive Differences
  * More efficient than other parametric tests

#### Assumptions
1. Independence within groups

#### Procedure
1. Using abs val of differences, order from smallest to largest
2. Drop zeros
3. Rank abs values from smallest to largest. Ties are averaged.
3. Sum ranks where differences are positive
  * Under null we expect the test statistic to be about n(n+1)/4

## Sign Test
Null: median difference is 0

* Not really used outside of the classroom
* Not robust

## Kruskal-Wallis ANOVA
* Non-parametric version of the ANOVA test
* Removes the assumption of normal populations and is resistant to outliers.
Null: All groups have same median

#### Procedure
1. Convert response to ranks
2. Ignore groups
3. Conduct ANOVA on ranks

## Diagnostic Tests

Testing for Equal Variances. Very sensitive to departures from Normality
  1. Breusch-Pagan (bptest)
  2. Bartlett Test (bartlett.test)
**General rule of thumb**: If max(sd) / min(sd) > 2, then you probably dont have equal spread. If yes, then you shouldnt be pooling.

Testing for Normality
  1. Shapiro-Wilk (shapiro.test).
  2. Lilliefors Kolmogorov-Smirnov Test (lille.test, nortest library)
  3. Anderson-Darling (nortest)
  4. Normality Probability Plots ()

### Levene's
* Test for equal population standard deviations
* Determines **Absolute deviation from the median**
* Failing to reject doesn't tell you anything about the standard deviation

### Shapiro-Wilk 
* Test for normality of data
* Will be way off with very large sample sizes that are slightly non-normal

# Simple Linear Regression (SLR)

X = Explanatory Var
Y = Response Var

y_hat = Intercept + Slope * X

sigma_hat = sqrt(sum sq res / df)
          = sqrt(SSE / n - 2)
          = pooled std dev.
          = Residual standard error when summary(model) is run

df = number of obs - number of estimated params
stderr_b1_hat = sigmat_hat * sqrt(1 / sum((x_1 - x_bar)^2))

b_hat1 = sum((x_i - xbar) * (y_i - ybar)) / sum((x_i - xbar)^2)

b_hat0 = ybar - b_hat1 * xbar

residual = y_i - fitted_i (b_hat0 + b_hat1 * x_i)

SE_bhat1 = sigma_hat * sqrt(1 / (n - 1) * var(X))
SE_bhat0 = sigma_hat * sqrt((1 / n) + (xbar^2 / (n - 1) * var(X)))

SE_est_mean = sigma_hat * sqrt((1 / n) + ((x0 - xbar)^2 / (n - 1) * var(X)))

95% Confidence Interval = 
  b_hat0 + bhat_1 * x0 +- pt(0.975, n - 2) * SE_est_mean

predicted value = b0_hat + b1_hat * X + (t * SE)

SE_pred = sigma_hat * sqrt(1 + (1 / n) + ((x0 - xbar)^2 / (n - 1) * var(X)))


  * closer to mean indicates smaller CI

## Definitions

**Slope**: Associated change in **mean** response for 1 unit increase in explanatory var.

**Scope**: min - max value of X in a model

* Do not interpret values outside the scope of the model
* Intercept doesn't generally matter but depends on the experiment.

**Least Squares Lines**: Lines that give the least possible sum of squared residuals = MVUE (Minimum Variance Unbiased Estimator) of line

**Consistent Estimator**: lim n-> inf (var(theata) = 0)

**R-squared**: Measures the proportion of variation in the response that is explained by the regression model for the mean. Between 0 (implies slope = 0) and 1 (perfect fit)

**Correlation**: Measures the strength of a linear relationship between X and Y. Correlation only makes sense if the data are pairs drawn from a population.


### Assumptions
* Normal subpopulation distribution of each value of X
* Means fall on a straight line - linear relationship
* Each subpopulation has the same standard deviation
* Independent observations

### Inference
1. Slope or Intercept
  * Uncertainty comes from sampling error in a single parameter
2. Inference about mean response at a given value of X
  * Uncertainty comes from sampling error in both params
3. Prediction of a new response at a given value of X
  * Uncertainty comes from sampling error in both params and variability in subpopulations

### Procedure
1. Plot response against explanatory - create a scatterplot
 * are there problems that make linear regression look inappropriate? Would a transformation help?
2. Fit linear regression model
3. Use residuals to re-examine assumptions

### Residual plots
1. Residuals against fitted values
  * Linearity and constant variance
  * **Not Robust** to Linearity
    * **Remedy**: consider a more complicated model for the mean, or transform response, or explanatory, or both.
  * **Not Robust** to constant spread. Least Squares Estimates will be unbiased by the standard errors will be wrong.
2. Res against explanatory variable (X)
  * Linearity and Constant variance
3. Normal Probability Plot
  * Robust with large sample sizes
  * **Remedy**: consider a transformation or GLM
4. Independence
  * **Not Robust**
  * **Remedy**: More complicated models

If linear assumption is true, the residuals should be equally spread either side of the zero, not systematically above or below zero

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

**The only way to tell if linear regression is appropriate is to examine the data (or the residuals)**