Methods\_hw4
================
Yishan Wang
2018-11-12

Problem 2
=========

``` r
heartdisease_data = read_csv("./data/HeartDisease.csv")
```

    ## Parsed with column specification:
    ## cols(
    ##   id = col_integer(),
    ##   totalcost = col_double(),
    ##   age = col_integer(),
    ##   gender = col_integer(),
    ##   interventions = col_integer(),
    ##   drugs = col_integer(),
    ##   ERvisits = col_integer(),
    ##   complications = col_integer(),
    ##   comorbidities = col_integer(),
    ##   duration = col_integer()
    ## )

### a)

#### Description of the Data Set

The main outcome is `totalcost` of patients diagnosed with heart disease. The main predictor is `ERvisits`, which is number of emergency room visits. Other important covariates are `age`, `gender`, `complications` and `duration`. `interventions`, `drugs` and `comorbidities` are potential covariates.

#### Descriptive Statistics for all Variables of Interest

##### Descriptive statistics for continous variables of interest:

``` r
heartdisease_data %>%
  select(totalcost, ERvisits, age, complications, duration) %>%
  summary()
```

    ##    totalcost          ERvisits           age        complications    
    ##  Min.   :    0.0   Min.   : 0.000   Min.   :24.00   Min.   :0.00000  
    ##  1st Qu.:  161.1   1st Qu.: 2.000   1st Qu.:55.00   1st Qu.:0.00000  
    ##  Median :  507.2   Median : 3.000   Median :60.00   Median :0.00000  
    ##  Mean   : 2800.0   Mean   : 3.425   Mean   :58.72   Mean   :0.05711  
    ##  3rd Qu.: 1905.5   3rd Qu.: 5.000   3rd Qu.:64.00   3rd Qu.:0.00000  
    ##  Max.   :52664.9   Max.   :20.000   Max.   :70.00   Max.   :3.00000  
    ##     duration     
    ##  Min.   :  0.00  
    ##  1st Qu.: 41.75  
    ##  Median :165.50  
    ##  Mean   :164.03  
    ##  3rd Qu.:281.00  
    ##  Max.   :372.00

##### Descriptive statistics for categorical variable of interest:

``` r
table(factor(heartdisease_data$gender, levels = c(0,1), labels = c('Male','Female'))) %>%
  addmargins()
```

    ## 
    ##   Male Female    Sum 
    ##    608    180    788

### b)

#### Plot the distribution for variable `totalcost`:

``` r
hist(heartdisease_data$totalcost, main = "Total Cost Distribution", xlab = "Total Cost ($)", col.main = "red", col.lab = "blue")
```

![](Methods_hw4_files/figure-markdown_github/unnamed-chunk-5-1.png)

#### Use log transformation:

``` r
hist(log(heartdisease_data$totalcost), main = "Total Cost Distribution", xlab = "Total Cost ($)", col.main = "red", col.lab = "blue")
```

![](Methods_hw4_files/figure-markdown_github/unnamed-chunk-6-1.png)

### c)

#### Create a new variable called `comp_bin` by dichotomizing `complications`: 0 if no complications, and 1 otherwise.

``` r
new_heartdisease_data = heartdisease_data %>%
  mutate(comp_bin = ifelse(complications == 0, 0, 1))
```

### d)

#### Fit a simple linear regression between the original `totalcost` and predictor `ERvisits`.

``` r
ggplot(heartdisease_data, aes(x = ERvisits, y = totalcost)) +
  geom_point() +
  geom_smooth(method = 'lm',formula = y~x)
```

![](Methods_hw4_files/figure-markdown_github/unnamed-chunk-8-1.png)

``` r
reg_original = lm(totalcost ~ ERvisits, heartdisease_data)
summary(reg_original)
```

    ## 
    ## Call:
    ## lm(formula = totalcost ~ ERvisits, data = heartdisease_data)
    ## 
    ## Residuals:
    ##    Min     1Q Median     3Q    Max 
    ## -15733  -2353  -1062    185  42098 
    ## 
    ## Coefficients:
    ##             Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)  -472.54     362.24  -1.304    0.192    
    ## ERvisits      955.44      83.81  11.399   <2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 6201 on 786 degrees of freedom
    ## Multiple R-squared:  0.1419, Adjusted R-squared:  0.1408 
    ## F-statistic: 129.9 on 1 and 786 DF,  p-value: < 2.2e-16

##### Comments on significance and interpretation of the slope:

#### Fit a simple linear regression between the transformed `totalcost` and predictor `ERvisits`.

``` r
trans_heartdisease_data = heartdisease_data %>%
  filter(totalcost != 0) %>%
  mutate(trans_totalcost = log(totalcost)) 

ggplot(trans_heartdisease_data, aes(x = ERvisits, y = trans_totalcost)) +
  geom_point() +
  geom_smooth(method = 'lm', formula = y~x)
```

![](Methods_hw4_files/figure-markdown_github/unnamed-chunk-9-1.png)

``` r
reg_trans = lm(trans_totalcost ~ ERvisits, trans_heartdisease_data)
summary(reg_trans)
```

    ## 
    ## Call:
    ## lm(formula = trans_totalcost ~ ERvisits, data = trans_heartdisease_data)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -6.2013 -1.1265  0.0191  1.2668  4.2797 
    ## 
    ## Coefficients:
    ##             Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)  5.53771    0.10362   53.44   <2e-16 ***
    ## ERvisits     0.22672    0.02397    9.46   <2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 1.772 on 783 degrees of freedom
    ## Multiple R-squared:  0.1026, Adjusted R-squared:  0.1014 
    ## F-statistic:  89.5 on 1 and 783 DF,  p-value: < 2.2e-16

##### Comments on significance and interpretation of the slope:
