FA7_Afundar
================
Audrie Lex L. Afundar
2024-02-29

## Number 1

``` r
num1_a<-pexp(0.25,4)*100
num1_b<-(1-pexp(0.5,4))*100
num1_c<-(pexp(1,4)-pexp(0.25,4))*100

cat("The probability that 15 seconds is the the time between submissions at most is:", num1_a)
```

    ## The probability that 15 seconds is the the time between submissions at most is: 63.21206

``` r
cat("\nThe probability that the time between submission is greater that 30 seconds is:", num1_b)
```

    ## 
    ## The probability that the time between submission is greater that 30 seconds is: 13.53353

``` r
cat("\nThe probability that the time between submission is in between 15seconds-1minute is:", num1_c)
```

    ## 
    ## The probability that the time between submission is in between 15seconds-1minute is: 34.95638

## Number 2

``` r
num2_a<-ppois(2,2,lower.tail=FALSE)*100
num2_c<-pexp(0.5,2)*100
num2_b<-(1-pexp(0.5,2))*100
num2_d<-(pexp(1,2)-pexp(0.5,2))*100

cat("The probability that more than 2 jobs will arrive in a minute is:",num2_a)
```

    ## The probability that more than 2 jobs will arrive in a minute is: 32.33236

``` r
cat("\nThe probability that atleast 30 seconds will elapse between any two jobs is:", num2_b)
```

    ## 
    ## The probability that atleast 30 seconds will elapse between any two jobs is: 36.78794

``` r
cat("\nThe probability that less than 30 seconds will elapse between jobs is:",num2_c)
```

    ## 
    ## The probability that less than 30 seconds will elapse between jobs is: 63.21206

``` r
cat("\nThe probability that a job will arrive in the next 30 seconds, if no jobs have arrived in the last 30 seconds is:",num2_d)
```

    ## 
    ## The probability that a job will arrive in the next 30 seconds, if no jobs have arrived in the last 30 seconds is: 23.25442

## Number 3

``` r
num3_a<-(1-pexp(0.167,15))*100
num3_b<-ppois(8,15)*100
num3_c<-pexp(0.25,15)*100
num3_d<-qexp(0.75,15)*100

cat("The probability that at least 10minutes will elapse without a visit is:",num3_a)
```

    ## The probability that at least 10minutes will elapse without a visit is: 8.16756

``` r
cat("\nThe probability that in any hour, there will be less than 8 visits is:", num3_b)
```

    ## 
    ## The probability that in any hour, there will be less than 8 visits is: 3.744649

``` r
cat("\nThe probability that a visit will occur if 15 minutes have elapsed since last visit is:", num3_c)
```

    ## 
    ## The probability that a visit will occur if 15 minutes have elapsed since last visit is: 97.64823

``` r
num3_d
```

    ## [1] 9.241962

For the letter D, the top quartile/75th quartile shows the number of
visits in the 75th mark.
