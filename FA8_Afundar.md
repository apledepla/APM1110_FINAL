FA8_Afundar
================
Audrie Lex L. Afundar
2024-04-11

## FA8 Afundar

Since the formative assessment did not specifically ask for a graph, the
probability around the normal distribution of the problem is only
displayed as is.

Number 1

1)  

``` r
v=256
sd=16
mean=200

p_224=1-pnorm(224,mean,sd)
cat("The probability that the signal will exceed 224 microvolts is:",p_224*100)
```

    ## The probability that the signal will exceed 224 microvolts is: 6.68072

2)  

``` r
p186_224=pnorm(186,mean,sd)-p_224

cat("The probability that the signal will be between 186 and 224 microvolts is:",p186_224*100)
```

    ## The probability that the signal will be between 186 and 224 microvolts is: 12.39798

3)  

``` r
first_quartile=qnorm(.25,mean,sd)


cat("The microvoltage below 25% is:",first_quartile)
```

    ## The microvoltage below 25% is: 189.2082

4)  

``` r
p_210=pnorm(210,mean,sd)
p210_240=pnorm(240,mean,sd)-p_210

cat("The probability that the signal will be between 210 and 240 microvolts is:",p210_240*100)
```

    ## The probability that the signal will be between 210 and 240 microvolts is: 25.97759

5)  

``` r
second_quartile=qnorm(.75,mean,sd)
interquartile_range=second_quartile-first_quartile


cat("The interquartile range is:",interquartile_range)
```

    ## The interquartile range is: 21.58367

6)  

``` r
p210_220=pnorm(220,mean,sd)-p_210


cat("The probability that the signal will be between 210 and 220 microvolts is:",p210_220*100)
```

    ## The probability that the signal will be between 210 and 220 microvolts is: 16.03358

7)  Since 220 is already greater than 200, the condition for 200 can be
    disregarded, therfore P(X\>220)

``` r
p_220=1-pnorm(220,mean,sd)



cat("The probability that the signal would be greater that 220 microvolts is:",p_220*100)
```

    ## The probability that the signal would be greater that 220 microvolts is: 10.56498

Number 2

1)  

``` r
v=144
mean=25
sd=12

quartile95=qnorm(0.95,mean,sd)
cat("The bound that includes 95% of the downtime is:",quartile95)
```

    ## The bound that includes 95% of the downtime is: 44.73824

2)  Since it stated that it wants the bound ABOVE the 10%, it can be
    assumed that it wants the 90% quartile.

``` r
quartile90=qnorm(0.9, mean, sd)
cat("The bound above which 10% of the downtime is included:",quartile90)
```

    ## The bound above which 10% of the downtime is included: 40.37862
