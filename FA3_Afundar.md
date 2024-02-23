FA3_Afundar
================
Audrie Lex L. Afundar
2024-02-23

1.  A binary communication channel carries data as one of two sets of
    signals denoted by 0 and 1. Owing to noise, a transmitted 0 is
    sometimes received as a 1, and a transmitted 1 is sometimes received
    as a 0. For a given channel, it can be assumed that a transmitted 0
    is correctly received with probability 0.95, and a transmitted 1 is
    correctly received with probability 0.75. Also, 70% of all messages
    are transmitted as a 0. If a signal is sent, determine the
    probability that:

<!-- -->

1)  a 1 was received;
2)  a 1 was transmitted given than a 1 was received.

------------------------------------------------------------------------

To calculate the probability of getting a 1 transmitted by 0 and 1,
calculate separately with the probability of each binary. 0 have a 5%
chance of transmitting a 1 and 1 have a 70% chance of transmitting a 1.
Find the probability of both and add together to find the overall
probability of a 1 received.

``` r
PR0_T0<-0.95
PR1_T1<-0.75
T0<-0.70
T1<-0.3

PR1 <- (1-PR0_T0)*T0+(PR1_T1*T1)

cat("Probability of receiving 1:\n", PR1*100)
```

    ## Probability of receiving 1:
    ##  26

With this, the probability of getting a 1 in the whole binary channel is
26%, this can be used in the next letter.

------------------------------------------------------------------------

To find the probability of getting a 1 when 1 is being transmitted, use
the formula for posterior probability. The answer for letter a as the
prior probability can be used to find the probability after the
evidences.

``` r
PR1_T<-((PR1_T1*T1)/PR1)*100

cat("Probability of receiving 1 when 1 is being transmitted:\n", PR1_T)
```

    ## Probability of receiving 1 when 1 is being transmitted:
    ##  86.53846

Therefore, 86.54% of the 1 being transmitted to the binary channel is
correctly transmitted by 1.

------------------------------------------------------------------------

2.  There are three employees working at an IT company: Jane, Amy, and
    Ava, doing 10%, 30%, and 60% of the programming, respectively. 8% of
    Jane’s work, 5% of Amy’s work, and just 1% of Ava‘s work is in
    error. What is the overall percentage of error? If a program is
    found with an error, who is the most likely person to have written
    it?

To answer this properly, first find the weighted average by multiplying
the work error of all the employees to their respective percentage of
work on the company. After doing so, adding all the weighted work error
will result to the overall percentage of error. Moreover, the weighted
average of each employee will result to their respective percentage of
error in the company.

``` r
JaneW<-0.1
AmyW<-0.3
AvaW<-0.6
JaneE<-0.08
AmyE<-0.05
AvaE<-0.01

JaneWE <- (JaneW*JaneE)*100
AmyWE<- (AmyW*AmyE)*100
AvaWE<-(AvaW*AvaE)*100

OverallE<-(JaneWE)+(AmyWE)+(AvaWE)

# Finding the weighted work error of all employees to find the overall error
cat("Jane's weigthed work error:", JaneWE)
```

    ## Jane's weigthed work error: 0.8

``` r
cat("\nAmy's weigthed work error:", AmyWE)
```

    ## 
    ## Amy's weigthed work error: 1.5

``` r
cat("\nAva's weigthed work error:", AvaWE)
```

    ## 
    ## Ava's weigthed work error: 0.6

``` r
cat("\nOverall weighted work error:", OverallE)
```

    ## 
    ## Overall weighted work error: 2.9

With this, the overall percentage of work error for the whole company is
2.9% and the greatest contributor to this percentage is Amy with 1.5% of
the overall work error.
