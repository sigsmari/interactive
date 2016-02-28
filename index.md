---
title       : Calculating if it's better to buy or rent
subtitle    : 
author      : Sigurdur Smari Sigurdsson
job         : Risk management
framework   : io2012        # {io2012, html5slides, shower, dzslides, ...}
highlighter : highlight.js  # {highlight.js, prettify, highlight}
hitheme     : tomorrow      # 
widgets     : []            # {mathjax, quiz, bootstrap}
mode        : selfcontained # {standalone, draft}

--- &twocol

## Variables that need to be considered


*** =left

- Value of the property
- Expected increase in the value of property over the investment horizon
- Years of planned ownership
  - Generally it's better to buy if you plan to own the property for longer than 10 years.
- Yield requirement (in %)
  - E.g. what return you can expect on your savings.
- Cost of maintenance, insurance and other yearly fees 
  
*** =right

- Cost of selling
  - This is what the real estate agent gets
- Loan amount
- Interest rate of the loan
- Initial lending cost 
- Years until maturity of the loan
- Collateral paid to the rentor
  - The rentor returns the collateral when you leave the property
- Annual rent of a similar property
- Annual expected increase in rent

---

## The function

The function below calculates which is better

```r
npv_calc <- function(price,price_incr,years_p,yield,cost_m,cost_s,amt_l,int,
                cost_l,years_l,rent_coll,rent,rent_incr) {
  price_incr <- price_incr/100
  yield <- yield/100
  int <- int/100
  rent_incr <- rent_incr/100
  cost_s <- cost_s/100
  cost_l <- cost_l/100
  cost_m <- cost_m/100
  
  CF <- vector(mode="numeric", length=0)
  outc <- vector(mode="numeric", length=0)
  CF[1] <- -(price - amt_l) - amt_l*cost_l
  for (i in 1:(years_p-1)) 
  {
    CF[i+1] <- -price*cost_m-(amt_l-amt_l/ 
                                years_l*(i-1))*int-amt_l/years_l
  }
  CF[years_p+1] <- price*(1+price_incr)*(1-cost_s)-price*cost_m-(amt_l-amt_l/years_l*(years_p-1))*(1+int)
  
  outc$npv_p <- round(npv(yield,CF))
  
  CF_r <- vector(mode="numeric", length=0)
  CF_r[1] <- -rent_coll
  for (i in 1:(years_p-1)) 
  {
    CF_r[i+1] <- -rent*(1+rent_incr*(i-1))+(price-amt_l-rent_coll)*((1+yield)^i-(1+yield)^(i-1))
  }
  
  CF_r[years_p+1] <- -rent*(1+rent_incr*(years_p-1))+(price-amt_l-rent_coll)*((1+yield)^years_p-(1+yield)^(years_p-1))+rent_coll
  
  outc$npv_r <- round(npv(yield,CF_r))
  
  out <- if (outc$npv_p > outc$npv_r) {
    paste0("It's ",format(outc$npv_p-outc$npv_r,big.mark = ",")," cheaper to buy property")
    }
    else {
    paste0("It's ",format(outc$npv_r-outc$npv_p,big.mark = ",")," cheaper to rent property")
    }
  
  return(out)
}
```

--- &twocol

## Example

Let's find out if it's better to buy or rent a property that costs ISK 35 million. 

*** =left

- Value of the property = ISK 35 million
- Expected increase in the value of property over the investment horizon = 20%
- Years of planned ownership = 10 years
- Yield requirement = 6%
- Cost of maintenance, insurance and other yearly fees = 2%
- Cost of selling = 2%
- Loan amount = ISK 20 million
  
*** =right

- Interest rate of the loan = 7%
- Initial lending cost = 1%
- Years until maturity of the loan = 40
- Collateral paid to the rentor = ISK 200 thousand
- Annual rent of a similar property = ISK 2.4 million
- Annual expected increase in rent = 4%

---

## The result
# Given the values on the previous slide the result is that


```r
library(FinCal)
npv_calc(price=35000000,price_incr=20,years_p=10,yield=6,cost_m=2,cost_s=2,
         amt_l=20000000,int=7,cost_l=1,years_l=40,rent_coll=200000,rent=2400000,rent_incr=4)
```

```
## [1] "It's 6,475,544 cheaper to rent property"
```
