Bayesian model selection and hierarchical regression
================

Setup
=====

``` r
setwd("C:/Users/yukic/Documents/kaggle/houseprices")
library(ggplot2)
library(dplyr)
```

    ## 
    ## Attaching package: 'dplyr'

    ## The following objects are masked from 'package:stats':
    ## 
    ##     filter, lag

    ## The following objects are masked from 'package:base':
    ## 
    ##     intersect, setdiff, setequal, union

``` r
library(corrplot)
```

    ## Warning: package 'corrplot' was built under R version 3.5.2

    ## corrplot 0.84 loaded

Data
====

``` r
train <- read.csv("train.csv")
head(train)
```

    ##   Id MSSubClass MSZoning LotFrontage LotArea Street Alley LotShape
    ## 1  1         60       RL          65    8450   Pave  <NA>      Reg
    ## 2  2         20       RL          80    9600   Pave  <NA>      Reg
    ## 3  3         60       RL          68   11250   Pave  <NA>      IR1
    ## 4  4         70       RL          60    9550   Pave  <NA>      IR1
    ## 5  5         60       RL          84   14260   Pave  <NA>      IR1
    ## 6  6         50       RL          85   14115   Pave  <NA>      IR1
    ##   LandContour Utilities LotConfig LandSlope Neighborhood Condition1
    ## 1         Lvl    AllPub    Inside       Gtl      CollgCr       Norm
    ## 2         Lvl    AllPub       FR2       Gtl      Veenker      Feedr
    ## 3         Lvl    AllPub    Inside       Gtl      CollgCr       Norm
    ## 4         Lvl    AllPub    Corner       Gtl      Crawfor       Norm
    ## 5         Lvl    AllPub       FR2       Gtl      NoRidge       Norm
    ## 6         Lvl    AllPub    Inside       Gtl      Mitchel       Norm
    ##   Condition2 BldgType HouseStyle OverallQual OverallCond YearBuilt
    ## 1       Norm     1Fam     2Story           7           5      2003
    ## 2       Norm     1Fam     1Story           6           8      1976
    ## 3       Norm     1Fam     2Story           7           5      2001
    ## 4       Norm     1Fam     2Story           7           5      1915
    ## 5       Norm     1Fam     2Story           8           5      2000
    ## 6       Norm     1Fam     1.5Fin           5           5      1993
    ##   YearRemodAdd RoofStyle RoofMatl Exterior1st Exterior2nd MasVnrType
    ## 1         2003     Gable  CompShg     VinylSd     VinylSd    BrkFace
    ## 2         1976     Gable  CompShg     MetalSd     MetalSd       None
    ## 3         2002     Gable  CompShg     VinylSd     VinylSd    BrkFace
    ## 4         1970     Gable  CompShg     Wd Sdng     Wd Shng       None
    ## 5         2000     Gable  CompShg     VinylSd     VinylSd    BrkFace
    ## 6         1995     Gable  CompShg     VinylSd     VinylSd       None
    ##   MasVnrArea ExterQual ExterCond Foundation BsmtQual BsmtCond BsmtExposure
    ## 1        196        Gd        TA      PConc       Gd       TA           No
    ## 2          0        TA        TA     CBlock       Gd       TA           Gd
    ## 3        162        Gd        TA      PConc       Gd       TA           Mn
    ## 4          0        TA        TA     BrkTil       TA       Gd           No
    ## 5        350        Gd        TA      PConc       Gd       TA           Av
    ## 6          0        TA        TA       Wood       Gd       TA           No
    ##   BsmtFinType1 BsmtFinSF1 BsmtFinType2 BsmtFinSF2 BsmtUnfSF TotalBsmtSF
    ## 1          GLQ        706          Unf          0       150         856
    ## 2          ALQ        978          Unf          0       284        1262
    ## 3          GLQ        486          Unf          0       434         920
    ## 4          ALQ        216          Unf          0       540         756
    ## 5          GLQ        655          Unf          0       490        1145
    ## 6          GLQ        732          Unf          0        64         796
    ##   Heating HeatingQC CentralAir Electrical X1stFlrSF X2ndFlrSF LowQualFinSF
    ## 1    GasA        Ex          Y      SBrkr       856       854            0
    ## 2    GasA        Ex          Y      SBrkr      1262         0            0
    ## 3    GasA        Ex          Y      SBrkr       920       866            0
    ## 4    GasA        Gd          Y      SBrkr       961       756            0
    ## 5    GasA        Ex          Y      SBrkr      1145      1053            0
    ## 6    GasA        Ex          Y      SBrkr       796       566            0
    ##   GrLivArea BsmtFullBath BsmtHalfBath FullBath HalfBath BedroomAbvGr
    ## 1      1710            1            0        2        1            3
    ## 2      1262            0            1        2        0            3
    ## 3      1786            1            0        2        1            3
    ## 4      1717            1            0        1        0            3
    ## 5      2198            1            0        2        1            4
    ## 6      1362            1            0        1        1            1
    ##   KitchenAbvGr KitchenQual TotRmsAbvGrd Functional Fireplaces FireplaceQu
    ## 1            1          Gd            8        Typ          0        <NA>
    ## 2            1          TA            6        Typ          1          TA
    ## 3            1          Gd            6        Typ          1          TA
    ## 4            1          Gd            7        Typ          1          Gd
    ## 5            1          Gd            9        Typ          1          TA
    ## 6            1          TA            5        Typ          0        <NA>
    ##   GarageType GarageYrBlt GarageFinish GarageCars GarageArea GarageQual
    ## 1     Attchd        2003          RFn          2        548         TA
    ## 2     Attchd        1976          RFn          2        460         TA
    ## 3     Attchd        2001          RFn          2        608         TA
    ## 4     Detchd        1998          Unf          3        642         TA
    ## 5     Attchd        2000          RFn          3        836         TA
    ## 6     Attchd        1993          Unf          2        480         TA
    ##   GarageCond PavedDrive WoodDeckSF OpenPorchSF EnclosedPorch X3SsnPorch
    ## 1         TA          Y          0          61             0          0
    ## 2         TA          Y        298           0             0          0
    ## 3         TA          Y          0          42             0          0
    ## 4         TA          Y          0          35           272          0
    ## 5         TA          Y        192          84             0          0
    ## 6         TA          Y         40          30             0        320
    ##   ScreenPorch PoolArea PoolQC Fence MiscFeature MiscVal MoSold YrSold
    ## 1           0        0   <NA>  <NA>        <NA>       0      2   2008
    ## 2           0        0   <NA>  <NA>        <NA>       0      5   2007
    ## 3           0        0   <NA>  <NA>        <NA>       0      9   2008
    ## 4           0        0   <NA>  <NA>        <NA>       0      2   2006
    ## 5           0        0   <NA>  <NA>        <NA>       0     12   2008
    ## 6           0        0   <NA> MnPrv        Shed     700     10   2009
    ##   SaleType SaleCondition SalePrice
    ## 1       WD        Normal    208500
    ## 2       WD        Normal    181500
    ## 3       WD        Normal    223500
    ## 4       WD       Abnorml    140000
    ## 5       WD        Normal    250000
    ## 6       WD        Normal    143000

``` r
dim(train)
```

    ## [1] 1460   81

Missing data
============

``` r
total <- sort(apply(apply(train, 2, is.na), 2, sum), 
              decreasing = TRUE)
percent <- sort(apply(apply(train, 2, is.na), 2, sum) / dim(train)[1],
                decreasing = TRUE)
missing_data <- data.frame(Total = total,
                           Percent = percent)
head(missing_data, 20)
```

    ##              Total      Percent
    ## PoolQC        1453 0.9952054795
    ## MiscFeature   1406 0.9630136986
    ## Alley         1369 0.9376712329
    ## Fence         1179 0.8075342466
    ## FireplaceQu    690 0.4726027397
    ## LotFrontage    259 0.1773972603
    ## GarageType      81 0.0554794521
    ## GarageYrBlt     81 0.0554794521
    ## GarageFinish    81 0.0554794521
    ## GarageQual      81 0.0554794521
    ## GarageCond      81 0.0554794521
    ## BsmtExposure    38 0.0260273973
    ## BsmtFinType2    38 0.0260273973
    ## BsmtQual        37 0.0253424658
    ## BsmtCond        37 0.0253424658
    ## BsmtFinType1    37 0.0253424658
    ## MasVnrType       8 0.0054794521
    ## MasVnrArea       8 0.0054794521
    ## Electrical       1 0.0006849315
    ## Id               0 0.0000000000

My main focus is variable selection, so follwoing the below idea, we delete columns and observatios.
<https://www.kaggle.com/pmarcelino/comprehensive-data-exploration-with-python>

Data cleaning
=============

``` r
COLUMNS_drop <- rownames(missing_data[missing_data$Total > 1,])
train <- train[, !(names(train) %in% COLUMNS_drop)]
dim(train)
```

    ## [1] 1460   63

``` r
train <- train[is.na(train$Electrical) == FALSE, ]
dim(train)
```

    ## [1] 1459   63

Column types
============

``` r
COLUMNS_type <- sapply(train, class)
COLUMNS_numeric <- names(COLUMNS_type[COLUMNS_type == "integer"])
train <- train[, COLUMNS_numeric]
dim(train)
```

    ## [1] 1459   35

Remove highly correlated variables
==================================

Before analysis, we exclude the predictors which are highly correlated. Solve error inverse matrix.

``` r
cormat <- cor(train)
ut <- upper.tri(cormat)
corcol <- data.frame(row = rownames(cormat)[row(cormat)[ut]],
                     column = rownames(cormat)[col(cormat)[ut]],
                     cor = (cormat)[ut])
corcol[abs(corcol$cor) > 0.55, ] %>%
  arrange(desc(cor))
```

    ##             row       column       cor
    ## 1    GarageCars   GarageArea 0.8826130
    ## 2     GrLivArea TotRmsAbvGrd 0.8255765
    ## 3   TotalBsmtSF    X1stFlrSF 0.8193933
    ## 4   OverallQual    SalePrice 0.7910687
    ## 5     GrLivArea    SalePrice 0.7086176
    ## 6     X2ndFlrSF    GrLivArea 0.6877263
    ## 7  BedroomAbvGr TotRmsAbvGrd 0.6766133
    ## 8    BsmtFinSF1 BsmtFullBath 0.6490250
    ## 9    GarageCars    SalePrice 0.6404729
    ## 10    GrLivArea     FullBath 0.6302831
    ## 11   GarageArea    SalePrice 0.6234229
    ## 12    X2ndFlrSF TotRmsAbvGrd 0.6163999
    ## 13  TotalBsmtSF    SalePrice 0.6139050
    ## 14    X2ndFlrSF     HalfBath 0.6095514
    ## 15    X1stFlrSF    SalePrice 0.6059679
    ## 16  OverallQual   GarageCars 0.6009909
    ## 17  OverallQual    GrLivArea 0.5930208
    ## 18    YearBuilt YearRemodAdd 0.5925116
    ## 19  OverallQual    YearBuilt 0.5733340
    ## 20    X1stFlrSF    GrLivArea 0.5660837
    ## 21  OverallQual   GarageArea 0.5619799
    ## 22     FullBath    SalePrice 0.5608806
    ## 23     FullBath TotRmsAbvGrd 0.5547589
    ## 24  OverallQual YearRemodAdd 0.5516054
    ## 25  OverallQual     FullBath 0.5512674

``` r
COLUMNS_drop <- c("TotalBsmtSF", "TotRmsAbvGrd", "GarageArea", "X2ndFlrSF")
train <- train[, !(names(train) %in% COLUMNS_drop)]
dim(train)
```

    ## [1] 1459   31

Model data development
======================

``` r
head(train)
```

    ##   Id MSSubClass LotArea OverallQual OverallCond YearBuilt YearRemodAdd
    ## 1  1         60    8450           7           5      2003         2003
    ## 2  2         20    9600           6           8      1976         1976
    ## 3  3         60   11250           7           5      2001         2002
    ## 4  4         70    9550           7           5      1915         1970
    ## 5  5         60   14260           8           5      2000         2000
    ## 6  6         50   14115           5           5      1993         1995
    ##   BsmtFinSF1 BsmtFinSF2 BsmtUnfSF X1stFlrSF LowQualFinSF GrLivArea
    ## 1        706          0       150       856            0      1710
    ## 2        978          0       284      1262            0      1262
    ## 3        486          0       434       920            0      1786
    ## 4        216          0       540       961            0      1717
    ## 5        655          0       490      1145            0      2198
    ## 6        732          0        64       796            0      1362
    ##   BsmtFullBath BsmtHalfBath FullBath HalfBath BedroomAbvGr KitchenAbvGr
    ## 1            1            0        2        1            3            1
    ## 2            0            1        2        0            3            1
    ## 3            1            0        2        1            3            1
    ## 4            1            0        1        0            3            1
    ## 5            1            0        2        1            4            1
    ## 6            1            0        1        1            1            1
    ##   Fireplaces GarageCars WoodDeckSF OpenPorchSF EnclosedPorch X3SsnPorch
    ## 1          0          2          0          61             0          0
    ## 2          1          2        298           0             0          0
    ## 3          1          2          0          42             0          0
    ## 4          1          3          0          35           272          0
    ## 5          1          3        192          84             0          0
    ## 6          0          2         40          30             0        320
    ##   ScreenPorch PoolArea MiscVal MoSold YrSold SalePrice
    ## 1           0        0       0      2   2008    208500
    ## 2           0        0       0      5   2007    181500
    ## 3           0        0       0      9   2008    223500
    ## 4           0        0       0      2   2006    140000
    ## 5           0        0       0     12   2008    250000
    ## 6           0        0     700     10   2009    143000

``` r
# First column is Id so exclude
X <- as.matrix(train[, 2:30])
y <- as.matrix(train[,"SalePrice"])

dim(X)
```

    ## [1] 1459   29

Function
========

``` r
# a function to compute the marginal probability
lpy.X <- function(y, X, 
                  g = length(y),
                  nu0 = 1,
                  s20 = try(summary(lm(y~-1+X))$sigma^2,
                            silent = TRUE)) {
  
  n <- dim(X)[1]
  p <- dim(X)[2]
  if (p == 0) {
    
    Hg <- 0
    s20 <- mean(y^2)
    
  } else if (p > 0) {
    
    Hg <- (g/(g+1)) * X %*% solve( t(X) %*% X) %*% t(X)
        
  }

  SSRg <- t(y) %*% ( diag(1, nrow = n) - Hg ) %*% y
  
  -.5 * ( n*log(pi) 
          + p*log(1+g) 
          + (nu0+n)*log(nu0*s20+SSRg) 
          - nu0*log(nu0*s20) ) + lgamma( (nu0+n)/2 ) + lgamma( nu0/2 )
  
}
```

``` r
# starting values and MCMC setup
z <- rep(1, dim(X)[2])
lpy.c <- lpy.X(y, X[,z==1, drop=FALSE])
# S <- 5000
S <- 100
# S <- 3
Z <- matrix(NA, S, dim(X)[2])
```

``` r
# Gibbs sampler
for (s in 1:S) {
  
  for (j in sample(1:dim(X)[2])) { # random rearrange from 1,2,3,4
  
    zp <- z
    zp[j] <- 1 - zp[j]
    lpy.p <- lpy.X(y, X[,zp == 1, drop = FALSE])
    r <- (lpy.p - lpy.c)*(-1)^(zp[j]==0)
    z[j] <- rbinom(1, 1, 1/(1+exp(-r)))
    if (z[j] == zp[j]) {
      
      lpy.c <- lpy.p
      
    }
  
  }
  
  Z[s,] <- z
  
}
```

Likely models
=============

``` r
colnames(Z) <- colnames(X)

models <- Z[,1]
for (i in 2:29) {
  models <- paste(models, Z[,i])
}
models_prob <- sort(table(models), decreasing = TRUE) / S
models_prob[1:5]
```

    ## models
    ## 1 1 1 1 1 0 1 0 1 0 0 1 1 0 0 0 1 0 0 1 1 0 0 0 1 0 0 0 1 
    ##                                                      0.13 
    ## 1 1 1 1 1 0 1 0 0 0 0 1 1 0 0 0 1 0 0 1 1 0 0 0 1 0 0 0 1 
    ##                                                      0.06 
    ## 1 1 1 1 1 0 1 0 1 0 0 1 1 0 0 0 1 0 0 1 1 0 0 0 0 0 0 0 1 
    ##                                                      0.05 
    ## 1 1 1 1 1 1 1 0 1 0 0 1 1 0 0 0 1 0 0 1 1 0 0 0 1 0 0 0 1 
    ##                                                      0.05 
    ## 1 1 1 1 1 1 1 0 1 0 0 1 1 0 0 0 1 0 1 1 1 0 0 0 1 0 0 0 1 
    ##                                                      0.05

``` r
likely_model <- models_prob[1]
Z_m <- cbind(Z, models)
INDEX_likely_model <- (Z_m[,30] == names(likely_model))
z_likely <- Z_m[INDEX_likely_model, ][1,]
COLUMNS_bda <- names(z_likely[z_likely == "1"])
COLUMNS_bda
```

    ##  [1] "MSSubClass"   "LotArea"      "OverallQual"  "OverallCond" 
    ##  [5] "YearBuilt"    "BsmtFinSF1"   "BsmtUnfSF"    "GrLivArea"   
    ##  [9] "BsmtFullBath" "BedroomAbvGr" "GarageCars"   "WoodDeckSF"  
    ## [13] "ScreenPorch"  "YrSold"

Compare accuracy
================

Use Mean Square Prediction Error (MSPE).

``` r
# Base model
COLUMNS_base <- c("OverallQual", "YearBuilt", "GrLivArea", "GarageCars")
X <- as.matrix(train[, COLUMNS_base])
y <- as.matrix(train[, "SalePrice"])

ols <- lm(y ~ X)
mean(residuals(ols)^2)
```

    ## [1] 1578192870

``` r
# Bayesian variable selection model
X <- as.matrix(train[, COLUMNS_bda])

bda <- lm(y ~ X)
mean(residuals(bda)^2)
```

    ## [1] 1238977532

``` r
COLUMNS_bda
```

    ##  [1] "MSSubClass"   "LotArea"      "OverallQual"  "OverallCond" 
    ##  [5] "YearBuilt"    "BsmtFinSF1"   "BsmtUnfSF"    "GrLivArea"   
    ##  [9] "BsmtFullBath" "BedroomAbvGr" "GarageCars"   "WoodDeckSF"  
    ## [13] "ScreenPorch"  "YrSold"

Make hirarchical data
=====================

Since train now contains only numerical variables, we need to add Neighborhood columns to perform hierarchical regression.

``` r
rawdata <- read.csv("train.csv")
neighbor_df <- rawdata[ , c("Id", "Neighborhood")] 
train <- left_join(train, neighbor_df, by = "Id")
head(train)
```

    ##   Id MSSubClass LotArea OverallQual OverallCond YearBuilt YearRemodAdd
    ## 1  1         60    8450           7           5      2003         2003
    ## 2  2         20    9600           6           8      1976         1976
    ## 3  3         60   11250           7           5      2001         2002
    ## 4  4         70    9550           7           5      1915         1970
    ## 5  5         60   14260           8           5      2000         2000
    ## 6  6         50   14115           5           5      1993         1995
    ##   BsmtFinSF1 BsmtFinSF2 BsmtUnfSF X1stFlrSF LowQualFinSF GrLivArea
    ## 1        706          0       150       856            0      1710
    ## 2        978          0       284      1262            0      1262
    ## 3        486          0       434       920            0      1786
    ## 4        216          0       540       961            0      1717
    ## 5        655          0       490      1145            0      2198
    ## 6        732          0        64       796            0      1362
    ##   BsmtFullBath BsmtHalfBath FullBath HalfBath BedroomAbvGr KitchenAbvGr
    ## 1            1            0        2        1            3            1
    ## 2            0            1        2        0            3            1
    ## 3            1            0        2        1            3            1
    ## 4            1            0        1        0            3            1
    ## 5            1            0        2        1            4            1
    ## 6            1            0        1        1            1            1
    ##   Fireplaces GarageCars WoodDeckSF OpenPorchSF EnclosedPorch X3SsnPorch
    ## 1          0          2          0          61             0          0
    ## 2          1          2        298           0             0          0
    ## 3          1          2          0          42             0          0
    ## 4          1          3          0          35           272          0
    ## 5          1          3        192          84             0          0
    ## 6          0          2         40          30             0        320
    ##   ScreenPorch PoolArea MiscVal MoSold YrSold SalePrice Neighborhood
    ## 1           0        0       0      2   2008    208500      CollgCr
    ## 2           0        0       0      5   2007    181500      Veenker
    ## 3           0        0       0      9   2008    223500      CollgCr
    ## 4           0        0       0      2   2006    140000      Crawfor
    ## 5           0        0       0     12   2008    250000      NoRidge
    ## 6           0        0     700     10   2009    143000      Mitchel

``` r
dim(train)
```

    ## [1] 1459   32

``` r
ids<-sort(unique(train$Neighborhood)) 
m<-length(ids)
Y<-list() ; X<-list() ; N<-NULL
COLUMNS <- COLUMNS_bda
for(j in 1:m) 
{
  Y[[j]]<-train[train$Neighborhood==ids[j], "SalePrice"] 
  N[j]<- sum(train$Neighborhood==ids[j])
  xj<-train[train$Neighborhood==ids[j], COLUMNS]
  xj<-scale(xj, center = TRUE, scale = FALSE)
  X[[j]]<-cbind( rep(1,N[j]), xj  )
}
```

OLS fits
========

``` r
#### OLS fits
# S2.LS is the within-group sample variance
S2.LS<-BETA.LS<-NULL
for(j in 1:m) {
  fit<-lm(Y[[j]]~-1+X[[j]] )
  BETA.LS<-rbind(BETA.LS,c(fit$coef)) 
  S2.LS<-c(S2.LS, summary(fit)$sigma^2) 
} 
```

Missing values
==============

``` r
BETA.LS[is.na(BETA.LS) == TRUE] <- 0
```

MCMC functions
==============

``` r
## mvnormal simulation
rmvnorm<-function(n,mu,Sigma)
{ 
  E<-matrix(rnorm(n*length(mu)),n,length(mu))
  t(  t(E%*%chol(Sigma)) +c(mu))
}

## Wishart simulation
rwish<-function(n,nu0,S0)
{
  sS0 <- chol(S0)
  S<-array( dim=c( dim(S0),n ) )
  for(i in 1:n)
  {
    Z <- matrix(rnorm(nu0 * dim(S0)[1]), nu0, dim(S0)[1]) %*% sS0
    S[,,i]<- t(Z)%*%Z
  }
  S[,,1:n]
}
```

MCMC setup
==========

``` r
p <- dim(X[[1]])[2]
theta <- apply(BETA.LS,2,mean, na.rm = TRUE)
mu0 <-apply(BETA.LS,2,mean, na.rm = TRUE)
nu0 <- 1
s2 <- mean(S2.LS, na.rm = TRUE)
s20 <- mean(S2.LS, na.rm = TRUE)
eta0 <- p+2
Sigma <- cov(BETA.LS)
S0 <- cov(BETA.LS)
L0 <- cov(BETA.LS)
BETA <- BETA.LS
THETA.b <- NULL
S2.b <- NULL
iL0 <- solve(L0)
iSigma<-solve(Sigma)
Sigma.ps<-matrix(0,p,p)
SIGMA.PS<-NULL
BETA.ps<-BETA*0
BETA.pp<-NULL
set.seed(0)
```

MCMC
====

``` r
S <- 5000

## MCMC
for(s in 1:S) {
  ##update beta_j 
  for(j in 1:m) 
  {  
    Vj<-solve( iSigma + t(X[[j]])%*%X[[j]]/s2 )
    Ej<-Vj%*%( iSigma%*%theta + t(X[[j]])%*%Y[[j]]/s2 )
    BETA[j,]<-rmvnorm(1,Ej,Vj) 
  } 
  ##
  
  ##update theta
  Lm<-  solve( iL0 +  m*iSigma )
  mum<- Lm%*%( iL0%*%mu0 + iSigma%*%apply(BETA,2,sum))
  theta<-t(rmvnorm(1,mum,Lm))
  ##
  
  ##update Sigma
  mtheta<-matrix(theta,m,p,byrow=TRUE)
  iSigma<-rwish(1, eta0+m, solve( S0+t(BETA-mtheta)%*%(BETA-mtheta) ) )
  ##
  
  ##update s2
  RSS<-0
  for(j in 1:m) { RSS<-RSS+sum( (Y[[j]]-X[[j]]%*%BETA[j,] )^2 ) }
  s2<-1/rgamma(1,(nu0+sum(N))/2, (nu0*s20+RSS)/2 )
  ##
  ##store results
  # if(s%%10==0) 
  # { 

    # cat(s,s2,"\n")
    S2.b<-c(S2.b,s2)
    THETA.b<-rbind(THETA.b,t(theta))
    Sigma.ps<-Sigma.ps+solve(iSigma)
    BETA.ps<-BETA.ps+BETA
    SIGMA.PS<-rbind(SIGMA.PS,c(solve(iSigma)))
    BETA.pp<-rbind(BETA.pp,rmvnorm(1,theta,solve(iSigma)) )
  # }
  ##
}
```

Check MCMC results
==================

``` r
param <- BETA.ps/S
param_df <- data.frame(Neighborhood = ids,
                       Intercept_coef = param[,1],
                       MSSubClass_coef = param[,2],
                       LotArea_coef = param[,3],
                       OverallQual_coef = param[,4],
                       OverallCond_coef = param[,5],
                       YearBuilt_coef = param[,6],
                       BsmtFinSF1_coef = param[,7],
                       BsmtUnfSF_coef = param[,8],
                       GrLivArea_coef = param[,9],
                       BsmtFullBath_coef = param[,10],
                       BedroomAbvGr_coef = param[,11],
                       GarageCars_coef = param[,12],
                       WoodDeckSF_coef = param[,13],
                       ScreenPorch_coef = param[,14],
                       YrSold_coef = param[,15])
param_df
```

    ##    Neighborhood Intercept_coef MSSubClass_coef LotArea_coef
    ## 1       Blmngtn      194944.20        48.85508   -4.1814099
    ## 2       Blueste      139955.82      -178.39227    2.9721988
    ## 3        BrDale      105208.97      -128.16679    2.7407554
    ## 4       BrkSide      124951.37      -122.75513    4.8420460
    ## 5       ClearCr      211910.67      -148.02126    0.5094270
    ## 6       CollgCr      197808.05      -137.67502    0.9899199
    ## 7       Crawfor      210893.91      -137.29890   -0.7480473
    ## 8       Edwards      128073.83       -88.54177   -1.4742194
    ## 9       Gilbert      192991.70      -186.97179    2.0668467
    ## 10       IDOTRR      100911.76       -98.64823    1.9074114
    ## 11      MeadowV       99169.45       -28.39881    1.2106987
    ## 12      Mitchel      156501.18      -107.14074    0.2893634
    ## 13        NAmes      145889.93       -36.36436    1.3433236
    ## 14      NoRidge      334550.95      -310.27554    2.6880558
    ## 15      NPkVill      143002.30      -102.03442    0.9530075
    ## 16      NridgHt      316231.89      -232.82144    1.0628743
    ## 17       NWAmes      188901.91      -147.04737    0.9179320
    ## 18      OldTown      128484.30      -113.05782    3.8514139
    ## 19       Sawyer      136849.15       -67.10679    0.4869605
    ## 20      SawyerW      186617.63      -114.41718    1.4922097
    ## 21      Somerst      225413.93       -26.81392    2.7012893
    ## 22      StoneBr      309054.74      -395.62678    1.6474046
    ## 23        SWISU      142811.67        18.52198   -0.8161116
    ## 24       Timber      243273.59      -244.94891    0.6362358
    ## 25      Veenker      239728.05      -219.75578    0.5000359
    ##    OverallQual_coef OverallCond_coef YearBuilt_coef BsmtFinSF1_coef
    ## 1         10382.353         8491.779       386.2592       19.087992
    ## 2          8536.741         7640.854       134.1005        9.259880
    ## 3          7088.020         6860.203      -426.2535        6.012336
    ## 4          7825.451         6389.937       500.7681       20.810419
    ## 5         13096.535         5463.279       123.7151       19.645063
    ## 6         11926.085         8569.589       517.7127       27.059012
    ## 7         16932.533        10918.615       469.7406       39.557915
    ## 8          3994.100         7373.733       376.5058       -4.204062
    ## 9         13296.802         7006.940       698.5086       38.396784
    ## 10         7517.574         8398.796       284.7787       21.117340
    ## 11         6872.774         6768.016      -395.7804       10.352280
    ## 12         9764.538         7077.249       506.1482       23.968941
    ## 13         8767.323         7336.128       590.9583       19.881146
    ## 14        14953.098        10551.662      4384.4472       44.782965
    ## 15         8948.750         7190.430      -225.1693        8.842591
    ## 16        12761.189        10485.699      5854.9831       70.799156
    ## 17        11040.900         8596.086       596.4268       19.937363
    ## 18         8645.755         8765.779       373.8640       19.743469
    ## 19         6838.194         6164.860       355.2081       12.225214
    ## 20        11452.400         8488.582       422.4796       19.657808
    ## 21        13259.139         8777.680      2422.9896       51.407593
    ## 22        17717.285        10807.663      2280.7779       45.888124
    ## 23         7877.678         7531.431       240.6497       12.818225
    ## 24        13781.366         7928.623      1465.3575       36.434611
    ## 25        11140.536         9416.699      2374.8143       22.858829
    ##    BsmtUnfSF_coef GrLivArea_coef BsmtFullBath_coef BedroomAbvGr_coef
    ## 1      7.26638989       40.82808        13146.5468        -6903.8761
    ## 2      1.19767228       57.06125         4292.7067         -433.5728
    ## 3      0.69089207       34.49595         4027.0351         3783.0676
    ## 4     13.47813771       45.08572         1633.8021          311.3123
    ## 5      5.40645372       55.90517        10882.4927        -5597.4769
    ## 6     10.84998587       68.24307         7727.5104        -2945.3967
    ## 7     16.87417703       74.95085         3134.6840       -16149.0840
    ## 8     -8.26221501       23.34721        13958.1872         8203.7540
    ## 9     26.04102210       63.72288         6777.0908        -7180.8208
    ## 10    14.75793341       35.86570         2271.5459         2261.3263
    ## 11     4.29015042       16.33575         5062.9512         3684.9219
    ## 12    13.69554015       43.50411         7931.4586         -550.2647
    ## 13     5.18360968       39.73810         3344.1932        -1751.1699
    ## 14    11.06019013      162.49371         3003.7154       -26997.2022
    ## 15    -0.76205106       43.88034         5954.6683         -503.2842
    ## 16    41.99413993      154.14443         2234.9255       -25260.6460
    ## 17     6.32201616       58.66732         8410.0037        -7145.2191
    ## 18    17.81980274       47.50562         5322.9459        -4207.8326
    ## 19     6.86122113       36.63713         5954.7937        -1271.5575
    ## 20     7.99682514       72.61540         5641.9526        -8625.1547
    ## 21    22.61573431       85.83124          304.0934        -8011.6134
    ## 22    35.89641006      131.30599        12585.6436       -23843.4531
    ## 23    -0.06663138       39.14078         5028.4793        -1732.2127
    ## 24    25.18276662       85.47331        11211.8372       -11054.3925
    ## 25     2.68897283      113.97701         5440.3724       -14302.8282
    ##    GarageCars_coef WoodDeckSF_coef ScreenPorch_coef YrSold_coef
    ## 1       5125.28196       44.632431        60.742045 -1372.93892
    ## 2       8099.48324       26.770934        80.080261  -137.52813
    ## 3       9599.85324       27.000868        92.212445   263.11598
    ## 4       6431.24376        3.198858        79.483325   157.35702
    ## 5      13931.57715       32.946534        -2.757536   220.29420
    ## 6      12609.03674       25.296216        64.262493   381.76130
    ## 7        -87.55113       -2.856960       -31.884157 -1729.87248
    ## 8      13989.57431       59.843100       155.157915   346.78878
    ## 9      10504.35263        5.369228        24.994037   166.48077
    ## 10      5624.06173       13.768841       116.898482   -53.82308
    ## 11      8084.26351       23.172780        91.741678    70.78049
    ## 12     11253.69080       26.687513        84.227876   484.91421
    ## 13      4620.69941       15.627038        57.973941  -333.28299
    ## 14     -1200.02876        5.654241        29.463827 -2179.11041
    ## 15      8896.70594       32.429142        61.629151  -145.00481
    ## 16     -2625.90114       -4.870155       121.294961 -1603.28318
    ## 17      5602.59976       17.044991        27.567031 -1002.59302
    ## 18      4510.09268       11.821352       109.899166  -809.31472
    ## 19      5319.83887       33.200500        87.671610  -720.53822
    ## 20      5448.77234       26.024496        61.277504  -857.06852
    ## 21      5904.76715        9.586412        76.416318   464.84902
    ## 22     -1066.90877       -2.591199       -33.627343 -2763.23789
    ## 23      6284.94947       30.825589        82.956609  -349.14668
    ## 24      8308.31532       13.589910        29.115578  -678.04757
    ## 25      3173.72907       28.173601        74.862795 -1268.57473

Prediction in training set
==========================

``` r
# from list to matrix
X_mat <- as.matrix(X[[1]])
y_mat <- as.matrix(Y[[1]])
model_mat <- cbind(y_mat, X_mat, rep(1, dim(X_mat)[1]))

m<-length(ids)

for (i in 2:m) {
  temp_x <- X[[i]]
  temp_y <- Y[[i]]
  temp <- cbind(temp_y, temp_x, rep(i, dim(temp_x)[1]))
  model_mat <- rbind(model_mat, temp)
}

cat_num <- seq(1, length(ids))
neighborhood_map <- data.frame(key = cat_num,
                               Neighborhood = ids)

model_mat <- data.frame(model_mat) %>%
  rename(key = V17,
         Intercept = V2,
         SalePrice = V1) %>%
  left_join(neighborhood_map, by = "key")

head(model_mat)
```

    ##   SalePrice Intercept MSSubClass   LotArea OverallQual OverallCond
    ## 1    167240         1   5.882353 -388.1765  -0.1764706           0
    ## 2    192500         1   5.882353 -216.1765  -0.1764706           0
    ## 3    192000         1   5.882353 -216.1765   0.8235294           0
    ## 4    172500         1   5.882353  523.8235  -0.1764706           0
    ## 5    178740         1   5.882353 -326.1765  -0.1764706           0
    ## 6    234000         1   5.882353 -202.1765  -0.1764706           0
    ##    YearBuilt BsmtFinSF1 BsmtUnfSF  GrLivArea BsmtFullBath BedroomAbvGr
    ## 1 -0.2352941       -180  103.5882 -179.94118   -0.1176471    0.1764706
    ## 2 -0.2352941       -180  228.5882  127.05882   -0.1176471    0.1764706
    ## 3 -1.2352941       -172  103.5882 -158.94118   -0.1176471    0.1764706
    ## 4  0.7647059       -196  129.5882 -169.94118   -0.1176471    0.1764706
    ## 5 -1.2352941       -196  246.5882  -13.94118   -0.1176471    0.1764706
    ## 6 -2.2352941       -196  245.5882  129.05882   -0.1176471    0.1764706
    ##   GarageCars WoodDeckSF ScreenPorch     YrSold key Neighborhood
    ## 1 -0.3529412 -39.294118   -8.470588 -1.4705882   1      Blmngtn
    ## 2 -0.3529412  -4.294118   -8.470588  1.5294118   1      Blmngtn
    ## 3 -0.3529412  -1.294118  135.529412  2.5294118   1      Blmngtn
    ## 4  0.6470588  -3.294118   -8.470588 -0.4705882   1      Blmngtn
    ## 5 -0.3529412  -3.294118   -8.470588 -1.4705882   1      Blmngtn
    ## 6 -0.3529412  -4.294118   -8.470588 -1.4705882   1      Blmngtn

``` r
model_df <- model_mat %>%
  left_join(param_df, by = "Neighborhood") %>%
  mutate(SalePrice_pred_bda = 
         Intercept_coef * Intercept +
         MSSubClass_coef * MSSubClass +
         LotArea_coef * LotArea +
         OverallQual_coef * OverallQual +
         OverallCond_coef * OverallCond +
         YearBuilt_coef * YearBuilt +
         BsmtFinSF1_coef * BsmtFinSF1 +
         BsmtUnfSF_coef * BsmtUnfSF +
         GrLivArea_coef * GrLivArea +
         BsmtFullBath_coef * BsmtFullBath +
         BedroomAbvGr_coef * BedroomAbvGr +
         GarageCars_coef * GarageCars +
         WoodDeckSF_coef * WoodDeckSF +
         ScreenPorch_coef * ScreenPorch +
         YrSold_coef * YrSold)

model_df %>%
  select(SalePrice, SalePrice_pred_bda) %>%
  round() %>%
  head()
```

    ##   SalePrice SalePrice_pred_bda
    ## 1    167240             180079
    ## 2    192500             190245
    ## 3    192000             195317
    ## 4    172500             182302
    ## 5    178740             188551
    ## 6    234000             193433

``` r
# MSPE

# Bayesian
mean((model_df$SalePrice - model_df$SalePrice_pred_bda)^2)
```

    ## [1] 393222788

393,222,788

Prediction in test set
======================

``` r
test <- read.csv("test.csv")
head(test)
```

    ##     Id MSSubClass MSZoning LotFrontage LotArea Street Alley LotShape
    ## 1 1461         20       RH          80   11622   Pave  <NA>      Reg
    ## 2 1462         20       RL          81   14267   Pave  <NA>      IR1
    ## 3 1463         60       RL          74   13830   Pave  <NA>      IR1
    ## 4 1464         60       RL          78    9978   Pave  <NA>      IR1
    ## 5 1465        120       RL          43    5005   Pave  <NA>      IR1
    ## 6 1466         60       RL          75   10000   Pave  <NA>      IR1
    ##   LandContour Utilities LotConfig LandSlope Neighborhood Condition1
    ## 1         Lvl    AllPub    Inside       Gtl        NAmes      Feedr
    ## 2         Lvl    AllPub    Corner       Gtl        NAmes       Norm
    ## 3         Lvl    AllPub    Inside       Gtl      Gilbert       Norm
    ## 4         Lvl    AllPub    Inside       Gtl      Gilbert       Norm
    ## 5         HLS    AllPub    Inside       Gtl      StoneBr       Norm
    ## 6         Lvl    AllPub    Corner       Gtl      Gilbert       Norm
    ##   Condition2 BldgType HouseStyle OverallQual OverallCond YearBuilt
    ## 1       Norm     1Fam     1Story           5           6      1961
    ## 2       Norm     1Fam     1Story           6           6      1958
    ## 3       Norm     1Fam     2Story           5           5      1997
    ## 4       Norm     1Fam     2Story           6           6      1998
    ## 5       Norm   TwnhsE     1Story           8           5      1992
    ## 6       Norm     1Fam     2Story           6           5      1993
    ##   YearRemodAdd RoofStyle RoofMatl Exterior1st Exterior2nd MasVnrType
    ## 1         1961     Gable  CompShg     VinylSd     VinylSd       None
    ## 2         1958       Hip  CompShg     Wd Sdng     Wd Sdng    BrkFace
    ## 3         1998     Gable  CompShg     VinylSd     VinylSd       None
    ## 4         1998     Gable  CompShg     VinylSd     VinylSd    BrkFace
    ## 5         1992     Gable  CompShg     HdBoard     HdBoard       None
    ## 6         1994     Gable  CompShg     HdBoard     HdBoard       None
    ##   MasVnrArea ExterQual ExterCond Foundation BsmtQual BsmtCond BsmtExposure
    ## 1          0        TA        TA     CBlock       TA       TA           No
    ## 2        108        TA        TA     CBlock       TA       TA           No
    ## 3          0        TA        TA      PConc       Gd       TA           No
    ## 4         20        TA        TA      PConc       TA       TA           No
    ## 5          0        Gd        TA      PConc       Gd       TA           No
    ## 6          0        TA        TA      PConc       Gd       TA           No
    ##   BsmtFinType1 BsmtFinSF1 BsmtFinType2 BsmtFinSF2 BsmtUnfSF TotalBsmtSF
    ## 1          Rec        468          LwQ        144       270         882
    ## 2          ALQ        923          Unf          0       406        1329
    ## 3          GLQ        791          Unf          0       137         928
    ## 4          GLQ        602          Unf          0       324         926
    ## 5          ALQ        263          Unf          0      1017        1280
    ## 6          Unf          0          Unf          0       763         763
    ##   Heating HeatingQC CentralAir Electrical X1stFlrSF X2ndFlrSF LowQualFinSF
    ## 1    GasA        TA          Y      SBrkr       896         0            0
    ## 2    GasA        TA          Y      SBrkr      1329         0            0
    ## 3    GasA        Gd          Y      SBrkr       928       701            0
    ## 4    GasA        Ex          Y      SBrkr       926       678            0
    ## 5    GasA        Ex          Y      SBrkr      1280         0            0
    ## 6    GasA        Gd          Y      SBrkr       763       892            0
    ##   GrLivArea BsmtFullBath BsmtHalfBath FullBath HalfBath BedroomAbvGr
    ## 1       896            0            0        1        0            2
    ## 2      1329            0            0        1        1            3
    ## 3      1629            0            0        2        1            3
    ## 4      1604            0            0        2        1            3
    ## 5      1280            0            0        2        0            2
    ## 6      1655            0            0        2        1            3
    ##   KitchenAbvGr KitchenQual TotRmsAbvGrd Functional Fireplaces FireplaceQu
    ## 1            1          TA            5        Typ          0        <NA>
    ## 2            1          Gd            6        Typ          0        <NA>
    ## 3            1          TA            6        Typ          1          TA
    ## 4            1          Gd            7        Typ          1          Gd
    ## 5            1          Gd            5        Typ          0        <NA>
    ## 6            1          TA            7        Typ          1          TA
    ##   GarageType GarageYrBlt GarageFinish GarageCars GarageArea GarageQual
    ## 1     Attchd        1961          Unf          1        730         TA
    ## 2     Attchd        1958          Unf          1        312         TA
    ## 3     Attchd        1997          Fin          2        482         TA
    ## 4     Attchd        1998          Fin          2        470         TA
    ## 5     Attchd        1992          RFn          2        506         TA
    ## 6     Attchd        1993          Fin          2        440         TA
    ##   GarageCond PavedDrive WoodDeckSF OpenPorchSF EnclosedPorch X3SsnPorch
    ## 1         TA          Y        140           0             0          0
    ## 2         TA          Y        393          36             0          0
    ## 3         TA          Y        212          34             0          0
    ## 4         TA          Y        360          36             0          0
    ## 5         TA          Y          0          82             0          0
    ## 6         TA          Y        157          84             0          0
    ##   ScreenPorch PoolArea PoolQC Fence MiscFeature MiscVal MoSold YrSold
    ## 1         120        0   <NA> MnPrv        <NA>       0      6   2010
    ## 2           0        0   <NA>  <NA>        Gar2   12500      6   2010
    ## 3           0        0   <NA> MnPrv        <NA>       0      3   2010
    ## 4           0        0   <NA>  <NA>        <NA>       0      6   2010
    ## 5         144        0   <NA>  <NA>        <NA>       0      1   2010
    ## 6           0        0   <NA>  <NA>        <NA>       0      4   2010
    ##   SaleType SaleCondition
    ## 1       WD        Normal
    ## 2       WD        Normal
    ## 3       WD        Normal
    ## 4       WD        Normal
    ## 5       WD        Normal
    ## 6       WD        Normal

Missing values
==============

``` r
apply(is.na(test[, COLUMNS_bda]), 2, sum)
```

    ##   MSSubClass      LotArea  OverallQual  OverallCond    YearBuilt 
    ##            0            0            0            0            0 
    ##   BsmtFinSF1    BsmtUnfSF    GrLivArea BsmtFullBath BedroomAbvGr 
    ##            1            1            0            2            0 
    ##   GarageCars   WoodDeckSF  ScreenPorch       YrSold 
    ##            1            0            0            0

Mean imputation
===============

``` r
# Calculate mean of each columns
mean_col <- apply(test[, COLUMNS_bda], 2, mean, na.rm = TRUE)

# Identify which row has NA
index_BsmtFinSF1 <- which(is.na(test[, "BsmtFinSF1"]) == TRUE)
index_BsmtUnfSF <- which(is.na(test[, "BsmtUnfSF"]) == TRUE)
index_GarageCars <- which(is.na(test[, "GarageCars"]) == TRUE)
index_BsmtFullBath <- which(is.na(test[, "BsmtFullBath"]) == TRUE)

# Impute NA by column mean
test[index_BsmtFinSF1, "BsmtFinSF1"] <- mean_col[names(mean_col) == "BsmtFinSF1"]
test[index_BsmtUnfSF, "BsmtUnfSF"] <- mean_col[names(mean_col) == "BsmtUnfSF"]
test[index_GarageCars, "GarageCars"] <- mean_col[names(mean_col) == "GarageCars"]
test[index_BsmtFullBath, "BsmtFullBath"] <- mean_col[names(mean_col) == "BsmtFullBath"]
```

``` r
apply(is.na(test[, COLUMNS_bda]), 2, sum)
```

    ##   MSSubClass      LotArea  OverallQual  OverallCond    YearBuilt 
    ##            0            0            0            0            0 
    ##   BsmtFinSF1    BsmtUnfSF    GrLivArea BsmtFullBath BedroomAbvGr 
    ##            0            0            0            0            0 
    ##   GarageCars   WoodDeckSF  ScreenPorch       YrSold 
    ##            0            0            0            0

Scaling predictors
==================

When we built hierarchical models, we scaled training data by deducting by mean of each predictor. To scale test data, we obtrain training mean.

``` r
mean_neighbor_train <- train %>%
  group_by(Neighborhood) %>%
  summarise_all(funs(mean(., na.rm = TRUE))) %>%
  select(-Id)

mean_neighbor_train <- mean_neighbor_train[, c("Neighborhood", COLUMNS_bda)]

head(mean_neighbor_train)
```

    ## # A tibble: 6 x 15
    ##   Neighborhood MSSubClass LotArea OverallQual OverallCond YearBuilt
    ##   <fct>             <dbl>   <dbl>       <dbl>       <dbl>     <dbl>
    ## 1 Blmngtn           114.    3398.        7.18        5        2005.
    ## 2 Blueste           160     1625         6           6        1980 
    ## 3 BrDale            160     1801         5.69        5.44     1971.
    ## 4 BrkSide            49.7   7360.        5.05        6.14     1931.
    ## 5 ClearCr            52.5  30876.        5.89        5.68     1967.
    ## 6 CollgCr            43.3   9619.        6.64        5.24     1998.
    ## # ... with 9 more variables: BsmtFinSF1 <dbl>, BsmtUnfSF <dbl>,
    ## #   GrLivArea <dbl>, BsmtFullBath <dbl>, BedroomAbvGr <dbl>,
    ## #   GarageCars <dbl>, WoodDeckSF <dbl>, ScreenPorch <dbl>, YrSold <dbl>

Subset test to only predictors that we need.

``` r
test <- test[, c("Id", "Neighborhood", COLUMNS_bda)]
```

Scale test set predictors by training set means
===============================================

``` r
for (COL in COLUMNS_bda) {
  temp_1 <- mean_neighbor_train[, c("Neighborhood", COL)]
  colnames(temp_1)[2] <- "scaling"
  test <- test %>%
    left_join(temp_1, by = "Neighborhood")
  test[,COL] <- test[,COL] - test[,"scaling"]
  test <- test %>%
    select(-scaling)
}

head(test)
```

    ##     Id Neighborhood MSSubClass   LotArea OverallQual OverallCond
    ## 1 1461        NAmes -18.777778  1482.084   -0.360000   0.2088889
    ## 2 1462        NAmes -18.777778  4127.084    0.640000   0.2088889
    ## 3 1463      Gilbert   1.772152  2450.848   -1.556962  -0.1265823
    ## 4 1464      Gilbert   1.772152 -1401.152   -0.556962   0.8734177
    ## 5 1465      StoneBr  36.800000 -5666.920   -0.160000   0.0000000
    ## 6 1466      Gilbert   1.772152 -1379.152   -0.556962  -0.1265823
    ##    YearBuilt BsmtFinSF1  BsmtUnfSF  GrLivArea BsmtFullBath BedroomAbvGr
    ## 1  1.0044444  -17.60889 -176.01333 -414.31111   -0.4577778  -0.93333333
    ## 2 -1.9955556  437.39111  -40.01333   18.68889   -0.4577778   0.06666667
    ## 3 -1.2531646  576.97468 -518.00000  -12.31646   -0.2531646  -0.10126582
    ## 4 -0.2531646  387.97468 -331.00000  -37.31646   -0.2531646  -0.10126582
    ## 5 -6.4800000 -559.92000  377.60000 -599.08000   -0.7600000  -0.32000000
    ## 6 -5.2531646 -214.02532  108.00000   13.68354   -0.2531646  -0.10126582
    ##    GarageCars WoodDeckSF ScreenPorch   YrSold
    ## 1 -0.51555556   77.21778   98.244444 2.133333
    ## 2 -0.51555556  330.21778  -21.755556 2.133333
    ## 3 -0.06329114  109.17722   -3.683544 2.443038
    ## 4 -0.06329114  257.17722   -3.683544 2.443038
    ## 5 -0.40000000 -164.96000  116.240000 2.080000
    ## 6 -0.06329114   54.17722   -3.683544 2.443038

``` r
round(apply(test[, COLUMNS_bda], 2, mean), 2)
```

    ##   MSSubClass      LotArea  OverallQual  OverallCond    YearBuilt 
    ##        -0.90      -422.45         0.00        -0.02         0.79 
    ##   BsmtFinSF1    BsmtUnfSF    GrLivArea BsmtFullBath BedroomAbvGr 
    ##         2.18        -9.26       -16.52         0.01         0.00 
    ##   GarageCars   WoodDeckSF  ScreenPorch       YrSold 
    ##         0.01         1.26         2.26        -0.05

``` r
test <- test %>%
  mutate(Intercept = 1) %>%
  left_join(param_df, by = "Neighborhood")
```

``` r
head(test)
```

    ##     Id Neighborhood MSSubClass   LotArea OverallQual OverallCond
    ## 1 1461        NAmes -18.777778  1482.084   -0.360000   0.2088889
    ## 2 1462        NAmes -18.777778  4127.084    0.640000   0.2088889
    ## 3 1463      Gilbert   1.772152  2450.848   -1.556962  -0.1265823
    ## 4 1464      Gilbert   1.772152 -1401.152   -0.556962   0.8734177
    ## 5 1465      StoneBr  36.800000 -5666.920   -0.160000   0.0000000
    ## 6 1466      Gilbert   1.772152 -1379.152   -0.556962  -0.1265823
    ##    YearBuilt BsmtFinSF1  BsmtUnfSF  GrLivArea BsmtFullBath BedroomAbvGr
    ## 1  1.0044444  -17.60889 -176.01333 -414.31111   -0.4577778  -0.93333333
    ## 2 -1.9955556  437.39111  -40.01333   18.68889   -0.4577778   0.06666667
    ## 3 -1.2531646  576.97468 -518.00000  -12.31646   -0.2531646  -0.10126582
    ## 4 -0.2531646  387.97468 -331.00000  -37.31646   -0.2531646  -0.10126582
    ## 5 -6.4800000 -559.92000  377.60000 -599.08000   -0.7600000  -0.32000000
    ## 6 -5.2531646 -214.02532  108.00000   13.68354   -0.2531646  -0.10126582
    ##    GarageCars WoodDeckSF ScreenPorch   YrSold Intercept Intercept_coef
    ## 1 -0.51555556   77.21778   98.244444 2.133333         1       145889.9
    ## 2 -0.51555556  330.21778  -21.755556 2.133333         1       145889.9
    ## 3 -0.06329114  109.17722   -3.683544 2.443038         1       192991.7
    ## 4 -0.06329114  257.17722   -3.683544 2.443038         1       192991.7
    ## 5 -0.40000000 -164.96000  116.240000 2.080000         1       309054.7
    ## 6 -0.06329114   54.17722   -3.683544 2.443038         1       192991.7
    ##   MSSubClass_coef LotArea_coef OverallQual_coef OverallCond_coef
    ## 1       -36.36436     1.343324         8767.323         7336.128
    ## 2       -36.36436     1.343324         8767.323         7336.128
    ## 3      -186.97179     2.066847        13296.802         7006.940
    ## 4      -186.97179     2.066847        13296.802         7006.940
    ## 5      -395.62678     1.647405        17717.285        10807.663
    ## 6      -186.97179     2.066847        13296.802         7006.940
    ##   YearBuilt_coef BsmtFinSF1_coef BsmtUnfSF_coef GrLivArea_coef
    ## 1       590.9583        19.88115        5.18361       39.73810
    ## 2       590.9583        19.88115        5.18361       39.73810
    ## 3       698.5086        38.39678       26.04102       63.72288
    ## 4       698.5086        38.39678       26.04102       63.72288
    ## 5      2280.7779        45.88812       35.89641      131.30599
    ## 6       698.5086        38.39678       26.04102       63.72288
    ##   BsmtFullBath_coef BedroomAbvGr_coef GarageCars_coef WoodDeckSF_coef
    ## 1          3344.193         -1751.170        4620.699       15.627038
    ## 2          3344.193         -1751.170        4620.699       15.627038
    ## 3          6777.091         -7180.821       10504.353        5.369228
    ## 4          6777.091         -7180.821       10504.353        5.369228
    ## 5         12585.644        -23843.453       -1066.909       -2.591199
    ## 6          6777.091         -7180.821       10504.353        5.369228
    ##   ScreenPorch_coef YrSold_coef
    ## 1         57.97394   -333.2830
    ## 2         57.97394   -333.2830
    ## 3         24.99404    166.4808
    ## 4         24.99404    166.4808
    ## 5        -33.62734  -2763.2379
    ## 6         24.99404    166.4808

``` r
temp_col <- c("Intercept")
temp_col <- c(temp_col, COLUMNS_bda)
temp_col
```

    ##  [1] "Intercept"    "MSSubClass"   "LotArea"      "OverallQual" 
    ##  [5] "OverallCond"  "YearBuilt"    "BsmtFinSF1"   "BsmtUnfSF"   
    ##  [9] "GrLivArea"    "BsmtFullBath" "BedroomAbvGr" "GarageCars"  
    ## [13] "WoodDeckSF"   "ScreenPorch"  "YrSold"

``` r
# Initial value before calculate predicted sale prices
SalePrice_vec <- rep(0, nrow(test))

for (col in temp_col) {
  col_coef <- paste(col, "_coef", sep = "")
  SalePrice_vec = SalePrice_vec + test[, col] * test[, col_coef]
}

SalePrice_vec[1:5]
```

    ## [1] 133719.7 166470.3 182388.3 192243.3 166006.4

``` r
summary(SalePrice_vec)
```

    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    ##   25930  128939  159983  179118  211015  580666

``` r
summary(train$SalePrice)
```

    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    ##   34900  129950  163000  180930  214000  755000

Submission
==========

``` r
sub <- data.frame(Id = test[, "Id"],
                  SalePrice = SalePrice_vec)
head(sub)
```

    ##     Id SalePrice
    ## 1 1461  133719.7
    ## 2 1462  166470.3
    ## 3 1463  182388.3
    ## 4 1464  192243.3
    ## 5 1465  166006.4
    ## 6 1466  172266.4

``` r
write.csv(sub, 
          "C:/Users/yukic/Documents/kaggle/houseprices/submission_190721.csv",
          row.names = FALSE)
```
