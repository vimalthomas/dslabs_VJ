knn algorithm vs logistic regression
================

## code for knn algorithm.

You can include R code in the document as
    follows:

``` r
library(tidyverse)
```

    ## ── Attaching packages ─────────────────────────────────────────────────── tidyverse 1.3.0 ──

    ## ✔ ggplot2 3.2.1     ✔ purrr   0.3.3
    ## ✔ tibble  3.0.1     ✔ dplyr   1.0.0
    ## ✔ tidyr   1.0.2     ✔ stringr 1.4.0
    ## ✔ readr   1.3.1     ✔ forcats 0.4.0

    ## Warning: package 'tibble' was built under R version 3.6.2

    ## Warning: package 'dplyr' was built under R version 3.6.2

    ## ── Conflicts ────────────────────────────────────────────────────── tidyverse_conflicts() ──
    ## ✖ dplyr::filter() masks stats::filter()
    ## ✖ dplyr::lag()    masks stats::lag()

``` r
library(dslabs)
library(caret)
```

    ## Loading required package: lattice

    ## 
    ## Attaching package: 'caret'

    ## The following object is masked from 'package:purrr':
    ## 
    ##     lift

``` r
data("mnist_27")
```

``` r
x <- as.matrix(mnist_27$train[,2:3])
y <- mnist_27$train$y
knn_fit <- knn3(x,y)
```

``` r
library(caret)
fit_glm<-glm(y~x_1+x_2,data = mnist_27$train,family="binomial")
p_hat_logistic <- predict(fit_glm, mnist_27$test)
y_hat_logistic <- ifelse(p_hat_logistic>0.5,7,2)%>%factor
length(y_hat_logistic)
```

    ## [1] 200

``` r
length(mnist_27$test$y)
```

    ## [1] 200

``` r
confusionMatrix(data = y_hat_logistic, reference = mnist_27$test$y)$overall[1]
```

    ## Accuracy 
    ##     0.76

``` r
#make the predictors (features) as matrix
x <- as.matrix(mnist_27$train[,2:3])
#make outcomes as a vector
y <- mnist_27$train$y
#one way to call knn 
knn_fit <- knn3(x, y)
#another way to call knn
knn_fit <- knn3(y ~ ., data = mnist_27$train, k=5)


y_hat_knn<-predict(knn_fit, mnist_27$test, type = "class")
length(y_hat_knn)
```

    ## [1] 200

``` r
length(mnist_27$test$y)
```

    ## [1] 200

``` r
confusionMatrix(data = y_hat_knn, reference = mnist_27$test$y)$overall["Accuracy"]
```

    ## Accuracy 
    ##    0.815

Note that the `echo = FALSE` parameter was added to the code chunk to
prevent printing of the R code that generated the plot.
