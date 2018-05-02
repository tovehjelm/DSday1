Our R Project
================

Packages
--------

Packages we'll look at today:

-   odbc / readxl / readr / dbplyr (sql), for data access
-   tidyverse, for data manipulation
-   DataExplorer, for providing automated EDA overview of our data
-   modelr / rsamples, for sampling strategy
-   recipes, for performing feature engineering - scaling, pca etc.
-   glmnet / h2o / FFTrees, for building models
-   yardstick / broom, for evaluation
-   rmarkdown, for documentation

Working with databases
----------------------

We need a database connection before we can do anything with our database.

``` r
library(DBI)
library(odbc)

driver="SQL Server"
server="fbmcsads.database.windows.net"
database="WideWorldImporters-Standard"
uid="adatumadmin"
pwd="Pa55w.rdPa55w.rd"


con<-dbConnect(odbc(), 
               driver = driver,
               server = server,
               database = database,
               uid = uid,
               pwd = pwd)
```

Now that we have a database connection, we can write SQL in a code chunk.

``` sql

select top 5 * from flights
```

|  year|  month|  day|  dep\_time|  sched\_dep\_time|  dep\_delay|  arr\_time|  sched\_arr\_time|  arr\_delay| carrier |  flight| tailnum | origin | dest |  air\_time|  distance|  hour|  minute| time\_hour          |
|-----:|------:|----:|----------:|-----------------:|-----------:|----------:|-----------------:|-----------:|:--------|-------:|:--------|:-------|:-----|----------:|---------:|-----:|-------:|:--------------------|
|  2013|      1|    1|        517|               515|           2|        830|               819|          11| UA      |    1545| N14228  | EWR    | IAH  |        227|      1400|     5|      15| 2013-01-01 05:00:00 |
|  2013|      1|    1|        533|               529|           4|        850|               830|          20| UA      |    1714| N24211  | LGA    | IAH  |        227|      1416|     5|      29| 2013-01-01 05:00:00 |
|  2013|      1|    1|        542|               540|           2|        923|               850|          33| AA      |    1141| N619AA  | JFK    | MIA  |        160|      1089|     5|      40| 2013-01-01 05:00:00 |
|  2013|      1|    1|        544|               545|          -1|       1004|              1022|         -18| B6      |     725| N804JB  | JFK    | BQN  |        183|      1576|     5|      45| 2013-01-01 05:00:00 |
|  2013|      1|    1|        554|               600|          -6|        812|               837|         -25| DL      |     461| N668DN  | LGA    | ATL  |        116|       762|     6|       0| 2013-01-01 06:00:00 |

We can use dbplyr to construct dplyr commands that work on the DB.

``` r
library(tidyverse)
```

    ## -- Attaching packages ---------------------------------------------------------------- tidyverse 1.2.1 --

    ## v ggplot2 2.2.1     v purrr   0.2.4
    ## v tibble  1.4.2     v dplyr   0.7.4
    ## v tidyr   0.8.0     v stringr 1.3.0
    ## v readr   1.1.1     v forcats 0.2.0

    ## -- Conflicts ------------------------------------------------------------------- tidyverse_conflicts() --
    ## x dplyr::filter() masks stats::filter()
    ## x dplyr::lag()    masks stats::lag()

``` r
library(dbplyr)
```

    ## 
    ## Attaching package: 'dbplyr'

    ## The following objects are masked from 'package:dplyr':
    ## 
    ##     ident, sql

``` r
flights_tbl <-tbl(con, "flights")

flights_tbl %>%
  filter(month<=6) %>% 
  group_by(origin) %>% 
  summarise(n = n(), #n() gives you the total number of rows for all groups
            mean_dist = mean(distance))
```

    ## # Source:   lazy query [?? x 3]
    ## # Database: Microsoft SQL Server
    ## #   12.00.0300[dbo@fbmcsads/WideWorldImporters-Standard]
    ##   origin     n mean_dist
    ##   <chr>  <int>     <dbl>
    ## 1 LGA    50074      789.
    ## 2 EWR    60718     1017.
    ## 3 JFK    55366     1252.

We can also work with tables that aren't in the default schema (dbo). The flights table is in the dbo schema, which is the default, to query tables in Purchasing for example we need to use the in\_schema function to let R know what schema to use.

``` r
purchaseorders_tbl <- tbl(con, in_schema("purchasing","purchaseorders"))

purchaseorders_tbl %>% 
  top_n(5)
```

    ## Selecting by LastEditedWhen

    ## # Source:   lazy query [?? x 12]
    ## # Database: Microsoft SQL Server
    ## #   12.00.0300[dbo@fbmcsads/WideWorldImporters-Standard]
    ##   PurchaseOrderID SupplierID OrderDate  DeliveryMethodID ContactPersonID
    ##             <int>      <int> <chr>                 <int>           <int>
    ## 1            2073          4 2016-05-31                7               2
    ## 2            2074          7 2016-05-31                2               2
    ## 3            2071          4 2016-05-30                7               2
    ## 4            2072          7 2016-05-30                2               2
    ## 5            2068          4 2016-05-27                7               2
    ## 6            2069          7 2016-05-27                2               2
    ## 7            2070          4 2016-05-28                7               2
    ## # ... with 7 more variables: ExpectedDeliveryDate <chr>,
    ## #   SupplierReference <chr>, IsOrderFinalized <lgl>, Comments <chr>,
    ## #   InternalComments <chr>, LastEditedBy <int>, LastEditedWhen <chr>

We can use the "Id()" function from DBI to work with schema more generically within a database. This means we aren't restricted to just SELECT statements. So you can create, delete, inserts, updates etc.

``` r
# Create a schema to work in / errors if already exists
dbGetQuery(con, "CREATE SCHEMA DBIexample3")
```

    ## Error: <SQL> 'CREATE SCHEMA DBIexample3'
    ##   nanodbc/nanodbc.cpp:1587: 42S01: [Microsoft][ODBC SQL Server Driver][SQL Server]There is already an object named 'DBIexample3' in the database.

``` r
# Write some data - drop and recreate the table if it exists already
dbWriteTable(con, "iris", iris, overwrite = TRUE)
# Read from newly written table
head(dbReadTable(con, "iris"))
```

    ##   Sepal.Length Sepal.Width Petal.Length Petal.Width Species
    ## 1          5.1         3.5          1.4         0.2  setosa
    ## 2          4.9         3.0          1.4         0.2  setosa
    ## 3          4.7         3.2          1.3         0.2  setosa
    ## 4          4.6         3.1          1.5         0.2  setosa
    ## 5          5.0         3.6          1.4         0.2  setosa
    ## 6          5.4         3.9          1.7         0.4  setosa

``` r
# Read from a table in the schema
head(dbReadTable(con, Id(schema="20774A", table = "CustomerTransactions")))
```

    ## Note: method with signature 'DBIConnection#SQL' chosen for function 'dbQuoteIdentifier',
    ##  target signature 'Microsoft SQL Server#SQL'.
    ##  "OdbcConnection#character" would also be valid

    ##                  CustomerName TransactionAmount OutstandingBalance
    ## 1             Aakriti Byrraju           2645.00                  0
    ## 2                  Bala Dixit            465.75                  0
    ## 3 Tailspin Toys (Head Office)            103.50                  0
    ## 4 Tailspin Toys (Head Office)            511.98                  0
    ## 5                Sara Huiting            809.60                  0
    ## 6                Alinne Matos            494.50                  0
    ##   TaxAmount PKIDDate TransactionDate
    ## 1    345.00 20130101      2013-01-01
    ## 2     60.75 20130101      2013-01-01
    ## 3     13.50 20130101      2013-01-01
    ## 4     66.78 20130101      2013-01-01
    ## 5    105.60 20130101      2013-01-01
    ## 6     64.50 20130101      2013-01-01

``` r
# If a write method is supported by the driver, this will work
dbWriteTable(con, Id(schema="DBIexample3",table="iris"), iris, overwrite = TRUE)
```

    ## Error in (function (classes, fdef, mtable) : unable to find an inherited method for function 'dbWriteTable' for signature '"Microsoft SQL Server", "SQL", "missing"'

Some of our code could fail in that section so we used "error = TRUE" to be able to carry on even if some of the code errored. Great for optional code or things with bad connection. The schema we create only shows up in the overview in the Connections tab if the schema has any objects.

Exploratory Data Analysis
-------------------------

We set eval = FALSE so that is doesn't actually create the report when we knit it.

``` r
flights_tbl %>% 
  as_data_frame() %>% 
  DataExplorer::GenerateReport()
```

Questions arising from the basic report:

1.  Why is there a day with the double number of flights?
2.  Why is there a negative correlation between flight and distance?
3.  Do we need to do anything about missings or can we just remove the rows?

Things to implement later in the workflow due to the EDA (exploratory data analysis): 1. We need to address the high correlation between time columns. 2. We need to group low frequency airline carries 3. Bivariate analysis

Answering our questions
-----------------------

> Why is there a day with the double number of flights?

Are there duplicate rows?

``` r
#1

flights_tbl %>% 
  filter(day == 15) %>% 
  distinct() %>% 
  summarise(n()) %>% 
  as_data_frame()-> #because otherwise it stores the sql object
  distinct_count

flights_tbl %>% 
  filter(day == 15) %>% 
  summarise(n()) %>% 
  as_data_frame() ->
  row_count
  
identical(distinct_count, row_count)
```

    ## [1] TRUE

Are the number of rows unusual?

``` r
library(ggplot2)
flights_tbl %>% 
  group_by(day) %>% 
  summarise(n_unique = n(), n_distinct(flight)) %>% 
  arrange(day)
```

    ## # Source:     lazy query [?? x 3]
    ## # Database:   Microsoft SQL Server
    ## #   12.00.0300[dbo@fbmcsads/WideWorldImporters-Standard]
    ## # Ordered by: day
    ##      day n_unique `n_distinct(flight)`
    ##    <int>    <int>                <int>
    ##  1     1    11036                 2532
    ##  2     2    10808                 2542
    ##  3     3    11211                 2491
    ##  4     4    11059                 2449
    ##  5     5    10858                 2463
    ##  6     6    11059                 2484
    ##  7     7    10985                 2427
    ##  8     8    11271                 2436
    ##  9     9    10857                 2496
    ## 10    10    11227                 2504
    ## # ... with more rows

There are NOT any duplicates. When looking at if the number of rows is unusual (by looking at number of flights for each day) we can see that there actually isn't a bigger a number of flights during day 15, so there's probably something wrong with the histogram. The histogram buts the data into bins, and might have put day 15 together with another day.

``` r
library(ggplot2)
flights_tbl %>% 
  group_by(day) %>% 
  summarise(n=n(), n_unique=n_distinct(flight)) %>% 
  as_data_frame() %>% 
  ggplot(aes(x=day, y=n)) +
  geom_col()
```

![](README_files/figure-markdown_github/unnamed-chunk-9-1.png) Looks like the problem was the visualization, and now we can see that 15 is not unusual at all.

### Bivariate analysis

``` r
flights_tbl %>% 
  select_if(is.numeric) %>% 
  as_data_frame %>% 
  gather(col, val, -dep_delay) %>% #unpivots the data
  filter(col!="arr_delay") %>% 
  ggplot(aes(x=val, y=dep_delay)) +
    geom_bin2d() + #heatmap with squaredots
    facet_wrap(~col, scales = "free") #use the range for the individual column, don't put them on the same scale
```

    ## Applying predicate on the first 100 rows

    ## Warning: Removed 100693 rows containing non-finite values (stat_bin2d).

    ## Warning: Computation failed in `stat_bin2d()`:
    ## 'from' must be a finite number

![](README_files/figure-markdown_github/unnamed-chunk-10-1.png)

Let\#s have a look at the data without the extremes

``` r
flights_tbl %>% 
  select_if(is.numeric) %>% 
  as_data_frame %>% 
  gather(col, val, -dep_delay) %>% #unpivots the data
  filter(col!="arr_delay", 
  dep_delay<500) %>% # takes out the extreme ones
  ggplot(aes(x=val, y=dep_delay)) +
    geom_bin2d() + #heatmap with squaredots
    facet_wrap(~col, scales = "free") + #use the range for the individual column, don't put them on the same scale
  scale_fill_gradientn(colours = viridisLite::viridis(256, option = "D"))
```

    ## Applying predicate on the first 100 rows

    ## Warning: Removed 1631 rows containing non-finite values (stat_bin2d).

    ## Warning: Computation failed in `stat_bin2d()`:
    ## 'from' must be a finite number

![](README_files/figure-markdown_github/unnamed-chunk-11-1.png)

### Theory

We have a highly uneven data where we have a lot more flights that are on time then delayed. Example, if we have 97% not delayed our model is just going to predict that they are not delayed all the time and get a 97% accuracy. So we need to make it look like we have more delays.

Options for sampling when dealing with a large class imbalance:

-   Downsampling - Takes as many majority rows as minority rows = many rows without delays will be left out.
-   No overfitting
-   Can drastically reduce training data size
-   Upsampling - repeat the minority rows until we get to a certain ration, for example 15%. Then the model wont be able to ignore those rows and wont be able to use the rule "always on time". You don't want to upsample too much because weird and unusual patterns would seem more common than it is. = risk of overfitting
-   Risk overfitting
-   Doesn't reduce training data set
-   Synthesise data - looks at the minority rows and creates similar data. Package: synthpop
-   Doesn't reduce training set
-   Avoids some of the overfitting risk
-   Can weaken prediction if minority data is very similar to majority

Hyperparameter tuning Trying different parameters for the model to see which is the best. To do this we need to split our training data into more training sets to train the different models on. k-fold cross validation explicity.

-   Run the same model and assess robustness of coefficients.
-   We have an algorithm that needs explicit cross validation because it doesn't do it internally
-   When we're going to run lots of models with hyperparameter tuning so the results are more consistent

Bootstrapping We use bootstrapping when we want to fit a single model and ensure the results are robust. This will often do many more iterations than k-fold cross validation, making it better in cases where there's relatively small amounts of data.

Packages we can use for sampling:

-   modelr, which facilitates bootstrap and k-folds cross validation strategies
-   rsample, allows to bootstrap and perform a wide variety of cross valudation tasks
-   recipes, allows us to upsample and downsample
-   synthpop allows us to build synthesised samples

### Practical

We split the data into training data and test data BEFORE we upsample/downsample/synthesise. We want to keep it CLEAN to be able to evaluate correctly.

``` r
flights_tbl %>% 
  as_data_frame() ->
  flights

flights %>% 
  mutate(was_delayed = ifelse(arr_delay>5, "Delayed","Not Delayed"),
         week = ifelse(day %/% 7 > 3, 3, day %/% 7)) -> # %/% is absolut division
  flights

flights %>% 
  modelr::resample_partition(c(train=0.7,test=0.3)) -> # creates an object with test and train-data
  splits 

splits %>% 
  pluck("train") %>% # picks out the training data from "splits" to a seperate object
  as_data_frame() ->
  train_raw

splits %>% 
  pluck("test") %>% # picks out the test data from "splits" to a seperate object
  as_data_frame() ->
  test_raw
```

During the investigation we'll look at the impact of upsampling on the model. We'll see it in action in a bit. Time to cook!

Step 1 is to say what we want to do with the data

1.1 remove variables that end with time and delay, and year, because they are very highly correlated with other variables. And tailnum and flight because we don't think it will help us predict anything. 1.2 removes all rows with missing values (because we only have very few missing values 1.3 remoces variables with zero variance 1.4 removes variables with near zero variance 1.5 groups nominal variables with few observations to an other value. Default = if less than 5% then put in other bin, we can set with threshold argument

Step 2 is to prepare the data - where and how to apply the steps to the dataset

Step 3 is to apply the changes to the train data set

``` r
library(recipes)
```

    ## Loading required package: broom

    ## 
    ## Attaching package: 'recipes'

    ## The following object is masked from 'package:stringr':
    ## 
    ##     fixed

    ## The following object is masked from 'package:stats':
    ## 
    ##     step

``` r
basic_fe <- recipe(train_raw, was_delayed ~ .) #what we will train on and what we want to predict

#1
basic_fe %>% 
  step_rm(ends_with("time"), ends_with("delay"), 
          tailnum, flight, 
          minute, time_hour, day) %>% #1.1
  step_naomit(all_predictors()) %>% 
  step_naomit(all_outcomes()) %>% #1.2
  step_zv(all_predictors()) %>%  #1.3
  step_nzv(all_predictors()) %>% #1.4
  step_other(all_nominal(), threshold = 0.03) -> #1.5 
  colscleaned_fe

#2
colscleaned_fe <- prep(colscleaned_fe, verbose = TRUE)
```

    ## oper 1 step rm [training] 
    ## oper 2 step naomit [training] 
    ## oper 3 step naomit [training] 
    ## oper 4 step zv [training] 
    ## oper 5 step nzv [training] 
    ## oper 6 step other [training]

``` r
#3
train_prep1 <- bake(colscleaned_fe, train_raw)
```

Now we need to process our numeric variables.

1.1 Logaritmathize distance 1.2 Turn month, day and hour to factors

``` r
colscleaned_fe %>% 
  step_log(distance) %>% 
  step_rm(tailnum, dest) %>% #1.1
step_num2factor(month, week, hour) -> #1.2
  numscleaned_fe

numscleaned_fe <- prep(numscleaned_fe, verbose = TRUE)
```

    ## oper 1 step rm [pre-trained]
    ## oper 2 step naomit [pre-trained]
    ## oper 3 step naomit [pre-trained]
    ## oper 4 step zv [pre-trained]
    ## oper 5 step nzv [pre-trained]
    ## oper 6 step other [pre-trained]
    ## oper 7 step log [training] 
    ## oper 8 step rm [training] 
    ## oper 9 step num2factor [training]

``` r
numscleaned_fe
```

    ## Data Recipe
    ## 
    ## Inputs:
    ## 
    ##       role #variables
    ##    outcome          1
    ##  predictor         20
    ## 
    ## Training data contained 235743 data points and 6524 incomplete rows. 
    ## 
    ## Operations:
    ## 
    ## Variables removed dep_time, sched_dep_time, arr_time, ... [trained]
    ## Removing rows with NA values in all_predictors()
    ## Removing rows with NA values in all_outcomes()
    ## Zero variance filter removed year [trained]
    ## Sparse, unbalanced variable filter removed no terms [trained]
    ## Collapsing factor levels for carrier, origin, dest, was_delayed [trained]
    ## Log transformation on distance [trained]
    ## Variables removed tailnum, dest [trained]
    ## Factor variables from month, week, hour [trained]

``` r
train_prep1 <- bake(numscleaned_fe, train_raw)
```

### Upsampling

w00t is's upsampling time!

First we need to see what the ratio is now, to see how much we want we to upsample it.This doesn't work now because we changed the varaible to factors, need to look up.

``` r
count(train_prep1$was_delayed=="Delayed")/count(train_prep1$was_delayed=="Not Delayed")
```

    ## Error in UseMethod("groups"): no applicable method for 'groups' applied to an object of class "logical"

``` r
mean(train_prep1$was_delayed, na.rm = TRUE)
```

    ## Warning in mean.default(train_prep1$was_delayed, na.rm = TRUE): argument is
    ## not numeric or logical: returning NA

    ## [1] NA

33,5% are delayed according to our definition of was\_delayed, where we set more than 5 minutes = late. But the definition of late is something that could be discussed. Importans business decision. So now we'll increase the ratio to 50/50.

``` r
numscleaned_fe %>% 
  step_upsample(all_outcomes(),ratio = 1) %>% 
  prep(retain=TRUE) %>% 
  juice() %>% 
  bake(numscleaned_fe, .) ->
  train_prep2
```

We save the dataset as train\_prep2 to be able to compare it with the data that has not been upsampled.

### Building models

Decide which types of models you want to consider -- perhaps using Microsoft's lovely \[cheat sheet\] (<https://docs.microsoft.com/en-us/azure/machine-learning/studio/media/algorithm-cheat-sheet/machine-learning-algorithm-cheat-sheet-small_v_0_6-01.png>). Then determine if you need any special processing to the data beyond what you've done so far.

Let's begin with glm - genereal linear model We want to predict was\_delayed by all other variables, it's binomial (because it's only delayed or not delayed) and we want to try using the data from both train\_prep1 and train\_prep2 to compare the upsampled to the unupsampled data.

``` r
glm_unbal <- glm(was_delayed ~ .-1, "binomial",data = train_prep1)
glm_bal <- glm(was_delayed ~ .-1, "binomial", data = train_prep2)
```

Then we can see how these models are constructed and how they perform. Adds together the coefficiens of all variable for a specific case. Higher means more likely to be late.

``` r
glm_unbal
```

    ## 
    ## Call:  glm(formula = was_delayed ~ . - 1, family = "binomial", data = train_prep1)
    ## 
    ## Coefficients:
    ##    month1    month10    month11    month12     month2     month3  
    ##  1.854974   2.230340   2.200034   1.346410   1.833642   1.958018  
    ##    month4     month5     month6     month7     month8     month9  
    ##  1.681214   2.065397   1.621735   1.557779   1.879622   2.624398  
    ## carrierAA  carrierB6  carrierDL  carrierEV  carrierMQ  carrierUA  
    ##  0.258498  -0.219876   0.301817  -0.410256  -0.304729   0.130391  
    ## carrierUS  carrierWN  originJFK  originLGA   distance     hour11  
    ##  0.046250  -0.189752   0.143669   0.034153  -0.107043  -0.009453  
    ##    hour12     hour13     hour14     hour15     hour16     hour17  
    ## -0.141190  -0.246113  -0.378547  -0.623905  -0.562192  -0.686512  
    ##    hour18     hour19     hour20     hour21     hour22     hour23  
    ## -0.757612  -0.762375  -0.690694  -0.709327  -0.650233  -0.480280  
    ##     hour5      hour6      hour7      hour8      hour9      week1  
    ##  0.391644   0.408565   0.353837   0.089500   0.055827  -0.426048  
    ##     week2      week3  
    ## -0.152805  -0.209075  
    ## 
    ## Degrees of Freedom: 221754 Total (i.e. Null);  221710 Residual
    ##   (7465 observations deleted due to missingness)
    ## Null Deviance:       307400 
    ## Residual Deviance: 267500    AIC: 267600

logLik (log likelihood) is a measure of how gap of what we predicted and what actually happened.

``` r
library(broom)
glance(glm_unbal)
```

    ##   null.deviance df.null    logLik      AIC      BIC deviance df.residual
    ## 1      307416.3  221754 -133742.1 267572.2 268025.8 267484.2      221710

Get the coefficients.

``` r
tidy(glm_unbal)
```

    ##         term     estimate   std.error   statistic       p.value
    ## 1     month1  1.854973553 0.059438986  31.2080282 8.291310e-214
    ## 2    month10  2.230339784 0.060035203  37.1505330 4.298021e-302
    ## 3    month11  2.200033589 0.060130589  36.5875940 4.504910e-293
    ## 4    month12  1.346410473 0.059560960  22.6055873 3.818627e-113
    ## 5     month2  1.833641659 0.059643629  30.7432946 1.503079e-207
    ## 6     month3  1.958018286 0.059623545  32.8396824 1.598969e-236
    ## 7     month4  1.681214486 0.059554778  28.2297163 2.525734e-175
    ## 8     month5  2.065396948 0.059795988  34.5407276 1.964057e-261
    ## 9     month6  1.621734531 0.059599953  27.2103322 4.901532e-163
    ## 10    month7  1.557779481 0.059552344  26.1581557 7.959649e-151
    ## 11    month8  1.879621591 0.059694868  31.4871556 1.302265e-217
    ## 12    month9  2.624397692 0.060753950  43.1971532  0.000000e+00
    ## 13 carrierAA  0.258498123 0.027257998   9.4833863  2.461621e-21
    ## 14 carrierB6 -0.219875683 0.023941746  -9.1837781  4.162257e-20
    ## 15 carrierDL  0.301816825 0.025319082  11.9205279  9.252044e-33
    ## 16 carrierEV -0.410255795 0.026005820 -15.7755377  4.584771e-56
    ## 17 carrierMQ -0.304728917 0.026790865 -11.3743590  5.611363e-30
    ## 18 carrierUA  0.130391237 0.027104190   4.8107409  1.503718e-06
    ## 19 carrierUS  0.046250374 0.029290638   1.5790156  1.143325e-01
    ## 20 carrierWN -0.189752419 0.033347223  -5.6902015  1.268896e-08
    ## 21 originJFK  0.143669221 0.015808188   9.0882787  1.006194e-19
    ## 22 originLGA  0.034153462 0.014282830   2.3912251  1.679226e-02
    ## 23  distance -0.107043441 0.007552081 -14.1740320  1.326599e-45
    ## 24    hour11 -0.009453025 0.031238944  -0.3026039  7.621918e-01
    ## 25    hour12 -0.141190490 0.029733627  -4.7485122  2.049186e-06
    ## 26    hour13 -0.246112937 0.029145106  -8.4444003  3.056104e-17
    ## 27    hour14 -0.378547129 0.028476621 -13.2932600  2.532971e-40
    ## 28    hour15 -0.623905467 0.027561266 -22.6370398 1.872003e-113
    ## 29    hour16 -0.562191588 0.028090653 -20.0134752  4.203008e-89
    ## 30    hour17 -0.686512190 0.027690574 -24.7922701 1.086234e-135
    ## 31    hour18 -0.757612212 0.028264339 -26.8045262 2.861609e-158
    ## 32    hour19 -0.762375346 0.028212548 -27.0225631 8.027822e-161
    ## 33    hour20 -0.690693598 0.029826774 -23.1568324 1.240680e-118
    ## 34    hour21 -0.709327201 0.032666769 -21.7140299 1.512013e-104
    ## 35    hour22 -0.650233369 0.054981104 -11.8264880  2.848107e-32
    ## 36    hour23 -0.480280159 0.079794355  -6.0189741  1.755259e-09
    ## 37     hour5  0.391643848 0.072523333   5.4002461  6.654954e-08
    ## 38     hour6  0.408565249 0.029011690  14.0827800  4.846511e-45
    ## 39     hour7  0.353837137 0.030229340  11.7050899  1.200320e-31
    ## 40     hour8  0.089499802 0.028051093   3.1905994  1.419780e-03
    ## 41     hour9  0.055826532 0.029960888   1.8633137  6.241814e-02
    ## 42     week1 -0.426047538 0.014444809 -29.4948538 3.351465e-191
    ## 43     week2 -0.152804578 0.014525982 -10.5193978  7.032161e-26
    ## 44     week3 -0.209074809 0.013449094 -15.5456430  1.703048e-54

Takes the original data and add the prediction and the accosiated error.

``` r
head(augment(glm_unbal))
```

    ##   .rownames was_delayed month carrier origin distance hour week  .fitted
    ## 1         1     Delayed     1      UA    EWR 7.244228    5    0 1.601562
    ## 2         2     Delayed     1      UA    LGA 7.255591    5    0 1.634499
    ## 3         3     Delayed     1      AA    JFK 6.993015    5    0 1.900228
    ## 4         4 Not Delayed     1      B6    JFK 7.362645    5    0 1.382288
    ## 5         5 Not Delayed     1      DL    LGA 6.635947    6    0 1.889175
    ## 6         6     Delayed     1      B6    EWR 6.970730    6    0 1.297492
    ##      .se.fit     .resid         .hat   .sigma      .cooksd .std.resid
    ## 1 0.07180654 -1.8895503 0.0007199003 1.098385 8.128203e-05 -1.8902308
    ## 2 0.07293258 -1.9040414 0.0007264693 1.098385 8.477144e-05 -1.9047334
    ## 3 0.07308455 -2.0196957 0.0006044336 1.098384 9.197712e-05 -2.0203064
    ## 4 0.07227158  0.6692475 0.0008377183 1.098391 4.786887e-06  0.6695280
    ## 5 0.02998311  0.5306635 0.0001025643 1.098391 3.525127e-07  0.5306907
    ## 6 0.03091502 -1.7544448 0.0001610799 1.098386 1.340362e-05 -1.7545861

Plot predicteds VS actuals

``` r
glm_unbal %>% 
  augment() %>% 
  ggplot(aes(x=.fitted, group=was_delayed, fill=was_delayed)) +
  geom_density(alpha =.5) +
  geom_vline(aes(xintercept=0))
```

![](README_files/figure-markdown_github/unnamed-chunk-22-1.png)

fitted=scored value In this model we classify all with logit bigger then 0 we classify as Not delayed, and under 0 we classify as Delayed. We have a lot of overlap, so we have a hard time separating the delayed from the not delayed. Best case scenario would be having the Delayed bump to the left of the intercept and the Not Delayed bump to the right of the intercept. We can also consider where we want our intercept. This effects the number of false positives vs false negatives etc. Depends on business goals.

#### Prep and predict on test data

We rename the scored column to glm\_unbal, so that we can compare it to the glm\_bal model, otherwise they will both have the column named pred and it's going to complain.

``` r
test_raw %>% 
  bake(numscleaned_fe, .) %>% 
  modelr::add_predictions(glm_unbal,var="glm_unbal") -> 
  test_scored
```

``` r
test_scored %>% 
  ggplot(aes(x=glm_unbal, group=was_delayed, fill=was_delayed)) +
  geom_density(alpha =.5) +
  geom_vline(aes(xintercept=0))
```

    ## Warning: Removed 3131 rows containing non-finite values (stat_density).

![](README_files/figure-markdown_github/unnamed-chunk-24-1.png)

But how many did we get right etc?

``` r
library(yardstick)
```

    ## 
    ## Attaching package: 'yardstick'

    ## The following object is masked from 'package:readr':
    ## 
    ##     spec

``` r
test_scored %>% 
  mutate(glm_unbal_class = as.factor(
    ifelse(glm_unbal<0, "Delayed","Not Delayed"))) %>% 
  conf_mat(was_delayed, glm_unbal_class)
```

    ##              Truth
    ## Prediction    Delayed Not Delayed
    ##   Delayed        5621        4514
    ##   Not Delayed   26031       58830

Let's calculate accuracy

``` r
test_scored %>% 
  mutate(glm_unbal_class=as.factor(
    ifelse(glm_unbal<0,"Delayed","Not Delayed"))) %>% 
  accuracy(was_delayed,glm_unbal_class)
```

    ## [1] 0.6784601
