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
  #step_corr(all_numeric()) %>% 
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
  step_rm(tailnum) %>% #1.1
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
    ## Training data contained 235743 data points and 6595 incomplete rows. 
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
    ## Variables removed tailnum [trained]
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
