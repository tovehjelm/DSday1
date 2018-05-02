Our R Project
================

Packages
--------

Packages we'll look at today:

-   odbc / readxl / readr / dbplyr (sql), for data access
-   tidyverse, for data manipulation
-   DataExplorer, for providing an overview of our data
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
    facet_wrap(~col, scales = "free") #use the range for the individual column, don't put them on the same scale
```

    ## Applying predicate on the first 100 rows

    ## Warning: Removed 1631 rows containing non-finite values (stat_bin2d).

    ## Warning: Computation failed in `stat_bin2d()`:
    ## 'from' must be a finite number

![](README_files/figure-markdown_github/unnamed-chunk-11-1.png)
