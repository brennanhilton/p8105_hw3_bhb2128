Homework\_3
================
Brennan Baker
October 9, 2018

-   [Problem 1](#problem-1)
-   [Problem 2](#problem-2)
-   [Problem 3](#problem-3)

Problem 1
=========

Load packages

``` r
library(tidyverse)
```

    ## -- Attaching packages ------------------------------------------------------------------------- tidyverse 1.2.1 --

    ## v ggplot2 3.0.0     v purrr   0.2.5
    ## v tibble  1.4.2     v dplyr   0.7.6
    ## v tidyr   0.8.1     v stringr 1.3.1
    ## v readr   1.1.1     v forcats 0.3.0

    ## -- Conflicts ---------------------------------------------------------------------------- tidyverse_conflicts() --
    ## x dplyr::filter() masks stats::filter()
    ## x dplyr::lag()    masks stats::lag()

``` r
library(p8105.datasets)
```

*Tidy the data*

``` r
brfss = brfss_smart2010 %>% 
  janitor::clean_names() %>% 
  filter(topic == "Overall Health") %>% 
  separate(locationdesc, into = c("state", "county"), sep = "- ") %>% 
  select(year, state, county, response, data_value) %>% 
  mutate(response = tolower(response)) %>% 
  mutate(response = ordered(response, levels = c("excellent", "very good", "good", "fair", "poor")))
```

I cleaned the data by: cleaning the names; keeping only the rows where the topic is overall health; separating "locationdesc" into state and county columns; and excluding unnecessary variables. I converted response to a factor varaible and arranged it with "Excellent"" on top.

*In 2002, which states were observed at 7 locations?*

``` r
brfss %>% 
  filter(year == "2002") %>% 
  distinct(state, county, .keep_all = TRUE) %>% 
  count(state) %>% 
  filter(n == "7") %>% 
  nrow()
```

    ## [1] 3

There were 3 states observed at 7 locations.

*Make a “spaghetti plot” that shows the number of locations in each state from 2002 to 2010*

``` r
brfss %>%
  distinct(state, county, .keep_all = TRUE) %>%
  group_by(year) %>% 
  count(state) %>% 
    ggplot(aes(x = year, y = n, color = state)) +
  geom_line() +
  labs(
    title = "Distinct locations in each state",
    x = "Year",
    y = "Number of locations"
  ) + 
  theme_bw()
```

![](homework_3_files/figure-markdown_github/spaghetti%20plot-1.png)

*Make a table showing, for the years 2002, 2006, and 2010, the mean and standard deviation of the proportion of “Excellent” responses across locations in NY State.*

``` r
brfss %>%
  filter(year %in% c("2002", "2006", "2010"), # filter years, state, and excellent responses
         str_detect(state, "NY"),
         response == "excellent",
         !is.na(data_value)) %>% 
  group_by(year) %>% 
  summarize(mean = mean(data_value), std_dev = sd(data_value)) %>% 
  knitr::kable(digits = 4)
```

|  year|     mean|  std\_dev|
|-----:|--------:|---------:|
|  2002|  24.0400|    4.4864|
|  2006|  22.5333|    4.0008|
|  2010|  22.7000|    3.5672|

*For each year and state, compute the average proportion in each response category (taking the average across locations in a state). Make a five-panel plot that shows, for each response category separately, the distribution of these state-level averages over time.*

``` r
brfss %>%
  group_by(year, state, response) %>%
  filter(!is.na(data_value)) %>% 
  summarize(mean = mean(data_value)) %>% 
  ggplot(aes(x = year, y = mean, color = response)) +
  geom_point() +
  facet_grid(~response) +
  labs(
    title = "Proportion of each response",
    x = "Mean proportion",
    y = "Year"
  ) + 
  theme_bw() +
  viridis::scale_color_viridis(discrete = TRUE) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
```

![](homework_3_files/figure-markdown_github/five%20panel%20plot-1.png)

Problem 2
=========

Instacart is an online grocery shopping service. This instacart dataset contains 1384617 rows and 15 columns. The dataset contains one order from each of 131209 distinct users. Each row is a distinct product from one of these orders. The data set includes more information for each product, including the department and aisle it belongs to. As an example, the table below shows the products from just one order (order\_id = 1). From the table we can see that the first product is "Bulgarian Yogurt", which belongs to the "yogurt" aisle in the "dairy eggs" department.

|  order\_id|  product\_id|  add\_to\_cart\_order|  reordered|  user\_id| eval\_set |  order\_number|  order\_dow|  order\_hour\_of\_day|  days\_since\_prior\_order| product\_name                                 |  aisle\_id|  department\_id| aisle                | department   |
|----------:|------------:|---------------------:|----------:|---------:|:----------|--------------:|-----------:|---------------------:|--------------------------:|:----------------------------------------------|----------:|---------------:|:---------------------|:-------------|
|          1|        49302|                     1|          1|    112108| train     |              4|           4|                    10|                          9| Bulgarian Yogurt                              |        120|              16| yogurt               | dairy eggs   |
|          1|        11109|                     2|          1|    112108| train     |              4|           4|                    10|                          9| Organic 4% Milk Fat Whole Milk Cottage Cheese |        108|              16| other creams cheeses | dairy eggs   |
|          1|        10246|                     3|          0|    112108| train     |              4|           4|                    10|                          9| Organic Celery Hearts                         |         83|               4| fresh vegetables     | produce      |
|          1|        49683|                     4|          0|    112108| train     |              4|           4|                    10|                          9| Cucumber Kirby                                |         83|               4| fresh vegetables     | produce      |
|          1|        43633|                     5|          1|    112108| train     |              4|           4|                    10|                          9| Lightly Smoked Sardines in Olive Oil          |         95|              15| canned meat seafood  | canned goods |
|          1|        13176|                     6|          0|    112108| train     |              4|           4|                    10|                          9| Bag of Organic Bananas                        |         24|               4| fresh fruits         | produce      |
|          1|        47209|                     7|          0|    112108| train     |              4|           4|                    10|                          9| Organic Hass Avocado                          |         24|               4| fresh fruits         | produce      |
|          1|        22035|                     8|          1|    112108| train     |              4|           4|                    10|                          9| Organic Whole String Cheese                   |         21|              16| packaged cheese      | dairy eggs   |

*How many aisles are there, and which aisles are the most items ordered from?*

``` r
instacart %>% distinct(aisle, .keep_all = TRUE) %>% nrow()
```

    ## [1] 134

There are 134 distinct aisles.

The table below shows the top 5 aisles that contain the most ordered items.

``` r
instacart %>% distinct(aisle, product_name, .keep_all = TRUE) %>%
  count(aisle) %>% 
  arrange(desc(n)) %>% 
  top_n(5) %>% 
  knitr::kable()
```

    ## Selecting by n

| aisle           |    n|
|:----------------|----:|
| candy chocolate |  943|
| yogurt          |  911|
| missing         |  905|
| ice cream ice   |  901|
| chips pretzels  |  844|

*Make a plot that shows the number of items ordered in each aisle. Order aisles sensibly, and organize your plot so others can read it.*

``` r
instacart %>% distinct(aisle, product_name, .keep_all = TRUE) %>%
  group_by(aisle, department) %>% 
  filter(!aisle %in% c("other", "missing")) %>% # removed other and missing because those aisles are un informative
  summarise(number_items = n()) %>%
  ggplot(aes(x = department, y = number_items)) +
  geom_boxplot() +
   labs(
    title = "Number of items ordered in each aisle by department",
    x = "Department",
    y = "Number of items per aisle"
  ) + 
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
```

![](homework_3_files/figure-markdown_github/items%20ordered%20in%20each%20aisle%20plot-1.png)

*Make a table showing the most popular item aisles “baking ingredients”, “dog food care”, and “packaged vegetables fruits”*

``` r
instacart %>% 
  filter(aisle %in% c("baking ingredients", "dog food care", "packaged vegetables fruits")) %>% 
  group_by(aisle, product_name) %>%
  summarize(number = n()) %>% 
  arrange(desc(number)) %>% 
  top_n(1) %>% 
  knitr::kable()
```

    ## Selecting by number

| aisle                      | product\_name                                 |  number|
|:---------------------------|:----------------------------------------------|-------:|
| packaged vegetables fruits | Organic Baby Spinach                          |    9784|
| baking ingredients         | Light Brown Sugar                             |     499|
| dog food care              | Snack Sticks Chicken & Rice Recipe Dog Treats |      30|

*Make a table showing the mean hour of the day at which Pink Lady Apples and Coffee Ice Cream are ordered on each day of the week; format this table for human readers (i.e. produce a 2 x 7 table).* tried this code in the below chunck it didnt work mutate(order\_dow = recode(order\_dow, 0 = "Sunday", 1 = "Monday", 2 = "Tuesday", 3 = "Wednesday", 4 = "Thursday", 5 = "Friday", 6 = "Saturday"))

``` r
instacart %>% 
  filter(product_name %in% c("Pink Lady Apples", "Coffee Ice Cream")) %>%
  group_by(product_name, order_dow) %>% 
  summarize(mean_hour = mean(order_hour_of_day)) %>%
  spread(key = order_dow, value = mean_hour) %>% 
  knitr::kable()
```

| product\_name    |         0|         1|         2|         3|         4|         5|         6|
|:-----------------|---------:|---------:|---------:|---------:|---------:|---------:|---------:|
| Coffee Ice Cream |  13.77419|  14.31579|  15.38095|  15.31818|  15.21739|  12.26316|  13.83333|
| Pink Lady Apples |  13.44118|  11.36000|  11.70213|  14.25000|  11.55172|  12.78431|  11.93750|

Problem 3
=========

The National Oceanic and Atmospheric Association (NOAA) provides weather data. This dataset contains 2595176 rows and 7 columns. The data are for five variables measured from all weahter stations daily in New York state from January 1, 1981 through December 31, 2010. The variables are precipiation, snowfall, snow depth, maximum temperature, and minimum temperature. There is a lot of missing data, which is an issue for calculating descriptive statistics in r. Thus, missing values will often need to be filtered out.

*Do some data cleaning. Create separate variables for year, month, and day. Ensure observations for temperature, precipitation, and snowfall are given in reasonable units. For snowfall, what are the most commonly observed values? Why?*

First I need to load this package for working with dates.

``` r
library(lubridate) # need this package below for separating the date into multiple columns
```

    ## 
    ## Attaching package: 'lubridate'

    ## The following object is masked from 'package:base':
    ## 
    ##     date

This code tidys the data by making new columns for year, month, and day, and converting any variable expressed in tenths of a unit to whole units.

``` r
noaa_data = ny_noaa %>%
  mutate_at(vars(date), funs(year, month, day)) %>% # need the lubridate package for this line
  mutate(prcp = prcp/10, tmax = as.numeric(tmax)/10, tmin = as.numeric(tmin)/10) # convert from tenths of mm and tench of degrees to whole units
```

*For snowfall, what are the most commonly observed values? Why?*

``` r
noaa_data %>% 
  count(snow) %>% 
  arrange(desc(n))
```

    ## # A tibble: 282 x 2
    ##     snow       n
    ##    <int>   <int>
    ##  1     0 2008508
    ##  2    NA  381221
    ##  3    25   31022
    ##  4    13   23095
    ##  5    51   18274
    ##  6    76   10173
    ##  7     8    9962
    ##  8     5    9748
    ##  9    38    9197
    ## 10     3    8790
    ## # ... with 272 more rows

The most commonly observed value is 0 because on most days it does not snow.

*Make a two-panel plot showing the average max temperature in January and in July in each station across years. Is there any observable / interpretable structure? Any outliers?*

``` r
noaa_data %>% 
  filter(month %in% c(1, 7)) %>% # keeps only data for months of Jan and July
  mutate(month = as.factor(month)) %>% # needed to convert to factor for the next line to work
  mutate(month = recode(month, "1" = "January", "7" = "July")) %>%
  group_by(month, id, year) %>% 
  filter(!is.na(tmax)) %>%
  summarize(mean_tmax = mean(tmax)) %>%
  ggplot(aes(x = year, y = mean_tmax, color = id)) +
  geom_line() +
  facet_grid(~month) +
  labs(
    title = "Maximum temperature plot",
    x = "Year",
    y = "Mean maximum temperature"
  ) + 
  theme_bw() +
  theme(legend.position = "none") +
  theme(panel.spacing = unit(2, "lines")) # adds space between faceted plots
```

![](homework_3_files/figure-markdown_github/jan%20and%20july%20temp%20plot-1.png)

*Make a two-panel plot showing (i) tmax vs tmin for the full dataset (note that a scatterplot may not be the best option); and (ii) make a plot showing the distribution of snowfall values greater than 0 and less than 100 separately by year.*

``` r
library(hexbin) # geom_hex did not work until I loaded this package
library(patchwork) # this package lets me make the two panel plot with the last line of code in this chunk
tmax_tmin = noaa_data %>% 
  filter(!is.na(tmax),
         !is.na(tmin)) %>%
  ggplot(aes(x = tmin, y = tmax)) +
  geom_hex() +
  labs(
    title = "Daily temperatures at all NY state weather stations",
    x = "Minimum daily temperature (C)",
    y = "Maxiumum daily temperature (C)"
  ) + 
  theme_bw() +
  guides(fill=guide_legend(title="Count from\nJanuary 1, 1981\nthrough\nDecember 31, 2010"))
 

snow_plot = noaa_data %>% 
  filter(snow > 0,
         snow < 100) %>% 
  mutate(year = as.factor(year)) %>% 
  ggplot(aes(x = year, y = snow)) +
  geom_boxplot() +
  labs(
    title = "Snowfall distribution by year",
    x = "Year",
    y = "Snowfall (mm)"
  ) + 
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

tmax_tmin / snow_plot
```

![](homework_3_files/figure-markdown_github/temp%20and%20snow%20plots%20plots-1.png)
