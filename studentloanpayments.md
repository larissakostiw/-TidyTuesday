Student Loan Payments
================
Larissa Kostiw
26 November 2019

``` r
library(tidyverse)
```

    ## -- Attaching packages -------------------------------------------------------------------------------- tidyverse 1.2.1 --

    ## v ggplot2 3.2.0     v purrr   0.3.3
    ## v tibble  2.1.3     v dplyr   0.8.3
    ## v tidyr   0.8.3     v stringr 1.4.0
    ## v readr   1.3.1     v forcats 0.4.0

    ## -- Conflicts ----------------------------------------------------------------------------------- tidyverse_conflicts() --
    ## x dplyr::filter() masks stats::filter()
    ## x dplyr::lag()    masks stats::lag()

``` r
loans <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-11-26/loans.csv")
```

    ## Parsed with column specification:
    ## cols(
    ##   agency_name = col_character(),
    ##   year = col_double(),
    ##   quarter = col_double(),
    ##   starting = col_double(),
    ##   added = col_double(),
    ##   consolidation = col_double(),
    ##   rehabilitation = col_double(),
    ##   voluntary_payments = col_double(),
    ##   wage_garnishments = col_double(),
    ##   total = col_double()
    ## )

``` r
#install.packages("rlang")
library(devtools)
```

    ## Loading required package: usethis

``` r
devtools::install_github("thebioengineer/tidytuesdayR")
```

    ## Skipping install of 'tidytuesdayR' from a github remote, the SHA1 (d7ac053b) has not changed since last install.
    ##   Use `force = TRUE` to force installation

``` r
tuesdata <- tidytuesdayR::tt_load("2019-11-26")
tuesdata <- tidytuesdayR::tt_load(2019, week = 48)

loans <- tuesdata$loans
```

``` r
library(zoo)
```

    ## 
    ## Attaching package: 'zoo'

    ## The following objects are masked from 'package:base':
    ## 
    ##     as.Date, as.Date.numeric

``` r
total_aggregated<- loans %>%
  group_by(quarter, year) %>%
  summarise(total2=sum(total))
```

``` r
options(scipen=999)
data<- loans %>% 
  mutate(quarter_year = paste("Q", loans$quarter,"/", loans$year, sep = "")) %>% 
  mutate(quarter_year_date=as.Date(as.yearqtr(quarter_year, format = "Q%q/%y"))) %>%
  select(-c(agency_name,
            year,
            quarter,
            starting,
            added,
            total)) %>%
  group_by(quarter_year, quarter_year_date) %>%
  replace(., is.na(.), 0) %>%
  summarise_at(
    .vars= vars( consolidation, rehabilitation, voluntary_payments, wage_garnishments), 
    .funs =  sum) %>%
  arrange(quarter_year_date)

#Go from wide to long format
data_long<- gather(data, method, total, consolidation:wage_garnishments) %>%
  mutate_if(is.character, 
            str_replace_all, pattern = "voluntary_payments", replacement="Voluntary Payments") %>%
    mutate_if(is.character, 
            str_replace_all, pattern = "wage_garnishments", replacement="Wage Garnishments") %>%
    mutate_if(is.character, 
            str_replace_all, pattern = "consolidation", replacement="Consolidation") %>%  
    mutate_if(is.character, 
            str_replace_all, pattern = "rehabilitation", replacement="Rehabilitation") 
```

    ## `mutate_if()` ignored the following grouping variables:
    ## Column `quarter_year`
    ## `mutate_if()` ignored the following grouping variables:
    ## Column `quarter_year`
    ## `mutate_if()` ignored the following grouping variables:
    ## Column `quarter_year`
    ## `mutate_if()` ignored the following grouping variables:
    ## Column `quarter_year`

``` r
library(ggthemes)
library(viridis)
```

    ## Loading required package: viridisLite

``` r
plot<- ggplot(data_long, aes(x=quarter_year_date, 
                             y=signif((total/1000000000),digits=1), #100 million 
                             fill=method))+
  geom_area() +
  theme_economist() +
  scale_fill_viridis(discrete = T)+
  #scale_fill_viridis(discrete = T, option="plasma")+
  scale_y_continuous(breaks=seq(0,8,1)) +
  #scale_y_continuous(labels=c("0"="0","10"="1","20"="2", "30"="3", "40"="4","50"="5","60"="6"))+
  #scale_y_discrete(breaks=c("0","10","20"),
  #                 labels=c("0","1","2"))+
  #scale_y_continuous(labels=c("0"="0","20"="2",  "40"="4"))+
  labs(
    title="Student Loan Payments",
    subtitle="By Quarter : 2015-2018",
    x="Year", 
    y="USD (billion)",
    caption="Plot: @LarissaKostiw. Data: Dep. of Education",
    fill = "Payment Method") +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.title=element_text(hjust=0.5, face="bold"),
        plot.subtitle = element_text(hjust=0.5, face="bold"),
        plot.caption = element_text(hjust=1),
        legend.text=element_text(size=9),
        legend.title=element_text(face="bold"),
        axis.text = element_text(face="italic"),
        axis.title = element_text(face="bold", size=12))
  

plot
```

![](studentloanpayments_files/figure-markdown_github/unnamed-chunk-1-1.png)

``` r
ggsave("Payments.png",
       plot = plot,
       height = 5,
       width = 7)
```
