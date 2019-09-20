National Park Visits
================
Larissa Kostiw
19 September 2019

``` r
park_visits <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-09-17/national_parks.csv")
```

    ## Parsed with column specification:
    ## cols(
    ##   year = col_character(),
    ##   gnis_id = col_character(),
    ##   geometry = col_character(),
    ##   metadata = col_character(),
    ##   number_of_records = col_double(),
    ##   parkname = col_character(),
    ##   region = col_character(),
    ##   state = col_character(),
    ##   unit_code = col_character(),
    ##   unit_name = col_character(),
    ##   unit_type = col_character(),
    ##   visitors = col_double()
    ## )

``` r
state_pop <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-09-17/state_pop.csv")
```

    ## Parsed with column specification:
    ## cols(
    ##   year = col_double(),
    ##   state = col_character(),
    ##   pop = col_double()
    ## )

``` r
gas_price <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-09-17/gas_price.csv")
```

    ## Parsed with column specification:
    ## cols(
    ##   year = col_double(),
    ##   gas_current = col_double(),
    ##   gas_constant = col_double()
    ## )

``` r
library(tidyverse)
```

    ## -- Attaching packages ------------------------------------------------------------------------ tidyverse 1.2.1 --

    ## v ggplot2 3.2.0     v purrr   0.3.2
    ## v tibble  2.1.3     v dplyr   0.8.3
    ## v tidyr   0.8.3     v stringr 1.4.0
    ## v readr   1.3.1     v forcats 0.4.0

    ## -- Conflicts --------------------------------------------------------------------------- tidyverse_conflicts() --
    ## x dplyr::filter() masks stats::filter()
    ## x dplyr::lag()    masks stats::lag()

``` r
#gnis_id: id for shapefil and long-lat lookup
```

``` r
# state_visits<- park_visits %>%
#   select(unit_name, visitors) %>%
#   group_by(unit_name) %>%
#   mutate(total=sum(visitors)) %>%
#   ungroup() %>%
#   arrange(-total) %>%
#   select(-visitors) %>%
#   unique() %>%
#   top_n(5) 


# state_visits_1<-park_visits%>% 
#   filter(unit_name %in% c("Blue Ridge Parkway","Golden Gate National Recreation Area", "Great Smoky Mountains National Park", "Natchez Trace Parkway", "Lake Mead National Recreation Area")) %>%
#            select(year,  visitors) %>%
#   filter(year !="Total") %>%
#   mutate(year=as.numeric(year))  %>%
#   group_by(year) %>%
#   mutate(total=sum(visitors)) %>%
#   select(-visitors) %>%
#   arrange(year) %>%
#   unique()
```

``` r
library(fivethirtyeight)
library(ggthemes)
national_parks <- park_visits %>%
  filter(unit_type=="National Park" & year!="Total") %>%
  mutate(year=as.numeric(year)) %>%
  select(year, visitors) %>%
  group_by(year) %>%
  mutate(total=sum(visitors)) %>%
  select(-visitors) %>%
  unique()

national_parks_plot<- ggplot(national_parks, aes(x=year, y=total/1000000)) +
  geom_line(type = "dashed")+
  geom_area(fill = "Dark Green", alpha=0.75) +
  theme_fivethirtyeight()+
  labs (title = "U.S. National Parks have never been so popular",
        subtitle="Annual recreational visits to national parks since 1904",
        x= "Year",
        y="Millions",
        caption= "Source: data.world | Plot: @LarissaKostiw") +
  theme(plot.title = element_text(hjust=0.5, face="bold"),
        plot.subtitle = element_text(hjust=0.5)) +
   scale_y_continuous(limits = c(0,90),
                     labels = c(0, 20, 40, 60, "80 m"),
                     breaks = c(0, 20, 40, 60, 80))
```

    ## Warning: Ignoring unknown parameters: type

``` r
national_parks_plot
```

![](national_parks_files/figure-markdown_github/National%20Parks-1.png)

``` r
ggsave("national_parks.png")
```

    ## Saving 7 x 5 in image

``` r
total_year <- park_visits %>%
  select(year, visitors) %>%
  group_by(year) %>%
  summarise(total=sum(visitors)) %>%
  arrange(year) %>%
  filter(year !="Total") %>%
  mutate(year=as.numeric(year))
```
