NFL
================
Larissa Kostiw
5 February 2020

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

    ## Parsed with column specification:
    ## cols(
    ##   team = col_character(),
    ##   team_name = col_character(),
    ##   year = col_double(),
    ##   total = col_double(),
    ##   home = col_double(),
    ##   away = col_double(),
    ##   week = col_double(),
    ##   weekly_attendance = col_double()
    ## )

    ## Parsed with column specification:
    ## cols(
    ##   team = col_character(),
    ##   team_name = col_character(),
    ##   year = col_double(),
    ##   wins = col_double(),
    ##   loss = col_double(),
    ##   points_for = col_double(),
    ##   points_against = col_double(),
    ##   points_differential = col_double(),
    ##   margin_of_victory = col_double(),
    ##   strength_of_schedule = col_double(),
    ##   simple_rating = col_double(),
    ##   offensive_ranking = col_double(),
    ##   defensive_ranking = col_double(),
    ##   playoffs = col_character(),
    ##   sb_winner = col_character()
    ## )

``` r
join <- attendance %>%
  left_join(standings, by=c("year","team_name","team"))
```

``` r
summary(join)
```

    ##      team            team_name              year          total        
    ##  Length:10846       Length:10846       Min.   :2000   Min.   : 760644  
    ##  Class :character   Class :character   1st Qu.:2005   1st Qu.:1040509  
    ##  Mode  :character   Mode  :character   Median :2010   Median :1081090  
    ##                                        Mean   :2010   Mean   :1080910  
    ##                                        3rd Qu.:2015   3rd Qu.:1123230  
    ##                                        Max.   :2019   Max.   :1322087  
    ##                                                                        
    ##       home             away             week    weekly_attendance
    ##  Min.   :202687   Min.   :450295   Min.   : 1   Min.   : 23127   
    ##  1st Qu.:504360   1st Qu.:524974   1st Qu.: 5   1st Qu.: 63246   
    ##  Median :543185   Median :541757   Median : 9   Median : 68334   
    ##  Mean   :540455   Mean   :540455   Mean   : 9   Mean   : 67557   
    ##  3rd Qu.:578342   3rd Qu.:557741   3rd Qu.:13   3rd Qu.: 72545   
    ##  Max.   :741775   Max.   :601655   Max.   :17   Max.   :105121   
    ##                                                 NA's   :638      
    ##       wins             loss          points_for    points_against 
    ##  Min.   : 0.000   Min.   : 0.000   Min.   :161.0   Min.   :165.0  
    ##  1st Qu.: 6.000   1st Qu.: 6.000   1st Qu.:299.0   1st Qu.:310.0  
    ##  Median : 8.000   Median : 8.000   Median :348.0   Median :347.0  
    ##  Mean   : 7.984   Mean   : 7.984   Mean   :350.3   Mean   :350.3  
    ##  3rd Qu.:10.000   3rd Qu.:10.000   3rd Qu.:396.0   3rd Qu.:392.0  
    ##  Max.   :16.000   Max.   :16.000   Max.   :606.0   Max.   :517.0  
    ##                                                                   
    ##  points_differential margin_of_victory    strength_of_schedule
    ##  Min.   :-261.0      Min.   :-16.300000   Min.   :-4.600000   
    ##  1st Qu.: -75.0      1st Qu.: -4.700000   1st Qu.:-1.100000   
    ##  Median :   1.5      Median :  0.100000   Median : 0.000000   
    ##  Mean   :   0.0      Mean   : -0.001881   Mean   : 0.001097   
    ##  3rd Qu.:  73.0      3rd Qu.:  4.600000   3rd Qu.: 1.200000   
    ##  Max.   : 315.0      Max.   : 19.700000   Max.   : 4.300000   
    ##                                                               
    ##  simple_rating   offensive_ranking   defensive_ranking  
    ##  Min.   :-17.4   Min.   :-1.17e+01   Min.   :-9.800000  
    ##  1st Qu.: -4.5   1st Qu.:-3.20e+00   1st Qu.:-2.400000  
    ##  Median :  0.0   Median : 0.00e+00   Median : 0.100000  
    ##  Mean   :  0.0   Mean   :-1.57e-04   Mean   :-0.001097  
    ##  3rd Qu.:  4.5   3rd Qu.: 2.70e+00   3rd Qu.: 2.500000  
    ##  Max.   : 20.1   Max.   : 1.59e+01   Max.   : 9.800000  
    ##                                                         
    ##    playoffs          sb_winner        
    ##  Length:10846       Length:10846      
    ##  Class :character   Class :character  
    ##  Mode  :character   Mode  :character  
    ##                                       
    ##                                       
    ##                                       
    ## 

``` r
attendance_year<- join %>%
  select(year, total) %>%
  distinct() %>%
  group_by(year) %>%
  summarise(total=sum(total))
```

``` r
#options(scipen=999)
attendance_year_plot <- ggplot(attendance_year, aes(x=year, (y=total/1000000))) +
  geom_line(size=2)+
  xlab("") +
  theme_light()+
  stat_smooth(method="loess") +
  labs(
    title="NFL attendances",
    subtitle="2000 - 2019",
    caption="Source: Pro Football Reference. Plot: @LarissaKostiw",
    x="Year",
    y="Attendance (m)"
  ) +
  theme(plot.title=element_text(hjust=0.5, face="bold", size=18),
        plot.subtitle=element_text(hjust=0.5, size=14),
        plot.caption = element_text(hjust=1),
        panel.grid=element_blank(),
        axis.title = element_text(face="bold"),
        axis.text=element_text(face="italic"))
  
attendance_year_plot
```

![](nfl_script_files/figure-markdown_github/Plot-1.png)
