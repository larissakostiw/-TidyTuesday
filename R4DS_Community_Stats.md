R for Data Science Online Learning Community Stats
================
Larissa Kostiw
22 July 2019

``` r
r4ds_members <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-07-16/r4ds_members.csv")
```

    ## Parsed with column specification:
    ## cols(
    ##   .default = col_double(),
    ##   date = col_date(format = "")
    ## )

    ## See spec(...) for full column specifications.

``` r
library(tidyverse)
```

    ## -- Attaching packages --------------------------------------------------------------------- tidyverse 1.2.1 --

    ## v ggplot2 3.1.0     v purrr   0.2.5
    ## v tibble  2.0.1     v dplyr   0.7.8
    ## v tidyr   0.8.2     v stringr 1.3.1
    ## v readr   1.3.1     v forcats 0.3.0

    ## -- Conflicts ------------------------------------------------------------------------ tidyverse_conflicts() --
    ## x dplyr::filter() masks stats::filter()
    ## x dplyr::lag()    masks stats::lag()

``` r
summary(r4ds_members)
```

    ##       date            total_membership  full_members        guests 
    ##  Min.   :2017-08-27   Min.   :   1.0   Min.   :   1.0   Min.   :0  
    ##  1st Qu.:2018-02-12   1st Qu.: 978.2   1st Qu.: 978.2   1st Qu.:0  
    ##  Median :2018-07-31   Median :1605.0   Median :1605.0   Median :0  
    ##  Mean   :2018-07-31   Mean   :1567.8   Mean   :1567.8   Mean   :0  
    ##  3rd Qu.:2019-01-16   3rd Qu.:2142.8   3rd Qu.:2142.8   3rd Qu.:0  
    ##  Max.   :2019-07-05   Max.   :3029.0   Max.   :3029.0   Max.   :0  
    ##  daily_active_members daily_members_posting_messages weekly_active_members
    ##  Min.   :  1.00       Min.   :  0.00                 Min.   :  1.0        
    ##  1st Qu.: 63.00       1st Qu.:  6.00                 1st Qu.:206.0        
    ##  Median : 88.00       Median : 11.00                 Median :239.0        
    ##  Mean   : 91.39       Mean   : 13.24                 Mean   :249.7        
    ##  3rd Qu.:110.00       3rd Qu.: 16.00                 3rd Qu.:307.8        
    ##  Max.   :258.00       Max.   :111.00                 Max.   :525.0        
    ##  weekly_members_posting_messages messages_in_public_channels
    ##  Min.   :  1.00                  Min.   :  0.00             
    ##  1st Qu.: 35.00                  1st Qu.:  9.25             
    ##  Median : 48.00                  Median : 19.00             
    ##  Mean   : 52.16                  Mean   : 28.46             
    ##  3rd Qu.: 59.00                  3rd Qu.: 35.00             
    ##  Max.   :278.00                  Max.   :326.00             
    ##  messages_in_private_channels messages_in_shared_channels messages_in_d_ms
    ##  Min.   : 0.000               Min.   :0                   Min.   :  0.00  
    ##  1st Qu.: 0.000               1st Qu.:0                   1st Qu.:  1.00  
    ##  Median : 0.000               Median :0                   Median :  4.00  
    ##  Mean   : 1.718               Mean   :0                   Mean   : 13.05  
    ##  3rd Qu.: 0.000               3rd Qu.:0                   3rd Qu.: 12.00  
    ##  Max.   :75.000               Max.   :0                   Max.   :227.00  
    ##  percent_of_messages_public_channels percent_of_messages_private_channels
    ##  Min.   :0.0000                      Min.   :0.0000                      
    ##  1st Qu.:0.5840                      1st Qu.:0.0000                      
    ##  Median :0.8000                      Median :0.0000                      
    ##  Mean   :0.7248                      Mean   :0.0305                      
    ##  3rd Qu.:0.9444                      3rd Qu.:0.0000                      
    ##  Max.   :1.0000                      Max.   :1.0000                      
    ##  percent_of_messages_d_ms percent_of_views_public_channels
    ##  Min.   :0.0000           Min.   :0.2726                  
    ##  1st Qu.:0.0345           1st Qu.:0.9115                  
    ##  Median :0.1595           Median :0.9519                  
    ##  Mean   :0.2270           Mean   :0.9285                  
    ##  3rd Qu.:0.3478           3rd Qu.:0.9744                  
    ##  Max.   :1.0000           Max.   :1.0000                  
    ##  percent_of_views_private_channels percent_of_views_d_ms      name  
    ##  Min.   :0.000000                  Min.   :0.00000       Min.   :0  
    ##  1st Qu.:0.000000                  1st Qu.:0.02235       1st Qu.:0  
    ##  Median :0.000000                  Median :0.04170       Median :0  
    ##  Mean   :0.009773                  Mean   :0.06176       Mean   :0  
    ##  3rd Qu.:0.006450                  3rd Qu.:0.07433       3rd Qu.:0  
    ##  Max.   :0.267400                  Max.   :0.72170       Max.   :0  
    ##  public_channels_single_workspace messages_posted
    ##  Min.   :10.0                     Min.   :   35  
    ##  1st Qu.:15.0                     1st Qu.:20543  
    ##  Median :19.0                     Median :33828  
    ##  Mean   :17.8                     Mean   :32936  
    ##  3rd Qu.:21.0                     3rd Qu.:40104  
    ##  Max.   :27.0                     Max.   :59627

``` r
line<- r4ds_members %>%
  select(date, total_membership, daily_active_members) %>%
  mutate(proportion=((daily_active_members/total_membership)*100)) %>%
  filter(total_membership>1)
```

``` r
library(ggthemes)
```

    ## Warning: package 'ggthemes' was built under R version 3.5.3

``` r
line_graph<- ggplot(line, aes(x=date, y=proportion))+
  geom_line(size=1)+
  geom_smooth()+
  theme_wsj()+
  labs(
    title="Proportion of Active users",
    source="Data=R4DS. Plot: @LarissaKostiw",
    x="Date",
    y="Active Proportion (%)",
    caption="Data: R4DS. Plot: @LarissaKostiw"
  ) +
  theme(
    plot.title=element_text(hjust=0.5, face="bold", size=15),
    plot.caption = element_text(hjust=1, size=8, face="bold"),
    axis.title.y=element_text(hjust=0.5, face="bold"),
    axis.title.x=element_text(hjust=0.5, face="bold"),
    axis.text.x=element_text(face="italic"),
    axis.text.y=element_text(face="italic")
  )
line_graph
```

    ## `geom_smooth()` using method = 'loess' and formula 'y ~ x'

![](R4DS_Community_Stats_files/figure-markdown_github/Line%20Graph-1.png)

``` r
double_line<- r4ds_members %>%
  select(date, total_membership, daily_active_members) %>%
  filter(total_membership>1)
```

``` r
double_line_graph<- ggplot(double_line, 
                           aes(x=date )) +
  geom_line(aes(y=total_membership, colour="Total"))+
  geom_line(aes(y=daily_active_members, colour="Active")) +
  theme_wsj()+
  labs(
    title="R4DS Community Users",
    subtitle = "Total and Active Members",
    x="Date",
    y="Member Count",
    colour="Members"
  ) +
  theme(plot.title=element_text(hjust=0.5, size=15),
        plot.subtitle=element_text(hjust=0.5, size=10),
        legend.position = "none",
        axis.title.y=element_text(hjust=0.5, face="bold"),
        axis.title.x=element_text(hjust=0.5, face="bold"),
        axis.text.x=element_text(face="italic"),
        axis.text.y=element_text(face="italic")) +
  geom_text(aes(x=as.Date("2019-07-05"), y=3100, label="Total"))+
  geom_text(aes(x=as.Date("2019-07-05"), y=300, label="Active"))       
double_line_graph
```

![](R4DS_Community_Stats_files/figure-markdown_github/Double%20Line-1.png)

``` r
#nstall.packages("gridExtra")
library(gridExtra)
```

    ## Warning: package 'gridExtra' was built under R version 3.5.3

    ## 
    ## Attaching package: 'gridExtra'

    ## The following object is masked from 'package:dplyr':
    ## 
    ##     combine

``` r
dev.new(width=30, height=80, unit="in")
plot_all<-grid.arrange(double_line_graph, line_graph,nrow=2)
```

    ## `geom_smooth()` using method = 'loess' and formula 'y ~ x'

``` r
ggsave("plot_all.png", plot_all,width=8,height=8)
```
