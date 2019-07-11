Women's World Cup Results
================
Larissa Kostiw
10 July 2019

``` r
library(tidyverse)
```

    ## -- Attaching packages --------------------------------------------------------------------------------- tidyverse 1.2.1 --

    ## v ggplot2 3.1.0     v purrr   0.2.5
    ## v tibble  2.0.1     v dplyr   0.7.8
    ## v tidyr   0.8.2     v stringr 1.3.1
    ## v readr   1.3.1     v forcats 0.3.0

    ## -- Conflicts ------------------------------------------------------------------------------------ tidyverse_conflicts() --
    ## x dplyr::filter() masks stats::filter()
    ## x dplyr::lag()    masks stats::lag()

``` r
wwc_outcomes <- wwc_outcomes %>%
  left_join(codes, by=c('team'='team'))
```

``` r
summary(wwc_outcomes)
```

    ##       year          team               score           round          
    ##  Min.   :1991   Length:568         Min.   : 0.000   Length:568        
    ##  1st Qu.:1999   Class :character   1st Qu.: 0.000   Class :character  
    ##  Median :2007   Mode  :character   Median : 1.000   Mode  :character  
    ##  Mean   :2007                      Mean   : 1.614                     
    ##  3rd Qu.:2015                      3rd Qu.: 2.000                     
    ##  Max.   :2019                      Max.   :13.000                     
    ##  yearly_game_id     team_num    win_status          country         
    ##  Min.   : 1.00   Min.   :1.0   Length:568         Length:568        
    ##  1st Qu.: 9.00   1st Qu.:1.0   Class :character   Class :character  
    ##  Median :18.00   Median :1.5   Mode  :character   Mode  :character  
    ##  Mean   :19.61   Mean   :1.5                                        
    ##  3rd Qu.:27.00   3rd Qu.:2.0                                        
    ##  Max.   :52.00   Max.   :2.0

``` r
# library(ggimage)
# image<-"football.png"
# 
# ggplot(country_score2, aes(goals_per_game, country), colour="grey") +
#     theme_light()+
#   geom_segment(aes(x=0, y=country, xend=goals_per_game, yend=country),
#                colour="darkgreen",
#                linetype="dotted",
#                size=1)+
#   geom_point() +
#   labs(
#     title="Countries that have played in all WWC games : Goals per game ",
#     subtitle = "Winning games only",
#     source="Data: data.world Plot: @LarissaKostiw",
#     x="Goals per game (1 d.p)",
#     y="Country"
#   ) +
#    #scale_x_discrete(expand=c(0,0))+
#   #scale_x_discrete(limits=country_score2$goals_per_game)+
#   geom_image(aes(image=image))+
#   theme_economist()+
#   scale_fill_economist()+
#   theme(plot.title=element_text(hjust=0.5, face="bold"),
#         plot.subtitle = element_text(hjust=0.5, face="italic"),
#         axis.title = element_text(face="bold", size=10),
#         panel.grid.major = element_blank(),
#         panel.grid.minor = element_blank(),
#         panel.border = element_blank(),
#         axis.line.y=element_blank(),
#         axis.text.x=element_text()) +
#   geom_vline(xintercept = 0)
#  
```

``` r
# ggplot(country_score2, aes(team, goals_per_game)) +
#   scale_x_discrete(limits=unique(rev(country_score2$goals_per_game)))+
#   coord_flip()+
#   geom_segment(aes(x=0, y=team, xend=goals_per_game, yend=team), colour="gray50")+
#   geom_point()
```

``` r
country_score<- wwc_outcomes %>%
  filter(round %in% c("Final", "Group", "Quarter Final", "Semi Final") )%>%
  group_by(team, year) %>%
  summarise(score2=sum(score),
            games= n_distinct(yearly_game_id))%>%
  select( team, score2, games, year) 


country_score3<- country_score %>%
  group_by(team) %>%
  summarise(total_score=sum(score2),
            year_count=n_distinct(year)) %>%
  select(team, total_score, year_count) %>%
  arrange(desc(year_count)) %>%
  left_join(codes, by=c('team'='team')) %>%
  mutate(team=fct_reorder(country,total_score)) %>%
  top_n(1, year_count)


all_games<- wwc_outcomes %>%
  left_join(country_score3, by=c('country'='country')) %>%
  filter(year_count==8) %>%
  group_by(country, year) %>%
  summarise(goals_year=sum(score)) %>%
  mutate(cumulative=cumsum(goals_year))%>%
  select(country, year, goals_year, cumulative) %>%
  arrange(country, year) 
```

``` r
library(ggthemes)
```

    ## Warning: package 'ggthemes' was built under R version 3.5.3

``` r
ggplot(all_games,
       aes(x=year,
           y=cumulative))+
  geom_line(aes(colour=country), size=1) +
  labs(
    title="Countries that have played in all WWC games ",
    subtitle = "Played in all games",
    source="Data: data.world Plot: @LarissaKostiw",
    x="Year",
    y="Cumulative Goals Scored",
    colour="Country"
  ) +
  theme_economist()+
  theme(plot.title=element_text(hjust=0.5, face="bold"),
        plot.subtitle = element_text(hjust=0.5, face="italic", size=15),
        axis.title = element_text(face="bold", size=12),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        axis.line.y=element_blank(),
        axis.text.x=element_text(),
        legend.position="right",
        legend.title.align = 0.5,
        legend.box.background = element_rect(colour="Black"),
        legend.text = element_text(size=8))
```

![](WWC_files/figure-markdown_github/unnamed-chunk-2-1.png)
