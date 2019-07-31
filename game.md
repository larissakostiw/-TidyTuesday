Video Games
================
Larissa Kostiw
30 July 2019

``` r
url <- "https://raw.githubusercontent.com/lizawood/apps-and-games/master/PC_Games/PCgames_2004_2018_raw.csv"
library(tidyverse)
```

    ## -- Attaching packages ------------------------------------------------------ tidyverse 1.2.1 --

    ## v ggplot2 3.2.0     v purrr   0.3.2
    ## v tibble  2.1.3     v dplyr   0.8.3
    ## v tidyr   0.8.3     v stringr 1.4.0
    ## v readr   1.3.1     v forcats 0.4.0

    ## -- Conflicts --------------------------------------------------------- tidyverse_conflicts() --
    ## x dplyr::filter() masks stats::filter()
    ## x dplyr::lag()    masks stats::lag()

``` r
# read in raw data
raw_df <- url %>% 
  read_csv() %>% 
  janitor::clean_names() 
```

    ## Parsed with column specification:
    ## cols(
    ##   `#` = col_double(),
    ##   Game = col_character(),
    ##   `Release date` = col_character(),
    ##   Price = col_character(),
    ##   `Score rank(Userscore / Metascore)` = col_character(),
    ##   Owners = col_character(),
    ##   `Playtime (Median)` = col_character(),
    ##   `Developer(s)` = col_character(),
    ##   `Publisher(s)` = col_character()
    ## )

``` r
#install.packages("plotly")
library(plotly)
```

    ## 
    ## Attaching package: 'plotly'

    ## The following object is masked from 'package:ggplot2':
    ## 
    ##     last_plot

    ## The following object is masked from 'package:stats':
    ## 
    ##     filter

    ## The following object is masked from 'package:graphics':
    ## 
    ##     layout

``` r
#Sys.setenv("plotly_username"="larissa.kostiw")
#Sys.setenv("plotly_api_key"="Id00VSW0JgRiXrYaGADL")
```

``` r
clean_df <- raw_df %>% 
  mutate(price = as.numeric(price),
         score_rank = word(score_rank_userscore_metascore, 1),
         average_playtime = word(playtime_median, 1),
         median_playtime = word(playtime_median, 2),
         median_playtime = str_remove(median_playtime, "\\("),
         median_playtime = str_remove(median_playtime, "\\)"),
         average_playtime = 60 * as.numeric(str_sub(average_playtime, 1, 2)) +
           as.numeric(str_sub(average_playtime, 4, 5)),
         median_playtime = 60 * as.numeric(str_sub(median_playtime, 1, 2)) +
           as.numeric(str_sub(median_playtime, 4, 5)),
         metascore = as.double(str_sub(score_rank_userscore_metascore, start = -4, end = -3))) %>% 
  select(-score_rank_userscore_metascore, -score_rank, -playtime_median) %>% 
  rename(publisher = publisher_s, developer = developer_s)
```

    ## Warning: NAs introduced by coercion

    ## Warning: NAs introduced by coercion

    ## Warning: NAs introduced by coercion

    ## Warning: NAs introduced by coercion

``` r
#Remove NAs
#clean_df<-na.omit(clean_df)
game<- clean_df %>%
  drop_na(release_date, metascore)

#Extract year
game$date<- format(as.Date(game$release_date, "%b %d, %Y"), "%d %B %Y")
game$date<- as.Date(game$date, "%d %B %Y")

library(lubridate)
```

    ## 
    ## Attaching package: 'lubridate'

    ## The following object is masked from 'package:base':
    ## 
    ##     date

``` r
game$year<- lubridate::year(game$date)
#game$year<- as.factor(game$year)

#game<- game %>%
#  drop_na(year)
```

``` r
game$developer<- as.factor(game$developer)
summary(game)
```

    ##      number           game           release_date           price      
    ##  Min.   :   1.0   Length:2850        Length:2850        Min.   : 0.49  
    ##  1st Qu.:  51.0   Class :character   Class :character   1st Qu.: 9.99  
    ##  Median : 116.0   Mode  :character   Mode  :character   Median :14.99  
    ##  Mean   : 160.2                                         Mean   :15.90  
    ##  3rd Qu.: 200.0                                         3rd Qu.:19.99  
    ##  Max.   :8251.0                                         Max.   :59.99  
    ##                                                         NA's   :159    
    ##     owners                               developer     publisher        
    ##  Length:2850        Telltale Games            :  20   Length:2850       
    ##  Class :character   Ubisoft Montreal          :  20   Class :character  
    ##  Mode  :character   Daedalic Entertainment    :  16   Mode  :character  
    ##                     Nihon Falcom              :  14                     
    ##                     Paradox Development Studio:  14                     
    ##                     (Other)                   :2755                     
    ##                     NA's                      :  11                     
    ##  average_playtime  median_playtime     metascore         date           
    ##  Min.   :   0.00   Min.   :   0.00   Min.   :20.0   Min.   :2004-01-14  
    ##  1st Qu.:   0.00   1st Qu.:   0.00   1st Qu.:66.0   1st Qu.:2011-10-11  
    ##  Median :   0.00   Median :   0.00   Median :73.0   Median :2014-11-18  
    ##  Mean   :  46.61   Mean   :  25.61   Mean   :71.9   Mean   :2014-02-19  
    ##  3rd Qu.:   0.00   3rd Qu.:   0.00   3rd Qu.:80.0   3rd Qu.:2016-11-18  
    ##  Max.   :5450.00   Max.   :3293.00   Max.   :98.0   Max.   :2018-12-19  
    ##  NA's   :2         NA's   :5                        NA's   :9           
    ##       year     
    ##  Min.   :2004  
    ##  1st Qu.:2011  
    ##  Median :2014  
    ##  Mean   :2014  
    ##  3rd Qu.:2016  
    ##  Max.   :2018  
    ##  NA's   :9

``` r
library(ggthemes)
library(ggridges)
```

    ## 
    ## Attaching package: 'ggridges'

    ## The following object is masked from 'package:ggplot2':
    ## 
    ##     scale_discrete_manual

``` r
library(viridis)
```

    ## Loading required package: viridisLite

``` r
boxplot<- game %>%
  ggplot() +
  geom_boxplot(aes(year,metascore,group=year,fill=year),color="pink",show.legend = F)+
  #ggplot(game, aes(year, metascore))+
  #geom_violin(aes(fill=factor(year)))+
  scale_x_continuous(breaks = seq(2004,2018,1))+
  scale_y_continuous(breaks = seq(0,100,10))+
  #scale_fill_viridis_c(option = "plasma")+
  scale_fill_viridis_c(option = "plasma")+
  coord_flip()+
  theme_ridges()+
  labs(title = "Are games getting better by year?",
       x="Year", 
       y = "Metascore",
       caption = "Source: Steam Spy. Plot: @LarissaKostiw")+
  theme(panel.background = element_rect(fill = "#1D2024"),
        plot.background = element_rect(fill = "#1D2024"),
        legend.background = element_rect(fill = "#1D2024"),
        text = element_text(colour = "white"),
        axis.text = element_text(colour = "white"),
        panel.grid = element_blank(),
        strip.background = element_rect(fill = "#1D2024"),
        strip.text = element_text(colour = "white"),
        axis.title.x =  element_text(hjust=0.5),
        axis.title.y =  element_text(hjust=0.5),
        plot.caption = element_text(hjust=1, size=8),
        plot.title = element_text(hjust=0.5, size=14, face="bold"))

boxplot
```

    ## Warning: Removed 9 rows containing missing values (stat_boxplot).

![](game_files/figure-markdown_github/Boxplot-1.png)

``` r
library(ggthemes)
library(ggridges)
# 
# density_data<- game %>%
#   summarise(count=n()) %>%
#   group_by(year, metascore) %>%
#   select( year, count )

game$year2<- as.factor(game$year)

density<- 
  ggplot(game, aes(x=metascore, y=year2, group=year2)) +
  #scale_x_continuous(breaks = seq(2004,2018,1))+
  #scale_y_continuous(breaks = seq(0,100,10))+
  #scale_fill_viridis_c(option = "B")+
  #coord_flip()+
  theme_ridges()+
  #geom_density_ridges(scale=5, size=0.25,rel_min_height = 0.01)+
  geom_density_ridges_gradient(scale = 3, rel_min_height = 0.01, gradient_lwd = 1.) +
  scale_x_continuous(limits=c(20, 100), expand = c(0.01, 0)) +
  scale_y_discrete(expand = c(0.01, 0)) +
  labs(title = "Metascores over the years",
       x="Metascore", 
       y = "Year",
       caption = "Source: Steam Spy. Plot: @LarissaKostiw")+
  theme(panel.background = element_rect(fill = "#000000"),
        plot.background = element_rect(fill = "#000000"),
        legend.background = element_rect(fill = "#000000"),
        text = element_text(colour = "white"),
        axis.text = element_text(colour = "white"),
        panel.grid = element_blank(),
        strip.background = element_rect(fill = "#000000"),
        strip.text = element_text(colour = "white"),
        axis.title.x =  element_text(hjust=0.5),
        axis.title.y =  element_text(hjust=0.5),
        plot.caption = element_text(hjust=1, size=8),
        plot.title = element_text(hjust=0.5, size=14, face="bold"))

density
```

    ## Picking joint bandwidth of 3.65

![](game_files/figure-markdown_github/density%20ggridges-1.png)

``` r
#ggsave("games.png")
```

``` r
library(ggthemes)
library(ggridges)
library(viridis)

jitter_data<- game %>%
  group_by(year, metascore) %>%
  summarise(count=n()) %>%
  select (year, metascore, count)

jitter_data$count<- as.factor(jitter_data$count)

#Remove na from year
jitter_data<- jitter_data %>%
  drop_na(year)

jitter<-   ggplot(jitter_data, aes(x=year, y=metascore, colour=year)) + #Case sensitive variable names
  geom_jitter(alpha=0.5, size=5, show.legend = FALSE)+
  scale_y_continuous(breaks = seq(0,100,10))+
  scale_x_continuous(breaks = seq(2004,2018,1),
                     expand=c(0.1, 0.1))+
  #scale_colour_viridis_c(option = "plasma")+
  #scale_fill_brewer(palette="Set3")+
  scale_colour_gradientn(colours = rainbow(14))+
  coord_flip()+
  theme_ridges()+
  labs(title = "Metascores of games",
       subtitle ="2004 - 2018",
       x="Year", 
       y = "Metascore",
       caption = "Source: Steam Spy. Plot: @LarissaKostiw")+
  theme(plot.title = element_text(hjust=0.5, size=14, face="bold"),
        plot.subtitle = element_text(hjust=0.5, size=12, face="italic"),
        panel.background = element_rect(fill = "#06172C"),
        plot.background = element_rect(fill = "#06172C"),
        legend.background = element_rect(fill = "#06172C"),
        text = element_text(colour = "white"),
        axis.text = element_text(colour = "white", face="italic"),
        panel.grid = element_blank(),
        #panel.grid.major.y = element_line(colour="#f1d7f4"),
        #panel.grid.major.x = element_blank(),
        strip.background = element_rect(fill = "#06172C"),
        strip.text = element_text(colour = "white"),
        axis.title.x =  element_text(hjust=0.5, face="bold", size=12),
        axis.title.y =  element_text(hjust=0.5, face="bold", size=12),
        plot.caption = element_text(hjust=1, size=8)
        )

jitter
```

![](game_files/figure-markdown_github/Jitter-1.png)

``` r
ggsave("jitter.png")
```

    ## Saving 7 x 5 in image

``` r
library(ggthemes)
library(ggridges)
library(viridis)

# jitter_data<- game %>%
#   group_by(year, metascore) %>%
#   summarise(count=n()) %>%
#   select (year, metascore, count)

#jitter_data$year<- as.factor(jitter_data$year)
#jitter_data$metascore<- as.factor(jitter_data$metascore)
#jitter_data$count<- as.factor(jitter_data$count)

jitter<- 
  ggplot(game, aes(x=metascore, y=year, colour=metascore)) + #Case sensitive variable names
  geom_jitter(alpha=0.5, size=4, show.legend = FALSE)+
  scale_x_continuous(breaks = seq(0,100,10))+
  scale_y_continuous(breaks = seq(2004,2018,1))+
  #scale_fill_viridis_c(option = "plasma")+
  scale_fill_viridis_c(option = "plasma")+
  #scale_fill_brewer(palette = 'RdBu')+
  #coord_flip()+
  theme_ridges()+
  labs(title = "Are games getting better by year?",
       x="Metascore", 
       y = "Year",
       caption = "Source: Steam Spy. Plot: @LarissaKostiw")+
  theme(panel.background = element_rect(fill = "#1D2024"),
        plot.background = element_rect(fill = "#1D2024"),
        legend.background = element_rect(fill = "#1D2024"),
        text = element_text(colour = "white"),
        axis.text = element_text(colour = "white"),
        panel.grid = element_blank(),
        strip.background = element_rect(fill = "#1D2024"),
        strip.text = element_text(colour = "white"),
        axis.title.x =  element_text(hjust=0.5),
        axis.title.y =  element_text(hjust=0.5),
        plot.caption = element_text(hjust=1, size=8),
        plot.title = element_text(hjust=0.5, size=14, face="bold"))

jitter
```

    ## Warning: Removed 9 rows containing missing values (geom_point).

![](game_files/figure-markdown_github/jitter%20v2-1.png)
