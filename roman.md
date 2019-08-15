Roman Emperors
================
Larissa Kostiw
14 August 2019

``` r
library(tidyverse)
```

    ## -- Attaching packages --------------------------------------------------------- tidyverse 1.2.1 --

    ## v ggplot2 3.2.0     v purrr   0.3.2
    ## v tibble  2.1.3     v dplyr   0.8.3
    ## v tidyr   0.8.3     v stringr 1.4.0
    ## v readr   1.3.1     v forcats 0.4.0

    ## -- Conflicts ------------------------------------------------------------ tidyverse_conflicts() --
    ## x dplyr::filter() masks stats::filter()
    ## x dplyr::lag()    masks stats::lag()

``` r
library(ggthemes)
library(RColorBrewer)
emperors <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-08-13/emperors.csv")
```

    ## Parsed with column specification:
    ## cols(
    ##   index = col_double(),
    ##   name = col_character(),
    ##   name_full = col_character(),
    ##   birth = col_date(format = ""),
    ##   death = col_date(format = ""),
    ##   birth_cty = col_character(),
    ##   birth_prv = col_character(),
    ##   rise = col_character(),
    ##   reign_start = col_date(format = ""),
    ##   reign_end = col_date(format = ""),
    ##   cause = col_character(),
    ##   killer = col_character(),
    ##   dynasty = col_character(),
    ##   era = col_character(),
    ##   notes = col_character(),
    ##   verif_who = col_character()
    ## )

``` r
summary(emperors)
```

    ##      index           name            name_full        
    ##  Min.   : 1.00   Length:68          Length:68         
    ##  1st Qu.:17.75   Class :character   Class :character  
    ##  Median :34.50   Mode  :character   Mode  :character  
    ##  Mean   :34.50                                        
    ##  3rd Qu.:51.25                                        
    ##  Max.   :68.00                                        
    ##                                                       
    ##      birth                death             birth_cty        
    ##  Min.   :0002-12-24   Min.   :0014-08-19   Length:68         
    ##  1st Qu.:0123-12-13   1st Qu.:0189-10-20   Class :character  
    ##  Median :0201-01-01   Median :0251-08-08   Mode  :character  
    ##  Mean   :0184-07-15   Mean   :0236-06-01                     
    ##  3rd Qu.:0250-01-01   3rd Qu.:0310-09-25                     
    ##  Max.   :0371-01-01   Max.   :0395-01-17                     
    ##  NA's   :5                                                   
    ##   birth_prv             rise            reign_start        
    ##  Length:68          Length:68          Min.   :0014-09-18  
    ##  Class :character   Class :character   1st Qu.:0173-01-17  
    ##  Mode  :character   Mode  :character   Median :0250-08-08  
    ##                                        Mean   :0228-06-24  
    ##                                        3rd Qu.:0305-05-01  
    ##                                        Max.   :0379-01-01  
    ##                                                            
    ##    reign_end             cause              killer         
    ##  Min.   :0014-08-19   Length:68          Length:68         
    ##  1st Qu.:0189-10-20   Class :character   Class :character  
    ##  Median :0251-08-08   Mode  :character   Mode  :character  
    ##  Mean   :0236-02-08                                        
    ##  3rd Qu.:0306-11-06                                        
    ##  Max.   :0395-01-17                                        
    ##                                                            
    ##    dynasty              era               notes          
    ##  Length:68          Length:68          Length:68         
    ##  Class :character   Class :character   Class :character  
    ##  Mode  :character   Mode  :character   Mode  :character  
    ##                                                          
    ##                                                          
    ##                                                          
    ##                                                          
    ##   verif_who        
    ##  Length:68         
    ##  Class :character  
    ##  Mode  :character  
    ##                    
    ##                    
    ##                    
    ## 

``` r
head(emperors)
```

    ## # A tibble: 6 x 16
    ##   index name  name_full birth      death      birth_cty birth_prv rise 
    ##   <dbl> <chr> <chr>     <date>     <date>     <chr>     <chr>     <chr>
    ## 1     1 Augu~ IMPERATO~ 0062-09-23 0014-08-19 Rome      Italia    Birt~
    ## 2     2 Tibe~ TIBERIVS~ 0041-11-16 0037-03-16 Rome      Italia    Birt~
    ## 3     3 Cali~ GAIVS IV~ 0012-08-31 0041-01-24 Antitum   Italia    Birt~
    ## 4     4 Clau~ TIBERIVS~ 0009-08-01 0054-10-13 Lugdunum  Gallia L~ Birt~
    ## 5     5 Nero  NERO CLA~ 0037-12-15 0068-06-09 Antitum   Italia    Birt~
    ## 6     6 Galba SERVIVS ~ 0002-12-24 0069-01-15 Terracina Italia    Seiz~
    ## # ... with 8 more variables: reign_start <date>, reign_end <date>,
    ## #   cause <chr>, killer <chr>, dynasty <chr>, era <chr>, notes <chr>,
    ## #   verif_who <chr>

``` r
stacked <- emperors %>%
  mutate(killer=as.factor(killer)) %>%
  mutate(cause=as.factor(cause)) %>%
  #group_by(cause) %>%
  #summarise(count=n())%>%
  group_by(cause, killer) %>%
  summarise(total = n()) %>%
  arrange(cause)%>%
  ggplot(aes(cause, total)) +
  geom_col(aes(fill = factor(killer)), colour="white") +
 # geom_text(aes(x=killer, y=pos, label=killer), size=2) +
  #scale_fill_brewer()+
  #scale_colour_viridis(discrete=TRUE)+
  theme_clean() +
  labs(title="Roman Emperors",
       subtitle = "How they died and what/whom killed them",
       x= "Total Deaths",
       y="Cause", 
       caption= "Source: Wikipedia Zonination. Plot: @LarissaKostiw",
       fill = "Killer")+ #Error: DOesn't know how to add stacked to the plot.
  coord_flip()+
  theme(
    panel.background = element_rect(fill = "black", color = "white", linetype = "dashed"),
    #panel.background = element_blank(),
    plot.background = element_rect(fill = "black"), 
    plot.caption = element_text(color = "white"),
    plot.title = element_text(size = 15, face = "bold", color = "white", hjust = 0.5),
    plot.subtitle = element_text(size = 12, face = "bold", color = "white", hjust = 0.5),
    axis.text = element_text(color = "white", size = 12),
    axis.title = element_text(face="bold", color = "white", size = 12),
    axis.title.x = element_blank(),
    legend.text = element_text(size = 10, color = "white"),
    legend.title = element_text(color = "white"),
    legend.background = element_rect(fill = "black", color = "white", linetype = "dashed"))

stacked
```

![](roman_files/figure-markdown_github/Stacked%20Bar%20Chart-1.png)

``` r
ggsave("roman.png")
```

    ## Saving 7 x 5 in image
