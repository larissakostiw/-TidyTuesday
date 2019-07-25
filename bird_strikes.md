Wildlife Strikes
================
Larissa Kostiw
25 July 2019

``` r
wildlife_impacts <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-07-23/wildlife_impacts.csv")
```

    ## Parsed with column specification:
    ## cols(
    ##   .default = col_character(),
    ##   incident_date = col_datetime(format = ""),
    ##   num_engs = col_double(),
    ##   incident_month = col_double(),
    ##   incident_year = col_double(),
    ##   time = col_double(),
    ##   height = col_double(),
    ##   speed = col_double(),
    ##   cost_repairs_infl_adj = col_double()
    ## )

    ## See spec(...) for full column specifications.

``` r
library(tidyverse)
```

    ## -- Attaching packages ------------------------------------------------------------------------- tidyverse 1.2.1 --

    ## v ggplot2 3.2.0     v purrr   0.3.2
    ## v tibble  2.1.3     v dplyr   0.8.3
    ## v tidyr   0.8.3     v stringr 1.4.0
    ## v readr   1.3.1     v forcats 0.4.0

    ## -- Conflicts ---------------------------------------------------------------------------- tidyverse_conflicts() --
    ## x dplyr::filter() masks stats::filter()
    ## x dplyr::lag()    masks stats::lag()

``` r
cols=c("state","airport_id","airport","operator", "atype", "type_eng", "species", "species_id","damage", "time_of_day","sky","precip")

wildlife_impacts[cols] <- lapply(wildlife_impacts[cols], factor)
sapply(wildlife_impacts, class) #Check that the classes have been changed to factors
```

    ## $incident_date
    ## [1] "POSIXct" "POSIXt" 
    ## 
    ## $state
    ## [1] "factor"
    ## 
    ## $airport_id
    ## [1] "factor"
    ## 
    ## $airport
    ## [1] "factor"
    ## 
    ## $operator
    ## [1] "factor"
    ## 
    ## $atype
    ## [1] "factor"
    ## 
    ## $type_eng
    ## [1] "factor"
    ## 
    ## $species_id
    ## [1] "factor"
    ## 
    ## $species
    ## [1] "factor"
    ## 
    ## $damage
    ## [1] "factor"
    ## 
    ## $num_engs
    ## [1] "numeric"
    ## 
    ## $incident_month
    ## [1] "numeric"
    ## 
    ## $incident_year
    ## [1] "numeric"
    ## 
    ## $time_of_day
    ## [1] "factor"
    ## 
    ## $time
    ## [1] "numeric"
    ## 
    ## $height
    ## [1] "numeric"
    ## 
    ## $speed
    ## [1] "numeric"
    ## 
    ## $phase_of_flt
    ## [1] "character"
    ## 
    ## $sky
    ## [1] "factor"
    ## 
    ## $precip
    ## [1] "factor"
    ## 
    ## $cost_repairs_infl_adj
    ## [1] "numeric"

``` r
wildlife_impacts$incident_month<-as.factor(wildlife_impacts$incident_month)

summary(wildlife_impacts)
```

    ##  incident_date                     state         airport_id   
    ##  Min.   :1990-01-01 00:00:00   N/A    :12671   ZZZZ   :11046  
    ##  1st Qu.:2001-11-15 00:00:00   TX     : 7146   KDFW   : 2455  
    ##  Median :2009-11-03 00:00:00   CA     : 5780   KDEN   : 2249  
    ##  Mean   :2008-05-21 04:57:11   FL     : 3686   KORD   : 1968  
    ##  3rd Qu.:2015-07-26 00:00:00   IL     : 2744   KSMF   : 1651  
    ##  Max.   :2018-12-31 00:00:00   CO     : 2441   KMCO   : 1067  
    ##                                (Other):22510   (Other):36542  
    ##                         airport                    operator    
    ##  UNKNOWN                    :11046   AMERICAN AIRLINES :14887  
    ##  DALLAS/FORT WORTH INTL ARPT: 2455   DELTA AIR LINES   : 9005  
    ##  DENVER INTL AIRPORT        : 2249   SOUTHWEST AIRLINES:17970  
    ##  CHICAGO O'HARE INTL ARPT   : 1968   UNITED AIRLINES   :15116  
    ##  SACRAMENTO INTL            : 1651                             
    ##  ORLANDO INTL               : 1067                             
    ##  (Other)                    :36542                             
    ##        atype       type_eng       species_id   
    ##  B-737-700: 9964   A   :    2   UNKBM  :15394  
    ##  B-737-300: 7641   C   :   34   UNKBS  :14765  
    ##  B-737-800: 5230   D   :56705   UNKB   : 3956  
    ##  B-757-200: 4235   F   :    3   NE1    : 1504  
    ##  A-320    : 3776   NA's:  234   O2205  : 1341  
    ##  A-319    : 3057                YI005  : 1171  
    ##  (Other)  :23075                (Other):18847  
    ##                   species       damage         num_engs    
    ##  Unknown bird - medium:15394   M   : 1892   Min.   :1.000  
    ##  Unknown bird - small :14765   M?  : 1085   1st Qu.:2.000  
    ##  Unknown bird         : 3956   N   :48650   Median :2.000  
    ##  Gulls                : 1504   S   : 1027   Mean   :2.059  
    ##  Mourning dove        : 1341   NA's: 4324   3rd Qu.:2.000  
    ##  Barn swallow         : 1171                Max.   :4.000  
    ##  (Other)              :18847                NA's   :233    
    ##  incident_month  incident_year  time_of_day        time      
    ##  9      : 7980   Min.   :1990   Dawn : 1270   Min.   : -84   
    ##  10     : 7754   1st Qu.:2001   Day  :25123   1st Qu.: 930   
    ##  8      : 7104   Median :2009   Dusk : 1717   Median :1426   
    ##  5      : 6161   Mean   :2008   Night:12735   Mean   :1428   
    ##  7      : 6133   3rd Qu.:2015   NA's :16133   3rd Qu.:1950   
    ##  6      : 4541   Max.   :2018                 Max.   :2359   
    ##  (Other):17305                                NA's   :26124  
    ##      height            speed       phase_of_flt               sky       
    ##  Min.   :    0.0   Min.   :  0.0   Length:56978       No Cloud  :18937  
    ##  1st Qu.:    0.0   1st Qu.:130.0   Class :character   Overcast  : 5449  
    ##  Median :   50.0   Median :140.0   Mode  :character   Some Cloud:12107  
    ##  Mean   :  983.8   Mean   :154.6                      NA's      :20485  
    ##  3rd Qu.: 1000.0   3rd Qu.:170.0                                        
    ##  Max.   :25000.0   Max.   :354.0                                        
    ##  NA's   :18038     NA's   :30046                                        
    ##        precip      cost_repairs_infl_adj
    ##  None     :32937   Min.   :      11     
    ##  Rain     : 1689   1st Qu.:    5128     
    ##  Fog      :  587   Median :   26783     
    ##  Snow     :   85   Mean   :  242388     
    ##  Fog, Rain:   58   3rd Qu.:   93124     
    ##  (Other)  :    8   Max.   :16380000     
    ##  NA's     :21614   NA's   :56363

``` r
operator_month_year<- wildlife_impacts %>%
    select(incident_year, incident_month, operator) %>%
    group_by(incident_year, incident_month, operator) %>%
    summarise(incidents=n()) %>%
    group_by(incident_year, incident_month, operator, incidents) %>%
  filter(incident_year>=2000)
```

``` r
p<- ggplot(operator_month_year,  mapping=aes(x=incident_month, y=incident_year, fill=incidents))+
  geom_tile(aes(fill=incidents), colour="white") +
  xlab(label="Month")+
  ylab(label="Year")+
  facet_wrap(~operator)+
  #facet_grid(~operator, space="free")+
  scale_fill_gradient(name="Bird Incidents",
                      low="#e7e1ef",
                      high="violetred2")+
  #scale_fill_distiller(palette = "RdPu")+ 
  labs(title="Airlines Bird Strikes",
       subtitle = "2000 - 2018",
       caption = "Data:FAA Wildlife Strike Database. By: @LarissaKostiw")+
    theme(strip.placement = "outside",
          panel.grid.major = element_blank(),
          panel.background = element_rect(fill="#e7e1ef"),
          #axis.text=element_text(hjust=0.5, size=0.5),
          plot.title=element_text(size=12, hjust=0.5, face="bold"),
          plot.subtitle=element_text(size=12, hjust=0.5, face="italic"),
          plot.caption = element_text(hjust=0.5, size=8),
          axis.title = element_text(face="bold"),
          axis.text = element_text(face="italic"),
          strip.background = element_rect(fill="seashell", colour="white"),
          strip.text.x=element_text(face="bold"),
          plot.background = element_rect(fill="#e7e1ef"),
          legend.background = element_rect(fill="#e7e1ef", colour="white"),
          legend.title= element_text(face="bold"))
ggsave("birdstrikes.png")
```

    ## Saving 7 x 5 in image
