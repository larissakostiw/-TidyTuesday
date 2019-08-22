Nuclear Explosions
================
Larissa Kostiw
22 August 2019

``` r
nuclear_explosions <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-08-20/nuclear_explosions.csv")
```

    ## Parsed with column specification:
    ## cols(
    ##   date_long = col_double(),
    ##   year = col_double(),
    ##   id_no = col_double(),
    ##   country = col_character(),
    ##   region = col_character(),
    ##   source = col_character(),
    ##   latitude = col_double(),
    ##   longitude = col_double(),
    ##   magnitude_body = col_double(),
    ##   magnitude_surface = col_double(),
    ##   depth = col_double(),
    ##   yield_lower = col_double(),
    ##   yield_upper = col_double(),
    ##   purpose = col_character(),
    ##   name = col_character(),
    ##   type = col_character()
    ## )

``` r
library(tidyverse)
```

    ## -- Attaching packages --------------------------------------------------------------------------- tidyverse 1.2.1 --

    ## v ggplot2 3.2.0     v purrr   0.3.2
    ## v tibble  2.1.3     v dplyr   0.8.3
    ## v tidyr   0.8.3     v stringr 1.4.0
    ## v readr   1.3.1     v forcats 0.4.0

    ## -- Conflicts ------------------------------------------------------------------------------ tidyverse_conflicts() --
    ## x dplyr::filter() masks stats::filter()
    ## x dplyr::lag()    masks stats::lag()

``` r
nuclear_explosions<- nuclear_explosions %>%
  mutate_at(vars(c("country","region","purpose","name","source","type")), as.factor)

summary(nuclear_explosions)
```

    ##    date_long             year          id_no         country    
    ##  Min.   :19450716   Min.   :1945   Min.   :45001   CHINA :  45  
    ##  1st Qu.:19621066   1st Qu.:1962   1st Qu.:62140   FRANCE: 210  
    ##  Median :19700501   Median :1970   Median :70021   INDIA :   3  
    ##  Mean   :19709736   Mean   :1971   Mean   :70934   PAKIST:   2  
    ##  3rd Qu.:19790920   3rd Qu.:1979   3rd Qu.:79045   UK    :  45  
    ##  Max.   :19980530   Max.   :1998   Max.   :98005   USA   :1032  
    ##                                                    USSR  : 714  
    ##          region        source       latitude        longitude      
    ##  NTS        :928   DOE    :712   Min.   :-49.50   Min.   :-169.32  
    ##  SEMI KAZAKH:455   ISC    :546   1st Qu.: 37.00   1st Qu.:-116.05  
    ##  MURUROA    :172   UGS    :341   Median : 37.10   Median :-116.00  
    ##  NZ RUSS    :128   MTM    :169   Mean   : 35.40   Mean   : -36.05  
    ##  LOP NOR    : 45   HFS    :113   3rd Qu.: 49.87   3rd Qu.:  78.00  
    ##  ENEWETAK   : 43   WTN    : 93   Max.   : 75.10   Max.   : 179.22  
    ##  (Other)    :280   (Other): 77                                     
    ##  magnitude_body  magnitude_surface     depth            yield_lower     
    ##  Min.   :0.000   Min.   :0.0000    Min.   :-400.0000   Min.   :    0.0  
    ##  1st Qu.:0.000   1st Qu.:0.0000    1st Qu.:   0.0000   1st Qu.:    0.0  
    ##  Median :0.000   Median :0.0000    Median :   0.0000   Median :    0.0  
    ##  Mean   :2.145   Mean   :0.3558    Mean   :  -0.4896   Mean   :  209.2  
    ##  3rd Qu.:5.100   3rd Qu.:0.0000    3rd Qu.:   0.0000   3rd Qu.:   20.0  
    ##  Max.   :7.400   Max.   :6.0000    Max.   :   1.4510   Max.   :50000.0  
    ##                                                        NA's   :3        
    ##   yield_upper          purpose          name            type     
    ##  Min.   :    0.00   WR     :1495   VEGA   :  15   SHAFT   :1015  
    ##  1st Qu.:   18.25   WE     : 181   LIRA   :   6   TUNNEL  : 310  
    ##  Median :   20.00   PNE    : 153   GELIY  :   5   ATMOSPH : 185  
    ##  Mean   :  323.43   SE     :  71   REGION :   5   SHAFT/GR:  85  
    ##  3rd Qu.:  150.00   FMS    :  33   ABLE   :   4   AIRDROP :  78  
    ##  Max.   :50000.00   (Other): 117   (Other):1351   TOWER   :  75  
    ##  NA's   :5          NA's   :   1   NA's   : 665   (Other) : 303

``` r
head(nuclear_explosions)
```

    ## # A tibble: 6 x 16
    ##   date_long  year id_no country region source latitude longitude
    ##       <dbl> <dbl> <dbl> <fct>   <fct>  <fct>     <dbl>     <dbl>
    ## 1  19450716  1945 45001 USA     ALAMO~ DOE        32.5     -106.
    ## 2  19450805  1945 45002 USA     HIROS~ DOE        34.2      132.
    ## 3  19450809  1945 45003 USA     NAGAS~ DOE        32.4      130.
    ## 4  19460630  1946 46001 USA     BIKINI DOE        11.4      165.
    ## 5  19460724  1946 46002 USA     BIKINI DOE        11.4      165.
    ## 6  19480414  1948 48001 USA     ENEWE~ DOE        11.3      162.
    ## # ... with 8 more variables: magnitude_body <dbl>,
    ## #   magnitude_surface <dbl>, depth <dbl>, yield_lower <dbl>,
    ## #   yield_upper <dbl>, purpose <fct>, name <fct>, type <fct>

``` r
nuclear_explosions2<- nuclear_explosions %>%
  mutate(condition_type = case_when(
  depth<0 ~ 1,
  depth>=0 ~ -1
  )) %>%
  select(country, depth, purpose, condition_type) %>%
  mutate(purpose_group=case_when(
    str_detect(purpose, "WR") ~ "Weapons Development Program",
    str_detect(purpose, "WE") ~ "Evaluate effects on various targets",
    str_detect(purpose, "WE") ~ "Evaluate effects on various targets",
    str_detect(purpose, "PNE") ~ "Peaceful Nuclear Explosion",
    str_detect(purpose, "SAM") ~ "Soviet Test (Accidental Mode / Emergency)",
    str_detect(purpose, "TRANSP") ~ "Transportation-storage purposes",
    str_detect(purpose, "FSM") ~ "Soviet Test (Study phenomenon of nuclear explosion)",
    str_detect(purpose, "ME") ~ "Military Exercise",
    str_detect(purpose, "COMBAT") ~ "Combat"
  )) %>%
  mutate(purpose_group=ifelse(is.na(purpose_group), purpose, purpose_group)) %>%
  mutate_at(vars(purpose_group), as.factor)
```

``` r
library(RColorBrewer)
library(ggthemes)

stacked<- nuclear_explosions2 %>%
  mutate_if(is.factor, fct_explicit_na, na_level="(Unknown)") %>%
  group_by(country) %>%
  mutate(count3=n()) %>%
  ungroup() %>%
  mutate(country=fct_reorder(country, -count3)) %>%
  group_by(country, purpose_group, condition_type) %>%
  summarise(count=n()) %>%
  arrange(-count) %>%
  arrange(country) %>%
  mutate(count2= ifelse(condition_type<0, (-1*count), count)) %>%
  mutate(count=count2) %>%
  select(-count2) %>%
  filter(country %in% c("USA","USSR","FRANCE","CHINA","UK"))
    
    
plot <- ggplot(stacked, aes(fill=purpose_group, x=country, y=count))+
  geom_bar(position="stack", stat="identity") +
  scale_fill_brewer(palette="Set3")+
  theme_clean() +
  scale_y_continuous(breaks = seq(-1000,250,100))+
  geom_hline(yintercept = 0, colour="black", size=1)+
  labs(title = "Nuclear Explosions",
       subtitle="Types per deploying country",
       x = "Deploying Country",
       y= "Count of explosions",
       fill="Deployment reason",
       caption="Stockholm International Peace Research Institute. Plot:@LarissaKostiw") +
  theme(
    plot.title = element_text(hjust=0.5, face="bold", size=18),
    plot.subtitle = element_text(hjust=0.5, size=15),
    plot.caption = element_text(hjust=1, face="bold"),
    axis.title=element_text(size=10),
    axis.text=element_text(size=10, face="bold"),
    #axis.text.x=element_text(size=10),
    legend.title = element_text(hjust=0.5)
  ) +
  annotate("text", x="CHINA", y= 100, label='Atmospheric', fontface=2)+ #fontfact=2 = bold
  annotate("text", x="CHINA", y= -500, label='Underground', fontface=2 )


plot
```

![](nuclear_explosions_files/figure-markdown_github/unnamed-chunk-1-1.png)

``` r
explosions<- nuclear_explosions %>%
  select(country) %>%
  group_by(country)%>%
  summarise(count=n())
```
