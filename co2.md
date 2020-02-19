Food's Carbon Footprint
================
Larissa Kostiw
19 February 2020

``` r
food_consumption <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-02-18/food_consumption.csv')
```

    ## Parsed with column specification:
    ## cols(
    ##   country = col_character(),
    ##   food_category = col_character(),
    ##   consumption = col_double(),
    ##   co2_emmission = col_double()
    ## )

``` r
library(tidyverse)
```

    ## -- Attaching packages ------------------------------------------------------------------------------------------------- tidyverse 1.2.1 --

    ## v ggplot2 3.2.0     v purrr   0.3.3
    ## v tibble  2.1.3     v dplyr   0.8.3
    ## v tidyr   0.8.3     v stringr 1.4.0
    ## v readr   1.3.1     v forcats 0.4.0

    ## -- Conflicts ---------------------------------------------------------------------------------------------------- tidyverse_conflicts() --
    ## x dplyr::filter() masks stats::filter()
    ## x dplyr::lag()    masks stats::lag()

``` r
library(colorspace)
library(ggthemes)
library(RColorBrewer)
```

``` r
head(food_consumption)
```

    ## # A tibble: 6 x 4
    ##   country   food_category consumption co2_emmission
    ##   <chr>     <chr>               <dbl>         <dbl>
    ## 1 Argentina Pork                10.5          37.2 
    ## 2 Argentina Poultry             38.7          41.5 
    ## 3 Argentina Beef                55.5        1712   
    ## 4 Argentina Lamb & Goat          1.56         54.6 
    ## 5 Argentina Fish                 4.36          6.96
    ## 6 Argentina Eggs                11.4          10.5

``` r
#Convert character vars to factor
food_consumption <- food_consumption %>%
  mutate_if(sapply(food_consumption, is.character), as.factor)

summary(food_consumption)
```

    ##       country                     food_category  consumption     
    ##  Albania  :  11   Beef                   :130   Min.   :  0.000  
    ##  Algeria  :  11   Eggs                   :130   1st Qu.:  2.365  
    ##  Angola   :  11   Fish                   :130   Median :  8.890  
    ##  Argentina:  11   Lamb & Goat            :130   Mean   : 28.110  
    ##  Armenia  :  11   Milk - inc. cheese     :130   3rd Qu.: 28.133  
    ##  Australia:  11   Nuts inc. Peanut Butter:130   Max.   :430.760  
    ##  (Other)  :1364   (Other)                :650                    
    ##  co2_emmission    
    ##  Min.   :   0.00  
    ##  1st Qu.:   5.21  
    ##  Median :  16.53  
    ##  Mean   :  74.38  
    ##  3rd Qu.:  62.60  
    ##  Max.   :1712.00  
    ## 

``` r
food_category<- food_consumption %>%
  select(food_category) %>%
  distinct()

food_category
```

    ## # A tibble: 11 x 1
    ##    food_category           
    ##    <fct>                   
    ##  1 Pork                    
    ##  2 Poultry                 
    ##  3 Beef                    
    ##  4 Lamb & Goat             
    ##  5 Fish                    
    ##  6 Eggs                    
    ##  7 Milk - inc. cheese      
    ##  8 Wheat and Wheat Products
    ##  9 Rice                    
    ## 10 Soybeans                
    ## 11 Nuts inc. Peanut Butter

``` r
countries_high_emissions<- food_consumption %>%
  select(country, co2_emmission) %>%
  group_by(country) %>%
  summarise(total_emission=sum(co2_emmission)) %>%
  arrange(-total_emission) %>%
  slice(1:5)

head(countries_high_emissions, 5)
```

    ## # A tibble: 5 x 2
    ##   country     total_emission
    ##   <fct>                <dbl>
    ## 1 Argentina            2172.
    ## 2 Australia            1939.
    ## 3 Albania              1778.
    ## 4 New Zealand          1751.
    ## 5 Iceland              1731.

``` r
#Filter to just the rows with these countries in the original df
food_consumption_top5 <- food_consumption %>%
  filter(country %in% countries_high_emissions$country) %>%
  left_join(countries_high_emissions, by="country") %>% 
  arrange(-total_emission) #join on the sum 

#Plot
 # scale_fill_gradientn(colours=colorspace::heat_hcl(11))
  
nb.cols<-11 #Number of colours
mycolors <- colorRampPalette(brewer.pal(8, "Reds"))(nb.cols) #Specify which pallette to use
names(mycolors)=rev(levels(food_consumption$food_category)) #Reverse the colour order

plot<- ggplot(food_consumption_top5, aes(fill=food_category, y=co2_emmission, x=reorder(country,   -total_emission),total_emission))+
  geom_bar(position="stack", stat="identity") +
  scale_fill_manual(values=mycolors) +
  labs(
    title="CO2 Emmissions by food category",
    subtitle="Top 5 countries with highest emmissions",
    caption="Source: nu3. Plot: @LarissaKostiw",
    x="Country",
    y="CO2 Emmission (Kg CO2/person/year",
    fill="Food Category"
  ) +
  theme_tufte()+
  theme(
    plot.title=element_text(hjust=0.5, face="bold", size=18),
    plot.subtitle=element_text(hjust=0.5, size=14),
    plot.caption=element_text(hjust=1),
    panel.grid = element_blank(),
    axis.title=element_text(face="bold"),
    axis.text=element_text(face="italic")
  ) 

plot
```

![](co2_files/figure-markdown_github/unnamed-chunk-3-1.png)
