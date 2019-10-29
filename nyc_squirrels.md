Squirrels
================
Larissa Kostiw
29 October 2019

``` r
nyc_squirrels <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-10-29/nyc_squirrels.csv")
```

    ## Parsed with column specification:
    ## cols(
    ##   .default = col_character(),
    ##   long = col_double(),
    ##   lat = col_double(),
    ##   date = col_double(),
    ##   hectare_squirrel_number = col_double(),
    ##   running = col_logical(),
    ##   chasing = col_logical(),
    ##   climbing = col_logical(),
    ##   eating = col_logical(),
    ##   foraging = col_logical(),
    ##   kuks = col_logical(),
    ##   quaas = col_logical(),
    ##   moans = col_logical(),
    ##   tail_flags = col_logical(),
    ##   tail_twitches = col_logical(),
    ##   approaches = col_logical(),
    ##   indifferent = col_logical(),
    ##   runs_from = col_logical(),
    ##   zip_codes = col_double(),
    ##   community_districts = col_double(),
    ##   borough_boundaries = col_double()
    ##   # ... with 2 more columns
    ## )

    ## See spec(...) for full column specifications.

``` r
summary(nyc_squirrels)
```

    ##       long             lat        unique_squirrel_id   hectare         
    ##  Min.   :-73.98   Min.   :40.76   Length:3023        Length:3023       
    ##  1st Qu.:-73.97   1st Qu.:40.77   Class :character   Class :character  
    ##  Median :-73.97   Median :40.78   Mode  :character   Mode  :character  
    ##  Mean   :-73.97   Mean   :40.78                                        
    ##  3rd Qu.:-73.96   3rd Qu.:40.79                                        
    ##  Max.   :-73.95   Max.   :40.80                                        
    ##                                                                        
    ##     shift                date          hectare_squirrel_number
    ##  Length:3023        Min.   :10062018   Min.   : 1.000         
    ##  Class :character   1st Qu.:10082018   1st Qu.: 2.000         
    ##  Mode  :character   Median :10122018   Median : 3.000         
    ##                     Mean   :10119487   Mean   : 4.124         
    ##                     3rd Qu.:10142018   3rd Qu.: 6.000         
    ##                     Max.   :10202018   Max.   :23.000         
    ##                                                               
    ##      age            primary_fur_color  highlight_fur_color
    ##  Length:3023        Length:3023        Length:3023        
    ##  Class :character   Class :character   Class :character   
    ##  Mode  :character   Mode  :character   Mode  :character   
    ##                                                           
    ##                                                           
    ##                                                           
    ##                                                           
    ##  combination_of_primary_and_highlight_color color_notes       
    ##  Length:3023                                Length:3023       
    ##  Class :character                           Class :character  
    ##  Mode  :character                           Mode  :character  
    ##                                                               
    ##                                                               
    ##                                                               
    ##                                                               
    ##    location         above_ground_sighter_measurement specific_location 
    ##  Length:3023        Length:3023                      Length:3023       
    ##  Class :character   Class :character                 Class :character  
    ##  Mode  :character   Mode  :character                 Mode  :character  
    ##                                                                        
    ##                                                                        
    ##                                                                        
    ##                                                                        
    ##   running         chasing         climbing         eating       
    ##  Mode :logical   Mode :logical   Mode :logical   Mode :logical  
    ##  FALSE:2293      FALSE:2744      FALSE:2365      FALSE:2263     
    ##  TRUE :730       TRUE :279       TRUE :658       TRUE :760      
    ##                                                                 
    ##                                                                 
    ##                                                                 
    ##                                                                 
    ##   foraging       other_activities      kuks           quaas        
    ##  Mode :logical   Length:3023        Mode :logical   Mode :logical  
    ##  FALSE:1588      Class :character   FALSE:2921      FALSE:2973     
    ##  TRUE :1435      Mode  :character   TRUE :102       TRUE :50       
    ##                                                                    
    ##                                                                    
    ##                                                                    
    ##                                                                    
    ##    moans         tail_flags      tail_twitches   approaches     
    ##  Mode :logical   Mode :logical   Mode :logical   Mode :logical  
    ##  FALSE:3020      FALSE:2868      FALSE:2589      FALSE:2845     
    ##  TRUE :3         TRUE :155       TRUE :434       TRUE :178      
    ##                                                                 
    ##                                                                 
    ##                                                                 
    ##                                                                 
    ##  indifferent     runs_from       other_interactions   lat_long        
    ##  Mode :logical   Mode :logical   Length:3023        Length:3023       
    ##  FALSE:1569      FALSE:2345      Class :character   Class :character  
    ##  TRUE :1454      TRUE :678       Mode  :character   Mode  :character  
    ##                                                                       
    ##                                                                       
    ##                                                                       
    ##                                                                       
    ##    zip_codes     community_districts borough_boundaries
    ##  Min.   :10090   Min.   :11          Min.   :4         
    ##  1st Qu.:12081   1st Qu.:19          1st Qu.:4         
    ##  Median :12420   Median :19          Median :4         
    ##  Mean   :11828   Mean   :19          Mean   :4         
    ##  3rd Qu.:12423   3rd Qu.:19          3rd Qu.:4         
    ##  Max.   :12423   Max.   :23          Max.   :4         
    ##  NA's   :3014                                          
    ##  city_council_districts police_precincts
    ##  Min.   :19.00          Min.   :10      
    ##  1st Qu.:19.00          1st Qu.:13      
    ##  Median :19.00          Median :13      
    ##  Mean   :19.07          Mean   :13      
    ##  3rd Qu.:19.00          3rd Qu.:13      
    ##  Max.   :51.00          Max.   :18      
    ## 

``` r
#install.packages("circlize")
#install.packages("chorddiag")
#devtools::install_github("mattflor/chorddiag")
library(ggplot2)
library(circlize)
```

    ## ========================================
    ## circlize version 0.4.8
    ## CRAN page: https://cran.r-project.org/package=circlize
    ## Github page: https://github.com/jokergoo/circlize
    ## Documentation: http://jokergoo.github.io/circlize_book/book/
    ## 
    ## If you use it in published research, please cite:
    ## Gu, Z. circlize implements and enhances circular visualization 
    ##   in R. Bioinformatics 2014.
    ## ========================================

``` r
library(tidyverse)
```

    ## -- Attaching packages -------------------------------------------------------------------------------- tidyverse 1.2.1 --

    ## v tibble  2.1.3     v purrr   0.3.2
    ## v tidyr   0.8.3     v dplyr   0.8.3
    ## v readr   1.3.1     v stringr 1.4.0
    ## v tibble  2.1.3     v forcats 0.4.0

    ## -- Conflicts ----------------------------------------------------------------------------------- tidyverse_conflicts() --
    ## x dplyr::filter() masks stats::filter()
    ## x dplyr::lag()    masks stats::lag()

``` r
#library(chorddiag)
```

``` r
#Split by "activity" and colour of squirrel.
chord<- nyc_squirrels %>%
  filter(!is.na(primary_fur_color)) %>% #Remove missing colour
  group_by(primary_fur_color) %>% #Sum the number per activity per fur colour
  summarise(
    Running=sum(running, na.rm=TRUE),
    Chasing=sum(chasing, na.rm=TRUE),
    Climbing=sum(climbing, na.rm=TRUE),
    Eating=sum(eating, na.rm=TRUE),
    Foraging=sum(foraging, na.rm=TRUE)
  )



colnames(chord)<-c("Colour", "Running", "Chasing", "Climbing", "Eating", "Foraging")
rownames(chord) <- c("Black", "Cinnamon", "Grey")
```

    ## Warning: Setting row names on a tibble is deprecated.

``` r
chord$Colour = gsub("Gray", "Grey", chord$Colour)

#Transpose the above table.
chord_long<-gather(data=chord, key='behavior', value='count',gather_cols=c(Running, Eating, Chasing, Foraging, Climbing))
chord_long$count<-as.numeric(chord_long$count)
```

``` r
#png(file="squirrels.png",width=400,height=400)

circos.clear()

chord_long2 <- chord_long %>%
  arrange(desc(Colour))

circos.par( gap.degree = 2, track.margin = c(-0.2, 0.2), points.overflow.warning = FALSE)
par(mar = rep(1, 4))
mycolour<- c(Grey = "grey60", Black = "black", Cinnamon = "orange",
             Running = "salmon4", Chasing = "salmon4", Climbing = "salmon4", Eating = "salmon4", Foraging = "salmon4")
# Base plot
chordDiagram(
  x = chord_long2,  #Long data format
  grid.col = mycolour,
  transparency = 0.25,
  link.sort = TRUE, 
  link.decreasing = TRUE,
  annotationTrack = "grid", 
  annotationTrackHeight = c(0.05, 0.1))
# Add text and adjust axis
circos.trackPlotRegion(
  track.index = 1, 
  bg.border = NA, 
  panel.fun = function(x, y) {
    xlim = get.cell.meta.data("xlim")
    sector.index = get.cell.meta.data("sector.index")
    # Add names to the sectors. 
    circos.text(
      x = mean(xlim), 
      y = 5, 
      labels = sector.index, 
      facing = "bending", 
      cex = 1
    )
      # Change the scaling for the sectors
    circos.axis(
      h = "top", 
      major.at = seq(from = 0, to = xlim[2], by = 250), 
      minor.ticks = 1, 
      major.tick.percentage = 0.5,labels.cex = 0.9,
      labels.niceFacing = FALSE)
  }
)
title (main = 'Behaviour of squirrels by squirrel type in Central Park')
```

![](nyc_squirrels_files/figure-markdown_github/test-1.png)
