Bob Ross Paintings
================
Larissa Kostiw
8 August 2019

``` r
bob_ross <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-08-06/bob-ross.csv")
```

    ## Parsed with column specification:
    ## cols(
    ##   .default = col_double(),
    ##   EPISODE = col_character(),
    ##   TITLE = col_character()
    ## )

    ## See spec(...) for full column specifications.

``` r
library(tidyverse)
```

    ## -- Attaching packages ----------------------------------------------------------------------------------------------------- tidyverse 1.2.1 --

    ## v ggplot2 3.2.0     v purrr   0.3.2
    ## v tibble  2.1.3     v dplyr   0.8.3
    ## v tidyr   0.8.3     v stringr 1.4.0
    ## v readr   1.3.1     v forcats 0.4.0

    ## -- Conflicts -------------------------------------------------------------------------------------------------------- tidyverse_conflicts() --
    ## x dplyr::filter() masks stats::filter()
    ## x dplyr::lag()    masks stats::lag()

``` r
library(janitor)
```

    ## 
    ## Attaching package: 'janitor'

    ## The following objects are masked from 'package:stats':
    ## 
    ##     chisq.test, fisher.test

``` r
# to clean up the episode information
bob_ross<- bob_ross %>% 
  janitor::clean_names() %>% 
  separate(episode, into = c("season", "episode"), sep = "E") %>% 
  mutate(season = str_extract(season, "[:digit:]+")) %>% 
  mutate_at(vars(season, episode), as.integer)  
```

``` r
summary(bob_ross)
```

    ##      season      episode      title            apple_frame      
    ##  Min.   : 1   Min.   : 1   Length:403         Min.   :0.000000  
    ##  1st Qu.: 8   1st Qu.: 4   Class :character   1st Qu.:0.000000  
    ##  Median :16   Median : 7   Mode  :character   Median :0.000000  
    ##  Mean   :16   Mean   : 7                      Mean   :0.002481  
    ##  3rd Qu.:24   3rd Qu.:10                      3rd Qu.:0.000000  
    ##  Max.   :31   Max.   :13                      Max.   :1.000000  
    ##  aurora_borealis         barn             beach            boat         
    ##  Min.   :0.000000   Min.   :0.00000   Min.   :0.000   Min.   :0.000000  
    ##  1st Qu.:0.000000   1st Qu.:0.00000   1st Qu.:0.000   1st Qu.:0.000000  
    ##  Median :0.000000   Median :0.00000   Median :0.000   Median :0.000000  
    ##  Mean   :0.004963   Mean   :0.04218   Mean   :0.067   Mean   :0.004963  
    ##  3rd Qu.:0.000000   3rd Qu.:0.00000   3rd Qu.:0.000   3rd Qu.:0.000000  
    ##  Max.   :1.000000   Max.   :1.00000   Max.   :1.000   Max.   :1.000000  
    ##      bridge           building            bushes           cabin       
    ##  Min.   :0.00000   Min.   :0.000000   Min.   :0.0000   Min.   :0.0000  
    ##  1st Qu.:0.00000   1st Qu.:0.000000   1st Qu.:0.0000   1st Qu.:0.0000  
    ##  Median :0.00000   Median :0.000000   Median :0.0000   Median :0.0000  
    ##  Mean   :0.01737   Mean   :0.002481   Mean   :0.2978   Mean   :0.1712  
    ##  3rd Qu.:0.00000   3rd Qu.:0.000000   3rd Qu.:1.0000   3rd Qu.:0.0000  
    ##  Max.   :1.00000   Max.   :1.000000   Max.   :1.0000   Max.   :1.0000  
    ##      cactus          circle_frame          cirrus            cliff        
    ##  Min.   :0.000000   Min.   :0.000000   Min.   :0.00000   Min.   :0.00000  
    ##  1st Qu.:0.000000   1st Qu.:0.000000   1st Qu.:0.00000   1st Qu.:0.00000  
    ##  Median :0.000000   Median :0.000000   Median :0.00000   Median :0.00000  
    ##  Mean   :0.009926   Mean   :0.004963   Mean   :0.06948   Mean   :0.01985  
    ##  3rd Qu.:0.000000   3rd Qu.:0.000000   3rd Qu.:0.00000   3rd Qu.:0.00000  
    ##  Max.   :1.000000   Max.   :1.000000   Max.   :1.00000   Max.   :1.00000  
    ##      clouds          conifer          cumulus         deciduous     
    ##  Min.   :0.0000   Min.   :0.0000   Min.   :0.0000   Min.   :0.0000  
    ##  1st Qu.:0.0000   1st Qu.:0.0000   1st Qu.:0.0000   1st Qu.:0.0000  
    ##  Median :0.0000   Median :1.0000   Median :0.0000   Median :1.0000  
    ##  Mean   :0.4442   Mean   :0.5261   Mean   :0.2134   Mean   :0.5633  
    ##  3rd Qu.:1.0000   3rd Qu.:1.0000   3rd Qu.:0.0000   3rd Qu.:1.0000  
    ##  Max.   :1.0000   Max.   :1.0000   Max.   :1.0000   Max.   :1.0000  
    ##   diane_andre            dock          double_oval_frame 
    ##  Min.   :0.000000   Min.   :0.000000   Min.   :0.000000  
    ##  1st Qu.:0.000000   1st Qu.:0.000000   1st Qu.:0.000000  
    ##  Median :0.000000   Median :0.000000   Median :0.000000  
    ##  Mean   :0.002481   Mean   :0.002481   Mean   :0.002481  
    ##  3rd Qu.:0.000000   3rd Qu.:0.000000   3rd Qu.:0.000000  
    ##  Max.   :1.000000   Max.   :1.000000   Max.   :1.000000  
    ##       farm              fence              fire         
    ##  Min.   :0.000000   Min.   :0.00000   Min.   :0.000000  
    ##  1st Qu.:0.000000   1st Qu.:0.00000   1st Qu.:0.000000  
    ##  Median :0.000000   Median :0.00000   Median :0.000000  
    ##  Mean   :0.002481   Mean   :0.05955   Mean   :0.002481  
    ##  3rd Qu.:0.000000   3rd Qu.:0.00000   3rd Qu.:0.000000  
    ##  Max.   :1.000000   Max.   :1.00000   Max.   :1.000000  
    ##  florida_frame         flowers             fog              framed      
    ##  Min.   :0.000000   Min.   :0.00000   Min.   :0.00000   Min.   :0.0000  
    ##  1st Qu.:0.000000   1st Qu.:0.00000   1st Qu.:0.00000   1st Qu.:0.0000  
    ##  Median :0.000000   Median :0.00000   Median :0.00000   Median :0.0000  
    ##  Mean   :0.002481   Mean   :0.02978   Mean   :0.05707   Mean   :0.1315  
    ##  3rd Qu.:0.000000   3rd Qu.:0.00000   3rd Qu.:0.00000   3rd Qu.:0.0000  
    ##  Max.   :1.000000   Max.   :1.00000   Max.   :1.00000   Max.   :1.0000  
    ##      grass            guest         half_circle_frame  half_oval_frame   
    ##  Min.   :0.0000   Min.   :0.00000   Min.   :0.000000   Min.   :0.000000  
    ##  1st Qu.:0.0000   1st Qu.:0.00000   1st Qu.:0.000000   1st Qu.:0.000000  
    ##  Median :0.0000   Median :0.00000   Median :0.000000   Median :0.000000  
    ##  Mean   :0.3524   Mean   :0.05459   Mean   :0.002481   Mean   :0.002481  
    ##  3rd Qu.:1.0000   3rd Qu.:0.00000   3rd Qu.:0.000000   3rd Qu.:0.000000  
    ##  Max.   :1.0000   Max.   :1.00000   Max.   :1.000000   Max.   :1.000000  
    ##      hills              lake            lakes     lighthouse      
    ##  Min.   :0.00000   Min.   :0.0000   Min.   :0   Min.   :0.000000  
    ##  1st Qu.:0.00000   1st Qu.:0.0000   1st Qu.:0   1st Qu.:0.000000  
    ##  Median :0.00000   Median :0.0000   Median :0   Median :0.000000  
    ##  Mean   :0.04467   Mean   :0.3548   Mean   :0   Mean   :0.002481  
    ##  3rd Qu.:0.00000   3rd Qu.:1.0000   3rd Qu.:0   3rd Qu.:0.000000  
    ##  Max.   :1.00000   Max.   :1.0000   Max.   :0   Max.   :1.000000  
    ##       mill               moon             mountain       mountains     
    ##  Min.   :0.000000   Min.   :0.000000   Min.   :0.000   Min.   :0.0000  
    ##  1st Qu.:0.000000   1st Qu.:0.000000   1st Qu.:0.000   1st Qu.:0.0000  
    ##  Median :0.000000   Median :0.000000   Median :0.000   Median :0.0000  
    ##  Mean   :0.004963   Mean   :0.007444   Mean   :0.397   Mean   :0.2457  
    ##  3rd Qu.:0.000000   3rd Qu.:0.000000   3rd Qu.:1.000   3rd Qu.:0.0000  
    ##  Max.   :1.000000   Max.   :1.000000   Max.   :1.000   Max.   :1.0000  
    ##      night            ocean           oval_frame        palm_trees     
    ##  Min.   :0.0000   Min.   :0.00000   Min.   :0.00000   Min.   :0.00000  
    ##  1st Qu.:0.0000   1st Qu.:0.00000   1st Qu.:0.00000   1st Qu.:0.00000  
    ##  Median :0.0000   Median :0.00000   Median :0.00000   Median :0.00000  
    ##  Mean   :0.0273   Mean   :0.08933   Mean   :0.09429   Mean   :0.02233  
    ##  3rd Qu.:0.0000   3rd Qu.:0.00000   3rd Qu.:0.00000   3rd Qu.:0.00000  
    ##  Max.   :1.0000   Max.   :1.00000   Max.   :1.00000   Max.   :1.00000  
    ##       path            person            portrait        rectangle_3d_frame
    ##  Min.   :0.0000   Min.   :0.000000   Min.   :0.000000   Min.   :0.000000  
    ##  1st Qu.:0.0000   1st Qu.:0.000000   1st Qu.:0.000000   1st Qu.:0.000000  
    ##  Median :0.0000   Median :0.000000   Median :0.000000   Median :0.000000  
    ##  Mean   :0.1216   Mean   :0.002481   Mean   :0.007444   Mean   :0.002481  
    ##  3rd Qu.:0.0000   3rd Qu.:0.000000   3rd Qu.:0.000000   3rd Qu.:0.000000  
    ##  Max.   :1.0000   Max.   :1.000000   Max.   :1.000000   Max.   :1.000000  
    ##  rectangular_frame      river            rocks        seashell_frame    
    ##  Min.   :0.000000   Min.   :0.0000   Min.   :0.0000   Min.   :0.000000  
    ##  1st Qu.:0.000000   1st Qu.:0.0000   1st Qu.:0.0000   1st Qu.:0.000000  
    ##  Median :0.000000   Median :0.0000   Median :0.0000   Median :0.000000  
    ##  Mean   :0.002481   Mean   :0.3127   Mean   :0.1911   Mean   :0.002481  
    ##  3rd Qu.:0.000000   3rd Qu.:1.0000   3rd Qu.:0.0000   3rd Qu.:0.000000  
    ##  Max.   :1.000000   Max.   :1.0000   Max.   :1.0000   Max.   :1.000000  
    ##       snow        snowy_mountain    split_frame         steve_ross    
    ##  Min.   :0.0000   Min.   :0.0000   Min.   :0.000000   Min.   :0.0000  
    ##  1st Qu.:0.0000   1st Qu.:0.0000   1st Qu.:0.000000   1st Qu.:0.0000  
    ##  Median :0.0000   Median :0.0000   Median :0.000000   Median :0.0000  
    ##  Mean   :0.1861   Mean   :0.2705   Mean   :0.002481   Mean   :0.0273  
    ##  3rd Qu.:0.0000   3rd Qu.:1.0000   3rd Qu.:0.000000   3rd Qu.:0.0000  
    ##  Max.   :1.0000   Max.   :1.0000   Max.   :1.000000   Max.   :1.0000  
    ##    structure           sun            tomb_frame            tree       
    ##  Min.   :0.0000   Min.   :0.00000   Min.   :0.000000   Min.   :0.0000  
    ##  1st Qu.:0.0000   1st Qu.:0.00000   1st Qu.:0.000000   1st Qu.:1.0000  
    ##  Median :0.0000   Median :0.00000   Median :0.000000   Median :1.0000  
    ##  Mean   :0.2109   Mean   :0.09926   Mean   :0.002481   Mean   :0.8958  
    ##  3rd Qu.:0.0000   3rd Qu.:0.00000   3rd Qu.:0.000000   3rd Qu.:1.0000  
    ##  Max.   :1.0000   Max.   :1.00000   Max.   :1.000000   Max.   :1.0000  
    ##      trees         triple_frame        waterfall           waves        
    ##  Min.   :0.0000   Min.   :0.000000   Min.   :0.00000   Min.   :0.00000  
    ##  1st Qu.:1.0000   1st Qu.:0.000000   1st Qu.:0.00000   1st Qu.:0.00000  
    ##  Median :1.0000   Median :0.000000   Median :0.00000   Median :0.00000  
    ##  Mean   :0.8362   Mean   :0.002481   Mean   :0.09677   Mean   :0.08437  
    ##  3rd Qu.:1.0000   3rd Qu.:0.000000   3rd Qu.:0.00000   3rd Qu.:0.00000  
    ##  Max.   :1.0000   Max.   :1.000000   Max.   :1.00000   Max.   :1.00000  
    ##     windmill         window_frame          winter        wood_framed      
    ##  Min.   :0.000000   Min.   :0.000000   Min.   :0.0000   Min.   :0.000000  
    ##  1st Qu.:0.000000   1st Qu.:0.000000   1st Qu.:0.0000   1st Qu.:0.000000  
    ##  Median :0.000000   Median :0.000000   Median :0.0000   Median :0.000000  
    ##  Mean   :0.002481   Mean   :0.002481   Mean   :0.1712   Mean   :0.002481  
    ##  3rd Qu.:0.000000   3rd Qu.:0.000000   3rd Qu.:0.0000   3rd Qu.:0.000000  
    ##  Max.   :1.000000   Max.   :1.000000   Max.   :1.0000   Max.   :1.000000

``` r
season<- bob_ross %>%
  select(-c(title, episode)) %>%
  group_by(season) %>%
  summarise_at(vars(apple_frame:wood_framed),
               .funs= sum)
```

``` r
library(reshape2)
```

    ## 
    ## Attaching package: 'reshape2'

    ## The following object is masked from 'package:tidyr':
    ## 
    ##     smiths

``` r
season$season<- as.factor(season$season)

season_transposed<- melt(season, id=c("season"))

season_transposed<- season_transposed %>%
  group_by(variable) %>%
  summarise(total=sum(value)) %>%
  arrange(-total)
```

``` r
#Wordcloud using ggwordcloud
#install.packages("ggwordcloud")
library(ggwordcloud)

wordcloud_gg<- ggplot(season_transposed, aes(label=variable, size=total)) +
  geom_text_wordcloud_area(shape="triangle-upright") +
 scale_size_area(max_size=25)+
  theme_minimal()
```

``` r
top_words<- season_transposed %>%
  top_n(10)
```

    ## Selecting by total

``` r
#top_words

#top_words


p<- ggplot(top_words, aes(x=reorder(variable, total), y=total))+
  geom_col() +
  coord_flip()+
  labs(
    title="Most painted objects",
    subtitle="The Joy of Paintings",
    caption="Source: 538. Plot: @LarissaKostiw",
    x="Total",
    y="Object"
  ) +
  theme_minimal()+
  theme(plot.title=element_text(hjust=0.5, face="bold", size=18),
        plot.subtitle = element_text(hjust=0.5),
        panel.grid.major.y=element_blank(),
        axis.title = element_text(face="bold"),
        axis.text = element_text())
p
```

![](bobrosspaintings_files/figure-markdown_github/Horizontal%20Bar%20Chart-1.png)

``` r
library(gridExtra)
```

    ## 
    ## Attaching package: 'gridExtra'

    ## The following object is masked from 'package:dplyr':
    ## 
    ##     combine

``` r
par(mfrow=c(1,2))
#wordcloud<- wordcloud2(data=season_transposed, size=1, shape='triangle-upright')
p
```

![](bobrosspaintings_files/figure-markdown_github/Arrange%20in%20grid-1.png)

``` r
grid.arrange(wordcloud_gg, p,  ncol=2)
```

    ## Warning in png(filename = tmp_file, width = gw_pix, height = gh_pix, res =
    ## dev_dpi, : 'width=12, height=16' are unlikely values in pixels

    ## Warning in png(filename = tmp_file, width = gw_pix, height = gh_pix, res =
    ## dev_dpi, : 'width=16, height=12' are unlikely values in pixels

    ## Warning in png(filename = tmp_file, width = gw_pix, height = gh_pix, res =
    ## dev_dpi, : 'width=12, height=12' are unlikely values in pixels

    ## Warning in png(filename = tmp_file, width = gw_pix, height = gh_pix, res =
    ## dev_dpi, : 'width=16, height=12' are unlikely values in pixels

    ## Warning in png(filename = tmp_file, width = gw_pix, height = gh_pix, res =
    ## dev_dpi, : 'width=16, height=12' are unlikely values in pixels

    ## Warning in png(filename = tmp_file, width = gw_pix, height = gh_pix, res =
    ## dev_dpi, : 'width=8, height=8' are unlikely values in pixels

    ## Warning in png(filename = tmp_file, width = gw_pix, height = gh_pix, res =
    ## dev_dpi, : 'width=16, height=12' are unlikely values in pixels

    ## Warning in png(filename = tmp_file, width = gw_pix, height = gh_pix, res =
    ## dev_dpi, : 'width=4, height=8' are unlikely values in pixels

    ## Warning in png(filename = tmp_file, width = gw_pix, height = gh_pix, res =
    ## dev_dpi, : 'width=16, height=12' are unlikely values in pixels

    ## Warning in png(filename = tmp_file, width = gw_pix, height = gh_pix, res =
    ## dev_dpi, : 'width=8, height=8' are unlikely values in pixels

    ## Warning in png(filename = tmp_file, width = gw_pix, height = gh_pix, res =
    ## dev_dpi, : 'width=12, height=12' are unlikely values in pixels

    ## Warning in png(filename = tmp_file, width = gw_pix, height = gh_pix, res =
    ## dev_dpi, : 'width=8, height=8' are unlikely values in pixels

    ## Warning in png(filename = tmp_file, width = gw_pix, height = gh_pix, res =
    ## dev_dpi, : 'width=8, height=8' are unlikely values in pixels

    ## Warning in png(filename = tmp_file, width = gw_pix, height = gh_pix, res =
    ## dev_dpi, : 'width=4, height=8' are unlikely values in pixels

    ## Warning in png(filename = tmp_file, width = gw_pix, height = gh_pix, res =
    ## dev_dpi, : 'width=16, height=12' are unlikely values in pixels

    ## Warning in png(filename = tmp_file, width = gw_pix, height = gh_pix, res =
    ## dev_dpi, : 'width=12, height=8' are unlikely values in pixels

    ## Warning in png(filename = tmp_file, width = gw_pix, height = gh_pix, res =
    ## dev_dpi, : 'width=8, height=8' are unlikely values in pixels

    ## Warning in png(filename = tmp_file, width = gw_pix, height = gh_pix, res =
    ## dev_dpi, : 'width=16, height=12' are unlikely values in pixels

    ## Warning in png(filename = tmp_file, width = gw_pix, height = gh_pix, res =
    ## dev_dpi, : 'width=12, height=12' are unlikely values in pixels

    ## Warning in png(filename = tmp_file, width = gw_pix, height = gh_pix, res =
    ## dev_dpi, : 'width=16, height=12' are unlikely values in pixels

    ## Warning in png(filename = tmp_file, width = gw_pix, height = gh_pix, res =
    ## dev_dpi, : 'width=16, height=12' are unlikely values in pixels

    ## Warning in png(filename = tmp_file, width = gw_pix, height = gh_pix, res =
    ## dev_dpi, : 'width=8, height=8' are unlikely values in pixels

    ## Warning in png(filename = tmp_file, width = gw_pix, height = gh_pix, res =
    ## dev_dpi, : 'width=16, height=12' are unlikely values in pixels

    ## Warning in png(filename = tmp_file, width = gw_pix, height = gh_pix, res =
    ## dev_dpi, : 'width=16, height=12' are unlikely values in pixels

    ## Warning in png(filename = tmp_file, width = gw_pix, height = gh_pix, res =
    ## dev_dpi, : 'width=8, height=8' are unlikely values in pixels

![](bobrosspaintings_files/figure-markdown_github/Arrange%20in%20grid-2.png)

``` r
#png("bob_ross.png", width=10, height=12)
#grid.arrange(wordcloud_gg, p,  ncol=2)
plot<- arrangeGrob(wordcloud_gg, p, nrow=1)
ggsave(file="bob_ross.png", plot, width = 10, height=8, dpi=300)
```

    ## Warning in png(filename = tmp_file, width = gw_pix, height = gh_pix, res =
    ## dev_dpi, : 'width=16, height=16' are unlikely values in pixels

    ## Warning in png(filename = tmp_file, width = gw_pix, height = gh_pix, res =
    ## dev_dpi, : 'width=16, height=12' are unlikely values in pixels

    ## Warning in png(filename = tmp_file, width = gw_pix, height = gh_pix, res =
    ## dev_dpi, : 'width=12, height=12' are unlikely values in pixels

    ## Warning in png(filename = tmp_file, width = gw_pix, height = gh_pix, res =
    ## dev_dpi, : 'width=12, height=12' are unlikely values in pixels

    ## Warning in png(filename = tmp_file, width = gw_pix, height = gh_pix, res =
    ## dev_dpi, : 'width=12, height=12' are unlikely values in pixels

    ## Warning in png(filename = tmp_file, width = gw_pix, height = gh_pix, res =
    ## dev_dpi, : 'width=16, height=12' are unlikely values in pixels

    ## Warning in png(filename = tmp_file, width = gw_pix, height = gh_pix, res =
    ## dev_dpi, : 'width=8, height=8' are unlikely values in pixels
