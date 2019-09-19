Election 2016 - Part 3
================
Daniel
2018-02-08

-   [White, non-Hispanic, native-born population with no bachelor's degree in states that Obama won](#white-non-hispanic-native-born-population-with-no-bachelors-degree-in-states-that-obama-won)
-   [US county and state boundaries](#us-county-and-state-boundaries)
-   [Geographical distribution of white, non-Hispanic, native-born population with no bachelor's degree](#geographical-distribution-of-white-non-hispanic-native-born-population-with-no-bachelors-degree)
-   [Geographical distribution of Presidential margin shift](#geographical-distribution-of-presidential-margin-shift)

``` r
# Libraries
library(tidyverse)
library(sf)
library(compare)
library(ggrepel)
library(scales)
library(viridis)

# Parameters
  # Albers projection for 48 contiguous US states
US_ALBERS <- "+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=37.5 +lon_0=-96 +x_0=0 +y_0=0 +datum=WGS84 +no_defs"
  # Colors used for New York Times maps
    # For states won by Democrat
NYT_DEM <- "#1a80c4"
    # For states won by Republican
NYT_REP <- "#CC3D41"
  # File with state names
file_state_names <- "../../data/us/states.csv"
  # File for answers to Part 1
file_answers_1 <- "../../data/election-2016/answers_1.rds"
  # File for answers to Part 2
file_answers_2 <- "../../data/election-2016/answers_2.rds"
  # File for answers
file_answers <- "../../data/election-2016/answers_3.rds"

# mac files
county_boundaries <- "~/Google Drive/classes/dcl/c15/cb_2015_us_county_20m_sf.rds"
state_boundaries <- "~/Google Drive/classes/dcl/c15/cb_2015_us_state_20m_sf.rds"

# windows files
# county_boundaries <- "C:/Users/djole/Google Drive/classes/dcl/c15/cb_2015_us_county_20m_sf.rds"
# state_boundaries <- "C:/Users/djole/Google Drive/classes/dcl/c15/cb_2015_us_state_20m_sf.rds"

#===============================================================================

# Election data from Part 1
election_2012_2016 <- read_rds(file_answers_1)$q4

# Proportion of white, non-Hispanic, native, with no college degree from Part 2
answers_2 <- read_rds(file_answers_2)
  # By county
demographic <- answers_2$q1.4
  # For country
prop_white_native_no_college <- answers_2$q1.5
rm(answers_2)

# State names
state_names <- read_csv(file_state_names)

# Read in answers
answers <- read_rds(file_answers)


  # Colors used in New York Times map
NYT_COLORS <- c(
  "#004364",
  "#195473",
  "#55889e",
  "#a2c0a1",
  "#fdf6a3",
  "#e9b777",
  "#d27952",
  "#b63132",
  "#740023"
)

  # Values corresponding to NYT_COLORS, scaled to 0 to 1
NYT_VALUES <- c(
  75.61617,
  77,
  77 + (79.42770 - 77) * 1 / 3,
  77 + (79.42770 - 77) * 2 / 3,
  79.42770,
  79.42770 + (83 - 79.42770) * 1 / 3,
  79.42770 + (83 - 79.42770) * 2 / 3,
  83,
  83.41358
) %>% 
  scales::rescale()


  # Color used in New York Times map for missing values
NYT_NA_COLOR <- "#f2f2f2"
```

Chris Oh (DCL 2017 Spring) contributed to this challenge.

In 2012, Barack Obama won the US presidential election by winning 332 electoral votes from 26 states and the District of Columbia ([Map](https://www.nytimes.com/elections/2012/results/president.html)). In 2016, Donald Trump won the election by winning 306 electoral votes from 30 states ([Map](https://www.nytimes.com/elections/results/president)). In Parts 1 and 2, we learned that roughly half of the electorate consists of those who are white, non-Hispanic, native-born, and with no a bachelor's degree, and that the greater the percentage of this demographic in a county, the greater the increase in the Republican margin between the two elections.

US presidential elections are not determined by the popular vote. Hillary Clinton received over 2% more of the popular vote than Donald Trump. Rather, electoral votes are assigned to the winners of the states and the District of Columbia. In 2016, 44 states and the District of Columbia voted for the candidate from the same party as in 2012. The election was determined by the six states that voted for Barack Obama in 2012 and Donald Trump in 2016: Florida, Iowa, Michigan, Ohio, Pennsylvania, and Wisconsin.

The ACS data used to estimate the proportion of white, non-Hispanic, native-born eligible voters with no bachelor's degree was available before the election. The last part of this challenge is to understand the geographic distribution of these voters. Before the election, the consensus among virtually all the professional political data analysts was that Clinton had a very high probability of winning. Even Trump and his data people expected Clinton to win. Let's we what's in the data you've created.

White, non-Hispanic, native-born population with no bachelor's degree in states that Obama won
----------------------------------------------------------------------------------------------

The tibble `election_2012_2016` contains election data calculated in Part 1. The tibble `demographic` and variable `prop_white_native_no_college` contain data on the proportion of white, non-Hispanic, native-born eligible voters with no bachelor's degree by county and for the country, respectively, as calculated in Part 2. The tibble `state_names` contains the state names.

**q1** Create a tibble that contains the following variables for each state that Obama won in 2012

-   `state_name`
-   `state_fips`
-   `total_2012`
-   `obama`
-   `romney`
-   `d_2012` = `(obama - romney) / total_2012`
-   `total_2016`
-   `clinton`
-   `trump`
-   `d_2016` = `(clinton - trump) / total_2016`
-   `prop_white_native_no_college`: The weighted average of `prop_white_native_no_college` by `total_2012` for each county in the state

Sort in order of descending `prop_white_native_no_college`.

``` r
election_2012_2016 <-
  election_2012_2016 %>%
  mutate(state_fips = county_fips %/% 1000) 

q1 <-
  election_2012_2016 %>%
  select(-c(r_d_change, county_name, type)) %>%
  group_by(state_fips) %>% 
  summarise_at(vars(total_2012:trump), funs(sum)) %>%
  mutate(
    d_2012 = (obama - romney) / total_2012,
    d_2016 = (clinton - trump) / total_2016
  )

prop_no_col <-
  election_2012_2016 %>%
  select(state_fips, county_fips, total_2012) %>%
  left_join(demographic, by = "county_fips") %>%
  mutate(count_no_col = total_2012 * prop_white_native_no_college) %>%
  group_by(state_fips) %>%
  select(-prop_white_native_no_college) %>%
  summarise_at(vars(total_2012, count_no_col), funs(sum)) %>%
  transmute(state_fips, prop_white_native_no_college = count_no_col/total_2012)
  
state_names <- 
  state_names %>%
  mutate(state_fips = as.integer(state_fips))

q1 <-
  q1 %>%
  left_join(prop_no_col, by = "state_fips") %>%
  left_join(state_names, by = "state_fips") %>%
  select(
    state_name, 
    state_fips, 
    total_2012, 
    obama, 
    romney, 
    d_2012, 
    total_2016, 
    clinton, 
    trump, 
    d_2016, 
    prop_white_native_no_college
  ) %>%
  filter(obama > romney) %>%
  arrange(desc(prop_white_native_no_college))

# Print results
if (exists("q1")) {
  q1 %>%
    select(
      state_name,
      total_2012,
      d_2012,
      prop_white_native_no_college
    ) %>% 
    knitr::kable()
} 
```

| state\_name          |  total\_2012|    d\_2012|  prop\_white\_native\_no\_college|
|:---------------------|------------:|----------:|---------------------------------:|
| Iowa                 |      1582180|  0.0581015|                         0.6939792|
| Maine                |       710126|  0.1518280|                         0.6907258|
| Vermont              |       299290|  0.3559792|                         0.6399945|
| Wisconsin            |      3068434|  0.0694227|                         0.6342331|
| New Hampshire        |       710972|  0.0557589|                         0.6341404|
| Ohio                 |      5580870|  0.0297932|                         0.6172013|
| Minnesota            |      2936561|  0.0769410|                         0.5918779|
| Oregon               |      1789270|  0.1208946|                         0.5875465|
| Pennsylvania         |      5742363|  0.0539569|                         0.5788318|
| Michigan             |      4730984|  0.0949724|                         0.5770005|
| Rhode Island         |       445716|  0.2742957|                         0.5473840|
| Washington           |      3125564|  0.1486855|                         0.5252146|
| Delaware             |       413921|  0.1862674|                         0.4894789|
| Massachusetts        |      3167767|  0.2314883|                         0.4736935|
| Colorado             |      2569522|  0.0536516|                         0.4587195|
| Illinois             |      5242014|  0.1686939|                         0.4578004|
| Nevada               |      1014918|  0.0668093|                         0.4571094|
| Connecticut          |      1558075|  0.1734255|                         0.4512518|
| Florida              |      8474179|  0.0087689|                         0.4449636|
| Virginia             |      3854489|  0.0387335|                         0.4298379|
| New York             |      7081536|  0.2817723|                         0.4003402|
| New Jersey           |      3642934|  0.1778404|                         0.3869867|
| Maryland             |      2707327|  0.2607646|                         0.3525348|
| New Mexico           |       783757|  0.1014945|                         0.2929539|
| California           |     13038547|  0.2311858|                         0.2913480|
| Hawaii               |       434221|  0.4268080|                         0.1560050|
| District of Columbia |       293764|  0.8363482|                         0.0880683|

``` r
# Compare result with answer
if (exists("q1")) compare(answers$q1, q1)
```

    ## TRUE

**q2** Create one or more data visualizations to understand the data in `q1`, including a visualization of the data that was available before the 2016 election. What conclusions can you draw?

Graph from before election:

``` r
q1 %>%
  filter(prop_white_native_no_college > 0.44) %>%
  ggplot(aes(prop_white_native_no_college, d_2012, label = state_name)) +
  geom_point() +
  geom_text(hjust = 0, position = position_jitter(height = 0.01)) +
  theme_bw() 
```

<img src="challenge_files/figure-markdown_github/unnamed-chunk-3-1.png" width="100%" /> Here we can see that a significant proportion of the states that Obama won in 2012 had a proportion of white native citizens that had not gone to college of greater than 0.40. It would have been useful for the Clinton campaign to pay attention to this demographic because it doesn't seem likely that these voters would always vote Democrat and so it might have been useful for the campaign to make sure that they attracted these voters.

``` r
q1 %>%
  filter(prop_white_native_no_college > 0.44) %>%
  mutate(
    Winner_2016 = if_else(clinton < trump, "Trump", "Clinton"),
    margin = percent(round((d_2016 - d_2012), 2))
  ) %>%
  unite(state_margin, state_name, margin, sep = " ") %>%
  ggplot(aes(prop_white_native_no_college, d_2012)) +
  geom_point(aes(color = Winner_2016)) +
  geom_text_repel(aes(label = state_margin)) +
  scale_color_manual(values = c("blue", "red")) + 
  theme_bw()
```

<img src="challenge_files/figure-markdown_github/unnamed-chunk-4-1.png" width="100%" /> This plot shows all states who had a proportion of white natives who did not go to college that was greater than 44% and went for Obama in 2012. Here we can see that out of these states, even ones that went for Clinton, there was a significant downward shift in the proportion of Democrat votes. A number of these states shifted dramatically away from Democrat in 2016, though, the biggest shifts were in Iowa and Ohio and Michigan, all states that went for Trump.

``` r
q1 %>%
  mutate(d_change = d_2016 - d_2012) %>%
  filter(prop_white_native_no_college > 0.44) %>%
  ggplot(
    aes(
      x = prop_white_native_no_college, 
      y = d_change, 
      label = state_name)
  ) + 
  geom_point() + 
  geom_smooth() +
  geom_text(
    hjust = 0, 
    position = position_dodge2(width = .01, padding = 0.1)
  ) +
  theme_bw() +
  geom_hline(yintercept = 0)
```

    ## `geom_smooth()` using method = 'loess' and formula 'y ~ x'

<img src="challenge_files/figure-markdown_github/unnamed-chunk-5-1.png" width="100%" />

This is another way to view the data and here we can see that the more uneducated the white native citizens of state were, the more likely that state was to swing away from Democrat and towards Republican.

US county and state boundaries
------------------------------

We have put US county and state boundaries in sf format on [Box](https://stanford.box.com/s/fo769gkcpw3mwzspt0kn00w78jmxyiyf). From this Box directory download the county boundaries `data/cb_2015_us_county_20m_sf.rds` and the state boundaries `data/cb_2015_us_state_20m_sf.rds` save them in a place other than a GitHub repo.

**q3.1** For this challenge, we will not look at Alaska or Hawaii. Read in the county boundaries into `counties` and the state boundaries into `states`. For both, remove Alaska and Hawaii, and then transform to the US Albers projection in `US_ALBERS`.

``` r
counties <-
  county_boundaries %>%
  read_rds() %>%
  filter(STATEFP != "02" & STATEFP != "15") %>%
  st_transform(crs = US_ALBERS)

states <-
  state_boundaries %>%
  read_rds() %>%
  filter(STUSPS != "AK" & STUSPS != "HI") %>%
  st_transform(crs = US_ALBERS)
```

**q3.2** Are all of the counties in `counties` in `demographic`?

``` r
counties <-
  counties %>%
  mutate(county_fips = as.integer(GEOID)) 

setdiff(
    demographic$county_fips,
    counties$county_fips
)
```

    ##  [1]  2013  2016  2020  2050  2060  2068  2070  2090  2100  2105  2110
    ## [12]  2122  2130  2150  2158  2164  2170  2180  2185  2188  2195  2198
    ## [23]  2220  2230  2240  2261  2275  2282  2290 15001 15003 15005 15007
    ## [34] 15009

No, 34 are different.

**q3.3** Are all of the counties in `counties` in `election_2012_2016`?

``` r
setdiff(
  election_2012_2016$county_fips,
  counties$county_fips
)
```

    ## [1] 15001 15003 15007 15009

No, 4 are different. Hawaii counties.

Geographical distribution of white, non-Hispanic, native-born population with no bachelor's degree
--------------------------------------------------------------------------------------------------

I decided to reverse the NYT health inequality challenge color scale for these maps because they have a range that goes from red to blue, which is good for visualizing changes along Democrat/Republican party lines.

``` r
'%!in%' <- function(x,y)!('%in%'(x,y))

#state_boundaries <- "C:/Users/djole/Google Drive/classes/dcl/c10/cb_2015_us_state_20m_sf.rds" 
state_boundaries <- "~/Google Drive/classes/dcl/c10/cb_2015_us_state_20m_sf.rds" 


state_boundaries <-
  state_boundaries %>%
  read_rds() %>%
  filter(NAME %!in% c("Alaska", "Hawaii")) %>%
  mutate(geometry = st_transform(geometry, US_ALBERS)) 
```

**q4** Create a choropleth map of the percentage of eligible voters who are white, native-born, with no bachelor's degree by county. What conclusions can you draw?

``` r
counties <-
  counties %>%
  left_join(demographic, by = "county_fips")

grad_values <- c(0, 0.125, 0.25, 0.375, 0.5, .625,  0.75, .875, 1)

ggplot() +
  geom_sf(data = counties, aes(fill = prop_white_native_no_college), color = "white", size = 0.01) +
  geom_sf(data = states, fill = NA, color = "grey50", size = 0.5) +  
  theme_void() + 
  coord_sf(datum = NA) +
  scale_fill_gradientn(
    colors = NYT_COLORS,
    values = grad_values,
    na.value = NYT_NA_COLOR,
    guide = "colorbar",
    breaks = seq(0, 1, 0.1)
  ) +
  guides(
     fill = guide_colorbar(
       barheight = 0.25,
       barwidth = 15,
       ticks = FALSE,
       raster = FALSE,
       nbin = 9,
       title.position = "top",
       title.hjust = 0.5,
       title = "Prop. White Native No College",
     )
   ) +
  theme(
    legend.position = "bottom",
    legend.text = element_text(size = 8, colour = "blue"),
    plot.title = element_text(hjust = 0.5)
  ) +
  labs(
    title = "Percentage of eligible voters who are 
    white, native-born, with no bachelor's degree by county"
  )
```

<img src="challenge_files/figure-markdown_github/unnamed-chunk-10-1.png" width="100%" />

Here we can see that the counties with highest educated white native population are parts of middle to southern California, sections of Arizona, New Mexico, southern Utah, parts of Alabama, and a strip of the southeast. Interestingly, the most uneducated counties in the country are in the southern rust belt and parts of the midwest. There are also areas of Michigan and Wisconsin that are pretty uneducated.

Geographical distribution of Presidential margin shift
------------------------------------------------------

**q5** Create a choropleth the presidential margin shift by county. What conclusions can you draw?

``` r
counties <-
  counties %>%
  left_join(demographic, by = "county_fips") %>%
  left_join(election_2012_2016, by = "county_fips")

pol_colors <- c(NYT_REP, NYT_REP, "#FFFFFF", NYT_DEM, NYT_DEM)
pol_values <- c()

counties %>%
  mutate(r_d_change = pmin(0.25, pmax(-0.25, r_d_change))) %>%
  ggplot() +
  geom_sf(aes(fill = r_d_change), color = "white", size = 0.1) +
  geom_sf(data = states, fill = NA, color = "grey50", size = 0.2) +  
  theme_void() + 
  coord_sf(datum = NA) +
  scale_fill_gradient2(
    low = NYT_DEM,
    high = NYT_REP,
    mid = "white",
    midpoint = 0,
    guide = "colorbar",
    na.value = NYT_NA_COLOR,
    breaks = seq(-0.2, 0.2, 0.1)
  ) +
  guides(
     fill = guide_colorbar(
       barheight = 0.25,
       barwidth = 15,
       ticks = FALSE,
       raster = FALSE,
       nbin = 8,
       title.position = "top",
       title.hjust = 0.5,
       title = expression(paste("\u2190 ", "More Democrat         ", "         More Republican", " \u2192"))
     )
   ) +
  theme(
    legend.position = "bottom",
    legend.text = element_text(size = 8, colour = "blue"),
    plot.title = element_text(hjust = 0.5, size = 16)
  ) +
  labs(
    title = "Presidential Margin Shift By County"
  ) 
```

<img src="challenge_files/figure-markdown_github/unnamed-chunk-11-1.png" width="100%" />

We can see that the largest shift in Republican margin is in the midwest and the rust belt.

The area of most overlap between areas that have the most uneducated white native population and show the largest shift in republican margin are the counties that are in the rust belt. Some of the midwest states, like Minnesota, Wisconsin, Iowa, and Missouri show a really large shift in Republican margin, but are slightly more educated than the other counties.

I think that the major takeaway is that there is a concentration of white natives without a college education who ended shifting from Democrat to Republican.
