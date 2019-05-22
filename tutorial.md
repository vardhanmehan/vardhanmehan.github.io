CMSC320 Final Project:
======================

*Tutorial on Data Science Pipeline*
-----------------------------------

*Impact of GDP on Global Suicide Rates*
---------------------------------------

Introduction
------------

Here we analyze if economic indicators can give insight into the well-being of human beings. Specifically, we look at how GDP is correlated with suicide rates on a global level, as well as other factors at play such as age and sex.

Dataset
-------

The dataset we used linked [here](https://www.kaggle.com/russellyates88/suicide-rates-overview-1985-to-2016) pulls from four other datasets linked by time and place, and was built to highlight factors correlated to increased suicide rates among different cohorts globally, across the socio-economic spectrum.

Setup
-----

We will be working in RStudio.

Importing libraries:

Here, we read the csv file we want to use and make a dataframe for it.

``` r
df <- read.csv("master.csv")

df <- df %>%
  mutate(gdp_per_capita = gdp_per_capita....)

countries <- df %>%
  group_by(country) %>%
  summarize(suicides = sum(as.numeric(suicides_no)), population = sum(as.numeric(population)), suicides_per_100k = 100000 * (suicides/population)) %>%
  arrange(desc(suicides_per_100k))

gdp <- df %>%
  group_by(gdp_per_capita) %>%
  summarize(suicides = sum(as.numeric(suicides_no)), population = sum(as.numeric(population)), suicides_per_100k = 100000 * (suicides/population))

time_df <- df %>%
  group_by(year) %>%
  summarize(population = sum(as.numeric(population)), suicides = sum(as.numeric(suicides_no)), suicides_per_100k = 100000* (suicides/population))

sex <- df %>%
  group_by(sex) %>%
  summarize(population = sum(as.numeric(population)), suicides = sum(as.numeric(suicides_no)), suicides_per_100k = 100000*(suicides/population))

head(df)
```

    ##   country year    sex         age suicides_no population suicides.100k.pop
    ## 1 Albania 1987   male 15-24 years          21     312900              6.71
    ## 2 Albania 1987   male 35-54 years          16     308000              5.19
    ## 3 Albania 1987 female 15-24 years          14     289700              4.83
    ## 4 Albania 1987   male   75+ years           1      21800              4.59
    ## 5 Albania 1987   male 25-34 years           9     274300              3.28
    ## 6 Albania 1987 female   75+ years           1      35600              2.81
    ##   country.year HDI.for.year gdp_for_year.... gdp_per_capita....
    ## 1  Albania1987           NA    2,156,624,900                796
    ## 2  Albania1987           NA    2,156,624,900                796
    ## 3  Albania1987           NA    2,156,624,900                796
    ## 4  Albania1987           NA    2,156,624,900                796
    ## 5  Albania1987           NA    2,156,624,900                796
    ## 6  Albania1987           NA    2,156,624,900                796
    ##        generation gdp_per_capita
    ## 1    Generation X            796
    ## 2          Silent            796
    ## 3    Generation X            796
    ## 4 G.I. Generation            796
    ## 5         Boomers            796
    ## 6 G.I. Generation            796

``` r
age_group <- df %>%
  group_by(age) %>%
  summarize(population = sum(as.numeric(population)), suicides = sum(as.numeric(suicides_no)), suicides_per_100k = 100000*(suicides/population))

head(countries) 
```

    ## # A tibble: 6 x 4
    ##   country            suicides population suicides_per_100k
    ##   <fct>                 <dbl>      <dbl>             <dbl>
    ## 1 Lithuania             28039   68085210              41.2
    ## 2 Russian Federation  1209742 3690802620              32.8
    ## 3 Sri Lanka             55641  182525626              30.5
    ## 4 Belarus               59892  197372292              30.3
    ## 5 Hungary               73891  248644256              29.7
    ## 6 Latvia                12770   44852640              28.5

``` r
head(gdp)
```

    ## # A tibble: 6 x 4
    ##   gdp_per_capita suicides population suicides_per_100k
    ##            <int>    <dbl>      <dbl>             <dbl>
    ## 1            251       47    2822500              1.67
    ## 2            291      556    4298100             12.9 
    ## 3            313      510    4369202             11.7 
    ## 4            345      560    4425784             12.7 
    ## 5            357      106    3366600              3.15
    ## 6            359      567    4473163             12.7

``` r
head(time_df)
```

    ## # A tibble: 6 x 4
    ##    year population suicides suicides_per_100k
    ##   <int>      <dbl>    <dbl>             <dbl>
    ## 1  1985 1008600086   116063              11.5
    ## 2  1986 1029909613   120670              11.7
    ## 3  1987 1095029726   126842              11.6
    ## 4  1988 1054094424   121026              11.5
    ## 5  1989 1225514347   160244              13.1
    ## 6  1990 1466620100   193361              13.2

``` r
head(sex)
```

    ## # A tibble: 2 x 4
    ##   sex     population suicides suicides_per_100k
    ##   <fct>        <dbl>    <dbl>             <dbl>
    ## 1 female 26272781857  1559510              5.94
    ## 2 male   25049376579  5188910             20.7

``` r
head(age_group)
```

    ## # A tibble: 6 x 4
    ##   age          population suicides suicides_per_100k
    ##   <fct>             <dbl>    <dbl>             <dbl>
    ## 1 15-24 years  8642946896   808542             9.35 
    ## 2 25-34 years  8438103587  1123912            13.3  
    ## 3 35-54 years 14375888123  2452141            17.1  
    ## 4 5-14 years   8398693237    52264             0.622
    ## 5 55-74 years  8803245340  1658443            18.8  
    ## 6 75+ years    2663281253   653118            24.5

Data Analysis
-------------

We will begin with an initial overview of the data.

### Suicide Rate by Country

``` r
countries_plot <- countries %>%
  ggplot(aes(x = factor(country, ordered=TRUE, levels=rev(countries$country)), y = suicides_per_100k, fill=suicides_per_100k)) +
  geom_bar(stat="identity") + 
  coord_flip() +
  scale_y_continuous(breaks=seq(0,45,5)) +
  labs(title = "Country vs Suicides per 100k", x = "Country", y = "Suicides per 100k")

countries_plot
```

![](tutorial_files/figure-markdown_github/countries-1.png)

Here, we can clearly see that suicide rates clearly differ by country. At the very least, we see that many European countries have high suicide rates. Therefore, we can conclude there is an association between the suicide rate and geographic locations. However, we cannot make any further assumptions on to the causation of suicide rates from this data alone due to the fact that some data may be misrepresented. For example, countries may not be reliable in collecting data on this topic and may have insufficient tools to accurately collect the data.

### GDP vs Suicide Rate

``` r
gdp_plot <- gdp %>%
  ggplot(aes(x = gdp_per_capita, y = suicides_per_100k)) +
  geom_point() +
  geom_smooth() +
  scale_x_continuous(breaks=seq(0,200000,10000)) +
  labs(title = "GDP per Capita vs Suicides per 100k", x = "GDP per Capita", y = "Suicides per 100k")

gdp_plot
```

    ## `geom_smooth()` using method = 'gam' and formula 'y ~ s(x, bs = "cs")'

![](tutorial_files/figure-markdown_github/gdp-1.png)

Here, we see that there are more suicides in countries with lower GDP. It is also important to note that there seem to be more suicides per 100k people in countries with lower GDP than countries with a higher GDP per capita.

Some issues that we foresee with trying to find a correlation between GDP and suicide rates are the possible inaccuracies of GDP as a measure of the economy. According to Investopedia, GDP doesn't include increases to Standards of Living, Negative Effects on Nature, and Household Production.

GDP calculations inherently undervalues certain products by undermining their contributions to overall productivity and standards of living. For example, a technological breakthrough will increase the standard of living yet GDP will not account for that. GDP calculations also do not take into account negative effects of certain events, such as environmental decay or natural disasters. It also does not account for long term growth, such as sustainable production, clean air, etc. Finally, it doesn't account for people who do not have "market value", such as stay at home moms and free apps and services.

These factors that GDP doesn't account for seems to have an impact on the mental wellbeing of a country's citizens and may therefore not be an accurate predictor of suicide rates. In order to get a better understanding of the impact of GDP on the suicide rate, we need to perform further regression analysis.

### Global Suicide Rates over Time

``` r
time_plot <- time_df %>%
  ggplot(aes(x = year, y = suicides_per_100k)) +
  geom_point() +
  geom_line() +
  labs(title = "Suicides per 100k over time", x = "Time", y = "Suicides per 100k")

time_plot
```

![](tutorial_files/figure-markdown_github/time-1.png)

When analyzing the suicide rates over time, we can see that the suicides rate fluctuate, peaking in 1995. It is possible that certain world events, such as war or poor economy, could have impacted the suicide rates during specific time periods. Note that it is also possible that countries, over time, developed better tools to determine suicide rates, so suicide rates in earlier periods of time may be misrepresented or innacurate.

From this data alone, we cannot clearly determine if time impacts the suicide rate.

### Suicide Rates by Gender

``` r
sex_plot <- sex %>%
  ggplot(aes(x = sex, y = suicides_per_100k, fill=sex)) +
  geom_bar(stat="identity") +
  labs(title = "Gender vs Suicides per 100k", x = "Gender", y = "Suicides per 100k")

sex_plot
```

![](tutorial_files/figure-markdown_github/sex-1.png)

It is clear to see that males comprise an extremely large portion of global suicide rates. This may be caused by the traditional male role in society and the economy. This clear difference in suicide rates between male and females should be taken into account when we create our regression model for predicting suicide rates.

### Suicide rates by Gender in each Country

``` r
sex_country <- df %>%
  group_by(country, sex) %>%
  summarize(population = sum(as.numeric(population)), suicides = sum(as.numeric(suicides_no)), suicides_per_100k = 100000 * (suicides/population)) %>%
  arrange(desc(suicides))

gender_country_plot <- sex_country %>%
  ggplot(aes(x = factor(country, ordered=TRUE, levels=rev(countries$country)), y = suicides_per_100k, fill = sex)) +
  geom_bar(stat = "Identity") +
  coord_flip() +
  labs(title="Suicide per 100k for each Gender in each Country", x="Country", y="Suicides per 100k")
 
gender_country_plot 
```

![](tutorial_files/figure-markdown_github/sex_country-1.png)

We see that in each country, there is a large difference between the number of men committing suicide vs the number of women committing suicide. We saw this difference in the global suicides of men vs women. This shows that most countries, although different in economic prosperity, GDP, and location, have a similar ratio of suicide rates between men and women, further backing the need to include gender differences into our regression model.

### Suicide Rates by Age Group

``` r
age_group_plot <- age_group %>%
  ggplot(aes(x = factor(age, ordered=TRUE, levels=c("5-14 years","15-24 years","25-34 years","35-54 years","55-74 years","75+ years")), y= suicides_per_100k, fill=age)) +
  geom_bar(stat="identity") +
  labs(title = "Age Group vs Suicides per 100k", x = "Age group", y = "Suicides per 100k")

age_group_plot
```

![](tutorial_files/figure-markdown_github/age-1.png)

There is a clear trend with age and suicide rates. Similar to sex, this may be due to environmental and societal pressures that may grow over time. For example, the average suicide rates for people in the work force (roughly between the ages of 25 and 75) are relatively high and increase with age. For the 75+ category, it is important to note that the definition of suicide may differ from country to country, so it possible that deaths due to euthanasia and assisted suicide (from doctors) may be added to this count.

``` r
age_country <- df %>%
  group_by(country, age) %>%
  summarize(population = sum(as.numeric(population)), suicides = sum(as.numeric(suicides_no)), suicides_per_100k = 100000 * (suicides/population)) %>%
  arrange(desc(suicides))

age_country_plot <- age_country %>%
  ggplot(aes(x = factor(country, ordered=TRUE, levels=rev(countries$country)), y = suicides_per_100k, fill = age)) +
  geom_bar(stat="Identity") +
  coord_flip() +
  labs(title="Age distribution of Suicides per 100k by Country", x="Country", y="Suicides per 100k")

age_country_plot
```

![](tutorial_files/figure-markdown_github/age_country-1.png)

Here, we can see that the distribution of suicides per 100k within each age group for each country is similar to that of the global comparison. However, we do note that there are countries such as Poland, Ireland, and Iceland, among others, that have a larger portion of suicides within the 25-54 and 54-74 age groups as opposed to the 75+ group.

Regression Analysis
-------------------

Now that we have analyzed our data and the interaction of variables with one another, we can test our original question: Does GDP impact suicide rates? If so, to what extent?

Here, our null hypothesis, *H*<sub>0</sub>, is that there is NO correlation between the GDP and suicide rates. Our alternate hypothesis, *H*<sub>1</sub>, is that there is a correlation between the two variables. In order to test this question, we will first construct a linear regression model of the two variables. To do this, we will implement the use of the lm function in r, which computes the regression for us!

``` r
head(gdp)
```

    ## # A tibble: 6 x 4
    ##   gdp_per_capita suicides population suicides_per_100k
    ##            <int>    <dbl>      <dbl>             <dbl>
    ## 1            251       47    2822500              1.67
    ## 2            291      556    4298100             12.9 
    ## 3            313      510    4369202             11.7 
    ## 4            345      560    4425784             12.7 
    ## 5            357      106    3366600              3.15
    ## 6            359      567    4473163             12.7

``` r
reg <- lm(suicides_per_100k~gdp_per_capita, data=gdp)

summary(reg)
```

    ## 
    ## Call:
    ## lm(formula = suicides_per_100k ~ gdp_per_capita, data = gdp)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -12.105  -6.514  -1.746   4.398  39.637 
    ## 
    ## Coefficients:
    ##                 Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)    1.132e+01  2.528e-01  44.759  < 2e-16 ***
    ## gdp_per_capita 2.710e-05  9.825e-06   2.758  0.00586 ** 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 8.851 on 2231 degrees of freedom
    ## Multiple R-squared:  0.003399,   Adjusted R-squared:  0.002952 
    ## F-statistic: 7.609 on 1 and 2231 DF,  p-value: 0.005856

From this regression, we must first note that it is in the form of *y* = *a**x* + *b*, where *y* represents the suicides per 100k and *x* represents the GDP per capita. From our regression, we have an intercept *b* = 10.32 and *a* = 2.710 \* 10<sup>−5</sup>. This means that when our GDP increases by 1, the suicides per 100k increases by 2.710 \* 10<sup>−5</sup>. Here, our *r*<sup>2</sup> value is 0.003399, which is extremely small. This means that the linear relationship between GDP and suicides per 100k is extremely weak. This is backed up by our very small *p* value. *p* = 0.005856 &lt; 0.05, so we reject our null hypothesis of no correlation and accept our alternate hypotheis that there is a correlation.

Increasing and Decreasing Suicide Rates By Country
--------------------------------------------------

``` r
year_country <- df %>%
  group_by(country,year) %>%
  summarize(population = sum(as.numeric(population)), suicides = sum(as.numeric(suicides_no)), suicides_per_100k = 100000 * (suicides/population))%>%
  filter(country != "Mongolia" & country != "Bosnia and Herzegovina" & country != "Montenegro" & country != "San Marino")%>%
  arrange(year)

trends <- year_country %>%
  mutate(first = !duplicated(country), last = rev(!duplicated(rev(country)))) %>%
  filter(first == TRUE | last == TRUE) %>%
  mutate(suicides_per_100k = ifelse(first == TRUE, suicides_per_100k*-1, suicides_per_100k)) %>%
  mutate(year = ifelse(first == TRUE, year*-1, year)) %>%
  group_by(country) %>%
  summarize(add_sp100k = sum(suicides_per_100k), yearDiff = sum(year)) %>%
  mutate(slope = 1.0*add_sp100k/(yearDiff)) %>%
  arrange(desc(slope))

head(trends)
```

    ## # A tibble: 6 x 4
    ##   country             add_sp100k yearDiff slope
    ##   <fct>                    <dbl>    <dbl> <dbl>
    ## 1 Guyana                   22.2        25 0.886
    ## 2 Republic of Korea        17.7        30 0.590
    ## 3 Trinidad and Tobago       8.36       26 0.321
    ## 4 Uruguay                   9.26       30 0.309
    ## 5 Cyprus                    4.94       17 0.291
    ## 6 Malta                     7.63       30 0.254

In here, we decided to see what countries had the most increasing suicide rate. To do that, we filtered only the first and last ocurrence of the country, due to us sorting by year earlier. Then we got the difference of those two suicides per 100k to create add\_sp100k. I did this by making all the data with first = TRUE to have a negative number, so when I added it together the total would be the difference. Then we created a slope based on that number, which is add\_sp100k/(the difference between the two years). This basically creates a slope for suicide rates in which we can order. After we got the order of the suicide rates, we graphed using a facet\_wrap to outline each of the countries slope and increasing trend of suicide rates. We remove Mongolia, Bosnia and Herzegovina, San Marino, and Montenegro as they do not have enough data points to make a fulfilling graph, making them outliers.

``` r
highest8 <- head(trends,8)
totalHighest <- highest8 %>%
  left_join(year_country, by = "country") %>%
  ggplot(aes(x = year, y = suicides_per_100k, color = country)) +
  geom_point() + 
  geom_smooth(method = "lm") +
  facet_wrap(~country + format(round(slope, 2), nsmall = 2)) + 
  labs(title = "Highest 8 Slopes in Increasing Suicide Rates", x = "Year", y = "Suicides Per 100k")
totalHighest
```

![](tutorial_files/figure-markdown_github/increasing-1.png) This is the graph of the top 8 countries with the highest slope. Most of them seem to gather around the .2 to .3 range, but Guyana has a.89 slope while the Republic of Korea has a .59 slope which are both huge for this data. Both could either be explained by culture and how it affects the lives of their daily citizens. Guyana compared to Korea is a bit weird, though. If you look at Korea you can see the points are all scattered around the line, and you can see it's definite trend as it goes up linearly. But for Guyana, the data points are more spread apart, nothing like Korea has. It's possible Guyana could have other types of influences like error in data collection that impact their suicide rates.

``` r
lowest8 <- tail(trends,8)
totalLowest <- lowest8 %>%
  left_join(year_country, by = "country") %>%
  ggplot(aes(x = year, y = suicides_per_100k, color = country)) +
  geom_point() + 
  geom_smooth(method = "lm") +
  facet_wrap(~country + format(round(slope, 2), nsmall = 2)) +
  labs(title = "Lowest 8 Slopes in Decreasing Suicide Rates", x = "Year", y = "Suicides Per 100k")
totalLowest
```

![](tutorial_files/figure-markdown_github/decreasing-1.png) This is the graph of the lowest 8 countries with the lowest slopes. We can note Estonia and Kiribati who have an incredibly low slope of -1.41 and -1.62 respectively. It is important to note that some of these data are incomplete for the entire time period. We see that Kiribati's slope could be affected by its lower than average year count. But unlike Guyana in the last example, most of the data points we see are closely clustered around the regression line made for it's country. There are no huge jumps. Some of these slopes could be affected by a drastic change in quality of life for its citizens, which would explain some graphs that start out very high and end up low.

Summary of Topics Learned
-------------------------

Throughout this project, we have looked at numerous concepts in data science that are essential to basic data analysis. We first covered the setup for using CSV files, making dataframes, and pipelines. From there, we learned how to filter data by specific categories. After we created our custom dataframes, we the plotted our data to find trends and relationships between the data. We utilized a basic plots, such as the boxplot, scatter plot, and facet grids, to help visualize our data.

We saw that data visualization is extremely important to understanding trends in the data. From the visualization stage, we began to see possible trends for us to test via hypothesis testing. We finally covered the linear regression model in machine learning. We made a null hypothesis of no relationship between our variables and an alternate hypothesis of there being a significant relationship between suicide rates and GDP per capita. We then learned how to read the summary of a linear regression model to verify its statistical correctness.

In the final section of this project, we learned how to cleverly maniuplate data to determine the trend of suicide data for each country by plotting the data for each country on it's own graph with it's own slope.

Insight to our Data and Analysis
================================

We have seen the power of data analysis and the linear regression model. For our data, we tried to find a trend between GDP per capita and suicide rates per 100k. We had enough statistical significance to back our claim that there is a correlation between the GDP of a country and their suicide rate. We also found that males have a much higher chance of committing suicide than women, regardless of country of origin. We further saw that the age group of a person has an impact on the suicide rate as well. These variables that have a high impact on the suicide rate are extremely important as they allow us to use them to make more accurate predictions. For example, we can make a linear regression model based on multiple factors, such as age, country of origin, gdp, and sex, among other categories, to then *p**r**e**d**i**c**t* the number of suicides of entities with such properties.
