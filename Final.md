### Contributors
Vardhan Mehan
Chanseok Lee
Raymond Bao

# CMSC320 Final Project: 
## *Tutorial on Data Science Pipeline*
## *Impact of GDP on Global Suicide Rates*


## Introduction
Here we analyze if economic indicators can give insight into the well-being of human beings. Specifically, we look at how GDP is correlated with suicide rates on a global level, as well as other factors at play such as age and sex. 

## Dataset 
The dataset we used linked [here](https://www.kaggle.com/russellyates88/suicide-rates-overview-1985-to-2016) pulls from four other datasets linked by time and place, and was built to highlight factors correlated to increased suicide rates among different cohorts globally, across the socio-economic spectrum. 

## Setup
We will be working in RStudio.

Importing libraries: 
```r
library(tidyverse)
library(dplyr)
library(ggplot2)
library(broom)
library(tidyr)
```

Here, we read the csv file we want to use and make a dataframe for it:
```r
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
df
age_group <- df %>%
  group_by(age) %>%
  summarize(population = sum(as.numeric(population)), suicides = sum(as.numeric(suicides_no)), suicides_per_100k = 100000*(suicides/population))
countries 
  
gdp
time_df
sex
age_group
```

## Data Analysis
We will begin with an initial overview of the data. 

### Suicide Rate by Country
```r
countries_plot <- countries %>%
  ggplot(aes(x = factor(country, ordered=TRUE, levels=rev(countries$country)), y = suicides_per_100k, fill=suicides_per_100k)) +
  geom_bar(stat="identity") + 
  coord_flip() +
  scale_y_continuous(breaks=seq(0,45,5)) +
  labs(title = "Country vs Suicides per 100k", x = "Country", y = "Suicides per 100k")
```

### GDP vs Suicide Rate
```r
gdp_plot <- gdp %>%
  ggplot(aes(x = gdp_per_capita, y = suicides_per_100k)) +
  geom_point() +
  geom_smooth() +
  scale_x_continuous(breaks=seq(0,200000,10000)) +
  labs(title = "GDP per Capita vs Suicides per 100k", x = "GDP per Capita", y = "Suicides per 100k")
```
### Global Suicide Rates over Time
```r
time_plot <- time_df %>%
  ggplot(aes(x = year, y = suicides_per_100k)) +
  geom_point() +
  geom_line() +
  labs(title = "Suicides per 100k over time", x = "Time", y = "Suicides per 100k")
```
### Suicide Rates by Gender
```r
sex_plot <- sex %>%
  ggplot(aes(x = sex, y = suicides_per_100k, fill=sex)) +
  geom_bar(stat="identity") +
  labs(title = "Gender vs Suicides per 100k", x = "Gender", y = "Suicides per 100k")
```
### Suicide Rates by Age Group
```r
age_group_plot <- age_group %>%
  ggplot(aes(x = factor(age, ordered=TRUE, levels=c("5-14 years","15-24 years","25-34 years","35-54 years","55-74 years","75+ years")), y= suicides_per_100k, fill=age)) +
  geom_bar(stat="identity") +
  labs(title = "Age Group vs Suicides per 100k", x = "Age group", y = "Suicides per 100k")
```



## Welcome to GitHub Pages

You can use the [editor on GitHub](https://github.com/RaymondBao1642/320-Final-Project/edit/master/README.md) to maintain and preview the content for your website in Markdown files.

Whenever you commit to this repository, GitHub Pages will run [Jekyll](https://jekyllrb.com/) to rebuild the pages in your site, from the content in your Markdown files.

### Markdown

Markdown is a lightweight and easy-to-use syntax for styling your writing. It includes conventions for

```markdown
Syntax highlighted code block

# Header 1
## Header 2
### Header 3

- Bulleted
- List

1. Numbered
2. List

**Bold** and _Italic_ and `Code` text

[Link](url) and ![Image](src)
```

For more details see [GitHub Flavored Markdown](https://guides.github.com/features/mastering-markdown/).

### Jekyll Themes

Your Pages site will use the layout and styles from the Jekyll theme you have selected in your [repository settings](https://github.com/RaymondBao1642/320-Final-Project/settings). The name of this theme is saved in the Jekyll `_config.yml` configuration file.

### Support or Contact

Having trouble with Pages? Check out our [documentation](https://help.github.com/categories/github-pages-basics/) or [contact support](https://github.com/contact) and we’ll help you sort it out.
