# Modelado

Notas de curso [Modeling with Data in the
Tidyverse](https://campus.datacamp.com/courses/modeling-with-data-in-the-tidyverse/introduction-to-modeling?ex=1)
de DataCamp

## Conceptos básicos

y = f(x) + E

* y: resultado de la variable de interés (variable dependiente)
* x: variables predictoras, explicativas o independientes
* f(): una función matemáica que explica la relación de x con y (la señal)
* E: Epsilon, error no sistematico, ruido

## Escenarios

* Modelado explicatorio: x son variables explicativas
* Modelado predictivo: x son variables predictoras

## Exploratory data analysis (EDA)

Pasos

1. Looking at your data
2. Creating visualizations
3. Computing summary statistics

```r
library(dplyr)
library(moderndive)

glimpse(evals)

library(ggplot2)
ggplot(evals, aes(x = score)) +
  geom_histogram(binwidth = 0.25) + 
  labs(x = "teaching score", y = "count")

# Compute mean, median, and standard deviation
evals %>%
  summarize(mean_score = mean(score), 
            median_score = median(score),
            sd_score = sd(score))  
```

## Modelado para predicción

# Histogram of original outcome variable
ggplot(house_prices, aes(x = price)) +
  geom_histogram() + 
  labs(x = "house price", y = "count")