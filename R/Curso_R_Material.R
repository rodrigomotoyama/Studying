#test git
library(dplyr)
library(tidyr)
library(tibble)
library(abjData)
library(readr)
library(stringr)
library(janitor)
library(ggplot2)
ggplot(data=mtcars)+
  geom_point(mapping = aes(x = disp, y = mpg, colour = cyl, size = wt))+
  labs(x = "Cilindradas", y = "Milhas/Galão")

ggplot(mtcars, aes(y = mpg, x = disp)) +
  geom_point(color = "red",  size = 2, shape = 6, alpha = 0.5)
mtcars
ggplot(mtcars) +
  geom_boxplot(aes(as.factor(cyl), mpg))+
  labs(x = "X", y = "Y")
ggplot(mtcars) +
  geom_histogram(color = "blue", fill = "red", aes(x = mpg), bins = 20)


ggplot(mtcars, aes(y = mpg, x = disp)) +
  geom_point() +
  geom_smooth()
## `geom_smooth()` using method = 'loess'
ggplot(mtcars, aes(y = mpg, x = disp, colour = as.factor(cyl))) +
  geom_point() +
  geom_smooth(method = "lm")

ggplot(mtcars, aes(y = mpg, x = disp)) +
  geom_point() +
  geom_smooth(method = "lm") +
  facet_wrap(~am)
mtcars

p <- ggplot(mtcars, aes(mpg, disp)) + geom_point()
mpg
# Use vars() to supply variables from the dataset:
p + facet_grid(rows = vars(drv))
p + facet_grid(cols = vars((mtcars[cyl]>4)))
p + facet_grid(vars(drv), vars(cyl))

PlantGrowth

bp <- ggplot(data = PlantGrowth, aes(x = group, y = weight, fill = group)) +
  geom_boxplot()

bp
bp + scale_fill_discrete(breaks=c("trt1", "trt2", "ctrl"))
bp + scale_fill_discrete(breaks=c("trt1", "trt2", "ctrl"))
bp + theme(legend.position=c(0.9, 0.2))
