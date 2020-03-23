library(tidyverse)
library(stringr)
s <- c("M", "F", "F", " M", " F ", "M")
as.factor(s)
string_aparada <- str_trim(s)
as.factor(string_aparada)
k <- c("São Paulo.", "Rio de Janeiro",
       "Rio Grande do Norte", "Acre")
strk <- str_trim(k)
as.factor(strk)
s <- c("01-Feminino", "02-Masculino", "03-Indefinido")
str_sub(s, start = 4)
str_sub(s, end = -2)
text <- "01-Feminino"
s <- c("Feminino-01", "Masculino-02", "Indefinido-03")
str_sub(s, end = -2)
k <- c('ey', 'eey', 'eyy', 'eeyy', 'ey yy', 'eeey', 'y', 'e', ' ')
str_detect(k, pattern = '[[:upper:]]+')

str_detect(k, pattern = '[c-e]y')

str_replace_all(k, '[e]', 'h')

str_replace_all('1 *+ 2 + 3', fixed('*+'), 'h')
r_core_group <- c(
  'DouglasBates', 'John Chambers', 'Peter Dalgaard',
  'Robert Gentleman', 'Kurt Hornik', 'Ross Ihaka', 'Tomas Kalibera',
  'Michael Lawrence', 'Friedrich Leisch', 'Uwe Ligges', '...'
)

str_extract(r_core_group, '+')

