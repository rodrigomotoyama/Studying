library(tidyverse)
library(stringr)
library(ggplot2)
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

cpf <- 12345678901
cpf_str <- as.character(cpf)
cpf_str %>% str_sub(end = 3)

str_c(str_sub(cpf_str, end = 3), '.',
      str_sub(cpf_str, start = 4, end = 6), '.',
      str_sub(cpf_str, start = 7, end = 9), '-',
      str_sub(cpf_str, start = -2))

s <- c('Alto', 'Médio', 'Baixo', 'minusculo')
for (i in 1:length(s)) {
  s[i] <- str_c('0', as.character(i), ' - ', s[i])
}
s
str_count(01)

str_replace(string = '12345678901',
            pattern = "([0-9]{4})([0-9]{4})([0-9]{1})",
            replacement = "\\2.\\3.\\1")
str_c("0", 1:length(s), " - ", s, sep = "")

s <- c('Casa', 'CASA', 'CaSa', 'CAsa')
str_detect(s, pattern = "[Cc][Aa][Ss][Aa]")

url <- c('/ac/rio-branco/xpto-xyz-1-0-1fds2396-5')
UF <- url %>% str_split(fixed('/'), simplify = T) %>%
  .[1,2] %>% str_to_upper()

city <- url %>% str_split(fixed('/'), simplify = T) %>%
  .[1,3] %>% str_replace("-", " ") %>% str_to_title()
city
str_c(UF, " - ", city)
state <- split_url[[1]][2] %>% str_to_upper() %>% str_c(' - ')

city_vector <- split_url[[1]][3] %>% str_split('-')
city_vector[[1]][1] %>% to_


#5. Crie uma função que retorna TRUE quando a string é um palíndromo e FALSO caso não seja.
s <- "aaaaaaaaa"
str_sub(s, -2, -2) <- '1'
str_sub(s, start = 4)
str_length(s)
palindromo <- function(palavra){
  if (str_length(palavra)==1) {
    saida <- T
  }else if(str_length(palavra)%%2==1){
    metade1 <- palavra %>% str_sub(end = (str_length(palavra)-1)/2)
    metade2 <- palavra %>% str_sub(start = (str_length(palavra)+3)/2)
    nova_metade1 <- ''
    for (i in 1:str_length(metade1)) {
      nova_metade1 <- paste(nova_metade1, str_sub(metade1, -i, -i), sep = '')
    }
    saida <- nova_metade1 == metade2
  }else{
    metade1 <- palavra %>% str_sub(end = (str_length(palavra))/2)
    metade2 <- palavra %>% str_sub(start = (str_length(palavra))/2+1)
    nova_metade1 <- ''
    for (i in 1:str_length(metade1)) {
      nova_metade1 <- paste(nova_metade1, str_sub(metade1, -i, -i), sep = '')
    }
    saida <- nova_metade1 == metade2
  }
  return(saida)
}
palindromo(s)
s %>% str_split("", simplify = T) %>% rev
palindromo2 <- function(palavra){
  palavra %>%
    str_split("", simplify = T) %>%
    rev %>%
    str_c(sep = "", collapse = "") %>%
    str_detect(s)
}
s <- 'abccba'
palindromo2(s)


# 6. De acordo com as regras da língua portuguesa,
# antes de “p” ou “b” devemos usar a letra “m”.
#Em outras palavras, com outras consoantes, usamos a letra “N”.
#Suponha que você tem o seguinte texto com erros gramaticais:
#   texto <- 'Nós chamamos os bonbeiros quando começou o incêmdio.'
# Crie uma função para corrigi-lo.

texto <- 'Nós chamamos os bonbeiros quando começou o incêmdio.'
texto %>% str_sub(1, 1)
texto %>% str_split(pattern = "[nm]", simplify = T)

m_por_n <- function(frase){
  for (i in 1:str_length(frase)) {
    if (str_sub(frase, i, i) == 'n' & str_sub(frase, i+1, i+1) %in% c('p', 'b')) {
        str_sub(frase, i, i) <- 'm'
    }else if (str_sub(frase, i, i) == 'm' &
              !str_detect(str_sub(frase, i+1, i+1), "[aeioupb]")) {
        str_sub(frase, i, i) <- 'n'
    }
  }
  return(frase)
}
m_por_n(texto)

str_detect(str_sub(texto, 8, 8), pattern = "[aeioupb]")
str_sub(texto, 1, 1)

corrige_mn <- function(s) {

  s %>%
    str_replace_all("m([^aeioubp[[:space:]]+])", "n\\1") %>%
    str_replace_all("n([pb])", "m\\1")

}



#7. txt <- "A função mais importante para leitura de dados no `lubridate` é a `ymd`.
#Essa função serve para ler qualquer data de uma `string` no formato `YYYY-MM-DD`.
#Essa função é útil pois funciona com qualquer separador entre os elementos da data
#e também porque temos uma função para cada formato (`mdy`, `dmy`, `dym`, `myd`, `ydm`)."
#Extraia todas as combinações da função ymd, sem repetições.
txt <- "A função mais ymd importante para leitura de dados no `lubridate` é a `ymd`. Essa função serve para ler qualquer data de uma `string` no formato `YYYY-MM-DD`. Essa função é útil pois funciona com qualquer separador entre os elementos da data e também porque temos uma função para cada formato (`mdy`, `dmy`, `dym`, `myd`, `ydm`)."

str_extract_all(txt, "[ymd][ymd][ymd]") %>% as_vector() %>% unique



# 8. Considere as frases abaixo
#
# s <- c(
#   'O produto é muito bom.',
#   'O produto não é bom.',
#   'O produto não é muito bom.',
#   'O produto não é ruim.',
#   'O produto não é não bom.'
# )
# Crie uma regra para identificar se o texto refere-se a um feedback positivo ou negativo sobre o produto (considere não bom = ruim e não ruim = bom). Retorne um vetor lógico que vale TRUE se o feedback é positivo e FALSE caso contrário


s <- c(
  'O produto é bom.',
  'O produto não é bom.',
  'O produto não é muito bom.',
  'O produto é muito bom',
  'O produto não é ruim.',
  'O produto não é não ruim.',
  'O produto não é não bom.'
)
s <- s %>% abjutils::rm_accent()
s
s1 <- s %>% str_replace("nao bom", "ruim")

s1 %>% str_detect("nao") + str_detect(s1, "bom") - str_detect(s1, "ruim")
s %>%
  str_replace("não bom", "ruim") %>%
  str_replace("não ruim", "bom") %>%
  str_replace("muito ", "") %>%
  str_detect(" produto(.*) é bom")

x <- c("a b", "a", "b", "aaa")
str_detect(x, "a")









