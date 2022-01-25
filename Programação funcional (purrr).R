#Programação funcional (purrr)
library(dplyr)
library(purrr)
library(ggplot2)
install.packages("usethis")
usethis::use_git()
usethis::use_github()

#1. Utilize a função map() para calcular a média de cada coluna da base mtcars.
"1."
map(mtcars, mean)

#2. Use a função map() para testar se cada elemento do vetor letters é uma vogal
#ou não. Faça o resultado ser (a) uma lista de TRUE/FALSE e (b) um vetor de
#TRUE/FALSE.

"2."
testar_vogal <- function(x) { #criando a função
  if(x %in% c("a", "e", "i", "o", "u")) { #criando condições
    return (TRUE)
  } else {
    return (FALSE)
  }
}
map(letters, testar_vogal) #retorna uma lista de true/false

#Faça uma função que divida um número por 2 se ele for par ou multiplique ele por
#2 caso seja ímpar. O resultado do código deve ser um vetor numérico.

"3."
operacao <- function(x) { #criando a função
  if(x %% 2 == 0) { #se o dividendo de 2 for zero (condição)
    return (x / 2) #consequência de ser par
  } else { #se não for par, é ímpar
    return( x * 2) #consequência de ser ímpar
  }
}
operacao(2)

#4. Use a função map() para criar gráficos de dispersão da receita vs orçamento
#para os filmes da base imdb. Os filmes de cada ano deverão compor um gráfico
#diferente.

"4."
imdb <- readr::read_rs("imdb.rds") #lendo a base
fazer_grafico <- function(tab,ano_){ #criando função
  tab %>%
    filter(ano == ano_) %>%
    ggplot(aes(x = orcamento, y = receita)) + #variáveis pedidas
    geom_point()
}
anos <- unique(imdb$ano) #"os filmes de cada ano deverão compor um gráfico"
graficos <- map(anos, fazer_grafico, tab = imdb)
fazer_grafico2 <- function(tab) {
  tab %>%
    ggplot(aes(x = orcamento, y = receita)) +
    geom_point()
}


imdb_com_graficos <- imdb %>%
  group_by(ano) %>%
  tidyr::nest() %>%
  mutate(
    grafico = map(data, fazer_grafico2)
  )
