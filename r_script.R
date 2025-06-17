# Pacotes utilizados
pacman::p_load(dplyr, jsonlite)

# Carregamento dos dados
filmes <- tibble(
    read.csv('tmdb_5000_movies.csv')
)

# Filtrando apenas filmes já lançados

filmes <- filmes %>% filter(status == 'Released')

# Apenas Filmes com mais de 100 avaliações
filmes <- filmes %>% filter(vote_count>100)



# Função que extrai os valores Json das colunas
convert <- function(text) {
    # Tenta parsear, retorna NA se falhar
    res <- tryCatch({
        json <- fromJSON(text)
        if (is.data.frame(json)) {
            json$name
        } else if (is.list(json)) {
            sapply(json, function(x) x$name)
        } else {
            NA
        }
    }, error = function(e) NA)
    
    return(res)
}
View(filmes )

# Tratamento dos dados 

# Decada de Lançamento dos filmes
filmes$decada <- as.numeric(format(as.Date(filmes$release_date), "%Y"))
filmes$decada <- floor(filmes$decada/10)*10
filmes$decada <- factor(paste0(filmes$decada, 's'))

# Gênero Principal

filmes$genero_principal <- sapply(filmes$genres, function(x) {
    res <- convert(x)
    if (length(res) > 0) res[1] else NA
}, USE.NAMES = F)

# Gênero Secundário
filmes$genero_secundario <- sapply(filmes$genres, function(x) {
    res <- convert(x)
    if (length(res) > 0) res[2] else NA
}, USE.NAMES = F)
    
# Orçamento (USD)

# Convertendo para milhões de doláres
filmes$budget <- filmes$budget/100000
filmes$orcamento <- ggplot2::cut_number(filmes$budget,4)
levels(filmes$orcamento) <- c('0 - 100 mi', '100 mi - 260 mi',
                           '260 mi - 560 mi', '+ 560 mi')

# Receita em milhões de doláres

filmes$revenue <- filmes$revenue/100000
levels(ggplot2::cut_number(filmes$revenue,4))
# Duração
filmes$duracao <- ggplot2::cut_number(filmes$runtime,5)
levels(filmes$duracao) <- c('Menor que 94', '94 - 102',
                            '102 - 111', '111 - 124',
                            '124 - 248')

# Classificao do idioma original em Inglês e Não Inglês
filmes$idioma_original <- ifelse(filmes$original_language != 'en', 'Não Inglês',
                                 'Inglês')

# Número de Avaliações
filmes$n_avaliacoes <- filmes$


levels(ggplot2::cut_number(filmes$budget,4))

levels(ggplot2::cut_number(filmes$runtime,5))
levels(ggplot2::cut_interval(filmes$popularity,4))

filmes %>% colnames()
sapply(filmes$genres, function(x) {
    res <- convert(x)
    if (length(res) > 0) res[1] else NA
})

filmes$title[3]
# seleção de variáveis
dados <- filmes %>% select(c(title, original_language, ))


table(filmes$original_language) %>% plot()

plot(table(round(filmes$vote_average)))


summary(filmes$vote_count)

filmes$runtime %>% hist()
