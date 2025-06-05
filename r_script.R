# Pacotes utilizados
pacman::p_load(dplyr, jsonlite)

# Carregamento dos dados
filmes <- tibble(
    read.csv('tmdb_5000_movies.csv')
)


filmes %>% colnames()

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
filmes$title[3]

levels(ggplot2::cut_number(filmes$runtime,4))

convert(filmes$genres[3])

filmes$genres_corrigido <- corrigir_json(filmes$genres)

print(jsonlite::fromJSON(filmes$genres_corrigido))
