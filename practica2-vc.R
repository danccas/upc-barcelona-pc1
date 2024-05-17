library(rvest)
library(httr)
library(xml2)
library(ggplot2)
library(dplyr)
library(gridExtra)


# URL de la página a scrapear
url <- "https://www.mediawiki.org/wiki/MediaWiki"

# Descargar la página utilizando httr
response <- GET(url)

# Verifica el estado de la respuesta
if (status_code(response) == 200) {
  # Parsea el contenido HTML
  page <- read_html(content(response, "text"))
  # Parsea el contenido XML
  page_xml <- read_html(content(response, "text"))
  # Extrae el título de la página
  title_node <- xml_find_first(page_xml, "//title")
  title <- xml_text(title_node)
  
  # Extrae información utilizando rvest
  #links <- page %>% html_nodes("a") %>% html_attr("href")
  links <- page_xml %>%
    xml_find_all("//a") %>%
    lapply(function(node) {
      text <- xml_text(node)
      url <- xml_attr(node, "href")
      
      # Verificar si la URL es relativa y agregar el dominio
      if (grepl("^//", url)) {
        url <- paste("https:", url, sep = "")
      } else if (grepl("^/", url)) {
        url <- paste("https://www.mediawiki.org", url, sep = "")
      }
      # Verificar si la URL es externa
      if (!grepl("^https?://www.mediawiki.org", url)) {
        status <- "Enlace Local"
      } else {
        # Verificar el estado del enlace
        head_response <- httr::HEAD(url)
        status <- status_code(head_response)
      }
      list(text = text, url = url, status = status)
      
      
      # Pausa entre peticiones
      # Sys.sleep(2) 
      
    })
  # Convertir la lista de enlaces a un data frame
  #links_df <- do.call(rbind, links)
  links_df <- do.call(rbind, lapply(links, as.data.frame))
  
  # Agregar la columna 'visto' que cuenta cuántas veces aparece cada enlace
  links_df <- links_df %>%
    group_by(url) %>%
    mutate(visto = n()) %>%
    ungroup()
  
  links_df <- links_df %>%
    mutate(type = ifelse(grepl("^https?://", url), "Absoluta", "Relativa"))
  
  # Crear el histograma
  histograma <- ggplot(links_df, aes(x = visto, fill = type)) +
    geom_histogram(binwidth = 1, position = "dodge") +
    labs(title = "Frecuencia de Aparición de Enlaces",
         x = "Número de Apariciones",
         y = "Frecuencia",
         fill = "Tipo de URL") +
    theme_minimal()
  
  links_df <- links_df %>% mutate(type = ifelse(grepl("^https?://www.mediawiki.org", url) | grepl("^/", url), "Interno", "Externo"))
  
  # Contar la cantidad de cada tipo de URL
  counts <- links_df %>%
    group_by(type) %>%
    summarise(count = n(), .groups = 'drop')
  
  # Crear el gráfico de barras
  bar_plot <- ggplot(counts, aes(x = type, y = count, fill = type)) +
    geom_bar(stat = "identity") +
    labs(title = "Comparación de Enlaces Internos y Externos",
         x = "Tipo de Enlace",
         y = "Cantidad",
         fill = "Tipo de Enlace") +
    theme_minimal()
  
  status_counts <- links_df %>%
    group_by(status) %>%
    summarise(count = n(), .groups = 'drop')
  
  # Crear el gráfico de tarta
  pie_plot <- ggplot(status_counts, aes(x = "", y = count, fill = status, label = paste0(round(count / sum(count) * 100), "%"))) +
    geom_bar(stat = "identity", width = 1) +
    geom_text(position = position_stack(vjust = 0.5)) +
    coord_polar("y") +
    labs(title = "Porcentajes de Status de Enlaces",
         fill = "Status") +
    theme_minimal() +
    theme(axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          axis.text.x = element_blank(),
          axis.ticks = element_blank(),
          panel.grid = element_blank())
  
  # Unir los gráficos en una sola imagen
  grid.arrange(bar_plot, pie_plot, histograma, nrow = 3)
  
  #print(links_df)
} else {
  cat("Error al descargar la página.")
}