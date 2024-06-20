library(shiny)              # For creating interactive applications
library(shinydashboard)     # For creating dashboards within the Shiny application
library(shinycssloaders)    # For adding loading indicators
library(httr)               # For making HTTP requests
library(readr)
library(dplyr)              # For data manipulation
library(tibble)             # For data manipulation in tibble format
library(future.apply)       # For running functions in parallel
library(DT)                 # For displaying interactive tables
library(rvest)              # For web scraping
library(visNetwork)         # For visualizing networks
library(bslib)
library(fastmap)
library(shinyBS)
library(shinyjs)


# Define la función para procesar los datos de KEGG y devolverlos
ancestors_kegg <- function(data) {
  relations <- unique(data$ONTOLOGY)
  processed_data <- data.frame()  # Inicializa un dataframe vacío
  
  for (i in seq_along(relations)) {
    relation <- relations[i] 
    
    # Obtener las reacciones e interacciones de la página web
    metabolic_domain <- ""   
    metabolic_subdomain <- ""    
    
    # Leer la página web y extraer los datos de la tabla
    url <- paste0("https://www.genome.jp/dbget-bin/www_bget?pathway:", relation)
    webpage <- read_html(url)
    
    # Realizar el web scraping para obtener la tabla
    tables <- html_nodes(webpage, "table")
    table <- html_table(tables[[1]], fill = TRUE)
    
    # Encontrar la fila que contiene la información relevante
    class_row_index <- which(table[, 1] == "Class")
    if (length(class_row_index) > 0) {
      class_row <- table[class_row_index, ]
      
      # Obtener el valor de interacción y reacción de la misma fila
      class_values <- unlist(strsplit(as.character(class_row), ";"))
      metabolic_domain <- class_values[2]
      metabolic_subdomain <- gsub("BRITE hierarchy", "", class_values[3])
    }
    
    # Crear un dataframe con los datos procesados
    subset_data <- subset(data, ONTOLOGY == relation)
    subset_data$metabolic_domain <- metabolic_domain
    subset_data$metabolic_subdomain <- metabolic_subdomain
    processed_data <- rbind(processed_data, subset_data)
  }
  
  return(processed_data)
}

ancestors_gene_ontologies <- function(ontologies, groups) {
  # Definir una función para obtener los ancestros de una ontología
  get_ancestors <- function(ontology) {
    url <- sprintf("https://www.ebi.ac.uk/QuickGO/services/ontology/go/terms/%s/ancestors?relations=is_a%%2Cpart_of%%2Coccurs_in%%2Cregulates",
                   URLencode(ontology, reserved = TRUE))
    response <- content(GET(url, accept("application/json")), "parsed")
    if (!response$results[[1]]$isObsolete) {
      ancestors <- response$results[[1]]$ancestors
      return(setdiff(ancestors, ontology))
    } else {
      return(NULL)
    }
  }
  
  # Obtener los ancestros para todas las ontologías de manera paralela
  plan(multisession)
  ancestors <- future_lapply(ontologies, get_ancestors)
  
  # Filtrar y combinar los resultados
  data <- data.frame(Group = groups, Ontology = ontologies, Ancestors = sapply(ancestors, toString))
  data <- data[!is.null(data$Ancestors), ]
  
  # Escribir los resultados en un archivo CSV
  write.csv(data, "go_gene_ontologies.csv", row.names = FALSE)
}

# Define function to retrieve children of a GO term
get_children_quickgo <- function(go_id) {
  # Construct URL to retrieve children of a GO term
  base_url <- "https://www.ebi.ac.uk/QuickGO/services/ontology/go/terms/"
  url <- paste0(base_url, go_id, "/children")
  response <- httr::GET(url)
  if (httr::http_type(response) == "application/json") {
    children <- httr::content(response, "parsed")
    return(children)
  } else {
    stop("Error: The response is not in JSON format.")
  }               
}

# Define function to retrieve information of children of a GO term
get_children_info <- function(go_term) {
  children_quickgo <- get_children_quickgo(go_term)
  children_list <- children_quickgo$results
  children_df <- data.frame(id = character(), name = character(), stringsAsFactors = FALSE)
  for (child_info in children_list[[1]]$children) {
    child_id <- child_info$id
    child_name <- child_info$name
    child_df <- data.frame(id = child_id, name = child_name, stringsAsFactors = FALSE)
    children_df <- rbind(children_df, child_df)
  }
  return(children_df)
}


# Define function to find the first matching ancestor
find_first_matching_ancestor <- function(ancestors) {
  for (ancestor in ancestors) {
    if (ancestor %in% children_info_mf$id) {
      return(ancestor)
    } else if (ancestor %in% children_info_bp$id) {
      return(ancestor)
    } else if (ancestor %in% children_info_cc$id) {
      return(ancestor)
    }
  }
  return(NA)
}

# Define function to get the name of the first ancestor
get_first_ancestor_name <- function(ancestor_id) {
  if (ancestor_id %in% children_info_mf$id) {
    return(children_info_mf$name[children_info_mf$id == ancestor_id])
  } else if (ancestor_id %in% children_info_bp$id) {
    return(children_info_bp$name[children_info_bp$id == ancestor_id])
  } else if (ancestor_id %in% children_info_cc$id) {
    return(children_info_cc$name[children_info_cc$id == ancestor_id])
  } else {
    return(NA)
  }
}

renderTable({
  req(loadedData())
  df <- loadedData()
  
  # Check if df is a data frame and has the selected columns
  if (!is.null(df) && is.data.frame(df) &&
      all(c(input$sample, input$up_down, input$ont_description, input$ontology, input$experimental_group, input$ea_value) %in% colnames(df))) {
    
    df <- df[, c(input$sample, input$up_down, input$ont_description, input$ontology, input$experimental_group, input$ea_value, "Disease")]
    colnames(df) <- c("sample", "UP_DOWN", "ONT_DESCRIPTION", "ONTOLOGY", "GROUP", "EA_VALUE", "Disease")
    
    return(df)
  } else {
    return(NULL)  # Return NULL if df is not a data frame or doesn't have selected columns
  }
})

# Define function remove_duplicate_observations before analyze_regulation
remove_duplicate_observations <- function(data) {
  # Encuentra las observaciones duplicadas por "ontology", "group" y "EA_VALUE"
  duplicated_rows <- duplicated(data[, c("ONTOLOGY", "GROUP", "EA_VALUE")]) | duplicated(data[, c("ONTOLOGY", "GROUP", "EA_VALUE")], fromLast = TRUE)
  
  # Cambia GOEA_up_DOWN a "NEUTRAL" en las observaciones duplicadas
  data[duplicated_rows, "UP_DOWN"] <- "NEUTRAL"
  
  # Elimina las filas duplicadas basadas en "ONTOLOGY", "GROUP_1", "GROUP_2", y "EA_VALUE"
  data <- data[!duplicated(data[, c("ONTOLOGY", "GROUP_1", "GROUP_2", "EA_VALUE")]), ]
  
  return(data)
}


# Define function analyze_regulation
analyze_regulation <- function(data) {
  
  # Filtra las observaciones que tienen el mismo "ontology" y "group"
  pairs <- split(data, paste(data$ONTOLOGY, data$GROUP))
  
  # Inicializa un vector para almacenar las observaciones a conservar
  observations_to_keep <- numeric()
  
  # Itera sobre cada par de observaciones
  for (pair in pairs) {
    pair_df <- as.data.frame(pair)  # Convertir el par en un dataframe
    
    if (nrow(pair_df) == 2) {  # Verifica que haya exactamente dos observaciones en el par
      EA_VALUE_1 <- pair_df[["EA_VALUE"]][1]
      EA_VALUE_2 <- pair_df[["EA_VALUE"]][2]
      
      if (EA_VALUE_1 != EA_VALUE_2) {
        observation_to_keep <- ifelse(EA_VALUE_1 > EA_VALUE_2, rownames(pair_df)[1], rownames(pair_df)[2])
        observations_to_keep <- c(observations_to_keep, observation_to_keep)
      } else {
        # Si los EA_VALUE son iguales, conservamos solo una observación
        observations_to_keep <- c(observations_to_keep, rownames(pair_df))
      }
    } else if (nrow(pair_df) == 1) {  # Si solo hay una observación en el par
      observations_to_keep <- c(observations_to_keep, rownames(pair_df)[1])
    }
  }
  
  # Conserva solo las observaciones seleccionadas
  data <- data[rownames(data) %in% observations_to_keep, ]
  return(remove_duplicate_observations(data))
}