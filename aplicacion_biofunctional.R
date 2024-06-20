source("/Users/alex/Desktop/Kegg&Go/Biofunctional_1.0/funciones_analisis_datos.R")

# Define UI
ui <- dashboardPage(
  skin = "blue",  # Puedes cambiar esto a cualquier tema disponible: "blue", "black", "purple", "green", "red", "yellow"
  dashboardHeader(title = div(
    style = "font-size: 14px; font-weight: bold; font-family: Times New Roman;",
    "Biofunctional AA"
  )),
  dashboardSidebar(
    sidebarMenu(
      menuItem("HOME", tabName = "inicio", icon = icon("home")),
      menuItem("KEGG", tabName = "kegg", icon = icon("chart-bar"),
               menuSubItem("Filter", tabName = "opcion3_kegg"),
               menuSubItem("Functional Analysis", tabName = "opcion1_kegg"),
               menuSubItem("Network Analysis", tabName = "opcion2_kegg")
      ),
      menuItem("Gene Ontologies", tabName = "gene_ontologies", icon = icon("dna"),
               menuSubItem("Filter", tabName = "opcion3_gene_ontologies"),
               menuSubItem("Functional Analysis", tabName = "opcion1_gene_ontologies"),
               menuSubItem("Network Analysis", tabName = "opcion2_gene_ontologies")
      ),
      menuItem("HELP", tabName = "contact", icon = icon("clipboard-user"))
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "inicio",
              fluidRow(
                box(title = "Welcome",
                    width = 12,
                    solidHeader = TRUE,
                    status = "info",
                    style = "font-size: 14px; font-family: Times New Roman;",
                    p("Welcome to the Biofunctional AA!"),
                    p("The main goal of this application is to facilitate the interpretation of functional analysis of biological data using the hierarchy of metabolic pathways (KEGG) and gene ontologies (GO)."),
                    p("The application allows loading and analyzing gene expression data, identifying the metabolic pathways and gene ontology terms associated with differentially expressed genes."),
                    p("Subsequently, an interactive visualization of the relationship between genes, metabolic pathways, and gene ontology terms is provided, making it easier to interpret the results of functional analysis."),
                    br(),
                    p("KEGG (Kyoto Encyclopedia of Genes and Genomes) is a comprehensive database that integrates information about gene sequences, gene functions and products, as well as the relationships of these elements in molecular networks and biological system information."),
                    p("Its hierarchical structure of pathways provides a systematic representation of biochemical interactions and cellular processes."),
                    p("Understanding the hierarchy of pathways in KEGG is crucial for interpreting the results of functional analysis, as it allows for the identification of metabolic pathways and biological processes associated with differentially expressed genes in a study."),
                    br(),
                    br(),
                    p("Gene Ontology is a standardized classification system that describes the functions of genes and their products in any organism."),
                    p("It is structured into three main ontologies: Biological Process, Molecular Function, and Cellular Component."),
                    p("Understanding the hierarchy of terms in Gene Ontology is essential for interpreting the biological function of genes and their products in the context of genomic or gene expression studies."),
                    br()
                ),
                div(
                  img(src = "https://biysc.org/sites/default/files/ub_facultat_biologia.png", height = 300, width = 600)
                )
              )
      ),
      tabItem(tabName = "kegg",
              fluidRow(
                box(title = "KEGG",
                    width = 12,
                    solidHeader = TRUE,
                    status = "primary"
                )
              )
      ),
      tabItem(tabName = "gene_ontologies",
              fluidRow(
                box(title = "Gene Ontologies",
                    width = 12,
                    solidHeader = TRUE,
                    status = "warning"
                )
              )
      ),
      tabItem(tabName = "contact",
              fluidRow(
                box(title = "Contact Information", width = 12, status = "info",
                    "Developed by Alejandro Rodríguez & Antonio Monleón. Section of Statistics. Department of Genetics, Microbiology and Statistics. UB. For any inquiries or support, please contact us at:",
                    br(),
                    br(),
                    "Alex: alejandro.rodriguez@alum.esci.upf.edu",
                    br(),
                    "Toni: amonleong@ub.edu"
                )
              )
      ),
      tabItem(tabName = "opcion3_kegg",
              fluidRow(
                box(title = "Filter",
                    width = 12,
                    solidHeader = TRUE,
                    status = "success",
                    fileInput("file_filter_kegg", "Upload File"),
                    uiOutput("column_selectors"),
                    textInput("diseases", "Enter Diseases (comma-separated):", value = ""),
                    downloadButton("downloadFile_filter_kegg", "Download Processed File")
                ),
                box(
                  withSpinner(
                    dataTableOutput("table_filter_kegg")
                  ),
                  br(),
                  actionButton("clear_filter_kegg", "Clear Data", icon = icon("trash"))
                )
              )
      ),
      tabItem(tabName = "opcion1_kegg",
              fluidRow(
                box(title = "Functional Analysis",
                    width = 12,
                    solidHeader = TRUE,
                    status = "success",
                    fileInput("file_kegg", "Upload File"),
                    downloadButton("downloadFile_kegg", "Download Processed File"),
                    br(),
                    br(),
                    br(),
                    withSpinner(
                      dataTableOutput("table_kegg")
                    ),
                    br(),
                    actionButton("clear_kegg", "Clear Data", icon = icon("trash"))
                )
              )
      ),
      tabItem(tabName = "opcion2_kegg",
              fluidRow(
                box(title = "Network Analysis Filters", width = 3, status = "danger",
                    fileInput("file_kegg_network", "Upload File"),
                    selectInput("sample_kegg", "Sample", choices = NULL, multiple = FALSE),
                    selectInput("disease_kegg", "Disease", choices = NULL, multiple = FALSE),
                    selectInput("group_by_kegg", "Group by:", choices = NULL, multiple = FALSE),
                    downloadButton("download_network_kegg", "Download Network"),
                    br(),
                    actionButton("clear_kegg_network", "Clear Data", icon = icon("trash"))
                ),
                box(title = "Network", width = 9, status = "danger",
                    withSpinner(visNetworkOutput("network_kegg", width = "100%", height = "800px")),
                    box(title = tagList("AI conclusions prompt", icon("question-circle", id = "helpIcon")), width = 12, status = "info",
                        textOutput("text_kegg"))
                )
              )
      ),
      tabItem(tabName = "opcion3_gene_ontologies",
              fluidRow(
                box(title = "Filter",
                    width = 12,
                    solidHeader = TRUE,
                    status = "success",
                    fileInput("file_filter_gene_ontologies", "Upload File"),
                    uiOutput("column_selectors_go"),
                    textInput("diseases_go", "Enter Diseases (comma-separated):", value = ""),
                    downloadButton("downloadFile_filter_go", "Download Processed File")
                ),
                box(
                  withSpinner(
                    dataTableOutput("table_filter_go")
                  ),
                  br(),
                  actionButton("clear_filter_go", "Clear Data", icon = icon("trash"))
                )
              )
      ),
      tabItem(tabName = "opcion1_gene_ontologies",
              fluidRow(
                box(title = "Functional Analysis",
                    width = 12,
                    solidHeader = TRUE,
                    status = "success",
                    fileInput("file_gene_ontologies", "Upload File"),
                    downloadButton("downloadFile_gene_ontologies", "Download Processed File"),
                    br(),
                    br(),
                    br(),
                    withSpinner(
                      dataTableOutput("table_gene_ontologies")
                    ),
                    br(),
                    actionButton("clear_go", "Clear Data", icon = icon("trash"))
                )
              )
      ),
      tabItem(tabName = "opcion2_gene_ontologies",
              fluidRow(
                box(title = "Network Analysis Filters", width = 3, status = "danger",
                    fileInput("file_go_network", "Upload File"),
                    selectInput("sample_go", "Sample", choices = NULL, multiple = FALSE),
                    selectInput("ont_description", "Ontology Description", choices = NULL, multiple = FALSE),
                    selectInput("disease_go", "Disease", choices = NULL, multiple = FALSE),
                    downloadButton("download_network_go", "Download Network"),
                    br(),
                    actionButton("clear_go_network", "Clear Data", icon = icon("trash"))
                ),
                box(title = "Network", width = 9, status = "danger",
                    withSpinner(visNetworkOutput("network_go", width = "100%", height = "800px")),
                    box(title = tagList("AI conclusions prompt", icon("question-circle", id = "helpIcon2")), width = 12, status = "info",
                        textOutput("text_go"))
                )
              )
      )
    )
  )
)




#Define server logic
server <- function(input, output, session) {
  # Render popovers
  output$helpIcon <- renderUI({
    bsPopover(id = "popover_helpIcon", title = "AI Conclusions Prompt",
              content = "This section provides insights and conclusions drawn by the AI based on the analysis performed. It helps in understanding the significant findings and their implications.",
              placement = "right", trigger = "click")
  })
  
  output$helpIcon2 <- renderUI({
    bsPopover(id = "popover_helpIcon2", title = "AI Conclusions Prompt",
              content = "This section provides insights and conclusions drawn by the AI based on the analysis performed. It helps in understanding the significant findings and their implications.",
              placement = "right", trigger = "click")
  })
  
  # Activate popovers
  observe({
    shinyjs::runjs("
      $('#helpIcon').click(function() {
        $('#popover_helpIcon').popover('toggle');
      });
      
      $('#helpIcon2').click(function() {
        $('#popover_helpIcon2').popover('toggle');
      });
    ")
  })
  
  observeEvent(input$file_filter_kegg, {
    loadedData <- reactive({
      req(input$file_filter_kegg)
      df <- read.csv(input$file_filter_kegg$datapath)
      df$Disease <- NA
      
      if (nchar(input$diseases) > 0) {
        for (disease in unlist(strsplit(input$diseases, ","))) {
          df$Disease <- ifelse(grepl(disease, df$GROUP), disease, df$Disease)
        }
      }
      
      # Asegúrate de incluir la columna ONTOLOGY en el conjunto de datos
      df <- df[, c("ONTOLOGY", colnames(df))]
      
      df
    })
    
    
    
    # Render column selectors based on loaded data
    output$column_selectors <- renderUI({
      req(loadedData())
      colnames <- colnames(loadedData())
      tagList(
        selectInput("sample", "Sample Column:", choices = colnames),
        selectInput("up_down", "Up/Down Column:", choices = colnames),
        selectInput("ont_description", "Ontology Description Column:", choices = colnames),
        selectInput("ontology_kegg_2", "Ontology Column:", choices = colnames),
        selectInput("experimental_group", "Experimental Group Column:", choices = colnames),
        selectInput("ea_value", "EA Value Column:", choices = colnames)
      )
    })
    
    # Render filtered table based on loaded data and selected columns
    output$table_filter_kegg <- renderTable({
      req(loadedData())
      df <- loadedData()
      if (!is.null(df) && is.data.frame(df)) {  # Add this check
        df <- df[, c(input$sample, input$up_down, input$ont_description, input$ontology_kegg_2, input$experimental_group, input$ea_value, "Disease")]
      }
      df
    })
    
    
    # Download filtered data
    output$downloadFile_filter_kegg <- downloadHandler(
      filename = function() {
        paste("filtered_data_", Sys.Date(), ".csv", sep = "")
      },
      content = function(file) {
        df_filtered <- loadedData()
        df_filtered <- df_filtered[, c(input$sample, input$up_down, input$ont_description, input$ontology_kegg_2, input$experimental_group, input$ea_value, "Disease")]
        write.csv(df_filtered, file, row.names = FALSE)
      }
    )
  })
  # Lógica para procesar el archivo de KEGG
  observeEvent(input$file_kegg, {
    req(input$file_kegg)
    
    filename <- input$file_kegg$name
    data_kegg <- read.csv(input$file_kegg$datapath)
    
    # Llama a la función para procesar los datos de KEGG
    processed_kegg_data <- ancestors_kegg(data_kegg)
  
    # Generar la respuesta para descargar el archivo procesado
    output$downloadFile_kegg <- downloadHandler(
      filename = function() {
        "processed_kegg_data.csv"  # Especifica el nombre del archivo correctamente
      },
      content = function(file) {
        write.csv(processed_kegg_data, file, row.names = FALSE)
      }
    )
    
    # Mostrar la tabla procesada de KEGG
    output$table_kegg <- renderDataTable({  
      processed_kegg_data[, c("ONTOLOGY","ONT_DESCRIPTION", "sample", "GROUP_1","GROUP_2","UP_DOWN", "Disease","metabolic_domain","metabolic_subdomain")]
    })
  })
  # Lógica para limpiar los datos de análisis funcional de KEGG
  observeEvent(input$clear_kegg, {
    # Eliminar la tabla y el archivo cargado
    output$table_kegg <- renderDataTable({ NULL })
    unlink(input$file_kegg$datapath)  # Eliminar el archivo cargado
  })
  
  # Lógica para procesar el archivo de Gene Ontologies
  observeEvent(input$file_gene_ontologies, {
    req(input$file_gene_ontologies)  # Asegurar que se haya cargado un archivo
    
    original_data <- read.csv(input$file_gene_ontologies$datapath)
    # Llamar a la función ancestors para Gene Ontologies
    
    processed_DATA <- ancestors_gene_ontologies(original_data$ONTOLOGY, original_data$GROUP)
    
    # Leer el archivo procesado
    processed_data <- read.csv("/Users/alex/Downloads/GANGO_5/R/go_gene_ontologies.csv")
    colnames(processed_data) <- c("GROUP", "ONTOLOGY", "ANCESTORS")
    # Modificar FILE.GOEA.ANCESTORS
    processed_data$ANCESTORS <- lapply(processed_data$ANCESTORS, function(x) {
      x <- unlist(strsplit(x, ", "))
      x <- x[!x %in% c("GO:0003674", "GO:0005575", "GO:0008150")]
      x <- trimws(x)  
      paste(x, collapse = ", ")
    })
    
    # Realizar el merge por la columna "ONTOLOGY"
    original_data <- merge(original_data, processed_data[, c("ONTOLOGY", "ANCESTORS")], by = "ONTOLOGY", all.x = TRUE)
    
    # Summarise
    original_data <- original_data %>%
      group_by(GROUP, ONTOLOGY, UP_DOWN) %>%
      summarise(
        ONT_DESCRIPTION = unique(ONT_DESCRIPTION),
        Disease = unique(Disease),
        EA_VALUE = mean(EA_VALUE),
        GROUP_1 = toString(unique(GROUP_1)),
        GROUP_2 = toString(unique(GROUP_2)),
        sample = toString(unique(sample)),
        UP_DOWN = unique(UP_DOWN),
        ANCESTORS = toString(unique(ANCESTORS)),
        .groups = 'drop'
      ) %>%
      filter(ANCESTORS != "NA" & ANCESTORS != "")
    
  
    # Get children information for different ontology types
    children_info_mf <- get_children_info("GO:0003674")
    children_info_bp <- get_children_info("GO:0008150")
    children_info_cc <- get_children_info("GO:0005575")
    
    # Apply function to find the first matching ancestor to original_data
    original_data <- original_data %>%
      mutate(first_ancestor = sapply(strsplit(ANCESTORS, ", "), find_first_matching_ancestor))
    
    # Apply function to get the name of the first ancestor to original_data
    original_data <- original_data %>%
      mutate(first_ancestor_name = sapply(first_ancestor, get_first_ancestor_name))
    
    # Definir la función downloadHandler para descargar el archivo original_data
  output$downloadFile_gene_ontologies <- downloadHandler(
    filename = function() {
      "go.csv"
    },
    content = function(file) {
      write.csv(original_data, file, row.names = FALSE)
    }
  )
    
    # Mostrar la tabla procesada en la interfaz de usuario
    output$table_gene_ontologies <- renderDataTable({  # Usar renderDataTable en lugar de renderTable
      original_data
    })
  })
  
  # Lógica para limpiar los datos de análisis funcional de KEGG
  observeEvent(input$clear_go, {
    # Eliminar la tabla y el archivo cargado
    output$table_gene_ontologies <- renderDataTable({ NULL })
    unlink(input$file_gene_ontologies$datapath)  # Eliminar el archivo cargado
  })
  
 
  # Lógica para procesar el archivo de KEGG
  observeEvent(input$file_kegg_network, {
    
    req(input$file_kegg_network)
    dataset <- read.csv(input$file_kegg_network$datapath, stringsAsFactors = FALSE)
    dataset <- analyze_regulation(dataset) 
    # Aquí se añade la parte para actualizar los selectInput
    observe({
      # Update the selectInput widgets based on the loaded data
      updateSelectInput(session, "sample_kegg", choices = unique(dataset$sample))
      updateSelectInput(session, "disease_kegg", choices = unique(dataset$Disease))
      updateSelectInput(session, "group_by_kegg", choices = c("Domain", "Subdomain", "Relation"))  
    })
    
    observeEvent(list(input$sample_kegg, input$disease_kegg, input$group_by_kegg), {
      req(input$sample_kegg, input$disease_kegg, input$group_by_kegg)  # Require all input values to be non-null
      
      # Filter the data based on the selected inputs
      filtered_data <- dataset %>%
        filter(sample == input$sample_kegg,
               Disease %in% input$disease_kegg)
      
      # Extract unique ontologies from the filtered data
      unique_ontologies <- unique(filtered_data$ONTOLOGY)
      
      # Define grouping column based on user selection
      group_by_col <- switch(input$group_by_kegg,
                             "Domain" = "metabolic_domain",
                             "Subdomain" = "metabolic_subdomain",
                             "Relation" = "ONT_DESCRIPTION")
      # Initialize empty lists to store nodes and edges
      all_nodes <- list()
      all_edges <- list()
      
      # Iterate over each ontology
      for (ontology in unique_ontologies) {
        # Filter data for the current ontology
        ontology_data <- filtered_data %>% filter(ONTOLOGY == ontology)
        
        # Get all unique groups between GROUP_1 and GROUP_2 for the current ontology
        all_groups <- unique(c(ontology_data$GROUP_1, ontology_data$GROUP_2))
        
        # Create nodes for all unique groups
        nodes <- data.frame(id = paste(ontology, all_groups, sep = "_"), label = ontology, group = all_groups,
                            Domain = unique(ontology_data$metabolic_domain),
                            Subdomain = unique(ontology_data$metabolic_subdomain),
                            Relation = unique(ontology_data$ONT_DESCRIPTION)) # Assigning group as color
        
        arrow_types <- ifelse(ontology_data$UP_DOWN == "UP", "to",
                              ifelse(ontology_data$UP_DOWN == "DOWN", "from", "none"))
        
        # Create edges based on filtered data for the current ontology
        edges <- data.frame(from = paste(ontology, ontology_data$GROUP_1, sep = "_"),
                            to = paste(ontology, ontology_data$GROUP_2, sep = "_"),
                            arrows = arrow_types,
                            color = ifelse(ontology_data$UP_DOWN == "UP", "blue",
                                           ifelse(ontology_data$UP_DOWN == "DOWN", "red", "black")))
        
        # Append nodes and edges to the lists
        all_nodes[[ontology]] <- nodes
        all_edges[[ontology]] <- edges
      }
      
      # Combine all nodes and edges
      all_nodes_combined <- do.call(rbind, all_nodes)
      all_edges_combined <- do.call(rbind, all_edges)
      
      # Render the network visualization
      output$network_kegg <- renderVisNetwork({
        visNetwork(all_nodes_combined, edges = all_edges_combined, main = "Sample", width = "100%", height = "100%") %>%
          visNodes(color = list(border = "black"), shadow = TRUE) %>%  # Setting node border color and adding shadow
          visEdges(arrows = "to") %>%
          visGroups(groupname = "group", color = list(background = rainbow(length(unique(all_nodes_combined$group))))) %>%  # Adding color legend for groups
          visLegend(main = "Groups", useGroups = TRUE) %>%  # Displaying group legend
          visOptions(highlightNearest = TRUE, selectedBy = input$group_by_kegg)  
      })
      output$download_network_kegg <- downloadHandler(
        filename = function() {
          paste('network-', Sys.Date(), '.html', sep='')
        },
        content = function(con) {
          visNetwork(all_nodes_combined, edges = all_edges_combined, main = "Sample", width = "100%", height = "100%") %>%
            visNodes(color = list(border = "black"), shadow = TRUE) %>%  # Setting node border color and adding shadow
            visEdges(arrows = "to") %>%
            visGroups(groupname = "group", color = list(background = rainbow(length(unique(all_nodes_combined$group))))) %>%  # Adding color legend for groups
            visLegend(main = "Groups", useGroups = TRUE) %>%  # Displaying group legend
            visOptions(highlightNearest = TRUE, selectedBy = input$group_by_kegg) %>% 
            visSave(con)
        }
      )
      output$text_kegg <- renderText({
        top_ontologies <- filtered_data %>%
          arrange(desc(EA_VALUE)) %>%
          head(10)
        
        intro_text <- "Write an exhaustive analysis focusing on biological experimental conclusions to learn how the diseases are doing on a Gene Ontology (GO) enrichment dataset for CU or EC, taking the main ideas for all the dataset without specifying in each of them. The dataset includes enrichment information for the 15 most enriched ontologies in the dataset. Each entry in the dataset has the following attributes:

        Ontology: The KEGG identifier.
        Sample: Sample used for enrichment analysis .
        Description: Description of the kegg term.
        Disease: Disease studied.
        Group_1: First group for comparison.
        Group_2: Second group for comparison.
        ea_value: Enrichment value.
        first_ancestor: The most general ancestor node in the GO hierarchy.

        So here are the gene ontologies to analyze:\n\n"
        
        
        ontology_list <- character(nrow(top_ontologies))
        
        for (i in seq_len(nrow(top_ontologies))) {
          ontology_info <- paste(
            "- Ontology:", top_ontologies$ONTOLOGY[i],
            "| Sample:", top_ontologies$sample[i],
            "| Description:", top_ontologies$ONT_DESCRIPTION[i],
            "| Disease:", top_ontologies$Disease[i],
            "| Group_1:", top_ontologies$GROUP_1[i],
            "| Group_2:", top_ontologies$GROUP_2[i],
            "| ea_value:", top_ontologies$EA_VALUE[i],
            "| metabolic domain:", top_ontologies$metabolic_domain[i],
            "| metabolic subdomain:", top_ontologies$metabolic_subdomain[i],
            sep = " "
          )
          
          # Append each ontology_info to ontology_list
          ontology_list[i] <- ontology_info
        }
        
        # Combine all ontology_list elements into a single string separated by newline characters
        ontology_list_text <- paste(ontology_list, collapse = "\n")
        
        # Combine the introductory text and the ontology list text
        final_text <- paste(intro_text, ontology_list_text, sep = "")
        
        final_text  # Return the final text to renderText
      })
    })
  })
  
  # Lógica para limpiar los datos de análisis funcional de KEGG
  observeEvent(input$clear_kegg_network, {
    # Eliminar la tabla y el archivo cargado
    output$network_kegg <- renderVisNetwork({ NULL })
    unlink(input$file_kegg_network$datapath)  # Eliminar el archivo cargado
  })
  

  
  observeEvent(input$file_go_network, {
    req(input$file_go_network)
    df_4 <- read.csv(input$file_go_network$datapath, stringsAsFactors = FALSE)
    df_4 <- analyze_regulation(df_4)
    
    observe({
      updateSelectInput(session, "sample_go", choices = unique(df_4$sample))
      updateSelectInput(session, "ont_description", choices = unique(df_4$ONT_DESCRIPTION))
      updateSelectInput(session, "disease_go", choices = unique(df_4$Disease))
    })
    
    observeEvent(list(input$sample_go, input$ont_description, input$disease_go), {
      req(input$sample_go, input$ont_description, input$disease_go)
      
      filtered_data <- df_4 %>%
        filter(sample == input$sample_go,
               ONT_DESCRIPTION == input$ont_description,
               Disease %in% input$disease_go)
      
      unique_ontologies <- unique(filtered_data$ONTOLOGY)
      
      # Initialize empty lists to store nodes and edges
      all_nodes <- list()
      all_edges <- list()
      
      # Iterate over each ontology
      for (ontology in unique_ontologies) {
        # Filter data for the current ontology
        ontology_data <- filtered_data %>% filter(ONTOLOGY == ontology)
        
        # Get all unique groups between GROUP_1 and GROUP_2 for the current ontology
        all_groups <- unique(c(ontology_data$GROUP_1, ontology_data$GROUP_2))
        
        # Create nodes for all unique groups
        nodes <- data.frame(id = paste(ontology, all_groups, sep = "_"), label = ontology, group = all_groups,
                            first_anc = unique(ontology_data$first_ancestor_name))
        
        # Create a vector of arrow types based on UP_DOWN values
        arrow_types_goea <- ifelse(ontology_data$UP_DOWN == "UP", "to",
                                   ifelse(ontology_data$UP_DOWN == "DOWN", "from", "none"))
        
        # Create edges based on filtered data for the current ontology
        edges <- data.frame(from = paste(ontology, ontology_data$GROUP_1, sep = "_"),
                            to = paste(ontology, ontology_data$GROUP_2, sep = "_"),
                            arrows = arrow_types_goea,
                            color = ifelse(ontology_data$UP_DOWN == "UP", "blue",
                                           ifelse(ontology_data$UP_DOWN == "DOWN", "red", "black")))
        
        # Append nodes and edges to the lists
        all_nodes[[ontology]] <- nodes
        all_edges[[ontology]] <- edges
      }
      
      # Combine all nodes and edges
      all_nodes_combined <- do.call(rbind, all_nodes)
      all_edges_combined <- do.call(rbind, all_edges)
      
      # Render the network visualization
      output$network_go <- renderVisNetwork({
        visNetwork(all_nodes_combined, edges = all_edges_combined, main = "Sample", width = "100%", height = "100%") %>%
          visNodes(color = list(border = "black"), shadow = TRUE) %>%  # Setting node border color and adding shadow
          visEdges(arrows = "to") %>%
          visGroups(groupname = "group", color = list(background = rainbow(length(unique(all_nodes_combined$group))))) %>%  # Adding color legend for groups
          visLegend(main = "Groups", useGroups = TRUE) %>%  # Displaying group legend
          visOptions(highlightNearest = TRUE, selectedBy = "first_anc")  
      })
      
      output$text_go <- renderText({
        top_ontologies <- filtered_data %>%
          arrange(desc(EA_VALUE)) %>%
          head(20)
        
        intro_text <- "Write an exhaustive analysis focusing on biological experimental conclusions to learn how the diseases are doing on a Gene Ontology (GO) enrichment dataset for CU or EC, taking the main ideas for all the dataset without specifying in each of them. The dataset includes enrichment information for the 15 most enriched ontologies in the dataset. Each entry in the dataset has the following attributes:

Ontology: The GO identifier.
Sample: Sample used for enrichment analysis .
Description: Description of the GO term.
Disease: Disease studied.
Group_1: First group for comparison.
Group_2: Second group for comparison.
ea_value: Enrichment value.
first_ancestor: The most general ancestor node in the GO hierarchy.

So here are the gene ontologies to analyze:\n\n"

        
        ontology_list <- character(nrow(top_ontologies))
        
        for (i in seq_len(nrow(top_ontologies))) {
          ontology_info <- paste(
            "- Ontology:", top_ontologies$ONTOLOGY[i],
            "| Sample:", top_ontologies$sample[i],
            "| Description:", top_ontologies$ONT_DESCRIPTION[i],
            "| Disease:", top_ontologies$Disease[i],
            "| Group_1:", top_ontologies$GROUP_1[i],
            "| Group_2:", top_ontologies$GROUP_2[i],
            "| ea_value:", top_ontologies$EA_VALUE[i],
            "| first_ancestor:", top_ontologies$first_ancestor_name[i],
            sep = " "
          )
          
          # Append each ontology_info to ontology_list
          ontology_list[i] <- ontology_info
        }
        
        # Combine all ontology_list elements into a single string separated by newline characters
        ontology_list_text <- paste(ontology_list, collapse = "\n")
        
        # Combine the introductory text and the ontology list text
        final_text <- paste(intro_text, ontology_list_text, sep = "")
        
        final_text  # Return the final text to renderText
      })
    })
  })
    output$download_network_go <- downloadHandler(
      filename = function() {
        paste('network-', Sys.Date(), '.html', sep='')
      },
      content = function(con) 
        
        visNetwork(nodes, edges = filtered_edges, main = "Sample", width = "100%") %>%
        visNodes(color = list(background = "white", border = "black"), size = "value") %>%
        visGroups(groupname = "group", color = list(border = "black", background = rainbow(length(group_names), start = 0, end = 1)), legend = TRUE) %>%
        visPhysics(
          enabled = TRUE,
          repulsion = list(nodeDistance = 1)
        ) %>%
        visOptions(highlightNearest = TRUE, selectedBy = "first_anc") %>%
        visLegend(main = "Group", useGroups = TRUE) %>% 
        visSave(con)
    )
    
  
  # Lógica para limpiar los datos de análisis funcional de KEGG
  observeEvent(input$clear_go_network, {
    # Eliminar la tabla y el archivo cargado
    output$network_go <- renderDataTable({ NULL })
    unlink(input$file_go_network$datapath)  # Eliminar el archivo cargado
  })
}

# Run the application
shinyApp(ui = ui, server = server)
getwd()
data<- read.csv("/Users/alex/Desktop/Kegg&Go/Gene_ONTOLOGY/dataset.csv", header = TRUE)
names(data)[names(data) == "GOEA_up_DOWN"] <- "UP_DOWN"
write_csv(data,file = "/Users/alex/Desktop/Kegg&Go/Gene_ONTOLOGY/dataset.csv" )
processed_DATA <- ancestors_gene_ontologies(data$ONTOLOGY, data$GROUP)


