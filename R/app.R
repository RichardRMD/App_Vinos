library(shiny)
library(datamods)
library(readxl)
library(tidyverse)
library(DT)
library(shinyWidgets)
library(GGally)
library(openxlsx)
#dendograma
library(rattle)
library(corrplot)
library(factoextra)
library(mclust)
library(FactoMineR)
#Manova
#ggpubr:: ggboxplot()
library(ggpubr)
#library(biotools)

#funciones----
data_statistics <- function(vector_column) {
  
  q1 <- quantile(vector_column, 0.25)
  q3 <- quantile(vector_column, 0.75)
  lim_sup = max(boxplot(vector_column)$stats)
  lim_inf = min(boxplot(vector_column)$stats)
  outliers  = vector_column < lim_inf | vector_column > lim_sup
  not_outliers = !outliers
  
  prom = mean(vector_column)
  desv = sd(vector_column[not_outliers])
  total_out <- length(boxplot(vector_column)$out)
  
  values <- c(q1,q3, prom, desv,lim_sup, lim_inf, total_out)
  return(values)
}
coef_var <- function(x) {
  coef <- sd(x) / mean(x)
  return(coef)
}
get_k <- function(cluster, value) {
  k=1
  for (v in cluster) {
    if (v == value){
      break
    }
    else{
      k=k+1
    }
  }
  return(k)
}

# User interface ----
ui <- fluidPage(
  mainPanel(
    titlePanel("PCA_simple"),
    
    tabsetPanel(id="tabset",
      # Panel load data ----
      tabPanel("Load Data",
               #widgets ----
               
               fileInput("file1",
                         label = h3("File input")),
               
               DT::dataTableOutput('load_excel')
               
      ),
      # Panel inspect data filter----
      tabPanel("Inspect_the_Data",
               fluidRow(
                 column(
                   width = 4,
                   filter_data_ui("filtering", max_height = '700px')
                   
                 ),
                 column(
                   width = 8,
                   progressBar(
                     id = "pbar", value = 100,
                     total = 100, display_pct = TRUE
                   ),
                   DT::dataTableOutput(outputId = "summary")
                 )
               ),
               downloadButton('download_filter', 'Descargar Filtro xlsx')
               
      ),
      # Panel Outliers ----
      tabPanel("Outliers",
               fluidRow(
                 column(
                   width = 6,
                   uiOutput("choose_columns"),
                   plotOutput("outliers"),
                   hr(),
                   tableOutput("statistics")
                 ),
                 column(
                   width = 6,
                   plotOutput("outliers_updated"),
                   hr(),
                   tableOutput("statistics_update")
                   
              
                 )
               ),
               uiOutput("choose_filter"),
               DT::dataTableOutput("Filter_outliers")
                   
                   
                 
               
              
               
      ),
      # Panel correlations ----
      tabPanel("Correlation Plots",
               uiOutput("choose_columns_biplot"),
               hr(),
               plotOutput("corr_plot"),
               hr(),
               p("Summary of correlations"),
               tableOutput("corr_tables")
      ),
      # Panel Compute PCA ----
      tabPanel("Compute PCA",
               p("Elija las columnas de sus datos para incluir en el PCA."),
               p("El PCA se vuelve a calcular automaticamente cada vez que cambia su selecccion"),
               p("Las observaciones (es decir, filas se eliminan automaticamente si contienen valores faltantes."),
               p("Las variables con variacion cero se han eliminado automaticamente porque no son utiles en un PCA."),
               fluidRow(
                 column(
                   width = 6,
                   uiOutput("choose_columns_pca")
                 ),
                 column(
                   width = 6,
                   verbatimTextOutput("pca_details")
                 )
               ),
               tags$hr(),
               p("Seleccione las opciones para el calculo de PCA (aqui estamos usando la funcion prcomp)"),
               radioButtons(inputId = 'center',  
                            label = 'Center',
                            choices = c('Cambiar las variables para que esten centradas en cero'='Yes',
                                        'No cambie las variables'='No'), 
                            selected = 'Yes'),
               
               radioButtons('scale.', 'Scale',
                            choices = c('Escalar variables para tener varianza unitaria'='Yes',
                                        'No escale variables'='No'), 
                            selected = 'Yes')
               
      ), # end  tab
      # PC Plots----
      tabPanel("PC Plots",
               fluidRow(
                 column(
                   width = 6,
                   h2("Scree plot"),
                   p("El grafico de pantalla muestra las varianzas de cada PC y la varianza acumulada ssexplicada por cada PC (en%)"),
                   plotOutput("plot2", height = "300px")
                 ),
                 column(
                   width = 6,
                   h2("Grafico de ejes"),
                   plotOutput("plot_pca_output")
                 )
               ),

               tags$hr(),
               #h2("Grafico de PC: zoom y seleccion de puntos"),
               p("Seleccione la variable de agrupacion"),
               uiOutput("the_grouping_variable"),
               tags$hr(),
               p("Seleccione las PC para trazar"),
               uiOutput("the_pcs_to_plot_x"),
               uiOutput("the_pcs_to_plot_y"),
               tags$hr(),
               
               fluidRow(
                 column(
                   width = 8,
                   #p("Haga clic y arrastre en el primer grafico a continuacion para hacer zoom en una region del grafico O puede ir directamente al segundo grafico a continuacion para seleccionar puntos y obtener mas informacion sobre ellos."),
                   #p("Luego, seleccione puntos en el grafico ampliado a continuacion para obtener mas informacion sobre los puntos."),
                   #p("Puede hacer clic en la pestana 'Calcular PCA' en cualquier momento para cambiar las variables incluidas en el PCA, y luego volver a esta pestana y los graficos se actualizaran automaticamente"),
                   plotOutput ("z_plot1", height = 400,
                               brush = brushOpts(
                                 id = "z_plot1Brush",
                                 resetOnNew = TRUE))
                   
                 ),
                 column(
                   width = 4,
                   uiOutput("view_graph")
                 )
               ),
               
               p("Haga clic y arrastre en el grafico de abajo para seleccionar puntos e inspeccione la tabla de puntos seleccionados a continuacion"),
               
               plotOutput("z_plot2", height = 400,
                          brush = brushOpts(
                            id = "plot_brush_after_zoom",
                            resetOnNew = TRUE)),
               tags$hr(),
               #p("Detalles de los puntos cepillados"),
               #tableOutput("brush_info_after_zoom")
      ),# end  tab 
      
      # Panel Dendrograma ----
      tabPanel("Dendrograma",
               h2("Datos Pcs"),
               fluidRow(
                 column(
                   width = 6,
                   br(),br(),
                   plotOutput("clast_opt_cent"),
                   
                 ),
                 column(
                   width = 6,
                   uiOutput("num_clast_cent"),
                   plotOutput("dend_cent")
                 )
               ),
               fluidRow(
                 column(
                   width = 9,
                   uiOutput("num_clast_pca"),
                   plotOutput("dend_by_PCs")
                 ),
  
                 column(
                   width = 3,
                   br(),br(),
                   plotOutput("clast_opt_pca")
                 )
               ),
               hr(),
               h2("Datos Crudo"),
               fluidRow(
                 column(
                   width = 9,
                   uiOutput("num_clast"),
                   plotOutput("dend_by_panelist")
                 ),
                 column(
                   width = 3,
                   br(),br(),
                   plotOutput("clast_opt_data")
                
                 )
               )
              
      ),#end panel
      
      # Panel Anova----
      tabPanel("Anova",
               uiOutput("selector_columns_anova"),
               plotOutput("boxplot_mean"),
               verbatimTextOutput("anova_details"),
               hr(),
               plotOutput("graph_diff")
               ),# end tab
      
      # Panel Validation anova----
      tabPanel("Validacion Anova",
               p("A partir de los residuos del modelo comprobaremos si el modelo ANOVA es adecuado. Los supuestos que se deben 
                 cumplir son tres: independencia, homocedasticidad y normalidad."),
               h2("Independencia"),
               plotOutput("plot_ind"),hr(),
               h2("Normalidad"),
               fluidRow(
                 column(
                   width = 6,
                   plotOutput("plot_nor")
                 ),
                column(
                  width = 6,
                  p("Valores residuales"),
                  verbatimTextOutput("plot_nor_test"),
                  p("	un valor p aproximado para la prueba. Royston (1995) dice que esto es adecuado para p.value < 0.1")
                )
               ),
               h2("Homocedasticidad")
               
               ),# end tab
      # Panel Manova----
      tabPanel("MANOVA",
               fluidRow(
                 column(
                   width = 3,
                   uiOutput("choose_columns_manova")
                 ),
                 column(
                   width = 9,
                   plotOutput("data_visual")
                 )
               ),
               hr(),
               h2("Estadisticas MANOVA"),
               verbatimTextOutput("stat_manova"),
               hr(),
               h2("Prueba de Varianza y Covarianza"),
               verbatimTextOutput("test_manova")
               ), #end tab----
      tabPanel("Modelo MANOVA",
               h1("Modelo MANOVA"),
               verbatimTextOutput("summary_manova")
               )
      
    )
  )
)

  

# Server logic ----
server <- function(input, output, session) {
  
  the_data_fn <- reactive({
    inFile <- input$file1
    if (is.null(inFile)) return(NULL)
    the_data <- read_excel(inFile$datapath)
    return(the_data)
  })
  
  data <- reactive({
    the_data <- the_data_fn()
    if (is.null(the_data)) return(NULL)
    the_data <- select(the_data, -c(feature_id, panelist_id, product_id))
    wide <- the_data %>%
      pivot_wider(names_from = feature_name, 
                  values_from = score)
    wide
    return(wide)
  })
  
  data_filter <- reactive({
    the_data <- data()
    if (is.null(the_data)) return(NULL)
    the_data <- the_data[,!apply(the_data, MARGIN = 2, function(x) max(x, na.rm = TRUE) == min(x, na.rm = TRUE))]
    return(the_data)
    
  })
    
  # Tab Filter and load----
  res_filter <- filter_data_server(
    
    id = "filtering",
    data = data_filter,
    name = reactive("data_filter"),
    vars = reactive(names(data_filter)),
    drop_ids = TRUE,
    widget_char = "select",
    widget_num = "slider",
    widget_date = "slider",
    label_na = "Missing"
  )
  
  observeEvent(res_filter$filtered(), {
    updateProgressBar(
      session = session, id = "pbar",
      value = nrow(res_filter$filtered()), total = nrow(data())
    )
  })
  
  
  
  output$load_excel <-  DT::renderDataTable({
    the_data <- the_data_fn()
    datatable(the_data, 
              options = list(dom = 'tp'))
  })
  # Filtros Excel
  output$summary <- DT::renderDataTable({
    res_filter$filtered()
  }, options = list(pageLength = 10))
  
  # Button download Filter----
  output$download_filter <- downloadHandler(
    
    filename = function() {
      paste('data-', Sys.Date(), '.xlsx', sep='')
    },
    content = function(con) {
      write.xlsx(res_filter$filtered(), con)
    }
    
  )
  #----
  # Tab Outliers ----
  without_outliers_fn <- reactive({
    
    the_data <- res_filter$filtered()
    the_data_num <- the_data[,sapply(the_data,is.numeric)]
    
    
    for (name_column in colnames(the_data_num)) {
      len_outlier <- length(boxplot(the_data[[name_column]])$out)
      while (len_outlier > 0) {
        
        lim_sup = max(boxplot(the_data[[name_column]])$stats)
        lim_inf = min(boxplot(the_data[[name_column]])$stats)
        outliers  = the_data[[name_column]] < lim_inf | the_data[[name_column]] > lim_sup
        not_outliers = !outliers
        
        the_data[[name_column]][outliers] = round(mean(the_data[[name_column]][not_outliers]),2)
        len_outlier <- length(boxplot(the_data[[name_column]])$out)
        
      }
    }
    
    the_data
    
  })
  
  # Select column feature_name
  output$choose_columns <- renderUI({
    
    the_data <- data_filter()
    the_data_num <- the_data[,sapply(the_data,is.numeric)]
    colnames <- names(the_data_num)
    
    selectInput("select_feature", label = h3("Seleccione feature_name"), 
                choices = colnames, 
                selected = colnames[1])
  })
  
  # chart Outliers
  output$outliers <- renderPlot({
    if (is.null(input$select_feature)) {
      return()
    }
    else {
      the_data <- res_filter$filtered()
      column_select <- input$select_feature
      boxplot(the_data[, column_select],
          horizontal = FALSE)
    }

  })
  
  # update Outliers chart
  output$outliers_updated <- renderPlot({
    the_data <- without_outliers_fn()
    if (is.null(input$select_feature) || is.null(the_data)) {
      return()
    }
    else {
      column_select <- input$select_feature
      boxplot(the_data[[column_select]])
    }
    
  })
  
  # Table statistics
  output$statistics <- renderTable({
    if (is.null(input$select_feature)) {
      return()
    }
    else {
      the_data <- res_filter$filtered()
      column_select <- input$select_feature
      values <- data_statistics(the_data[[column_select]])
      
      df <- data.frame(
        "1st_Quartil" = values[1],
        "3rd_Quartil" = values[2],
        "Promedio" = values[3],
        "Desviacion" = values[4],
        "limSup" = values[5],
        "limInf" = values[6],
        "Total_Out" = values[7],
        row.names = NULL
      )
    }
    
  })
  
  # Table statistics update
  output$statistics_update <- renderTable({
    if (is.null(input$select_feature)) {
      return()
    }
    else {
      the_data <- without_outliers_fn()
      column_select <- input$select_feature
      
      values <- data_statistics(the_data[[column_select]])
      
      df <- data.frame(
        "1st_Quartil" = values[1],
        "3rd_Quartil" = values[2],
        "Promedio" = values[3],
        "Desviacion" = values[4],
        "limSup" = values[5],
        "limInf" = values[6],
        "Total_Out" = values[7],
        row.names = NULL
      )
    }
  })
  
  output$choose_filter <- renderUI({
    #the_data <- res_filter$filtered()
    #the_data2 <- without_outliers_fn()
    selectInput("select_filter", label = h3("Seleccionar filtro con o sin Outliers"), 
                choices = list("Filtro original" = 1, "Filtro sin Outliers" = 2), 
                selected = 1)
  })
  
  output$Filter_outliers <- DT::renderDataTable({
    
    if (is.null(input$select_filter)) {
      the_data <- res_filter$filtered()
    }
    else {
      if (input$select_filter == 1) {
        the_data <- res_filter$filtered()
        
      }
      if (input$select_filter == 2) {
        the_data <- without_outliers_fn()
      }
    }
    the_data
  })
  
  # Tab Correlations----
  output$choose_columns_biplot <- renderUI({
    the_data <- data_filter()
    the_data_num <- the_data[,sapply(the_data,is.numeric)]
    colnames <- names(the_data_num)
    
    # Create the checkboxes and select them all by default
    checkboxGroupInput("columns_biplot", "Choose up to five columns to display on the scatterplot matrix", 
                       choices  = colnames,
                       selected = colnames[1:5],
                       inline = TRUE)
  })
  # corr plot
  output$corr_plot <- renderPlot({

    select_filter <- input$select_filter
    if (is.null(select_filter)) {
      the_data <- res_filter$filtered()
    }
    else {
      if (input$select_filter == 1) {
        the_data <- res_filter$filtered()
      }
      if (input$select_filter == 2) {
        the_data <- without_outliers_fn()
      }
    }
    # Keep the selected columns
    columns_biplot <- input$columns_biplot
    if (is.null(columns_biplot)){return()}
    else {
      the_data_subset_biplot <- the_data[, columns_biplot, drop = FALSE]
      ggpairs(the_data_subset_biplot)
    }
  })
  
  # corr tables
  output$corr_tables <- renderTable({
    filter_select <- input$select_filter
    if (is.null(filter_select)) {
      the_data <- res_filter$filtered()
    }
    else {
      if (input$select_filter == 1) {
        the_data <- res_filter$filtered()
      }
      if (input$select_filter == 2) {
        the_data <- without_outliers_fn()
      }
    }
    the_data_num <- the_data[,sapply(the_data,is.numeric)]
    columns_select <- input$columns_biplot
    if (is.null(columns_select)) {return()}
    else {
      res <- Hmisc::rcorr(as.matrix(the_data_num[, columns_select]))
      cormat <- res$r
      pmat <- res$P
      ut <- upper.tri(cormat)
      df <- data.frame(
        row = rownames(cormat)[row(cormat)[ut]],
        column = rownames(cormat)[col(cormat)[ut]],
        cor  = (cormat)[ut],
        p = pmat[ut]
      )
      with(df, df[order(-cor), ])
      
    }
  })
  
  # Tab Compute PCA----
  output$choose_columns_pca <- renderUI({
    if (is.null(input$select_filter)) {
      the_data <- res_filter$filtered()
      
    } else {
      if (input$select_filter == 1) {
        the_data <- res_filter$filtered()
      }
      if (input$select_filter==2) {
        the_data <- without_outliers_fn()
      }
    }
    the_data_num <- na.omit(the_data[,sapply(the_data,is.numeric)])
    colnames <- names(the_data_num)
    
    
    # Create the checkboxes and select them all by default
    checkboxGroupInput("columns_pca", "Choose columns", 
                       choiceNames  = colnames,
                       choiceValues = colnames,
                       selected = colnames[1:5])
  })
  
  pca_objects <- reactive({
    # Keep the selected columns
    if (is.null(input$select_filter)) {
      the_data <- res_filter$filtered()
      
    } else {
      if (input$select_filter == 1) {
        the_data <- res_filter$filtered()
      }
      if (input$select_filter==2) {
        the_data <- without_outliers_fn()
      }
    }
    columns <- input$columns_pca
    if(is.null(columns)){return()}
    else{
      #split(the_data, product_name)
    
      the_data_subset <- na.omit(the_data[, columns, drop = FALSE])

      

      # from http://rpubs.com/sinhrks/plot_pca
      pca_output <- prcomp(na.omit(the_data_subset), 
                           center = (input$center == 'Yes'), 
                           scale. = (input$scale. == 'Yes'))
      
      # data.frame of PCs
      pcs_df <- cbind(the_data, pca_output$x)
      
      
      return(list(the_data = the_data, 
                  the_data_subset = the_data_subset,
                  pca_output = pca_output, 
                  pcs_df = pcs_df))
      
    }
  })
  
  # PCA details
  output$pca_details <- renderPrint({
    
    print(pca_objects()$pca_output$rotation)
    summary(pca_objects()$pca_output)
    
  })
  
  # PC Plots----
  # choose a grouping variable
  output$the_grouping_variable <- renderUI({
    
    if (is.null(input$select_filter)) {
      the_data <- res_filter$filtered()
      
    } else {
      if (input$select_filter == 1) {
        the_data <- res_filter$filtered()
      }
      if (input$select_filter==2) {
        the_data <- without_outliers_fn()
      }
    }
    selectInput(inputId = "the_grouping_variable", 
                label = "Variable de agrupacion:",
                choices=c("None", names(the_data[1:3])),
                selected = names(the_data[1]))
    
  })
  
  output$the_pcs_to_plot_x <- renderUI({
    pca_output <- pca_objects()$pca_output$x
    
    # drop down selection
    selectInput(inputId = "the_pcs_to_plot_x", 
                label = "X axis:",
                choices= colnames(pca_output), 
                selected = 'PC1')
  })
  
  output$the_pcs_to_plot_y <- renderUI({
    pca_output <- pca_objects()$pca_output$x
    
    # drop down selection
    selectInput(inputId = "the_pcs_to_plot_y", 
                label = "Y axis:",
                choices= colnames(pca_output), 
                selected = 'PC2')
  })
  
  output$view_graph <- renderUI({
    selectInput("select_graph",
                label = "Seleccion de grafico",
                choices = c("Polygono","Centroide"),
                selected = "Centroide")
  })
  
  output$plot2 <- renderPlot({
    pca_output <- pca_objects()$pca_output
    
    if(is.null(pca_output)) {return()}
    else {
      eig = (pca_output$sdev)^2
      variance <- eig*100/sum(eig)
      cumvar <- paste(round(cumsum(variance),1), "%")
      eig_df <- data.frame(eig = eig,
                           PCs = colnames(pca_output$x),
                           cumvar =  cumvar)
      ggplot(eig_df, aes(reorder(PCs, -eig), eig)) +
        geom_bar(stat = "identity", fill = "white", colour = "black") +
        geom_text(label = cumvar, size = 4,
                  vjust=-0.4) +
        theme_bw(base_size = 14) +
        xlab("PC") +
        ylab("Variances") +
        ylim(0,(max(eig_df$eig) * 1.1))
    }
  })
  
  # PC plot
  pca_biplot <- reactive({
    
    select_graph <- input$select_graph
    pcs_df <- pca_objects()$pcs_df
    pca_output <-  pca_objects()$pca_output
    pca_df <- as.data.frame(pca_output$x)
    
    if(is.null(select_graph) || is.null(input$the_grouping_variable) || is.null(pca_objects())) {return()}
    else {
      
      var_expl_x <- round(100 * pca_output$sdev[as.numeric(gsub("[^0-9]", "", input$the_pcs_to_plot_x))]^2/sum(pca_output$sdev^2), 1)
      var_expl_y <- round(100 * pca_output$sdev[as.numeric(gsub("[^0-9]", "", input$the_pcs_to_plot_y))]^2/sum(pca_output$sdev^2), 1)
      labels <- rownames(pca_df)
      grouping <- input$the_grouping_variable
      
      if(grouping == 'None'){
        # plot without grouping variable
        pc_plot_no_groups  <- ggplot(pcs_df, 
                                     aes_string(input$the_pcs_to_plot_x, 
                                                input$the_pcs_to_plot_y
                                     )) +
          
          
          geom_text(aes(label = labels),  size = 5) +
          theme_bw(base_size = 14) +
          coord_equal() +
          xlab(paste0(input$the_pcs_to_plot_x, " (", var_expl_x, "% explained variance)")) +
          ylab(paste0(input$the_pcs_to_plot_y, " (", var_expl_y, "% explained variance)")) 
        # the plot
        pc_plot_no_groups
        
        
      } else {
        
        #Filtrando data frame pcs, para obtener centroide
        centroids <- get_cent()
        
        #data frame objetos, obtenemos los valores de los feature name respecto a los pcs
        df_output <- as.data.frame(pca_objects()$pca_output$rotation)
        x <- df_output[[input$the_pcs_to_plot_x]]
        y <- df_output[[input$the_pcs_to_plot_y]]
        row_name <- row.names(df_output)
        
        x_pcs <- pcs_df[[input$the_pcs_to_plot_x]]
        y_pcs <- pcs_df[[input$the_pcs_to_plot_y]]
        
        
        
        if(select_graph == "Centroide") {
          pc_plot_groups  <- ggplot(df_output,aes(x,y))+
            geom_point(size=5)+
            geom_segment(aes(x=0.0, y=0.0, xend=x, yend=y), show.legend = NA)+
            geom_text(hjust = 'outside', nudge_x = 0.2, label=row_name)+
            geom_point(data=centroids,size=6,
                       aes(x_pcs,y_pcs,
                           color=factor(grouping2)))+
            
            xlab(paste0(input$the_pcs_to_plot_x, " (", var_expl_x, "% explained variance)")) +
            ylab(paste0(input$the_pcs_to_plot_y, " (", var_expl_y, "% explained variance)")) +
            geom_vline(xintercept=0)+ 
            geom_hline(yintercept=0)+
            scale_color_discrete(
              name = toupper(grouping))+
            guides(color = guide_legend(override.aes = list(size = 6)))
          
        }
        else {
          pc_plot_groups  <- ggplot(pcs_df,aes(x_pcs,y_pcs,color=factor(product_name))) +
            stat_ellipse(geom = "polygon", alpha = 0.1) +
            geom_text(aes(label = row.names(pcs_df)),  size = 3)+
            xlab(paste0(input$the_pcs_to_plot_x, " (", var_expl_x, "% explained variance)")) +
            ylab(paste0(input$the_pcs_to_plot_y, " (", var_expl_y, "% explained variance)")) +
            scale_color_discrete(
              name = toupper(grouping))
        }
        
        
        pc_plot_groups
      }
    }
    
    

 
    
    
  })
  
  get_cent <- reactive({
    pcs_df <- pca_objects()$pcs_df
    
    if(is.null(pcs_df) || is.null(input$the_pcs_to_plot_x) || 
       is.null(input$the_pcs_to_plot_y) || is.null(input$the_grouping_variable)) {return()}
    else {
      x_pcs <- pcs_df[[input$the_pcs_to_plot_x]]
      y_pcs <- pcs_df[[input$the_pcs_to_plot_y]]
      grouping2 <- pcs_df[[input$the_grouping_variable]]
      
      df <- data.frame(x_pcs, y_pcs, grouping2)
      centroids <- aggregate(cbind(x_pcs,y_pcs)~grouping2,df,mean)
    }
  })
  
  output$brush_info <- renderTable({
    # the brushing function
    brushedPoints(pca_objects()$pcs_df, input$plot_brush)
  })
  
  # for zooming
  output$z_plot1 <- renderPlot({
    pca_biplot() 
    
  })
  # zoom ranges
  zooming <- reactiveValues(x = NULL, y = NULL)
  
  observe({
    brush <- input$z_plot1Brush
    if (!is.null(brush)) {
      zooming$x <- c(brush$xmin, brush$xmax)
      zooming$y <- c(brush$ymin, brush$ymax)
    }
    else {
      zooming$x <- NULL
      zooming$y <- NULL
    }
  })
  # for zooming
  output$z_plot2 <- renderPlot({
    
    pca_biplot() + coord_cartesian(xlim = zooming$x, ylim = zooming$y) 
    
    
  })

  "output$brush_info_after_zoom <- renderTable({
    # the brushing function
    brushedPoints(pca_objects()$pcs_df, input$plot_brush_after_zoom)
  })"
  

  
  # Tab Dendrograma----
  
  output$num_clast_cent <- renderUI({
    centroids <- get_cent()
    large_data_by_gruop <- 2:(nrow(centroids)-1)
    
    selectInput("num_k_for_cent",
                "Select k",
                choices = large_data_by_gruop,
                selected = large_data_by_gruop[1])
    
  })
  
  output$num_clast <- renderUI({
    data_or_pca <- pca_objects()$pcs_df
    large_data_by_gruop <- 2:length(unique(data_or_pca$product_name))
    
    selectInput("num_k",
                "Select k",
                choices = large_data_by_gruop,
                selected = large_data_by_gruop[1])
    
  })
  
  output$num_clast_pca <- renderUI({
    data_or_pca <- pca_objects()$pcs_df
    large_data_by_gruop <- 2:length(unique(data_or_pca$product_name))
    
    selectInput("num_k_pca",
                "Select k",
                choices = large_data_by_gruop,
                selected = large_data_by_gruop[1])
    
  })
  
  output$clast_opt_cent <- renderPlot({
    centroids <- get_cent()
    if(is.null(centroids)) {return()}
    else {
      e_centroids <- data.frame(centroids, row.names = centroids$grouping2)
      e_centroids <- select(e_centroids, -c(grouping2))
      
      #silhouette", "wss", "gap_stat
      kmax <- length(centroids$grouping)-1
      cluster.optimo <- fviz_nbclust(e_centroids,
                                     hcut,
                                     method = "gap_stat",
                                     k.max = kmax,
                                     verbose = FALSE)+
        labs(title= "Clastering optimo PCA centroide")
                         
        
      cluster.optimo
    }
    
  })
  
  output$clast_opt_pca <- renderPlot({
    pcs_df <- pca_objects()$pcs_df
    pcs_output <- pca_objects()$pca_output$x
    if(is.null(pcs_df) || is.null(pcs_output)) {return()}
    else {
      add_label_pcs <- data.frame(pcs_output,
                                  row.names = make.names(pcs_df$panelist_name, 
                                                         unique = TRUE))
      #Zdata <- scale(add_label_pcs)
      #Estidistica de brecha
      cluster.optimo <- fviz_nbclust(add_label_pcs,
                                     hcut,
                                     method = "gap_stat",
                                     verbose = FALSE,
                                     k.max = 10)+
        labs(title= "Clastering optimo Datos PCA")
      
      cluster.optimo
    }
  })
  
  output$clast_opt_data <- renderPlot({
    filter_select <- input$select_filter
    
    if(is.null(filter_select) || filter_select==1 ) {
      the_data <- res_filter$filtered()
    }
    else {
      the_data <- without_outliers_fn()
    }

    the_data_num <- the_data[,sapply(the_data,is.numeric)]
    Zdata <- scale(the_data_num)

    cluster.optimo <- fviz_nbclust(the_data_num,
                                   hcut,
                                   method = "gap_stat",
                                   verbose = FALSE)+
      labs(title= "Clastering optimo datos crudo")
    cluster.optimo
    
    
  })
  
  output$dend_cent<- renderPlot({

    centroids <- get_cent()
    if (is.null(centroids) || is.null(input$num_k_for_cent)) {return()}
    else {
      e_centroids <- data.frame(centroids, 
                                row.names = centroids$grouping2)
      e_centroids <- select(e_centroids, -c(grouping2))
    
      k <- as.numeric(input$num_k_for_cent)
      
      res.hc <- eclust(e_centroids, "hclust", graph = TRUE, k = k)
      fviz_dend(res.hc, rect = TRUE, show_labels = TRUE )+
        labs(title= "Dendrograma PCA Centroides (tipo de vinos)")
    }
    
  })
  
  output$dend_by_panelist <- renderPlot({
    filter_select <- input$select_filter
    if(is.null(filter_select) || filter_select==1 ) {
      the_data <- res_filter$filtered()
    }
    else {
      the_data <- without_outliers_fn()
    }
    
    last_panelist <- list()
    for (name in the_data$panelist_name) {
      split <- str_split(name, " ")
      last_name <- split[[1]][2]
      last_panelist[[length(last_panelist)+1]] <- last_name
    }
    add_label_pcs <- data.frame(the_data, 
                                row.names = make.names(last_panelist,
                                                       unique = TRUE))
    
    k <- as.numeric(input$num_k)
    
    res.hc <- eclust(add_label_pcs,
                     "hclust",
                     graph = FALSE,
                     k=k)
    fviz_dend(res.hc, rect = TRUE, show_labels = TRUE)+
      labs(title= "Dendrograma datos PCA")
  
    

  })
  
  output$dend_by_PCs <- renderPlot({
    pcs_df <- pca_objects()$pcs_df
    pcs_output <- pca_objects()$pca_output$x
    
    if(is.null(pcs_df) || is.null(pcs_output) || is.null(input$num_k_pca)) {return()}
    else {
      add_label_pcs <- data.frame(pcs_output, 
                                  row.names = make.names(pcs_df$panelist_name,
                                                         unique = TRUE))
      
      k <- as.numeric(input$num_k_pca)
      
      res.hc <- eclust(add_label_pcs,
                       "hclust",
                       graph = FALSE,
                       k=k)
      fviz_dend(res.hc, rect = TRUE, show_labels = TRUE)+
        labs(title= "Dendrograma datos PCA")
    }
  })
  
  # Tab Anova----
  
  output$selector_columns_anova <- renderUI({
    filter_select <- input$select_filter
    if(is.null(filter_select) || filter_select==1 ) {
      the_data <- res_filter$filtered()
    }
    else {
      the_data <- without_outliers_fn()
    }
    the_data_num <- the_data[,sapply(the_data,is.numeric)]
    
    selectInput("select_column_anova",
                "Seleccione Componente",
                choices = colnames(the_data_num),
                selected =colnames(the_data_num)[2])
    
  })
  
  output$boxplot_mean <-renderPlot({
    filter_select <- input$select_filter
    if(is.null(filter_select) || filter_select==1 ) {
      the_data <- res_filter$filtered()
    }
    else {
      the_data <- without_outliers_fn()
    }
    if (is.null(input$select_column_anova)) {return()}
    else{
      var <- the_data[[input$select_column_anova]]
      
      boxplot(var~the_data$product_name)
    }
  })
  
  output$anova_details <- renderPrint({
    filter_select <- input$select_filter
    if(is.null(filter_select) || filter_select==1 ) {
      the_data <- res_filter$filtered()
    }
    else {
      the_data <- without_outliers_fn()
    }
    if (is.null(input$select_column_anova)) {return()}
    else{
      var <- the_data[[input$select_column_anova]]
      
      fm = aov( lm(var ~ the_data$product_name) )
      print(summary(fm))
      
      #Diferencias significativas entre los grupos
      intervals = TukeyHSD(fm)
      print(intervals) 
    }
  })
  
  output$graph_diff <- renderPlot({
    filter_select <- input$select_filter
    if(is.null(filter_select) || filter_select==1 ) {
      the_data <- res_filter$filtered()
    }
    else {
      the_data <- without_outliers_fn()
    }
    if (is.null(input$select_column_anova)) {return()}
    else{
      var <- the_data[[input$select_column_anova]]
      
      fm = aov( lm(var ~ the_data$product_name))
      intervals = TukeyHSD(fm, conf.level = 0.95 )
      plot(intervals)
        
      
    }

    
    
    
  })
  
  # Tab Validation anova----
  
  output$plot_ind <- renderPlot({
    filter_select <- input$select_filter
    if(is.null(filter_select) || filter_select==1 ) {
      the_data <- res_filter$filtered()
    }
    else {
      the_data <- without_outliers_fn()
    }
    if (is.null(input$select_column_anova)) {return()}
    else{
      var <- the_data[[input$select_column_anova]]
      
      fm = aov( lm(var ~ the_data$product_name))
      plot(fm$residuals)
      
      
    }
  })
  
  output$plot_nor <- renderPlot({
    filter_select <- input$select_filter
    if(is.null(filter_select) || filter_select==1 ) {
      the_data <- res_filter$filtered()
    }
    else {
      the_data <- without_outliers_fn()
    }
    if (is.null(input$select_column_anova)) {return()}
    else{
      var <- the_data[[input$select_column_anova]]
      
      fm = aov( lm(var ~ the_data$product_name))
      
      qqnorm(fm$residuals)
      qqline(fm$residuals)
      
    }
  })
  
  output$plot_nor_test <- renderPrint({
    filter_select <- input$select_filter
    if(is.null(filter_select) || filter_select==1 ) {
      the_data <- res_filter$filtered()
    }
    else {
      the_data <- without_outliers_fn()
    }
    if (is.null(input$select_column_anova)) {return()}
    else{
      var <- the_data[[input$select_column_anova]]
      
      fm = aov( lm(var ~ the_data$product_name))
      
      print(summary(fm$residuals))
      shapiro.test(fm$residuals)
      
      
    }
    
    
  })
  # Tab Manova----
  
  output$choose_columns_manova <- renderUI({
    the_data <- data_filter()
    the_data_num <- na.omit(the_data[,sapply(the_data,is.numeric)])
    colnames <- names(the_data_num)
    
    checkboxGroupInput("columns_manova", "Choose columns", 
                       choiceNames  = colnames,
                       choiceValues = colnames,
                       selected = colnames[1:3])
  })
  
  output$data_visual <- renderPlot({
    if(is.null(input$columns_manova)) {return()}
    else {
      filter_select <- input$select_filter
      if(is.null(filter_select) || filter_select==1 ) {
        the_data <- res_filter$filtered()
      }
      else {
        the_data <- without_outliers_fn()
      }
      
      if(length(input$columns_manova) > 10) {
        stop("Permito graficar un maximo de 10 variable (Limite de colores)")
      }
      else{
        ggboxplot(the_data,
                  x="product_name",
                  y=input$columns_manova,
                  merge=T,
                  palette = "jco",)
      }
    }
    
  })
  
  output$stat_manova <- renderPrint({
    filter_select <- input$select_filter
    if(is.null(filter_select) || filter_select==1 ) {
      the_data <- res_filter$filtered()
    }
    else {
      the_data <- without_outliers_fn()
    }
    #the_data_num <- the_data[,sapply(the_data,is.numeric)]
    
    stat_mean <- aggregate(the_data[,input$columns_manova],list(the_data$product_name),mean)
    stat_sd <- aggregate(the_data[,input$columns_manova],list(the_data$product_name),sd)
    
    print("Promedio por grupo")
    print(stat_mean)
    
    print("Desvacion por grupo")
    print(stat_sd)
    
  })
  
  output$test_manova <- renderPrint({
    
    filter_select <- input$select_filter
    if(is.null(filter_select) || filter_select==1 ) {
      the_data <- res_filter$filtered()
    }
    else {
      the_data <- without_outliers_fn()
    }
    
    #test_cov <- boxM(the_data[,input$columns_manova],grouping = the_data$product_name)
    #print(test_cov)
    
  })

  # Tab Model manova----
  output$summary_manova <- renderPrint({
    filter_select <- input$select_filter
    if(is.null(filter_select) || filter_select==1 ) {
      the_data <- res_filter$filtered()
    }
    else {
      the_data <- without_outliers_fn()
    }
    if(is.null(input$columns_manova)){return()}
    else{
      str(input$columns_manova)
      mod<-manova(c(the_data$Intensidad)~the_data$product_name,data=the_data)
      summary(mod)
    }
  })
  
}



# Run app ----
shinyApp(ui, server)