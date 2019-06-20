
#' @param input,output,session standards \code{shiny} server arguments.
#' @param data    a \code{reactiveValues} with at least a slot \code{data} containing a \code{data.frame}
#'  to use in the module. And a slot \code{name} corresponding to the name of the \code{data.frame}.
#' @param dataModule Data module to use, choose between \code{"GlobalEnv"}
#'  or \code{"ImportFile"}.
#' @param sizeDataModule Size for the modal window for selecting data.
#'
#' @export
#' 
#' @rdname module-esquisse
#'
#' @importFrom shiny callModule reactiveValues observeEvent renderPrint
#'  renderPlot stopApp plotOutput showNotification isolate reactiveValuesToList
#' @importFrom ggplot2 ggplot_build ggsave
#' @importFrom rlang expr_deparse
#'
esquisserServer <- function(input, output, session, data = NULL, dataModule = c("GlobalEnv", "ImportFile"), sizeDataModule = "m") {
  
  geomSelected <- reactiveValues(x = "auto")
  ggplotCall <- reactiveValues(code = "")
  
  observeEvent(data$data, {
    dataChart$data <- data$data
    dataChart$name <- data$name
  }, ignoreInit = FALSE)
  
  output$dragula <- renderUI({
    ns <- session$ns
    var_choices <- setdiff(names(dataChart$data), attr(dataChart$data, "sf_column"))
    esquisse::dragulaInput(
      inputId = ns("dragvars"),
      sourceLabel = "Variables",
      targetsLabels = c("X", "Y", "Fill", "Color", "Size", "Group", "Facet"),
      targetsIds = c("xvar", "yvar", "fill", "color", "size", "group", "facet"),
      choiceValues = var_choices,
      choiceNames = badgeType(
        col_name = var_choices,
        col_type = col_type(dataChart$data[, var_choices])
      ),
      badge = FALSE,
      width = "100%",
      height = "100%",
      replace = TRUE)
  })

  dataChart <- callModule(
    module = chooseDataServer, 
    id = "choose-data",
    data = isolate(data$data),
    name = isolate(data$name),
    launchOnStart = is.null(isolate(data$data)),
    coerceVars = getOption(x = "esquisse.coerceVars", default = FALSE),
    dataModule = dataModule, size = sizeDataModule
  )
  
  observeEvent(dataChart$data, {
    # special case: geom_sf
    if (inherits(dataChart$data, what = "sf")) {
      geom_possible$x <- c("sf", geom_possible$x)
    }
  }, ignoreInit = FALSE)
    
  geom_possible <- reactiveValues(x = "auto")
  geom_controls <- reactiveValues(x = "auto")
  
  shiny::observeEvent(list(input$dragvars$target, input$geom), {
    geoms <- geoms <- potential_geoms(
      data = dataChart$data,
      mapping = build_aes(
        data = dataChart$data,
        x = input$dragvars$target$xvar, 
        y = input$dragvars$target$yvar
      )
    )
    geom_possible$x <- c("auto", geoms)

    geom_controls$x <- select_geom_controls(input$geom, geoms)
    
    if (!is.null(input$dragvars$target$fill) | !is.null(input$dragvars$target$color)) {
      geom_controls$palette <- TRUE
    } else {
      geom_controls$palette <- FALSE
    }
  })
  
  observeEvent(input$geom, {
    geomSelected$x <- input$geom
  })
  observeEvent(geom_possible$x, {
    geoms <- c(
      "auto", "line", "area", "bar", "histogram",
      "point", "boxplot", "violin", "density",
      "tile", "sf"
    )
    updateDropInput(
      session = session,
      inputId = "geom",
      selected = setdiff(geom_possible$x, "auto")[1],
      disabled = setdiff(geoms, geom_possible$x)
    )
  })

  # Module chart controls : title, xlabs, colors, export...
  # paramsChart <- reactiveValues(inputs = NULL)
  paramsChart <- callModule(
    module = chartControlsServer, 
    id = "controls", 
    type = geom_controls, 
    data_table = reactive(dataChart$data),
    data_name = reactive({
      req(dataChart$name)
      dataChart$name
    }),
    ggplot_rv = ggplotCall,
    use_facet = reactive({
      !is.null(input$dragvars$target$facet)
    }),
    use_transX = reactive({
      if (is.null(input$dragvars$target$xvar))
        return(FALSE)
      identical(
        x = col_type(dataChart$data[[input$dragvars$target$xvar]]),
        y = "continuous"
      )
    }),
    use_transY = reactive({
      if (is.null(input$dragvars$target$yvar))
        return(FALSE)
      identical(
        x = col_type(dataChart$data[[input$dragvars$target$yvar]]),
        y = "continuous"
      )
    })
  )

  # aesthetics from drag-and-drop
  mapping <- reactiveValues(x = NULL, y = NULL, fill = NULL, color = NULL, size = NULL, group = NULL, facet = NULL)
  observeEvent(input$dragvars$target$xvar, {
    mapping$x <- input$dragvars$target$xvar
  }, ignoreNULL = FALSE)
  observeEvent(input$dragvars$target$yvar, {
    mapping$y <- input$dragvars$target$yvar
  }, ignoreNULL = FALSE)
  observeEvent(input$dragvars$target$fill, {
    mapping$fill <- input$dragvars$target$fill
  }, ignoreNULL = FALSE)
  observeEvent(input$dragvars$target$color, {
    mapping$color <- input$dragvars$target$color
  }, ignoreNULL = FALSE)
  observeEvent(input$dragvars$target$size, {
    mapping$size <- input$dragvars$target$size
  }, ignoreNULL = FALSE)
  observeEvent(input$dragvars$target$group, {
    mapping$group <- input$dragvars$target$group
  }, ignoreNULL = FALSE)
  observeEvent(input$dragvars$target$facet, {
    mapping$facet <- input$dragvars$target$facet
  }, ignoreNULL = FALSE)
  
  #plot generated
  ggplot_r <- reactiveValues(p = NULL)
  
  i <- 0
  
  output$plooooooot <- renderPlot({
    req(input$play_plot, cancelOutput = TRUE)
    req(dataChart$data)
    req(paramsChart$data)
    req(paramsChart$inputs)
    req(input$geom)
    
    data <- dataChart$data
    if (!is.null(paramsChart$index) && is.logical(paramsChart$index) & length(paramsChart$index) > 0) {
      data <- data[paramsChart$index, , drop = FALSE]
    }
    
    # aes_input <- make_aes(input$dragvars$target)
    # 
    # req(unlist(aes_input) %in% names(dataChart$data))
    # 
    # mapping <- build_aes(
    #   data = dataChart$data,
    #   .list = aes_input, 
    #   geom = input$geom
    # )
    
    geoms <- potential_geoms(
      data = dataChart$data,
      mapping = mapping
    )
    req(input$geom %in% geoms)
    
    # data <- paramsChart$data
    
    scales <- which_pal_scale(
      mapping = mapping,
      palette = paramsChart$inputs$palette,
      data = data
    )
    
    if (identical(input$geom, "auto")) {
      geom <- "blank"
    } else {
      geom <- input$geom
    }
    
    geom_args <- match_geom_args(input$geom, paramsChart$inputs, mapping = mapping)
    
    if (isTRUE(paramsChart$smooth$add) & input$geom %in% c("point", "line")) {
      geom <- c(geom, "smooth")
      geom_args <- c(
        setNames(list(geom_args), input$geom),
        list(smooth = paramsChart$smooth$args)
      )
    }
    
    scales_args <- scales$args
    scales <- scales$scales

    if (isTRUE(paramsChart$transX$use)) {
      scales <- c(scales, "x_continuous")
      scales_args <- c(scales_args, list(x_continuous = paramsChart$transX$args))
    }
    
    if (isTRUE(paramsChart$transY$use)) {
      scales <- c(scales, "y_continuous")
      scales_args <- c(scales_args, list(y_continuous = paramsChart$transY$args))
    }
    
    gg_call <- ggcall(
      data = dataChart$name, 
      mapping = mapping, 
      geom = geom,
      geom_args = geom_args, 
      scales = scales, 
      scales_args = scales_args,
      labs = paramsChart$labs, 
      theme = paramsChart$theme$theme,
      theme_args = paramsChart$theme$args, 
      coord = paramsChart$coord, 
      facet = input$dragvars$target$facet, 
      facet_args = paramsChart$facet
    )

    ggplotCall$code <- expr_deparse(gg_call, width = 1e4)
    ggplotCall$call <- gg_call
    
    ggplotCall$ggobj <- safe_ggplot(
      expr = gg_call, 
      data = setNames(list(data), dataChart$name)
    )
    ggplotCall$ggobj$plot
  })


  # Close addin
  observeEvent(input$close, shiny::stopApp())

  # Ouput of module (if used in Shiny)
  output_module <- reactiveValues(code_plot = NULL, code_filters = NULL, data = NULL)
  observeEvent(ggplotCall$code, {
    output_module$code_plot <- ggplotCall$code
  }, ignoreInit = TRUE)
  observeEvent(paramsChart$data, {
    output_module$code_filters <- reactiveValuesToList(paramsChart$code)
    output_module$data <- paramsChart$data
  }, ignoreInit = TRUE)

  return(output_module)
}

