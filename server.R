source("global.R")
#library("R.utils")
#options(warn=-1)
server <- function(input, output, session) {
  # --- Send to Galaxy -------------------------------------------------------
  # Each download button of this application has a "Send to Galaxy" companion
  # that puts the file it produces into the history of this interactive session
  # (see scripts/galaxy_downloads.R). The registry below is what makes that
  # possible without a second implementation of any table or plot:

  # metadavis_download() is downloadHandler() plus a note of the file name and
  # content function, and the observer produces the file again on request.
  metadavis_downloads <- metadavis_download_registry()
  metadavis_download <- function(...) {
    handler <- metadavis_download_with(metadavis_downloads, ...)
    handler
  }

  observeEvent(input$metadavis_galaxy_send, {
    id <- input$metadavis_galaxy_send
    # Always let the browser re-enable its button, whatever happens below.
    on.exit(session$sendCustomMessage("metadavis_galaxy_send_done", id), add = TRUE)

    handler <- get0(id, envir = metadavis_downloads, inherits = FALSE)
    if (is.null(handler)) {
      showNotification(
        paste0("There is nothing to send for ", id, "."),
        type = "warning"
      )
      return()
    }

    name <- metadavis_download_name(handler, id)
    # Write directly into the Galaxy job's discovered output directory. This
    # keeps the result available when the interactive job ends even if the API
    # upload below cannot reach Galaxy.
    path <- metadavis_galaxy_output_path(name)
    if (is.na(path)) {
      path <- file.path(tempdir(), paste0("metadavis_", session$token, "_", name))
    }
    produced <- tryCatch(
      {
        handler$content(path)
        TRUE
      },
      error = function(e) {
        showNotification(
          paste0("The file could not be produced: ", conditionMessage(e)),
          type = "error",
          duration = 15
        )
        FALSE
      }
    )
    if (!produced || !file.exists(path)) {
      return()
    }

    persisted <- identical(dirname(path), normalizePath(
      metadavis_galaxy_output_dir(), mustWork = FALSE
    ))
    if (persisted) {
      upload_started <- tryCatch(
        {
          metadavis_send_to_galaxy_async(path, name)
          TRUE
        },
        error = function(e) {
          metadavis_galaxy_log("could not start direct upload name=", name, " error=", conditionMessage(e))
          FALSE
        }
      )
      metadavis_galaxy_log(
        "saved output name=", name,
        " path=", normalizePath(path, mustWork = FALSE),
        " bytes=", file.info(path)$size,
        " direct_upload=", if (upload_started) "started asynchronously" else "not started"
      )
      result <- list(
        ok = TRUE,
        message = paste0(
          "Saved ", name, ". Direct history upload was started in the background; ",
          "the file will also appear in MetaDAVis results when this interactive job ends."
        )
      )
    } else {
      result <- list(
        ok = FALSE,
        message = "No Galaxy output directory is available for this session."
      )
      unlink(path)
    }
    showNotification(
      result$message,
      type = if (isTRUE(result$ok)) "message" else "error",
      duration = if (isTRUE(result$ok)) 8 else 30
    )
  }, ignoreNULL = TRUE, ignoreInit = TRUE)

  #Timeout
  observeEvent(input$timeOut, { 
    print(paste0("Session (", session$token, ") timed out at: ", Sys.time()))
    showModal(modalDialog(
      title = "Timeout",
      paste("Session timeout due to", input$timeOut, "inactivity -", Sys.time()),
      footer = NULL
    ))
    session$close()
  })
  
  points <- eventReactive(input$recalc, {
    cbind(rnorm(40) * 2 + 13, rnorm(40) + 48)
  }, ignoreNULL = FALSE)
  

    # Increment exactly once when the session first renders
  session$onFlushed(function() {
    current <- increment_count()
    output$view_count <- renderText(format(current, big.mark = ","))
  }, once = TRUE)

  default_if_missing <- function(x, y) {
    if (is.null(x) || length(x) == 0) {
      return(y)
    }

    if (is.character(x) && length(x) == 1 && !nzchar(x)) {
      return(y)
    }

    x
  }

  format_timestamp <- function(x) {
    format(x, "%Y-%m-%d %I:%M:%S %p")
  }

  format_scope <- function(menu, tab) {
    menu <- default_if_missing(menu, "")
    tab <- default_if_missing(tab, "")

    if (!nzchar(menu)) {
      return(tab)
    }

    if (!nzchar(tab) || identical(menu, tab)) {
      return(menu)
    }

    paste(menu, tab, sep = " > ")
  }

  status_class <- function(status) {
    tolower(gsub("[^a-z0-9]+", "-", status))
  }

  extract_ui_input_labels <- function(path = "ui.R") {
    if (!file.exists(path)) {
      return(character(0))
    }

    lines <- readLines(path, warn = FALSE)
    labels <- list()
    input_pattern <- "(?:selectInput|numericInput|radioButtons|sliderInput|checkboxInput|checkboxGroupInput|dateInput|fileInput|textInput)"
    patterns <- c(
      paste0(".*", input_pattern, "\\(\"([^\"]+)\"\\s*,\\s*label\\s*=\\s*h[1-6]\\(\"([^\"]+)\".*"),
      paste0(".*", input_pattern, "\\(\"([^\"]+)\"\\s*,\\s*label\\s*=\\s*\"([^\"]+)\".*"),
      paste0(".*", input_pattern, "\\(\"([^\"]+)\"\\s*,\\s*h[1-6]\\(\"([^\"]+)\".*"),
      paste0(".*", input_pattern, "\\(\"([^\"]+)\"\\s*,\\s*\"([^\"]+)\".*")
    )

    for (line in lines) {
      for (pattern in patterns) {
        match <- regmatches(line, regexec(pattern, line, perl = TRUE))[[1]]

        if (length(match) == 3) {
          labels[[match[2]]] <- trimws(match[3])
          break
        }
      }
    }

    unlist(labels, use.names = TRUE)
  }

  ui_input_labels <- extract_ui_input_labels("ui.R")

  format_input_value <- function(value) {
    if (is.null(value) || length(value) == 0) {
      return("Not selected")
    }

    if (is.data.frame(value) && "name" %in% names(value)) {
      value <- value$name
    } else if (is.list(value) && !is.data.frame(value)) {
      value <- unlist(value, use.names = FALSE)
    }

    if (is.null(value) || length(value) == 0) {
      return("Not selected")
    }

    value <- value[!is.na(value)]

    if (!length(value)) {
      return("Not selected")
    }

    paste(as.character(value), collapse = ", ")
  }

  format_input_label <- function(input_id) {
    default_if_missing(
      ui_input_labels[[input_id]],
      tools::toTitleCase(gsub("_", " ", input_id))
    )
  }

  collect_parameters <- function(param_ids = character(0)) {
    if (!length(param_ids)) {
      return(character(0))
    }

    vapply(
      param_ids,
      function(input_id) {
        paste0(
          format_input_label(input_id),
          ": ",
          format_input_value(isolate(input[[input_id]]))
        )
      },
      character(1)
    )
  }

  build_run_card <- function(entry) {
    params_text <- if (length(entry$parameters)) {
      paste(entry$parameters, collapse = "\n")
    } else {
      "No parameters recorded."
    }

    div(
      class = paste("run-log-card", status_class(entry$status)),
      div(class = "run-log-title", format_scope(entry$menu, entry$tab)),
      div(
        class = "run-log-meta",
        paste(
          c(
            format_timestamp(entry$timestamp),
            if (!is.na(entry$run_id)) paste0("Run #", sprintf("%03d", entry$run_id)),
            paste("Status:", entry$status)
          ),
          collapse = " | "
        )
      ),
      div(
        class = "run-log-message",
        span(class = paste("run-status-badge", status_class(entry$status)), entry$status),
        span(entry$message)
      ),
      tags$pre(class = "run-log-params", params_text)
    )
  }

  run_history <- reactiveVal(list())
  run_state <- reactiveValues(
    counter = 0L,
    current_status = "Idle",
    current_menu = "",
    current_tab = "No active analysis",
    current_message = "Load data or submit an analysis to see progress here.",
    current_parameters = character(0),
    current_timestamp = Sys.time()
  )
  action_run_cache <- reactiveValues()
  completed_actions <- reactiveVal(character(0))

  set_current_run_state <- function(status, menu = "", tab = "", message = "", parameters = character(0)) {
    run_state$current_status <- status
    run_state$current_menu <- default_if_missing(menu, "")
    run_state$current_tab <- default_if_missing(tab, "No active analysis")
    run_state$current_message <- default_if_missing(message, "")
    run_state$current_parameters <- default_if_missing(parameters, character(0))
    run_state$current_timestamp <- Sys.time()
  }

  append_run_log <- function(status, menu, tab, message, parameters = character(0), run_id = NA_integer_) {
    entry <- list(
      run_id = run_id,
      timestamp = Sys.time(),
      status = status,
      menu = menu,
      tab = tab,
      message = message,
      parameters = parameters
    )

    run_history(c(list(entry), run_history()))
    invisible(entry)
  }

  mark_action_completed <- function(action_id) {
    completed_actions(unique(c(isolate(completed_actions()), action_id)))
  }

  mark_action_incomplete <- function(action_id) {
    completed_actions(setdiff(isolate(completed_actions()), action_id))
  }

  tracked_actions <- list(
    action_level = list(
      menu = "Upload files",
      tab = "Upload files",
      task = "Data upload and preprocessing",
      params = c("select_file_type", "file1", "file1_split", "metadata", "metadata_split", "select_RA_type"),
      loading_detail = "Validating the uploaded inputs",
      running_detail = "Loading and preparing the selected data"
    ),
    action_m1_bar_plot_group = list(
      menu = "Distribution",
      tab = "Group",
      task = "Group abundance distribution",
      params = c("input_RA_bar_plot_group", "select_plot_type_group", "color_palette_group", "top_n_bar_plot_group", "select_image_type_group")
    ),
    action_m1_bar_plot_individual = list(
      menu = "Distribution",
      tab = "Individual",
      task = "Sample abundance distribution",
      params = c("input_RA_bar_plot_individual", "select_plot_type_individual", "color_palette_individual", "top_n_bar_plot_individual", "select_image_type_individual")
    ),
    action_alpha_diversity = list(
      menu = "Diversity",
      tab = "Alpha",
      task = "Alpha diversity analysis",
      params = c("input_RA_Alpha", "select_alpha", "select_alpha_pvalue", "select_plot", "select_alpha_color_palette", "select_image_type_alpha")
    ),
    action_beta_diversity = list(
      menu = "Diversity",
      tab = "Beta",
      task = "Beta diversity analysis",
      params = c("input_RA_Beta", "select_beta", "adonis_permutations", "adonis_dissimilarities", "select_beta_boxplot_pvalue", "select_beta_pvalue", "select_method", "select_beta_color_palette", "select_image_type_beta")
    ),
    action_pca = list(
      menu = "Dimension reduction",
      tab = "PCA-2D",
      task = "PCA-2D analysis",
      params = c("input_RA_pca", "select_pca_label", "select_pca_label_size", "select_pca_frame", "select_pca_color_palette", "select_image_type_pca")
    ),
    action_pca3d = list(
      menu = "Dimension reduction",
      tab = "PCA-3D",
      task = "PCA-3D analysis",
      params = c("input_RA_pca3d", "select_pca3d_color_palette")
    ),
    action_tsne = list(
      menu = "Dimension reduction",
      tab = "t-SNE",
      task = "t-SNE analysis",
      params = c("input_tsne", "select_tsne_method", "select_tsne_dimension", "select_tsne_color_palette", "select_image_type_tsne")
    ),
    action_umap = list(
      menu = "Dimension reduction",
      tab = "UMAP",
      task = "UMAP analysis",
      params = c("input_umap", "select_umap_method", "select_umap_kvalue", "select_umap_color_palette", "select_image_type_umap")
    ),
    action_taxa_condition_based_correlation = list(
      menu = "Correlation",
      tab = "Taxa-based",
      task = "Taxa-condition correlation analysis",
      params = c("input_taxa_condition_based_correlation", "input_taxa_condition_based_correlation2", "select_taxa_condition_based_correlation_method", "select_taxa_condition_based_correlation_label_size", "select_taxa_condition_geom_shape", "select_image_taxa_condition_based_correlation")
    ),
    action_samples_based_correlation = list(
      menu = "Correlation",
      tab = "Sample-based",
      task = "Sample correlation analysis",
      params = c("input_samples_based_correlation", "input_samples_based_correlation2", "select_samples_based_correlation_method", "select_samples_based_correlation_label_size", "select_sample_geom_shape", "select_image_type_samples_based_correlation")
    ),
    action_heatmap = list(
      menu = "Heatmap",
      tab = "Heatmap",
      task = "Heatmap analysis",
      params = c("input_heatmap", "heatmap_clustering_method_rows", "heatmap_clustering_method_columns", "heatmap_normalization", "heatmap_color_palette", "heatmap_row_names", "heatmap_row_names_size", "heatmap_column_names", "heatmap_column_names_size", "heatmap_row_dend", "heatmap_column_dend", "select_image_type_heatmap")
    ),
    action_wilcoxtest = list(
      menu = "Differential abundance",
      tab = "Wilcoxon Rank Sum test",
      task = "Wilcoxon Rank Sum test",
      params = c("input_wilcoxtest", "group1_wilcoxtest", "group2_wilcoxtest", "select_wilcoxtest_pvalue", "wilcoxtest_pvalue", "wilcox_color_palette", "select_wilcoxtest_plot", "wilcoxtest_plot_taxa_mode", "wilcoxtest_plot_top_n", "show_wilcoxtest_plot_labels", "select_image_type_wilcoxtest")
    ),
    action_ttest = list(
      menu = "Differential abundance",
      tab = "t-test",
      task = "t-test",
      params = c("input_ttest", "group1_ttest", "group2_ttest", "select_ttest_pvalue", "ttest_pvalue", "ttest_color_palette", "select_ttest_plot", "ttest_plot_taxa_mode", "ttest_plot_top_n", "show_ttest_plot_labels", "select_image_type_ttest")
    ),
    action_metagenomeseq = list(
      menu = "Differential abundance",
      tab = "metagenomeSeq",
      task = "metagenomeSeq analysis",
      params = c("input_RA_metagenomeseq", "group1_metagenomeseq", "group2_metagenomeseq", "select_metagenomeseq_pvalue", "metagenomeseq_pvalue", "metagenomeseq_color_palette", "select_metagenomeseq_plot", "metagenomeseq_plot_taxa_mode", "metagenomeseq_plot_top_n", "show_metagenomeseq_plot_labels", "select_image_type_metagenomeseq")
    ),
    action_deseq2 = list(
      menu = "Differential abundance",
      tab = "DESeq2",
      task = "DESeq2 analysis",
      params = c("input_RA_deseq2", "group1_deseq2", "group2_deseq2", "select_deseq2_pvalue", "deseq2_pvalue", "deseq2_color_palette", "select_deseq2_plot", "deseq2_plot_taxa_mode", "deseq2_plot_top_n", "show_deseq2_plot_labels", "select_image_type_deseq2")
    ),
    action_LEfSe = list(
      menu = "Differential abundance",
      tab = "LEfSe",
      task = "LEfSe analysis",
      params = c("input_RA_LEfSe", "group1_LEfSe", "group2_LEfSe", "select_LEfSe_method", "select_LEfSe_pvalue", "select_LEfSe_threshold", "select_LEfSe_color_palette", "select_image_type_LEfSe")
    ),
    action_MaAsLin3 = list(
      menu = "Differential abundance",
      tab = "MaAsLin3",
      task = "MaAsLin3 analysis",
      params = c("input_RA_MaAsLin3", "group1_MaAsLin3", "group2_MaAsLin3", "select_MaAsLin3_normalization", "select_MaAsLin3_transformation", "select_MaAsLin3_correction", "select_MaAsLin3_pvalue")
    ),
    action_limma = list(
      menu = "Differential abundance",
      tab = "Limma-Voom",
      task = "Limma-Voom analysis",
      params = c("input_RA_limma", "group1_limma", "group2_limma", "select_limma_pvalue", "limma_pvalue", "limma_color_palette", "select_limma_plot", "limma_plot_taxa_mode", "limma_plot_top_n", "show_limma_plot_labels", "select_image_type_limma")
    ),
    action_edger = list(
      menu = "Differential abundance",
      tab = "edgeR",
      task = "edgeR analysis",
      params = c("input_RA_edger", "group1_edger", "group2_edger", "select_edger_pvalue", "edger_pvalue", "edger_color_palette", "select_edger_plot", "edger_plot_taxa_mode", "edger_plot_top_n", "show_edger_plot_labels", "select_image_type_edger")
    ),
    action_kruskal_wallis_test = list(
      menu = "Differential abundance",
      tab = "Kruskal-Wallis test",
      task = "Kruskal-Wallis test",
      params = c("input_kruskal_wallis_test", "select_kruskal_wallis_test_pvalue", "kruskal_wallis_test_pvalue", "kruskal_wallis_test_ad_hoc", "kruskal_wallis_test_color_palette", "kruskal_wallis_test_plot", "kruskal_wallis_test_plot_taxa_mode", "kruskal_wallis_test_plot_top_n", "show_kruskal_wallis_test_plot_labels", "select_image_type_kruskal_wallis_test")
    ),
    action_anova = list(
      menu = "Differential abundance",
      tab = "ANOVA",
      task = "ANOVA",
      params = c("input_anova", "select_anova_pvalue", "anova_pvalue", "anova_ad_hoc", "anova_color_palette", "anova_plot", "anova_plot_taxa_mode", "anova_plot_top_n", "show_anova_plot_labels", "select_image_type_anova")
    )
  )

  run_with_tracking <- function(action_id, runner) {
    cfg <- tracked_actions[[action_id]]

    if (is.null(cfg)) {
      stop("No tracking configuration found for action: ", action_id)
    }

    action_click <- isolate(input[[action_id]])
    cache_key <- paste(action_id, action_click, sep = "::")
    failure_notification_id <- paste0("run-failed-", action_id)
    cached_run <- isolate(action_run_cache[[cache_key]])

    if (!is.null(cached_run)) {
      if (identical(cached_run$status, "success")) {
        mark_action_completed(action_id)
        return(cached_run$value)
      }

      mark_action_incomplete(action_id)
      validate(need(FALSE, cached_run$message))
    }

    run_id <- isolate(run_state$counter) + 1L
    params <- collect_parameters(cfg$params)
    loading_message <- default_if_missing(cfg$loading_detail, paste("Loading", tolower(cfg$task), "..."))
    running_message <- default_if_missing(cfg$running_detail, paste("Running", tolower(cfg$task), "..."))
    completed_message <- paste(cfg$task, "completed successfully.")
    progress_title <- paste(format_scope(cfg$menu, cfg$tab), "Running")

    run_state$counter <- run_id
    removeNotification(failure_notification_id)
    set_current_run_state("Loading", cfg$menu, cfg$tab, loading_message, params)
    append_run_log("Loading", cfg$menu, cfg$tab, loading_message, params, run_id)

    tryCatch({
      progress <- shiny::Progress$new(session, min = 0, max = 1, style = "notification")
      on.exit(progress$close(), add = TRUE)
      progress$set(message = progress_title, detail = loading_message, value = 0)
      progress$inc(0.2, detail = loading_message)
      
      result <- {
        set_current_run_state("Running", cfg$menu, cfg$tab, running_message, params)
        append_run_log("Running", cfg$menu, cfg$tab, running_message, params, run_id)
        progress$inc(0.55, detail = running_message)
        result_value <- runner()
        progress$inc(0.25, detail = "Completed successfully.")
        result_value
      }

      action_run_cache[[cache_key]] <- list(status = "success", value = result)
      mark_action_completed(action_id)
      set_current_run_state("Completed successfully", cfg$menu, cfg$tab, completed_message, params)
      append_run_log("Completed successfully", cfg$menu, cfg$tab, completed_message, params, run_id)
      removeNotification(failure_notification_id)
      showNotification(completed_message, type = "message", duration = 5)
      result
    }, error = function(err) {
      failed_message <- if (inherits(err, "shiny.silent.error")) {
        paste(cfg$task, "needs valid input before it can run.")
      } else {
        paste(cfg$task, "failed:", conditionMessage(err))
      }

      action_run_cache[[cache_key]] <- list(status = "failed", message = failed_message)
      mark_action_incomplete(action_id)
      set_current_run_state("Failed", cfg$menu, cfg$tab, failed_message, params)
      append_run_log("Failed", cfg$menu, cfg$tab, failed_message, params, run_id)
      showNotification(
        failed_message,
        id = failure_notification_id,
        type = if (inherits(err, "shiny.silent.error")) "warning" else "error",
        duration = NULL,
        closeButton = TRUE
      )
      
      validate(need(FALSE, failed_message))
    })
  }

  output$run_status_panel <- renderUI({
    build_run_card(list(
      run_id = if (run_state$counter > 0) run_state$counter else NA_integer_,
      timestamp = run_state$current_timestamp,
      status = run_state$current_status,
      menu = run_state$current_menu,
      tab = run_state$current_tab,
      message = run_state$current_message,
      parameters = run_state$current_parameters
    ))
  })

  output$run_log_ui <- renderUI({
    entries <- run_history()

    if (!length(entries)) {
      return(div(class = "run-log-empty", "Run history will appear here after data loading or analysis submission."))
    }

    do.call(tagList, lapply(entries, build_run_card))
  })

  observeEvent(input$main_navbar, ignoreInit = TRUE, {
    selected_tab <- input$main_navbar

    if (is.null(selected_tab) || !nzchar(selected_tab) || identical(selected_tab, "Run Log")) {
      return()
    }

    append_run_log(
      status = "Viewed",
      menu = "Navigation",
      tab = as.character(selected_tab),
      message = paste("Opened", as.character(selected_tab), "tab."),
      parameters = character(0)
    )
  })
  
######session info
  
  # --- server ---
  # Show session info in the tab and enable download as .txt
  sess_txt <- reactive({
    paste(capture.output(utils::sessionInfo()), collapse = "\n")
  })
  
  output$sess <- renderPrint({
    cat(sess_txt())
  })
  
  output$download_sess <- metadavis_download("download_sess", 
    filename = function() {
      paste0("MetaDAVis_session-info_", format(Sys.time(), "%Y-%m-%d_%H-%M-%S"), ".txt")
    },
    content = function(file) {
      writeLines(sess_txt(), con = file, useBytes = TRUE)
    }
  )
  
###########################
##         Input         ##
###########################  
  observe({
    # The Galaxy input format reads the datasets staged by the container's
    # entry point, so there is nothing to upload in the browser. Same for the
    # bundled example data, which is why both hide the two upload boxes.
    if (input$select_file_type %in% c("example", "galaxy")) {
      shinyjs::hide("box1")
      shinyjs::hide("box2")
      }
    else {
      shinyjs::show("box1")
      shinyjs::show("box2") 
    }
  })
  
  dataInput_RA_level <- eventReactive(input$action_level, {
    run_with_tracking("action_level", function() {
      # Check the selected file type
      if (input$select_file_type %in% c("Megan", "check", "qiime_format")) {
        inFile1 <- input$file1
        inFile2 <- input$metadata
        sep <- input$file1_split
        sep1 <- input$metadata_split
        type <- input$select_file_type
        
        req(inFile1, inFile2)  # Ensure files are uploaded
        
        # Check file extension and validate
        ext <- tools::file_ext(inFile1$name)
        ext1 <- tools::file_ext(inFile2$name)

        validate(
          need(
            ((type %in% c("Megan", "check") && ext == "tsv" && sep == "\t") ||
               (type == "qiime_format" && ext == "csv" && sep == ",")),
            "Please select the proper input format and 'Fields separated by'."
          ),
          need(
            ((type %in% c("Megan", "check") && ext1 == "tsv" && sep1 == "\t") ||
               (type == "qiime_format" && ext1 == "csv" && sep1 == ",")),
            "Please select the proper input format and 'Fields separated by'."
          )
        )
} else if (input$select_file_type == "galaxy") {
        # Galaxy Interactive Tool: the three datasets were staged by the
        # container entry point and their paths handed over in environment
        # variables. Nothing is uploaded through the browser here.
        inFile1 <- Sys.getenv("METADAVIS_OTU_TABLE", unset = "")
        inFile2 <- Sys.getenv("METADAVIS_METADATA_FILE", unset = "")
        taxonomy <- Sys.getenv("METADAVIS_TAXONOMY_TABLE", unset = "")

        validate(
          need(
            nzchar(inFile1) && file.exists(inFile1),
            "No OTU table was staged for this job. Upload files from the browser instead."
          ),
          need(
            nzchar(taxonomy) && file.exists(taxonomy),
            "No taxonomy table was staged for this job. Upload files from the browser instead."
          ),
          need(
            nzchar(inFile2) && file.exists(inFile2),
            "No metadata table was staged for this job. Upload files from the browser instead."
          )
        )
      } else if (input$select_file_type == "example") {
        # Load example data
        inFile1 <- "www/example_data/Megan_WGS_output.tsv"
        inFile2 <- "www/example_data/Megan_WGS_metadata.tsv"
      } else {
        return(list("Input is missing", "Input is missing", 30, "Input is missing"))
      }
      
      # Check if files are uploaded
      if (is.null(inFile1) || is.null(inFile2)) {
        return(list("Input is missing", "Input is missing", 30, "Input is missing"))
      }
      
# Call the data_input_RA function with provided inputs
      if (input$select_file_type == "galaxy") {
        # Separators are detected per file inside data_input_RA(): Galaxy
        # datasets carry no extension, so the staged files cannot be classified
        # by name and the user should not have to repeat the separator per file.
        # "galaxy" is the UI label for the phyloseq-style three-file input.
        data_input_RA(
          Input = inFile1,
          Taxonomy = taxonomy,
          Index = inFile2,
          type = input$select_RA_type,
          file_type = "phyloseq"
        )
      } else if (input$select_file_type == "example") {
        data_input_RA(
          Input = inFile1,
          Index = inFile2,
          type = input$select_RA_type,
          # sep = sep,
          # sep1 = sep1,
          file_type = input$select_file_type
        )
      } else {
        data_input_RA(
          Input = inFile1$datapath,
          Index = inFile2$datapath,
          type = input$select_RA_type,
          sep = input$file1_split,
          sep1 = input$metadata_split,
          file_type = input$select_file_type
        )
      }
    })
  })
  
  
  output$text_level<- renderText({
    paste(dataInput_RA_level()[[1]])
  })
  output$text_metadata<- renderText({
    paste(dataInput_RA_level()[[2]])
  })
  output$taxonomy_table <- renderDataTable(DT::datatable(dataInput_RA_level()[[8]],options = list(pageLength = 15,scrollX = TRUE)))
  output$metadata_table <- renderDataTable(DT::datatable(dataInput_RA_level()[[7]],options = list(pageLength = 15,scrollX = TRUE)))
  output$conditions_table <- renderDataTable(DT::datatable(dataInput_RA_level()[[10]],options = list(pageLength = 15,scrollX = TRUE)))
  output$count_table <- renderDataTable(DT::datatable(dataInput_RA_level()[[11]],options = list(pageLength = 15,scrollX = TRUE)))
  
  output$download_taxonomy_table <- metadavis_download("download_taxonomy_table", 
    filename = function() { 
      paste("taxonomy_table", '.csv', sep='') },
    content = function(file){
      write.csv(dataInput_RA_level()[[4]], file, row.names = TRUE)
    }
  )
  output$download_metadata_table <- metadavis_download("download_metadata_table", 
    filename = function() { 
      paste("metadata_table", '.csv', sep='') },
    content = function(file){
      write.csv(dataInput_RA_level()[[7]], file, row.names = TRUE)
    }
  )
  output$download_conditions_table <- metadavis_download("download_conditions_table", 
    filename = function() { 
      paste("conditions_table", '.csv', sep='') },
    content = function(file){
      write.csv(dataInput_RA_level()[[10]], file, row.names = TRUE)
    }
  )
  output$download_count_table <- metadavis_download("download_count_table", 
    filename = function() { 
      paste("count_table", '.csv', sep='') },
    content = function(file){
      write.csv(dataInput_RA_level()[[11]], file, row.names = FALSE)
    }
  )

  
  
  ###########################
  ##    Bar-Plot: group    ##
  ###########################   

  observe({
    labels_data_type <- input$file1$name
    updateSelectInput(session, "input_RA_bar_plot_group", choices = labels_data_type)
    updateSelectInput(session, "input_RA_bar_plot_individual", choices = labels_data_type)
  })
  
  data_bar_plot_group <- eventReactive(input$action_m1_bar_plot_group,{
    run_with_tracking("action_m1_bar_plot_group", function() {
      source("scripts/group_bar_plot.R")
      Bar_Plot_Group_OTU(OTU_input = dataInput_RA_level()[[4]], group_index = dataInput_RA_level()[[5]], color_palette_group = input$color_palette_group, n_top = input$top_n_bar_plot_group, names = dataInput_RA_level()[[6]], plot_type=input$select_plot_type_group)
    })
  })
  
  output$bar_plot_group <- renderPlot({
    data_bar_plot_group()
  })
  
  output$download_bar_plot_group<- metadavis_download("download_bar_plot_group", 
    filename = function(){
      paste("Bar_Plot_Group_Top_", input$top_n_bar_plot_group,"_", dataInput_RA_level()[[6]],input$select_image_type_group, sep="")
    },
    content = function(file){
      ggsave(file,plot = data_bar_plot_group(), width = input$bar_plot_group_output_width, height = input$bar_plot_group_output_height, dpi = input$bar_plot_group_output_dpi, units = "in")
    }
  )
  
  
 
  ###########################
  ##  Bar-Plot: individual ##
  ###########################   

  
  data_bar_plot_individual <- eventReactive(input$action_m1_bar_plot_individual,{
    run_with_tracking("action_m1_bar_plot_individual", function() {
      source("scripts/individual_bar_plot.R")
      Bar_Plot_Individual_OTU(OTU_input = dataInput_RA_level()[[4]], group_index = dataInput_RA_level()[[9]], color_palette_individual = input$color_palette_individual, n_top = input$top_n_bar_plot_individual, names = dataInput_RA_level()[[6]], plot_type=input$select_plot_type_individual)
    })
  })
  
  output$bar_plot_individual <- renderPlot({
    data_bar_plot_individual()
  })
  
  
  output$download_bar_plot_individual<- metadavis_download("download_bar_plot_individual", 
    filename = function(){
      paste("Bar_Plot_Individual_Top_", input$top_n_bar_plot_individual,"_", dataInput_RA_level()[[6]],input$select_image_type_individual, sep="")
    },
    content = function(file){
      ggsave(file,plot = data_bar_plot_individual(), width = input$bar_plot_individual_output_width, height = input$bar_plot_individual_output_height, dpi = input$bar_plot_individual_output_dpi, units = "in")
    }
  )
  
  
  ###########################
  ##        Heatmap        ##
  ########################### 
  
  #Bar-Plot: group
  observe({
    labels_data_type <- input$file1$name
    updateSelectInput(session, "input_heatmap", choices = labels_data_type)
    })
  
  data_heatmap <- eventReactive(input$action_heatmap,{
    run_with_tracking("action_heatmap", function() {
      source("scripts/heatmap.R")
      heatmap_abundance(OTU_input = dataInput_RA_level()[[4]], group_index = dataInput_RA_level()[[9]], heatmap_clustering_method_rows = input$heatmap_clustering_method_rows, heatmap_clustering_method_columns = input$heatmap_clustering_method_columns, heatmap_color_palette = input$heatmap_color_palette, heatmap_normalization = input$heatmap_normalization, heatmap_row_names = input$heatmap_row_names, heatmap_row_names_size = input$heatmap_row_names_size, heatmap_column_names = input$heatmap_column_names, heatmap_column_names_size = input$heatmap_column_names_size, heatmap_row_dend = input$heatmap_row_dend, heatmap_column_dend = input$heatmap_column_dend)
    })
  })
  
  output$plot_heatmap <- renderPlot({
    data_heatmap()[[1]]
  })
  
  output$download_heatmap<- metadavis_download("download_heatmap", 
    filename = function(){
      paste("Heatmap",input$select_image_type_heatmap, sep="")
    },
    content = function(file){
      ggsave(file,plot = data_heatmap()[[2]], width = input$heatmap_output_width, height = input$heatmap_output_height, dpi = input$heatmap_output_dpi, units = "in")
    }
  )
  
  
  ###########################
  ##    Alpha Diversity    ##
  ###########################   

  observe({
    labels_data_type<- input$file1$name
    updateSelectInput(session, "input_RA_Alpha", choices = labels_data_type)
  })
  
  data_Alpha_Div_plot <- eventReactive(input$action_alpha_diversity,{
    run_with_tracking("action_alpha_diversity", function() {
      source("scripts/alpha_diversity.R")
      labels_data_type<- input$file1$name
      alpha_diversity_boxplot(OTU_input = dataInput_RA_level()[[4]], group_index = dataInput_RA_level()[[7]], tax_index = dataInput_RA_level()[[8]], index_method = input$select_alpha, select_alpha_color_palette = input$select_alpha_color_palette, plot_method = input$select_plot, pvalue=input$select_alpha_pvalue)
    })
  })
  
  output$boxplot_Alpha_Div <- renderPlot({
    data_Alpha_Div_plot()[1]
  })
  
  
  output$download_Boxplot_Alpha_Div<- metadavis_download("download_Boxplot_Alpha_Div", 
    filename = function(){
      method_tmp <- c("Observed", "Chao1", "ACE", "Shannon", "Simpson", "InvSimpson", "Fisher", "All_Combined")
      paste("Alpha_Diversity_", method_tmp[as.numeric(input$select_alpha)],"_index", input$select_image_type_alpha, sep="")
    },
    content = function(file){
      ggsave(file,plot = data_Alpha_Div_plot()[[1]], width = input$Boxplot_alpha_div_output_width, height = input$Boxplot_alpha_div_output_height, dpi = input$Boxplot_alpha_div_output_dpi, units = "in")
    }
  )
  
  output$alpha_table <- renderDataTable(DT::datatable(data_Alpha_Div_plot()[[2]],options = list(pageLength = 15,scrollX = TRUE)))
  
  
  output$download_result_alpha <- metadavis_download("download_result_alpha", 
    filename = function() { 
      paste("Alpha_diversity_result", '.csv', sep='') },
    content = function(file){
      write.csv(data_Alpha_Div_plot()[[2]], file)
    }
  )

  
  ###########################
  ##     Beta Diversity    ##
  ########################### 
  

  observe({
    labels_data_type<- input$file1$name
    updateSelectInput(session, "input_RA_Beta", choices = labels_data_type)
  })
  
  data_Beta_Div_plot <- eventReactive(input$action_beta_diversity,{
    run_with_tracking("action_beta_diversity", function() {
      source("scripts/beta_diversity.R")
      labels_data_type<- input$file1$name
      beta_diversity_boxplot(OTU_input = dataInput_RA_level()[[4]], group_index = dataInput_RA_level()[[7]], tax_index = dataInput_RA_level()[[8]], select_beta_color_palette=input$select_beta_color_palette, index_method = input$select_beta, plot_method = input$select_method, adonis_dissimilarities = input$adonis_dissimilarities, adonis_permutations = input$adonis_permutations, pvalue = input$select_beta_pvalue, boxplot_pvalue = input$select_beta_boxplot_pvalue)
    })
  })
  
  output$boxplot_Beta_Div <- renderPlot({
    data_Beta_Div_plot()[1]
  })
  
  output$download_Boxplot_beta_Div<- metadavis_download("download_Boxplot_beta_Div", 
    filename = function(){
      paste("Beta_Diversity_", input$select_beta, "_",input$select_method, input$select_image_type_beta, sep="")
    },
    content = function(file){
      ggsave(file,plot = data_Beta_Div_plot()[[1]], width = input$Boxplot_beta_div_output_width, height = input$Boxplot_beta_div_output_height, dpi = input$Boxplot_beta_div_output_dpi, units = "in")
    }
  )


  output$beta_table <- renderDataTable(DT::datatable(data_Beta_Div_plot()[[2]],options = list(pageLength = 15,scrollX = TRUE)))
  
  output$download_result_beta <- metadavis_download("download_result_beta", 
    filename = function() { 
      paste("Beta_diversity_result", '.csv', sep='') },
    content = function(file){
      write.csv(data_Beta_Div_plot()[[2]], file)
    }
  )
  
  output$beta_table2 <- renderDataTable(DT::datatable(data_Beta_Div_plot()[[3]],options = list(pageLength = 15,scrollX = TRUE)))
  
  output$download_result_beta2 <- metadavis_download("download_result_beta2", 
    filename = function() { 
      paste("Beta_diversity_adonis_result", '.csv', sep='') },
    content = function(file){
      write.csv(data_Beta_Div_plot()[[3]], file)
    }
  )
  
  
  ###########################
  ##         PCA-2D        ##
  ###########################  
  
  observe({
    labels_data_type<- input$file1$name
    updateSelectInput(session, "input_RA_pca", choices = labels_data_type)
  })
  
  
  data_pca_plot <- eventReactive(input$action_pca,{
    run_with_tracking("action_pca", function() {
      source("scripts/pca_plot.R")
      labels_data_type<- input$file1$name
      pca_plot(OTU_input = dataInput_RA_level()[[4]], group_index = dataInput_RA_level()[[7]], pca_label1 = input$select_pca_label, pca_label_size = input$select_pca_label_size, pca_frame = input$select_pca_frame, select_pca_color_palette=input$select_pca_color_palette)
    })
  })
  
  output$plot_pca <- renderPlot({
    data_pca_plot()[[1]]
  })
  
  output$download_plot_pca <- metadavis_download("download_plot_pca", 
    filename = function(){
     paste("PCA_plot", input$select_image_type_pca, sep="")
    },
    content = function(file){
      ggsave(file,plot = data_pca_plot()[[1]], width = input$pca_plot_output_width, height = input$pca_plot_output_height, dpi = input$pca_plot_output_dpi, units = "in")
    }
  )  
  
  output$pca_table <- renderDataTable(DT::datatable(data_pca_plot()[[2]],options = list(pageLength = 15,scrollX = TRUE)))
  
  output$download_result_pca <- metadavis_download("download_result_pca", 
    filename = function() { 
      paste("pca_result", '.csv', sep='') },
    content = function(file){
      write.csv(data_pca_plot()[[2]], file)
    }
  )
    
  ###########################
  ##         PCA-3D        ##
  ###########################  
  
  
  observe({
    labels_data_type<- input$file1$name
    updateSelectInput(session, "input_RA_pca3d", choices = labels_data_type)
  })
  
  data_pca3d_table <- eventReactive(input$action_pca3d,{
    run_with_tracking("action_pca3d", function() {
      source("scripts/pca3d_plot.R")
      labels_data_type<- input$file1$name
      pca3d_plot(OTU_input = dataInput_RA_level()[[4]], group_index = dataInput_RA_level()[[7]], select_pca3d_color_palette=input$select_pca3d_color_palette)
    })
  })
  
  output$pca3d_table <- renderDataTable(DT::datatable(data_pca3d_table()[[2]],options = list(pageLength = 15,scrollX = TRUE)))
  
  output$download_result_pca3d <- metadavis_download("download_result_pca3d", 
    filename = function() { 
      paste("pca3d_result", '.csv', sep='') },
    content = function(file){
      write.csv(data_pca3d_table()[[2]], file)
    }
  )
  
    output$plot_pca3d <- renderPlotly({
      data_pca3d_table()[[1]]
  })
  
  
  
  ###########################
  ##          t-SNE        ##
  ###########################  
  
  
  observe({
    labels_data_type<- input$file1$name
    updateSelectInput(session, "input_tsne", choices = labels_data_type)
  })
  
  data_tsne_table <- eventReactive(input$action_tsne,{
    run_with_tracking("action_tsne", function() {
      source("scripts/tsne.R")
      labels_data_type<- input$file1$name
      tsne_plot_table(OTU_input = dataInput_RA_level()[[4]], group_index = dataInput_RA_level()[[7]], tax_index = dataInput_RA_level()[[8]], method = input$select_tsne_method, dimension = input$select_tsne_dimension, select_tsne_color_palette = input$select_tsne_color_palette)
    })
  })
  
  output$tsne_table <- renderDataTable(DT::datatable(data_tsne_table()[[2]],options = list(pageLength = 15,scrollX = TRUE)))
  
  output$download_result_tsne <- metadavis_download("download_result_tsne", 
    filename = function() { 
      paste("tsne_result_", input$select_tsne_method, '.csv', sep='') },
    content = function(file){
      write.csv(data_tsne_table()[[2]], file)
    }
  )
  
  output$plot_tsne <- renderPlot({
    data_tsne_table()[[1]]
  })
  
  output$download_plot_tsne <- metadavis_download("download_plot_tsne", 
    filename = function(){
      paste("tsne_plot_", input$select_tsne_method, input$select_image_type_tsne, sep="")
    },
    content = function(file){
      ggsave(file,plot = data_tsne_table()[[1]], width = input$tsne_plot_output_width, height = input$tsne_plot_output_height, dpi = input$tsne_plot_output_dpi, units = "in")
    }
  )  
  
  
  
  ###########################
  ##          UMAP         ##
  ###########################  
  
  
  observe({
    labels_data_type<- input$file1$name
    updateSelectInput(session, "input_umap", choices = labels_data_type)
  })
  
  data_umap_table <- eventReactive(input$action_umap,{
    run_with_tracking("action_umap", function() {
      source("scripts/umap.R")
      labels_data_type<- input$file1$name
      umap_plot_table(OTU_input = dataInput_RA_level()[[4]], group_index = dataInput_RA_level()[[7]], tax_index = dataInput_RA_level()[[8]], method = input$select_umap_method, kvalue = input$select_umap_kvalue, select_umap_color_palette=input$select_umap_color_palette)
    })
  })
  
  output$umap_table <- renderDataTable(DT::datatable(data_umap_table()[[2]],options = list(pageLength = 15,scrollX = TRUE)))
  
  output$download_result_umap <- metadavis_download("download_result_umap", 
    filename = function() { 
      paste("umap_result_based_on_condition_", input$select_umap_method, '.csv', sep='') },
    content = function(file){
      write.csv(data_umap_table()[[2]], file)
    }
  )
  
  output$umap_table1 <- renderDataTable(DT::datatable(data_umap_table()[[3]],options = list(pageLength = 15,scrollX = TRUE)))
  
  output$download_result_umap1 <- metadavis_download("download_result_umap1", 
    filename = function() { 
      paste("umap_resul_based_on_cluster_", input$select_umap_method, '.csv', sep='') },
    content = function(file){
      write.csv(data_umap_table()[[3]], file)
    }
  )
  
  output$plot_umap <- renderPlot({
    data_umap_table()[[1]]
  })
  
  output$download_plot_umap <- metadavis_download("download_plot_umap", 
    filename = function(){
      paste("umap_plot_", input$select_umap_method, input$select_image_type_umap, sep="")
    },
    content = function(file){
      ggsave(file,plot = data_umap_table()[[1]], width = input$umap_plot_output_width, height = input$umap_plot_output_height, dpi = input$umap_plot_output_dpi, units = "in")
    }
  )  
  
  
  #############################
  ## Correlation: Taxa-based ##
  #############################  
  
  
  # observe({
  #   labels_data_type<- input$file1$name
  #   updateSelectInput(session, "input_taxa_based_correlation", choices = labels_data_type)
  # })
  # 
  # data_taxa_based_correlation_table <- eventReactive(input$action_taxa_based_correlation,{
  #   source("scripts/taxa_based_correlation.R")
  #   labels_data_type<- input$file1$name
  #   taxa_based_correlation_plot_table(OTU_input = dataInput_RA_level()[[4]], group_index = dataInput_RA_level()[[7]], method = input$select_taxa_based_correlation_method, labe_size =input$select_taxa_based_correlation_label_size,  select_taxa_geom_shape=input$select_taxa_geom_shape)
  # })
  # 
  # output$taxa_based_correlation_table <- renderDataTable(DT::datatable(data_taxa_based_correlation_table()[[2]],options = list(pageLength = 15,scrollX = TRUE)))
  # 
  # output$download_result_taxa_based_correlation <- metadavis_download("download_result_taxa_based_correlation", 
  #   filename = function() { 
  #     paste("taxa_based_correlation_result_", input$select_taxa_based_correlation_method, '.csv', sep='') },
  #   content = function(file){
  #     write.csv(data_taxa_based_correlation_table()[[2]], file)
  #   }
  # )
  # 
  # output$plot_taxa_based_correlation <- renderPlot({
  #   data_taxa_based_correlation_table()[[1]]
  # })
  # 
  # output$download_plot_taxa_based_correlation <- metadavis_download("download_plot_taxa_based_correlation", 
  #   filename = function(){
  #     paste("taxa_based_correlation_plot_", input$select_taxa_based_correlation_method, input$select_image_type_taxa_based_correlation, sep="")
  #   },
  #   content = function(file){
  #     ggsave(file,plot = data_taxa_based_correlation_table()[[1]], width = input$taxa_based_correlation_output_width, height = input$taxa_based_correlation_output_height, dpi = input$taxa_based_correlation_output_dpi, units = "in")
  #   }
  # )  
  
  ################################
  ## Correlation: Samples-based ##
  ################################ 
  
  
  observe({
    labels_data_type<- input$file1$name
    updateSelectInput(session, "input_samples_based_correlation", choices = labels_data_type)
  })
  
  #Update conditions
  observeEvent(input$action_level,{
    label_condition1 <- "Please upload metadata in upload page"
    if(is.null(dataInput_RA_level()[[10]])){
      label_type1 <- "Please upload metadata in upload page"
    }
    else{
      label_condition1<- dataInput_RA_level()[[10]]
    }
    updateSelectInput(session, "input_samples_based_correlation2", choices = label_condition1, selected = label_condition1[1])
  })
  
  data_samples_based_correlation_table <- eventReactive(input$action_samples_based_correlation,{
    run_with_tracking("action_samples_based_correlation", function() {
      source("scripts/samples_based_correlation.R")
      labels_data_type<- input$file1$name
      samples_based_correlation_plot_table(OTU_input = dataInput_RA_level()[[4]], group_index = dataInput_RA_level()[[9]], group1 = input$input_samples_based_correlation2, method = input$select_samples_based_correlation_method, labe_size =input$select_samples_based_correlation_label_size, select_sample_geom_shape=input$select_sample_geom_shape)
    })
  })
  
  output$samples_based_correlation_table <- renderDataTable(DT::datatable(data_samples_based_correlation_table()[[2]],options = list(pageLength = 15,scrollX = TRUE)))
  
  output$download_result_samples_based_correlation <- metadavis_download("download_result_samples_based_correlation", 
    filename = function() { 
      paste("samples_based_correlation_result_", input$select_samples_based_correlation_method, '.csv', sep='') },
    content = function(file){
      write.csv(data_samples_based_correlation_table()[[2]], file)
    }
  )
  
  output$plot_samples_based_correlation <- renderPlot({
    data_samples_based_correlation_table()[[1]]
  })
  
  output$download_plot_samples_based_correlation <- metadavis_download("download_plot_samples_based_correlation", 
    filename = function(){
      paste("samples_based_correlation_plot_", input$select_samples_based_correlation_method, input$select_image_type_samples_based_correlation, sep="")
    },
    content = function(file){
      ggsave(file,plot = data_samples_based_correlation_table()[[1]], width = input$samples_based_correlation_output_width, height = input$samples_based_correlation_output_height, dpi = input$samples_based_correlation_output_dpi, units = "in")
    }
  )  
  
  ##########################################
  ##   Correlation: Taxa-condition based  ##
  ##########################################  
  
  
  observe({
    labels_data_type<- input$file1$name
    updateSelectInput(session, "input_taxa_condition_based_correlation", choices = labels_data_type)
  })
  
  #Update conditions
  observeEvent(input$action_level,{
    label_condition1 <- "Please upload metadata in upload page"
    if(is.null(dataInput_RA_level()[[10]])){
      label_type1 <- "Please upload metadata in upload page"
    }
    else{
      label_condition1<- dataInput_RA_level()[[10]]
    }
    updateSelectInput(session, "input_taxa_condition_based_correlation2", choices = label_condition1, selected = label_condition1[1])
  })
  
  data_taxa_condition_based_correlation_table <- eventReactive(input$action_taxa_condition_based_correlation,{
    run_with_tracking("action_taxa_condition_based_correlation", function() {
      source("scripts/taxa_condition_based_correlation.R")
      labels_data_type<- input$file1$name
      taxa_condition_based_correlation_plot_table(OTU_input = dataInput_RA_level()[[4]], group_index = dataInput_RA_level()[[9]], group1 = input$input_taxa_condition_based_correlation2, method = input$select_taxa_condition_based_correlation_method, labe_size =input$select_taxa_condition_based_correlation_label_size,  select_taxa_condition_geom_shape=input$select_taxa_condition_geom_shape)
    })
  })
  
  output$taxa_condition_based_correlation_table <- renderDataTable(DT::datatable(data_taxa_condition_based_correlation_table()[[2]],options = list(pageLength = 15,scrollX = TRUE)))
  
  output$download_result_taxa_condition_based_correlation <- metadavis_download("download_result_taxa_condition_based_correlation", 
    filename = function() { 
      paste("taxa_condition_based_correlation_result_", input$select_taxa_condition_based_correlation_method, '.csv', sep='') },
    content = function(file){
      write.csv(data_taxa_condition_based_correlation_table()[[2]], file)
    }
  )

  taxa_condition_based_correlation_plot_dims <- reactive({
    plot_data <- data_taxa_condition_based_correlation_table()[[2]]

    if (is.null(plot_data) || nrow(plot_data) == 0) {
      return(list(width = 1100, height = 1000))
    }

    axis_labels <- unique(c(as.character(plot_data$x), as.character(plot_data$y)))
    axis_labels <- axis_labels[!is.na(axis_labels) & nzchar(axis_labels)]

    label_count <- length(axis_labels)
    max_label_length <- if (label_count > 0) max(nchar(axis_labels), na.rm = TRUE) else 0

    list(
      width = max(1100, min(3200, 220 + label_count * 22 + max_label_length * 7)),
      height = max(1000, min(3200, 220 + label_count * 20 + max_label_length * 4))
    )
  })

  output$taxa_condition_based_correlation_plot_ui <- renderUI({
    req(input$action_taxa_condition_based_correlation > 0)

    dims <- taxa_condition_based_correlation_plot_dims()

    withSpinner(
      div(
        style = "width: 100%; height: 20%; overflow-x: auto;",
        plotOutput(
          "plot_taxa_condition_based_correlation",
          width = paste0(dims$width, "px"),
          height = paste0(dims$height, "px")
        )
      )
    )
  })
  
  output$plot_taxa_condition_based_correlation <- renderPlot({
    data_taxa_condition_based_correlation_table()[[1]]
  })
  
  output$download_plot_taxa_condition_based_correlation <- metadavis_download("download_plot_taxa_condition_based_correlation", 
    filename = function(){
      paste("taxa_condition_based_correlation_plot_", input$select_taxa_condition_based_correlation_method, input$select_image_taxa_condition_based_correlation, sep="")
    },
    content = function(file){
      ggsave(file,plot = data_taxa_condition_based_correlation_table()[[1]], width = input$taxa_condition_based_correlation_output_width, height = input$taxa_condition_based_correlation_output_height, dpi = input$taxa_condition_based_correlation_output_dpi, units = "in")
    }
  )  

  metadata_condition_choices <- function() {
    placeholder <- "Please upload metadata in upload page"
    conditions <- tryCatch(dataInput_RA_level()[[10]], error = function(err) NULL)

    if (is.null(conditions)) {
      return(placeholder)
    }

    if (is.data.frame(conditions)) {
      if ("Condition" %in% colnames(conditions)) {
        conditions <- conditions$Condition
      } else {
        conditions <- conditions[[1]]
      }
    }

    conditions <- unique(as.character(conditions))
    conditions <- conditions[!is.na(conditions) & nzchar(conditions)]

    if (!length(conditions)) {
      return(placeholder)
    }

    conditions
  }

  has_metadata_conditions <- function(conditions) {
    length(conditions) > 0 && !identical(conditions, "Please upload metadata in upload page")
  }

  update_differential_condition_pair <- function(group1_id, group2_id) {
    conditions <- metadata_condition_choices()

    if (!has_metadata_conditions(conditions)) {
      updateSelectInput(session, group1_id, choices = conditions, selected = conditions[1])
      updateSelectInput(session, group2_id, choices = conditions, selected = conditions[1])
      return(invisible(NULL))
    }

    selected_group1 <- input[[group1_id]]
    if (is.null(selected_group1) || !length(selected_group1) || !(selected_group1 %in% conditions)) {
      selected_group1 <- conditions[1]
    }

    group2_choices <- conditions[conditions != selected_group1]
    selected_group2 <- input[[group2_id]]

    if (!length(group2_choices)) {
      selected_group2 <- character(0)
    } else if (is.null(selected_group2) || !length(selected_group2) || !(selected_group2 %in% group2_choices)) {
      selected_group2 <- group2_choices[1]
    }

    updateSelectInput(session, group1_id, choices = conditions, selected = selected_group1)
    updateSelectInput(session, group2_id, choices = group2_choices, selected = selected_group2)
  }

  observe_differential_condition_pair <- function(group1_id, group2_id) {
    observeEvent(input$action_level, {
      update_differential_condition_pair(group1_id, group2_id)
    })

    observeEvent(input[[group1_id]], {
      update_differential_condition_pair(group1_id, group2_id)
    }, ignoreInit = TRUE)
  }

  differential_plot_pages <- function(result, plot_index) {
    if (is.null(result) || !length(result)) {
      return(list())
    }

    individual_plots <- result[["individual_plots"]]
    if (is.list(individual_plots) && length(individual_plots)) {
      return(individual_plots)
    }

    plot_obj <- if (length(result) >= plot_index) result[[plot_index]] else NULL
    if (is.null(plot_obj)) {
      return(list())
    }

    list("Plot 1" = plot_obj)
  }

  differential_selected_page <- function(page_input_id, page_count) {
    page_index <- suppressWarnings(as.integer(input[[page_input_id]]))

    if (length(page_index) == 0 || is.na(page_index[1]) || page_index[1] < 1 || page_index[1] > page_count) {
      return(1L)
    }

    page_index[1]
  }

  differential_selected_plot <- function(result, plot_index, page_input_id) {
    plot_pages <- differential_plot_pages(result, plot_index)
    validate(need(length(plot_pages) > 0, "No plot is available."))
    plot_pages[[differential_selected_page(page_input_id, length(plot_pages))]]
  }

  differential_individual_plot_page_ui <- function(result, plot_index, page_input_id) {
    plot_pages <- differential_plot_pages(result, plot_index)

    if (length(plot_pages) <= 1) {
      return(NULL)
    }

    plot_names <- names(plot_pages)
    if (is.null(plot_names) || any(!nzchar(plot_names))) {
      plot_names <- paste("Plot", seq_along(plot_pages))
    }

    selectInput(
      page_input_id,
      label = "Individual plot image",
      choices = stats::setNames(seq_along(plot_pages), plot_names),
      selected = differential_selected_page(page_input_id, length(plot_pages)),
      width = "360px"
    )
  }

  differential_plot_filename <- function(filename_base, extension, result, plot_index, page_input_id) {
    plot_pages <- differential_plot_pages(result, plot_index)
    filename <- filename_base

    if (length(plot_pages) > 1) {
      filename <- paste0(filename, "_page_", sprintf("%02d", differential_selected_page(page_input_id, length(plot_pages))))
    }

    paste0(filename, default_if_missing(extension, ".jpg"))
  }
  
  ###########################
  ##      wilcox-test      ##
  ###########################  
  
  observe({
    labels_data_type<- input$file1$name
    updateSelectInput(session, "input_wilcoxtest", choices = labels_data_type)
  })
  
  observe_differential_condition_pair("group1_wilcoxtest", "group2_wilcoxtest")
  
  data_wilcoxtest <- eventReactive(input$action_wilcoxtest,{
    run_with_tracking("action_wilcoxtest", function() {
      source("scripts/wilcoxtest.R")
      labels_data_type<- input$file1$name
      wilcoxtest_summary(OTU_input = dataInput_RA_level()[[4]], group_index = dataInput_RA_level()[[9]], index_pvalue = input$select_wilcoxtest_pvalue, group1 = input$group1_wilcoxtest, group2 = input$group2_wilcoxtest,  plot_method = input$select_wilcoxtest_plot, alpha = input$wilcoxtest_pvalue, wilcox_color_palette=input$wilcox_color_palette, show_plot_labels = input$show_wilcoxtest_plot_labels, plot_top_n = input$wilcoxtest_plot_top_n, plot_taxa_mode = input$wilcoxtest_plot_taxa_mode)
    })
  })
  
  output$text_wilcoxtest_level<- renderText({
    paste(data_wilcoxtest()[[1]])
  })
  
  output$wilcoxtest_table <- renderDataTable(DT::datatable((data_wilcoxtest()[[2]]),options = list(pageLength = 15,scrollX = TRUE)))
  
  output$download_result_wilcoxtest_1 <- metadavis_download("download_result_wilcoxtest_1", 
    filename = function() { 
      paste("wilcoxtest_result_significant", '.csv', sep='') },
    content = function(file){
      write.csv(data_wilcoxtest()[[2]], file, row.names = FALSE)
    }
  )
  output$download_result_wilcoxtest_2 <- metadavis_download("download_result_wilcoxtest_2", 
    filename = function() { 
      paste("wilcoxtest_result_all", '.csv', sep='') },
    content = function(file){
      write.csv(data_wilcoxtest()[[3]], file, row.names = FALSE)
    }
  )
  output$download_result_wilcoxtest_3 <- metadavis_download("download_result_wilcoxtest_3", 
    filename = function() { 
      paste("wilcoxtest_relative_frequency", '.csv', sep='') },
    content = function(file){
      write.csv(data_wilcoxtest()[[4]], file)
    }
  )
  output$download_result_wilcoxtest_4 <- metadavis_download("download_result_wilcoxtest_4", 
    filename = function() { 
      paste("total_counts_in_each_samples", '.csv', sep='') },
    content = function(file){
      write.csv(data_wilcoxtest()[[5]], file, row.names = FALSE)
    }
  )
  

  output$boxplot_wilcoxtest <- renderPlot({
    differential_selected_plot(data_wilcoxtest(), 6, "wilcoxtest_individual_plot_page")
  })

  output$wilcoxtest_individual_plot_page_ui <- renderUI({
    differential_individual_plot_page_ui(data_wilcoxtest(), 6, "wilcoxtest_individual_plot_page")
  })
  
  
  output$download_Boxplot_wilcoxtest<- metadavis_download("download_Boxplot_wilcoxtest", 
    filename = function(){
      differential_plot_filename("wilcoxtest_plot", input$select_image_type_wilcoxtest, data_wilcoxtest(), 6, "wilcoxtest_individual_plot_page")
    },
    content = function(file){
      ggsave(file,plot = differential_selected_plot(data_wilcoxtest(), 6, "wilcoxtest_individual_plot_page"), width = input$Boxplot_wilcoxtest_output_width, height = input$Boxplot_wilcoxtest_output_height, dpi = input$Boxplot_wilcoxtest_output_dpi, units = "in")
    }
  )    
  
  
  
  ###########################
  ##         T-test        ##
  ###########################  
  
  observe({
    labels_data_type<- input$file1$name
    updateSelectInput(session, "input_ttest", choices = labels_data_type)
  })
  
  observe_differential_condition_pair("group1_ttest", "group2_ttest")
  
  data_ttest <- eventReactive(input$action_ttest,{
    run_with_tracking("action_ttest", function() {
      source("scripts/ttest.R")
      labels_data_type<- input$file1$name
      ttest_summary(OTU_input = dataInput_RA_level()[[4]], group_index = dataInput_RA_level()[[9]], index_pvalue = input$select_ttest_pvalue, group1 = input$group1_ttest, group2 = input$group2_ttest,  plot_method = input$select_ttest_plot, alpha = input$ttest_pvalue, ttest_color_palette = input$ttest_color_palette, show_plot_labels = input$show_ttest_plot_labels, plot_top_n = input$ttest_plot_top_n, plot_taxa_mode = input$ttest_plot_taxa_mode)
    })
  })
  
  output$text_ttest_level<- renderText({
    paste(data_ttest()[[1]])
  })
  
  output$ttest_table <- renderDataTable(DT::datatable((data_ttest()[[2]]), options = list( pageLength = 15, scrollX = TRUE)))
  
  output$download_result_ttest_1 <- metadavis_download("download_result_ttest_1", 
    filename = function() { 
      paste("ttest_result_significant", '.csv', sep='') },
    content = function(file){
      write.csv(data_ttest()[[2]], file, row.names = FALSE)
    }
  )
  output$download_result_ttest_2 <- metadavis_download("download_result_ttest_2", 
    filename = function() { 
      paste("ttest_result_all", '.csv', sep='') },
    content = function(file){
      write.csv(data_ttest()[[3]], file, row.names = FALSE)
    }
  )
  output$download_result_ttest_3 <- metadavis_download("download_result_ttest_3", 
    filename = function() { 
      paste("ttest_relative_frequency", '.csv', sep='') },
    content = function(file){
      write.csv(data_ttest()[[4]], file)
    }
  )
  output$download_result_ttest_4 <- metadavis_download("download_result_ttest_4", 
    filename = function() { 
      paste("total_counts_in_each_samples", '.csv', sep='') },
    content = function(file){
      write.csv(data_ttest()[[5]], file, row.names = FALSE)
    }
  )
  

  output$boxplot_ttest <- renderPlot({
    differential_selected_plot(data_ttest(), 6, "ttest_individual_plot_page")
  })

  output$ttest_individual_plot_page_ui <- renderUI({
    differential_individual_plot_page_ui(data_ttest(), 6, "ttest_individual_plot_page")
  })
  
  
  output$download_Boxplot_ttest<- metadavis_download("download_Boxplot_ttest", 
    filename = function(){
      differential_plot_filename("ttest_plot", input$select_image_type_ttest, data_ttest(), 6, "ttest_individual_plot_page")
    },
    content = function(file){
      ggsave(file,plot = differential_selected_plot(data_ttest(), 6, "ttest_individual_plot_page"), width = input$Boxplot_ttest_output_width, height = input$Boxplot_ttest_output_height, dpi = input$Boxplot_ttest_output_dpi, units = "in")
    }
  )    
  
  
  ###########################
  ##     metagenomeSeq     ##
  ###########################  
  
  observe({
    labels_data_type<- input$file1$name
    updateSelectInput(session, "input_RA_metagenomeseq", choices = labels_data_type)
  })
  
  observe_differential_condition_pair("group1_metagenomeseq", "group2_metagenomeseq")
  
  
  data_metagenomeseq <- eventReactive(input$action_metagenomeseq,{
    run_with_tracking("action_metagenomeseq", function() {
      source("scripts/metagenomeseq.R")
      labels_data_type<- input$file1$name
      metagenomeseq_summary(OTU_input = dataInput_RA_level()[[4]], group_index = dataInput_RA_level()[[9]], index_pvalue = input$select_metagenomeseq_pvalue, group1 = input$group1_metagenomeseq, group2 = input$group2_metagenomeseq,  plot_method = input$select_metagenomeseq_plot, alpha = input$metagenomeseq_pvalue, metagenomeseq_color_palette = input$metagenomeseq_color_palette, show_plot_labels = input$show_metagenomeseq_plot_labels, plot_top_n = input$metagenomeseq_plot_top_n, plot_taxa_mode = input$metagenomeseq_plot_taxa_mode)
    })
  })
  
  output$text_metagenomeseq_level<- renderText({
    paste(data_metagenomeseq()[[1]])
  })
  
  output$metagenomeseq_table <- renderDataTable(DT::datatable((data_metagenomeseq()[[2]]), options = list( pageLength = 15, scrollX = TRUE)))
  
  output$download_result_metagenomeseq_1 <- metadavis_download("download_result_metagenomeseq_1", 
    filename = function() { 
      paste("metagenomeseq_result_significant", '.csv', sep='') },
    content = function(file){
      write.csv(data_metagenomeseq()[[2]], file, row.names = FALSE)
    }
  )
  output$download_result_metagenomeseq_2 <- metadavis_download("download_result_metagenomeseq_2", 
    filename = function() { 
      paste("metagenomeseq_result_all", '.csv', sep='') },
    content = function(file){
      write.csv(data_metagenomeseq()[[3]], file, row.names = FALSE)
    }
  )
  output$download_result_metagenomeseq_3 <- metadavis_download("download_result_metagenomeseq_3", 
    filename = function() { 
      paste("total_counts_in_each_samples", '.csv', sep='') },
    content = function(file){
      write.csv(data_metagenomeseq()[[4]], file, row.names = FALSE)
    }
  )
  
  output$boxplot_metagenomeseq <- renderPlot({
    differential_selected_plot(data_metagenomeseq(), 5, "metagenomeseq_individual_plot_page")
  })

  output$metagenomeseq_individual_plot_page_ui <- renderUI({
    differential_individual_plot_page_ui(data_metagenomeseq(), 5, "metagenomeseq_individual_plot_page")
  })
  
  
  output$download_Boxplot_metagenomeseq<- metadavis_download("download_Boxplot_metagenomeseq", 
    filename = function(){
      differential_plot_filename("metagenomeseq_plot", input$select_image_type_metagenomeseq, data_metagenomeseq(), 5, "metagenomeseq_individual_plot_page")
    },
    content = function(file){
      ggsave(file,plot = differential_selected_plot(data_metagenomeseq(), 5, "metagenomeseq_individual_plot_page"), width = input$Boxplot_metagenomeseq_output_width, height = input$Boxplot_metagenomeseq_output_height, dpi = input$Boxplot_metagenomeseq_output_dpi, units = "in")
    }
  )    
  
  
  
  ###########################
  ##         DESeq2        ##
  ###########################  
  
  observe({
    labels_data_type<- input$file1$name
    updateSelectInput(session, "input_RA_deseq2", choices = labels_data_type)
  })
  
  observe_differential_condition_pair("group1_deseq2", "group2_deseq2")
  
  
  
  data_deseq2 <- eventReactive(input$action_deseq2,{
    run_with_tracking("action_deseq2", function() {
      source("scripts/deseq2.R")
      labels_data_type<- input$file1$name
      deseq2_summary(OTU_input = dataInput_RA_level()[[4]], group_index = dataInput_RA_level()[[9]], index_pvalue = input$select_deseq2_pvalue, group1 = input$group1_deseq2, group2 = input$group2_deseq2,  plot_method = input$select_deseq2_plot, alpha = input$deseq2_pvalue, deseq2_color_palette = input$deseq2_color_palette, show_plot_labels = input$show_deseq2_plot_labels, plot_top_n = input$deseq2_plot_top_n, plot_taxa_mode = input$deseq2_plot_taxa_mode)
    })
  })
  
  output$text_deseq2_level<- renderText({
    paste(data_deseq2()[[1]])
  })
 
  output$deseq2_table <- renderDataTable(DT::datatable((data_deseq2()[[2]]), options = list( pageLength = 15, scrollX = TRUE)))
  
  output$download_result_deseq2_1 <- metadavis_download("download_result_deseq2_1", 
    filename = function() { 
      paste("deseq2_result_significant", '.csv', sep='') },
    content = function(file){
      write.csv(data_deseq2()[[2]], file, row.names = FALSE)
    }
  )
  output$download_result_deseq2_2 <- metadavis_download("download_result_deseq2_2", 
    filename = function() { 
      paste("deseq2_result_all", '.csv', sep='') },
    content = function(file){
      write.csv(data_deseq2()[[3]], file, row.names = FALSE)
    }
  )
  output$download_result_deseq2_3 <- metadavis_download("download_result_deseq2_3", 
    filename = function() { 
      paste("deseq2_normalized_count", '.csv', sep='') },
    content = function(file){
      write.csv(data_deseq2()[[4]], file)
    }
  )
  output$download_result_deseq2_4 <- metadavis_download("download_result_deseq2_4", 
    filename = function() { 
      paste("total_counts_in_each_samples", '.csv', sep='') },
    content = function(file){
      write.csv(data_deseq2()[[5]], file, row.names = FALSE)
    }
  )
  output$boxplot_deseq2 <- renderPlot({
    differential_selected_plot(data_deseq2(), 6, "deseq2_individual_plot_page")
  })

  output$deseq2_individual_plot_page_ui <- renderUI({
    differential_individual_plot_page_ui(data_deseq2(), 6, "deseq2_individual_plot_page")
  })
  
  
  output$download_Boxplot_deseq2<- metadavis_download("download_Boxplot_deseq2", 
    filename = function(){
      differential_plot_filename("deseq2_plot", input$select_image_type_deseq2, data_deseq2(), 6, "deseq2_individual_plot_page")
    },
    content = function(file){
      ggsave(file,plot = differential_selected_plot(data_deseq2(), 6, "deseq2_individual_plot_page"), width = input$Boxplot_deseq2_output_width, height = input$Boxplot_deseq2_output_height, dpi = input$Boxplot_deseq2_output_dpi, units = "in")
    }
  )    
  
  
  
  ###########################
  ##         LEfSe        ##
  ###########################  
  
  observe({
    labels_data_type<- input$file1$name
    updateSelectInput(session, "input_RA_LEfSe", choices = labels_data_type)
  })
  
  observe_differential_condition_pair("group1_LEfSe", "group2_LEfSe")
  
  
  
  data_LEfSe <- eventReactive(input$action_LEfSe,{
    run_with_tracking("action_LEfSe", function() {
      source("scripts/LEfSe.R")
      labels_data_type<- input$file1$name
      LEfSe_summary(OTU_input = dataInput_RA_level()[[4]], group_index = dataInput_RA_level()[[9]], group1 = input$group1_LEfSe, group2 = input$group2_LEfSe,  select_LEfSe_method = input$select_LEfSe_method, select_LEfSe_pvalue = input$select_LEfSe_pvalue, select_LEfSe_threshold = input$select_LEfSe_threshold, select_LEfSe_color_palette = input$select_LEfSe_color_palette)
    })
  })
  
  output$text_LEfSe_level<- renderText({
    paste(data_LEfSe()[[1]])
  })
  
  output$LEfSe_table <- renderDataTable(DT::datatable((data_LEfSe()[[2]]), options = list( pageLength = 15, scrollX = TRUE)))
  
  output$download_result_LEfSe_1 <- metadavis_download("download_result_LEfSe_1", 
    filename = function() { 
      paste("LEfSe_result_significant", '.csv', sep='') },
    content = function(file){
      write.csv(data_LEfSe()[[2]], file, row.names = FALSE)
    }
  )
  
  output$download_result_LEfSe_4 <- metadavis_download("download_result_LEfSe_4", 
    filename = function() { 
      paste("total_counts_in_each_samples", '.csv', sep='') },
    content = function(file){
      write.csv(data_LEfSe()[[3]], file, row.names = FALSE)
    }
  )
  output$boxplot_LEfSe <- renderPlot({
    data_LEfSe()[4]
  })
  
  
  output$download_Boxplot_LEfSe<- metadavis_download("download_Boxplot_LEfSe", 
    filename = function(){
      paste("LEfSe_plot", input$select_image_type_LEfSe, sep="")
    },
    content = function(file){
      ggsave(file,plot = data_LEfSe()[[4]], width = input$Boxplot_LEfSe_output_width, height = input$Boxplot_LEfSe_output_height, dpi = input$Boxplot_LEfSe_output_dpi, units = "in")
    }
  )  
  
  
  ###########################
  ##        MaAsLin3       ##
  ###########################  
  
  observe({
    labels_data_type<- input$file1$name
    updateSelectInput(session, "input_RA_MaAsLin3", choices = labels_data_type)
  })
  
  observe_differential_condition_pair("group1_MaAsLin3", "group2_MaAsLin3")
  
  data_MaAsLin3 <- eventReactive(input$action_MaAsLin3,{
    run_with_tracking("action_MaAsLin3", function() {
      files_to_delete <- c(
        "www/hmp2_output",
        "www/hmp2_output.zip"
      )

      for (file in files_to_delete) {
        full_path <- file.path(getwd(), file)

        if (file.exists(full_path)) {
          unlink(full_path, recursive = TRUE, force = TRUE)
          cat("Deleted:", full_path, "\n")
        } else {
          cat("File or folder not found, skipping:", full_path, "\n")
        }
      }

      source("scripts/MaAsLin3.R")
      labels_data_type<- input$file1$name
      MaAsLin3_summary(OTU_input = dataInput_RA_level()[[4]], group_index = dataInput_RA_level()[[9]], group1 = input$group1_MaAsLin3, group2 = input$group2_MaAsLin3,  select_MaAsLin3_normalization = input$select_MaAsLin3_normalization, select_MaAsLin3_transformation = input$select_MaAsLin3_transformation, select_MaAsLin3_correction = input$select_MaAsLin3_correction, select_MaAsLin3_pvalue = input$select_MaAsLin3_pvalue)
    })
  })
  
  output$text_MaAsLin3_level<- renderText({
    paste(data_MaAsLin3()[[1]])
  })

  
  #temp_dir <- getwd()
  zip_path <- file.path(getwd(),"/www/hmp2_output.zip")
  # Provide the ZIP file for download
  output$download_zip_MaAsLin3 <- metadavis_download("download_zip_MaAsLin3", 
    filename = function() {
      "Maaslin3_output.zip"
    },
    content = function(file) {
      # Copy the zip file to the location Shiny specifies
      file.copy(zip_path, file)
    },
    contentType = "application/zip"
  )

  
  ###########################
  ##      limma-voom       ##
  ###########################  
  
  observe({
    labels_data_type<- input$file1$name
    updateSelectInput(session, "input_RA_limma", choices = labels_data_type)
  })
  
  observe_differential_condition_pair("group1_limma", "group2_limma")
  
  
  
  data_limma <- eventReactive(input$action_limma,{
    run_with_tracking("action_limma", function() {
      source("scripts/limma.R")
      labels_data_type<- input$file1$name
      limma_summary(OTU_input = dataInput_RA_level()[[4]], group_index = dataInput_RA_level()[[9]], index_pvalue = input$select_limma_pvalue, group1 = input$group1_limma, group2 = input$group2_limma,  plot_method = input$select_limma_plot, alpha = input$limma_pvalue, limma_color_palette = input$limma_color_palette, show_plot_labels = input$show_limma_plot_labels, plot_top_n = input$limma_plot_top_n, plot_taxa_mode = input$limma_plot_taxa_mode)
    })
  })
  
  output$text_limma_level<- renderText({
    paste(data_limma()[[1]])
  })
  
  output$limma_table <- renderDataTable(DT::datatable((data_limma()[[2]]), options = list( pageLength = 15, scrollX = TRUE)))
  
  output$download_result_limma_1 <- metadavis_download("download_result_limma_1", 
    filename = function() { 
      paste("limma_result_significant", '.csv', sep='') },
    content = function(file){
      write.csv(data_limma()[[2]], file, row.names = FALSE)
    }
  )
  output$download_result_limma_2 <- metadavis_download("download_result_limma_2", 
    filename = function() { 
      paste("limma_result_all", '.csv', sep='') },
    content = function(file){
      write.csv(data_limma()[[3]], file, row.names = FALSE)
    }
  )
  output$download_result_limma_3 <- metadavis_download("download_result_limma_3", 
    filename = function() { 
      paste("total_counts_in_each_samples", '.csv', sep='') },
    content = function(file){
      write.csv(data_limma()[[4]], file, row.names = FALSE)
    }
  )
 
  
  output$boxplot_limma <- renderPlot({
    differential_selected_plot(data_limma(), 5, "limma_individual_plot_page")
  })

  output$limma_individual_plot_page_ui <- renderUI({
    differential_individual_plot_page_ui(data_limma(), 5, "limma_individual_plot_page")
  })
  
  
  output$download_Boxplot_limma<- metadavis_download("download_Boxplot_limma", 
    filename = function(){
      differential_plot_filename("limma_plot", input$select_image_type_limma, data_limma(), 5, "limma_individual_plot_page")
    },
    content = function(file){
      ggsave(file,plot = differential_selected_plot(data_limma(), 5, "limma_individual_plot_page"), width = input$Boxplot_limma_output_width, height = input$Boxplot_limma_output_height, dpi = input$Boxplot_limma_output_dpi, units = "in")
    }
  )    
  
  
  
  
  ###########################
  ##         EdgeR         ##
  ###########################  
  
  observe({
    labels_data_type<- input$file1$name
    updateSelectInput(session, "input_RA_edger", choices = labels_data_type)
  })
  
  observe_differential_condition_pair("group1_edger", "group2_edger")
  
  
  data_edger <- eventReactive(input$action_edger,{
    run_with_tracking("action_edger", function() {
      source("scripts/edger.R")
      labels_data_type<- input$file1$name
      edger_summary(OTU_input = dataInput_RA_level()[[4]], group_index = dataInput_RA_level()[[9]], index_pvalue = input$select_edger_pvalue, group1 = input$group1_edger, group2 = input$group2_edger, plot_method = input$select_edger_plot, alpha = input$edger_pvalue, edger_color_palette = input$edger_color_palette, show_plot_labels = input$show_edger_plot_labels, plot_top_n = input$edger_plot_top_n, plot_taxa_mode = input$edger_plot_taxa_mode)
    })
  })
  
  output$text_edger_level<- renderText({
    paste(data_edger()[[1]])
  })
  
  output$edger_table <- renderDataTable(DT::datatable((data_edger()[[2]]), options = list( pageLength = 15, scrollX = TRUE)))
  
  output$download_result_edger_1 <- metadavis_download("download_result_edger_1", 
    filename = function() { 
      paste("edger_result_significant", '.csv', sep='') },
    content = function(file){
      write.csv(data_edger()[[2]], file, row.names = FALSE)
    }
  )
  output$download_result_edger_2 <- metadavis_download("download_result_edger_2", 
    filename = function() { 
      paste("edger_result_all", '.csv', sep='') },
    content = function(file){
      write.csv(data_edger()[[3]], file, row.names = FALSE)
    }
  )
  output$download_result_edger_3 <- metadavis_download("download_result_edger_3", 
    filename = function() { 
      paste("total_counts_in_each_samples", '.csv', sep='') },
    content = function(file){
      write.csv(data_edger()[[4]], file, row.names = FALSE)
    }
  )
  
 
  output$boxplot_edger <- renderPlot({
    differential_selected_plot(data_edger(), 5, "edger_individual_plot_page")
  })

  output$edger_individual_plot_page_ui <- renderUI({
    differential_individual_plot_page_ui(data_edger(), 5, "edger_individual_plot_page")
  })
  
  
  output$download_Boxplot_edger<- metadavis_download("download_Boxplot_edger", 
    filename = function(){
      differential_plot_filename("edger_plot", input$select_image_type_edger, data_edger(), 5, "edger_individual_plot_page")
    },
    content = function(file){
      ggsave(file,plot = differential_selected_plot(data_edger(), 5, "edger_individual_plot_page"), width = input$Boxplot_edger_output_width, height = input$Boxplot_edger_output_height, dpi = input$Boxplot_edger_output_dpi, units = "in")
    }
  )   
  
  ###########################
  ##  Kruskal-Wallis Test  ##
  ###########################  
  
  observe({
    labels_data_type<- input$file1$name
    updateSelectInput(session, "input_kruskal_wallis_test", choices = labels_data_type)
  })
  
  data_kruskal_wallis_test <- eventReactive(input$action_kruskal_wallis_test,{
    run_with_tracking("action_kruskal_wallis_test", function() {
      source("scripts/kruskal_wallis_test.R")
      labels_data_type<- input$file1$name
      kruskal_wallis_test_summary(OTU_input = dataInput_RA_level()[[4]], group_index = dataInput_RA_level()[[9]], index_pvalue = input$select_kruskal_wallis_test_pvalue, ad_hoc = input$kruskal_wallis_test_ad_hoc, plot_method = input$kruskal_wallis_test_plot, alpha = input$kruskal_wallis_test_pvalue, kruskal_wallis_test_color_palette = input$kruskal_wallis_test_color_palette, show_plot_labels = input$show_kruskal_wallis_test_plot_labels, plot_top_n = input$kruskal_wallis_test_plot_top_n, plot_taxa_mode = input$kruskal_wallis_test_plot_taxa_mode)
    })
  })
  
  output$text_kruskal_wallis_test_level<- renderText({
    paste(data_kruskal_wallis_test()[[1]])
  })
  
  output$kruskal_wallis_test_table <- renderDataTable(DT::datatable((data_kruskal_wallis_test()[[2]]), options = list( pageLength = 15, scrollX = TRUE)))
  
  output$download_result_kruskal_wallis_test_1 <- metadavis_download("download_result_kruskal_wallis_test_1", 
    filename = function() { 
      paste("kruskal_wallis_test_result_significant", '.csv', sep='') },
    content = function(file){
      write.csv(data_kruskal_wallis_test()[[2]], file, row.names = FALSE)
    }
  )
  output$download_result_kruskal_wallis_test_2 <- metadavis_download("download_result_kruskal_wallis_test_2", 
    filename = function() { 
      paste("kruskal_wallis_test_result_all", '.csv', sep='') },
    content = function(file){
      write.csv(data_kruskal_wallis_test()[[3]], file, row.names = FALSE)
    }
  )
  output$download_result_kruskal_wallis_test_3 <- metadavis_download("download_result_kruskal_wallis_test_3", 
    filename = function() { 
      paste("kruskal_wallis_test_relative_frequency", '.csv', sep='') },
    content = function(file){
      write.csv(data_kruskal_wallis_test()[[4]], file)
    }
  )
  output$download_result_kruskal_wallis_test_4 <- metadavis_download("download_result_kruskal_wallis_test_4", 
    filename = function() { 
      paste("total_counts_in_each_samples", '.csv', sep='') },
    content = function(file){
      write.csv(data_kruskal_wallis_test()[[5]], file, row.names = FALSE)
    }
  )
  
   output$boxplot_kruskal_wallis_test <- renderPlot({
    differential_selected_plot(data_kruskal_wallis_test(), 6, "kruskal_wallis_test_individual_plot_page")
  })

  output$kruskal_wallis_test_individual_plot_page_ui <- renderUI({
    differential_individual_plot_page_ui(data_kruskal_wallis_test(), 6, "kruskal_wallis_test_individual_plot_page")
  })
  
  
  output$download_Boxplot_kruskal_wallis_test<- metadavis_download("download_Boxplot_kruskal_wallis_test", 
    filename = function(){
      differential_plot_filename("kruskal_wallis_test_plot", input$select_image_type_kruskal_wallis_test, data_kruskal_wallis_test(), 6, "kruskal_wallis_test_individual_plot_page")
    },
    content = function(file){
      ggsave(file,plot = differential_selected_plot(data_kruskal_wallis_test(), 6, "kruskal_wallis_test_individual_plot_page"), width = input$Boxplot_kruskal_wallis_test_output_width, height = input$Boxplot_kruskal_wallis_test_output_height, dpi = input$Boxplot_kruskal_wallis_test_output_dpi, units = "in")
    }
  ) 
  
  ###########################
  ##         ANOVA         ##
  ###########################  
  
  observe({
    labels_data_type<- input$file1$name
    updateSelectInput(session, "input_anova", choices = labels_data_type)
  })
  
  data_anova <- eventReactive(input$action_anova,{
    run_with_tracking("action_anova", function() {
      source("scripts/anova.R")
      labels_data_type<- input$file1$name
      anova_summary(OTU_input = dataInput_RA_level()[[4]], group_index = dataInput_RA_level()[[9]], index_pvalue = input$select_anova_pvalue, ad_hoc = input$anova_ad_hoc, plot_method = input$anova_plot, alpha = input$anova_pvalue, anova_color_palette = input$anova_color_palette, show_plot_labels = input$show_anova_plot_labels, plot_top_n = input$anova_plot_top_n, plot_taxa_mode = input$anova_plot_taxa_mode)
    })
  })
  
  output$text_anova_level<- renderText({
    paste(data_anova()[[1]])
  })
  
  output$anova_table <- renderDataTable(DT::datatable((data_anova()[[2]]), options = list( pageLength = 15, scrollX = TRUE)))
  
  output$download_result_anova_1 <- metadavis_download("download_result_anova_1", 
    filename = function() { 
      paste("anova_result_significant", '.csv', sep='') },
    content = function(file){
      write.csv(data_anova()[[2]], file, row.names = FALSE)
    }
  )
  output$download_result_anova_2 <- metadavis_download("download_result_anova_2", 
    filename = function() { 
      paste("anova_result_all", '.csv', sep='') },
    content = function(file){
      write.csv(data_anova()[[3]], file, row.names = FALSE)
    }
  )
  output$download_result_anova_3 <- metadavis_download("download_result_anova_3", 
    filename = function() { 
      paste("anova_relative_frequency", '.csv', sep='') },
    content = function(file){
      write.csv(data_anova()[[4]], file)
    }
  )
  output$download_result_anova_4 <- metadavis_download("download_result_anova_4", 
    filename = function() { 
      paste("total_counts_in_each_samples", '.csv', sep='') },
    content = function(file){
      write.csv(data_anova()[[5]], file, row.names = FALSE)
    }
  )
  
  
  output$boxplot_anova <- renderPlot({
    differential_selected_plot(data_anova(), 6, "anova_individual_plot_page")
  })

  output$anova_individual_plot_page_ui <- renderUI({
    differential_individual_plot_page_ui(data_anova(), 6, "anova_individual_plot_page")
  })
  
  
  output$download_Boxplot_anova<- metadavis_download("download_Boxplot_anova", 
    filename = function(){
      differential_plot_filename("anova_plot", input$select_image_type_anova, data_anova(), 6, "anova_individual_plot_page")
    },
    content = function(file){
      ggsave(file,plot = differential_selected_plot(data_anova(), 6, "anova_individual_plot_page"), width = input$Boxplot_anova_output_width, height = input$Boxplot_anova_output_height, dpi = input$Boxplot_anova_output_dpi, units = "in")
    }
  )    

  ###########################
  ##    Bulk Download ZIP   ##
  ###########################

  as_bulk_fun <- function(value) {
    if (is.function(value)) {
      return(value)
    }

    function() value
  }

  bulk_table_item <- function(action, path, filename, data, row_names = TRUE) {
    list(
      type = "table",
      action = action,
      path = path,
      filename = as_bulk_fun(filename),
      data = data,
      row_names = row_names
    )
  }

  bulk_plot_item <- function(action, path, filename_base, plot, width, height, dpi) {
    list(
      type = "plot",
      action = action,
      path = path,
      filename_base = as_bulk_fun(filename_base),
      plot = plot,
      width = as_bulk_fun(width),
      height = as_bulk_fun(height),
      dpi = as_bulk_fun(dpi)
    )
  }

  bulk_plot_pages_item <- function(action, path, filename_base, plots, width, height, dpi) {
    list(
      type = "plot_pages",
      action = action,
      path = path,
      filename_base = as_bulk_fun(filename_base),
      plots = plots,
      width = as_bulk_fun(width),
      height = as_bulk_fun(height),
      dpi = as_bulk_fun(dpi)
    )
  }

  bulk_plotly_item <- function(action, path, filename_base, plot, width, height) {
    list(
      type = "plotly",
      action = action,
      path = path,
      filename_base = as_bulk_fun(filename_base),
      plot = plot,
      width = as_bulk_fun(width),
      height = as_bulk_fun(height)
    )
  }

  bulk_maaslin3_item <- function(action, path) {
    list(
      type = "maaslin3",
      action = action,
      path = path
    )
  }

  sanitize_path_part <- function(value, fallback = "Untitled") {
    value <- paste(as.character(value), collapse = "_")
    value <- gsub("[<>:\"/\\\\|?*]+", "_", value)
    value <- gsub("[[:cntrl:]]+", "", value)
    value <- gsub("[[:space:]]+", " ", value)
    value <- trimws(value)

    if (!nzchar(value)) {
      return(fallback)
    }

    value
  }

  sanitize_filename <- function(value) {
    sanitize_path_part(value, fallback = "output")
  }

  safe_numeric_input <- function(value, fallback) {
    value <- suppressWarnings(as.numeric(value))

    if (length(value) == 0 || is.na(value[1]) || value[1] <= 0) {
      return(fallback)
    }

    value[1]
  }

  clamp_numeric <- function(value, minimum, maximum) {
    value <- safe_numeric_input(value, minimum)
    max(minimum, min(maximum, value))
  }

  bulk_count_samples <- function() {
    tryCatch({
      max(1, ncol(dataInput_RA_level()[[4]]))
    }, error = function(err) {
      1
    })
  }

  bulk_count_taxa <- function() {
    tryCatch({
      max(1, nrow(dataInput_RA_level()[[4]]))
    }, error = function(err) {
      1
    })
  }

  bulk_count_conditions <- function() {
    tryCatch({
      metadata <- dataInput_RA_level()[[7]]

      if (is.data.frame(metadata) && "Condition" %in% names(metadata)) {
        return(max(1, length(unique(metadata$Condition))))
      }

      max(1, length(dataInput_RA_level()[[10]]))
    }, error = function(err) {
      1
    })
  }

  bulk_result_rows <- function(action_id) {
    tryCatch({
      rows <- switch(
        action_id,
        action_wilcoxtest = nrow(data_wilcoxtest()[[2]]),
        action_ttest = nrow(data_ttest()[[2]]),
        action_metagenomeseq = nrow(data_metagenomeseq()[[2]]),
        action_deseq2 = nrow(data_deseq2()[[2]]),
        action_LEfSe = nrow(data_LEfSe()[[2]]),
        action_limma = nrow(data_limma()[[2]]),
        action_edger = nrow(data_edger()[[2]]),
        action_kruskal_wallis_test = nrow(data_kruskal_wallis_test()[[2]]),
        action_anova = nrow(data_anova()[[2]]),
        1
      )

      max(1, rows)
    }, error = function(err) {
      1
    })
  }

  auto_bulk_plot_dimensions <- function(item, plot_obj = NULL) {
    sample_count <- bulk_count_samples()
    taxa_count <- bulk_count_taxa()
    condition_count <- bulk_count_conditions()
    dims <- list(width = 8, height = 6, dpi = 300)

    dims <- switch(
      item$action,
      action_m1_bar_plot_group = list(
        width = clamp_numeric(4 + condition_count * 0.9, 8, 18),
        height = clamp_numeric(5 + safe_numeric_input(input$top_n_bar_plot_group, 15) * 0.28, 7, 26),
        dpi = 300
      ),
      action_m1_bar_plot_individual = list(
        width = clamp_numeric(5 + sample_count * 0.35, 10, 40),
        height = clamp_numeric(5 + safe_numeric_input(input$top_n_bar_plot_individual, 15) * 0.28, 7, 26),
        dpi = 300
      ),
      action_alpha_diversity = list(
        width = clamp_numeric(5 + condition_count * 1.1 + ifelse(identical(input$select_alpha, "8"), 4, 0), 8, 18),
        height = if (identical(input$select_alpha, "8")) 10 else 7,
        dpi = 300
      ),
      action_beta_diversity = list(
        width = clamp_numeric(8 + condition_count * 1.1, 12, 22),
        height = 7,
        dpi = 300
      ),
      action_pca = list(
        width = clamp_numeric(7 + sqrt(sample_count), 8, 18),
        height = 7,
        dpi = 300
      ),
      action_pca3d = list(
        width = clamp_numeric(7 + sqrt(sample_count), 8, 18),
        height = 7,
        dpi = 300
      ),
      action_tsne = list(
        width = clamp_numeric(7 + sqrt(sample_count), 8, 18),
        height = 7,
        dpi = 300
      ),
      action_umap = list(
        width = clamp_numeric(7 + sqrt(sample_count), 8, 18),
        height = 7,
        dpi = 300
      ),
      action_taxa_condition_based_correlation = {
        plot_dims <- tryCatch(taxa_condition_based_correlation_plot_dims(), error = function(err) list(width = 1100, height = 1000))
        list(
          width = clamp_numeric(plot_dims$width / 100, 9, 32),
          height = clamp_numeric(plot_dims$height / 100, 8, 32),
          dpi = 300
        )
      },
      action_samples_based_correlation = list(
        width = clamp_numeric(8 + sample_count * 0.15, 10, 32),
        height = clamp_numeric(8 + sample_count * 0.15, 10, 32),
        dpi = 300
      ),
      action_heatmap = list(
        width = clamp_numeric(6 + sample_count * 0.28, 10, 35),
        height = clamp_numeric(6 + taxa_count * 0.16, 8, 40),
        dpi = 300
      ),
      action_wilcoxtest = list(
        width = clamp_numeric(8 + sample_count * 0.18, 8, 28),
        height = clamp_numeric(6 + bulk_result_rows(item$action) * 0.28, 8, 40),
        dpi = 300
      ),
      action_ttest = list(
        width = clamp_numeric(8 + sample_count * 0.18, 8, 28),
        height = clamp_numeric(6 + bulk_result_rows(item$action) * 0.28, 8, 40),
        dpi = 300
      ),
      action_metagenomeseq = list(
        width = clamp_numeric(8 + sample_count * 0.18, 8, 28),
        height = clamp_numeric(6 + bulk_result_rows(item$action) * 0.28, 8, 40),
        dpi = 300
      ),
      action_deseq2 = list(
        width = clamp_numeric(8 + sample_count * 0.18, 8, 28),
        height = clamp_numeric(6 + bulk_result_rows(item$action) * 0.28, 8, 40),
        dpi = 300
      ),
      action_LEfSe = list(
        width = clamp_numeric(8 + sample_count * 0.18, 8, 28),
        height = clamp_numeric(6 + bulk_result_rows(item$action) * 0.28, 8, 40),
        dpi = 300
      ),
      action_limma = list(
        width = clamp_numeric(8 + sample_count * 0.18, 8, 28),
        height = clamp_numeric(6 + bulk_result_rows(item$action) * 0.28, 8, 40),
        dpi = 300
      ),
      action_edger = list(
        width = clamp_numeric(8 + sample_count * 0.18, 8, 28),
        height = clamp_numeric(6 + bulk_result_rows(item$action) * 0.28, 8, 40),
        dpi = 300
      ),
      action_kruskal_wallis_test = list(
        width = clamp_numeric(8 + sample_count * 0.18, 8, 30),
        height = clamp_numeric(7 + bulk_result_rows(item$action) * 0.3, 10, 45),
        dpi = 300
      ),
      action_anova = list(
        width = clamp_numeric(8 + sample_count * 0.18, 8, 30),
        height = clamp_numeric(7 + bulk_result_rows(item$action) * 0.3, 10, 45),
        dpi = 300
      ),
      dims
    )

    dims$width <- clamp_numeric(dims$width, 4, 50)
    dims$height <- clamp_numeric(dims$height, 4, 50)
    dims$dpi <- safe_numeric_input(dims$dpi, 300)
    dims
  }

  bulk_pca3d_static_plot <- function() {
    pca3d_data <- data_pca3d_table()[[2]]
    required_columns <- c("Samples", "PC1", "PC2", "PC3", "Condition")
    missing_columns <- setdiff(required_columns, names(pca3d_data))

    if (length(missing_columns)) {
      stop("PCA-3D result is missing columns: ", paste(missing_columns, collapse = ", "))
    }

    condition_values <- unique(pca3d_data$Condition)
    condition_values <- condition_values[!is.na(condition_values)]
    palette_name <- default_if_missing(input$select_pca3d_color_palette, "RdYlBu")

    if (!palette_name %in% rownames(RColorBrewer::brewer.pal.info)) {
      palette_name <- "RdYlBu"
    }

    max_colors <- RColorBrewer::brewer.pal.info[palette_name, "maxcolors"]
    base_colors <- RColorBrewer::brewer.pal(
      min(max_colors, max(3, min(max_colors, length(condition_values)))),
      palette_name
    )
    colors <- colorRampPalette(base_colors)(max(1, length(condition_values)))

    ggplot2::ggplot(
      pca3d_data,
      ggplot2::aes(x = PC1, y = PC2, color = Condition, size = PC3)
    ) +
      ggplot2::geom_point(alpha = 0.9) +
      ggplot2::scale_color_manual(values = colors) +
      ggplot2::scale_size_continuous(range = c(2.5, 8)) +
      ggplot2::theme_bw() +
      ggplot2::labs(
        title = "PCA-3D",
        subtitle = "Static export: PC1 vs PC2 with PC3 encoded by point size",
        x = "PC1",
        y = "PC2",
        color = "Condition",
        size = "PC3"
      )
  }

  bulk_path <- function(root, path, filename) {
    safe_parts <- vapply(path, sanitize_path_part, character(1))
    dir_path <- do.call(file.path, c(list(root), as.list(safe_parts)))
    dir.create(dir_path, recursive = TRUE, showWarnings = FALSE)
    file.path(dir_path, sanitize_filename(filename))
  }

  relative_path_parts <- function(relative_path) {
    dir_part <- dirname(relative_path)

    if (identical(dir_part, ".") || !nzchar(dir_part)) {
      return(character(0))
    }

    unlist(strsplit(dir_part, "[/\\\\]"), use.names = FALSE)
  }

  write_bulk_table <- function(value, file, row_names) {
    if (is.null(value)) {
      stop("No table data is available.")
    }

    if (!is.data.frame(value)) {
      value <- as.data.frame(value)
    }

    utils::write.csv(value, file, row.names = isTRUE(row_names))
  }

  write_bulk_zip <- function(zip_file, archive_root, temp_root) {
    if (!requireNamespace("zip", quietly = TRUE)) {
      stop("The zip package is required to create the bulk download.")
    }

    export_folder <- basename(archive_root)
    zip_args <- list(
      zipfile = zip_file,
      files = export_folder,
      recurse = TRUE,
      root = temp_root
    )

    if ("include_directories" %in% names(formals(zip::zipr))) {
      zip_args$include_directories <- TRUE
    }

    do.call(zip::zipr, zip_args)
  }

  save_bulk_item <- function(item, root, image_ext) {
    if (identical(item$type, "table")) {
      output_file <- bulk_path(root, item$path, item$filename())
      write_bulk_table(item$data(), output_file, item$row_names)
      return(character(0))
    }

    if (identical(item$type, "plot")) {
      plot_obj <- item$plot()
      plot_dims <- auto_bulk_plot_dimensions(item, plot_obj)
      output_file <- bulk_path(root, item$path, paste0(item$filename_base(), image_ext))
      ggsave(
        output_file,
        plot = plot_obj,
        width = plot_dims$width,
        height = plot_dims$height,
        dpi = plot_dims$dpi,
        units = "in"
      )
      return(character(0))
    }

    if (identical(item$type, "plot_pages")) {
      plot_pages <- item$plots()

      if (inherits(plot_pages, "ggplot")) {
        plot_pages <- list(plot_pages)
      }

      if (!is.list(plot_pages) || !length(plot_pages)) {
        stop("No plot pages are available.")
      }

      plot_dims <- list(
        width = clamp_numeric(item$width(), 4, 50),
        height = clamp_numeric(item$height(), 4, 50),
        dpi = safe_numeric_input(item$dpi(), 300)
      )

      for (page_index in seq_along(plot_pages)) {
        filename_suffix <- if (length(plot_pages) > 1) {
          paste0("_page_", sprintf("%02d", page_index))
        } else {
          ""
        }

        output_file <- bulk_path(root, item$path, paste0(item$filename_base(), filename_suffix, image_ext))
        ggsave(
          output_file,
          plot = plot_pages[[page_index]],
          width = plot_dims$width,
          height = plot_dims$height,
          dpi = plot_dims$dpi,
          units = "in"
        )
      }

      return(character(0))
    }

    if (identical(item$type, "plotly")) {
      plotly_format <- switch(
        tolower(image_ext),
        ".jpg" = "jpeg",
        ".jpeg" = "jpeg",
        ".png" = "png",
        ".pdf" = "pdf",
        ".svg" = "svg",
        ".eps" = "eps",
        NULL
      )

      if (is.null(plotly_format)) {
        stop("Plotly image export does not support ", image_ext, " format.")
      }

      if (!requireNamespace("plotly", quietly = TRUE)) {
        stop("The plotly package is not available for static image export.")
      }

      plot_dims <- auto_bulk_plot_dimensions(item)
      output_file <- bulk_path(root, item$path, paste0(item$filename_base(), image_ext))
      plotly::save_image(
        item$plot(),
        file = output_file,
        format = plotly_format,
        width = round(plot_dims$width * 100),
        height = round(plot_dims$height * 100)
      )
      return(character(0))
    }

    if (identical(item$type, "maaslin3")) {
      result <- data_MaAsLin3()
      output_dir <- result$output_dir

      if (is.null(output_dir) || is.na(output_dir) || !dir.exists(output_dir)) {
        return("MaAsLin3 output directory was not available.")
      }

      warnings <- character(0)
      output_zip <- result$output_zip

      if (!is.null(output_zip) && length(output_zip) && !is.na(output_zip) && file.exists(output_zip)) {
        output_file <- bulk_path(root, c(item$path, "MaAsLin3 ZIP"), "Maaslin3_output.zip")
        file.copy(output_zip, output_file, overwrite = TRUE)
      } else {
        fallback_zip <- file.path(dirname(output_dir), paste0(basename(output_dir), ".zip"))

        if (file.exists(fallback_zip)) {
          output_file <- bulk_path(root, c(item$path, "MaAsLin3 ZIP"), "Maaslin3_output.zip")
          file.copy(fallback_zip, output_file, overwrite = TRUE)
        } else {
          warnings <- c(warnings, "MaAsLin3 ZIP file was not available for inclusion in the bulk download.")
        }
      }

      table_files <- list.files(
        output_dir,
        pattern = "\\.(csv|tsv)$",
        recursive = TRUE,
        full.names = FALSE,
        ignore.case = TRUE
      )

      for (relative_file in table_files) {
        source_file <- file.path(output_dir, relative_file)
        table_data <- if (grepl("\\.tsv$", relative_file, ignore.case = TRUE)) {
          utils::read.delim(source_file, check.names = FALSE)
        } else {
          utils::read.csv(source_file, check.names = FALSE)
        }

        output_name <- paste0(tools::file_path_sans_ext(basename(relative_file)), ".csv")
        output_file <- bulk_path(
          root,
          c(item$path, "Tables", relative_path_parts(relative_file)),
          output_name
        )
        utils::write.csv(table_data, output_file, row.names = FALSE)
      }

      if (!is.null(result$total_counts)) {
        output_file <- bulk_path(root, c(item$path, "Tables"), "total_counts_in_each_samples.csv")
        write_bulk_table(result$total_counts, output_file, row_names = FALSE)
      }

      figures_dir <- file.path(output_dir, "figures")

      if (dir.exists(figures_dir)) {
        image_pattern <- paste0("\\", image_ext, "$")
        image_files <- list.files(
          figures_dir,
          pattern = image_pattern,
          recursive = TRUE,
          full.names = FALSE,
          ignore.case = TRUE
        )

        for (relative_file in image_files) {
          source_file <- file.path(figures_dir, relative_file)
          output_file <- bulk_path(
            root,
            c(item$path, "Figures", relative_path_parts(relative_file)),
            basename(relative_file)
          )
          file.copy(source_file, output_file, overwrite = TRUE)
        }

        available_images <- list.files(
          figures_dir,
          pattern = "\\.(png|pdf|jpg|jpeg|tiff|svg|bmp|eps|ps)$",
          recursive = TRUE,
          full.names = FALSE,
          ignore.case = TRUE
        )

        if (!length(image_files) && length(available_images)) {
          warnings <- c(
            warnings,
            paste0("MaAsLin3 figures were not available in ", image_ext, " format.")
          )
        }
      }

      return(warnings)
    }

    paste("Unsupported bulk item type:", item$type)
  }

  bulk_result_items <- function() {
    alpha_methods <- c("Observed", "Chao1", "ACE", "Shannon", "Simpson", "InvSimpson", "Fisher", "All_Combined")
    alpha_index <- suppressWarnings(as.integer(default_if_missing(input$select_alpha, 1)))

    if (!length(alpha_index) || is.na(alpha_index) || alpha_index < 1 || alpha_index > length(alpha_methods)) {
      alpha_index <- 1
    }

    alpha_method <- alpha_methods[alpha_index]

    if (is.na(alpha_method)) {
      alpha_method <- "Alpha"
    }

    list(
      bulk_table_item("action_level", c("Upload files", "Taxonomy table"), "taxonomy_table.csv", function() dataInput_RA_level()[[4]], row_names = TRUE),
      bulk_table_item("action_level", c("Upload files", "Metadata table"), "metadata_table.csv", function() dataInput_RA_level()[[7]], row_names = TRUE),
      bulk_table_item("action_level", c("Upload files", "No. of conditions"), "conditions_table.csv", function() dataInput_RA_level()[[10]], row_names = TRUE),
      bulk_table_item("action_level", c("Upload files", "Counts in samples"), "count_table.csv", function() dataInput_RA_level()[[11]], row_names = FALSE),

      bulk_plot_item("action_m1_bar_plot_group", c("Distribution", "Group", "Plot"), function() paste0("Bar_Plot_Group_Top_", input$top_n_bar_plot_group, "_", dataInput_RA_level()[[6]]), function() data_bar_plot_group(), function() input$bar_plot_group_output_width, function() input$bar_plot_group_output_height, function() input$bar_plot_group_output_dpi),
      bulk_plot_item("action_m1_bar_plot_individual", c("Distribution", "Individual", "Plot"), function() paste0("Bar_Plot_Individual_Top_", input$top_n_bar_plot_individual, "_", dataInput_RA_level()[[6]]), function() data_bar_plot_individual(), function() input$bar_plot_individual_output_width, function() input$bar_plot_individual_output_height, function() input$bar_plot_individual_output_dpi),

      bulk_plot_item("action_alpha_diversity", c("Diversity", "Alpha", "Alpha diversity plot"), function() paste0("Alpha_Diversity_", alpha_method, "_index"), function() data_Alpha_Div_plot()[[1]], function() input$Boxplot_alpha_div_output_width, function() input$Boxplot_alpha_div_output_height, function() input$Boxplot_alpha_div_output_dpi),
      bulk_table_item("action_alpha_diversity", c("Diversity", "Alpha", "Summary Table"), "Alpha_diversity_result.csv", function() data_Alpha_Div_plot()[[2]], row_names = TRUE),

      bulk_plot_item("action_beta_diversity", c("Diversity", "Beta", "Beta diversity Plot"), function() paste0("Beta_Diversity_", input$select_beta, "_", input$select_method), function() data_Beta_Div_plot()[[1]], function() input$Boxplot_beta_div_output_width, function() input$Boxplot_beta_div_output_height, function() input$Boxplot_beta_div_output_dpi),
      bulk_table_item("action_beta_diversity", c("Diversity", "Beta", "Summary Table"), "Beta_diversity_result.csv", function() data_Beta_Div_plot()[[2]], row_names = TRUE),
      bulk_table_item("action_beta_diversity", c("Diversity", "Beta", "Adonis Table"), "Beta_diversity_adonis_result.csv", function() data_Beta_Div_plot()[[3]], row_names = TRUE),

      bulk_plot_item("action_pca", c("Dimension reduction", "PCA-2D", "PCA 2D Plot"), "PCA_plot", function() data_pca_plot()[[1]], function() input$pca_plot_output_width, function() input$pca_plot_output_height, function() input$pca_plot_output_dpi),
      bulk_table_item("action_pca", c("Dimension reduction", "PCA-2D", "Summary Table"), "pca_result.csv", function() data_pca_plot()[[2]], row_names = TRUE),
      bulk_plot_item("action_pca3d", c("Dimension reduction", "PCA-3D", "PCA 3D Plot"), "PCA3D_plot", function() bulk_pca3d_static_plot(), 8, 7, 300),
      bulk_table_item("action_pca3d", c("Dimension reduction", "PCA-3D", "Summary Table"), "pca3d_result.csv", function() data_pca3d_table()[[2]], row_names = TRUE),
      bulk_plot_item("action_tsne", c("Dimension reduction", "t-SNE", "t-SNE Plot"), function() paste0("tsne_plot_", input$select_tsne_method), function() data_tsne_table()[[1]], function() input$tsne_plot_output_width, function() input$tsne_plot_output_height, function() input$tsne_plot_output_dpi),
      bulk_table_item("action_tsne", c("Dimension reduction", "t-SNE", "Summary Table"), function() paste0("tsne_result_", input$select_tsne_method, ".csv"), function() data_tsne_table()[[2]], row_names = TRUE),
      bulk_plot_item("action_umap", c("Dimension reduction", "UMAP", "UMAP Plot"), function() paste0("umap_plot_", input$select_umap_method), function() data_umap_table()[[1]], function() input$umap_plot_output_width, function() input$umap_plot_output_height, function() input$umap_plot_output_dpi),
      bulk_table_item("action_umap", c("Dimension reduction", "UMAP", "Summary Table based on condition"), function() paste0("umap_result_based_on_condition_", input$select_umap_method, ".csv"), function() data_umap_table()[[2]], row_names = TRUE),
      bulk_table_item("action_umap", c("Dimension reduction", "UMAP", "Summary Table based on cluster"), function() paste0("umap_result_based_on_cluster_", input$select_umap_method, ".csv"), function() data_umap_table()[[3]], row_names = TRUE),

      bulk_plot_item("action_taxa_condition_based_correlation", c("Correlation", "Taxa-based", "Correlation plot"), function() paste0("taxa_condition_based_correlation_plot_", input$select_taxa_condition_based_correlation_method), function() data_taxa_condition_based_correlation_table()[[1]], function() input$taxa_condition_based_correlation_output_width, function() input$taxa_condition_based_correlation_output_height, function() input$taxa_condition_based_correlation_output_dpi),
      bulk_table_item("action_taxa_condition_based_correlation", c("Correlation", "Taxa-based", "Summary Table"), function() paste0("taxa_condition_based_correlation_result_", input$select_taxa_condition_based_correlation_method, ".csv"), function() data_taxa_condition_based_correlation_table()[[2]], row_names = TRUE),
      bulk_plot_item("action_samples_based_correlation", c("Correlation", "Sample-based", "Correlation plot"), function() paste0("samples_based_correlation_plot_", input$select_samples_based_correlation_method), function() data_samples_based_correlation_table()[[1]], function() input$samples_based_correlation_output_width, function() input$samples_based_correlation_output_height, function() input$samples_based_correlation_output_dpi),
      bulk_table_item("action_samples_based_correlation", c("Correlation", "Sample-based", "Summary Table"), function() paste0("samples_based_correlation_result_", input$select_samples_based_correlation_method, ".csv"), function() data_samples_based_correlation_table()[[2]], row_names = TRUE),

      bulk_plot_item("action_heatmap", c("Heatmap", "Heatmap", "Plot"), "Heatmap", function() data_heatmap()[[2]], function() input$heatmap_output_width, function() input$heatmap_output_height, function() input$heatmap_output_dpi),

      bulk_table_item("action_wilcoxtest", c("Differential abundance", "Wilcoxon Rank Sum test", "Summary Table"), "wilcoxtest_result_significant.csv", function() data_wilcoxtest()[[2]], row_names = FALSE),
      bulk_table_item("action_wilcoxtest", c("Differential abundance", "Wilcoxon Rank Sum test", "Summary Table"), "wilcoxtest_result_all.csv", function() data_wilcoxtest()[[3]], row_names = FALSE),
      bulk_table_item("action_wilcoxtest", c("Differential abundance", "Wilcoxon Rank Sum test", "Summary Table"), "wilcoxtest_relative_frequency.csv", function() data_wilcoxtest()[[4]], row_names = TRUE),
      bulk_table_item("action_wilcoxtest", c("Differential abundance", "Wilcoxon Rank Sum test", "Summary Table"), "total_counts_in_each_samples.csv", function() data_wilcoxtest()[[5]], row_names = FALSE),
      bulk_plot_pages_item("action_wilcoxtest", c("Differential abundance", "Wilcoxon Rank Sum test", "Plot"), "wilcoxtest_plot", function() differential_plot_pages(data_wilcoxtest(), 6), function() input$Boxplot_wilcoxtest_output_width, function() input$Boxplot_wilcoxtest_output_height, function() input$Boxplot_wilcoxtest_output_dpi),

      bulk_table_item("action_ttest", c("Differential abundance", "t-test", "Summary Table"), "ttest_result_significant.csv", function() data_ttest()[[2]], row_names = FALSE),
      bulk_table_item("action_ttest", c("Differential abundance", "t-test", "Summary Table"), "ttest_result_all.csv", function() data_ttest()[[3]], row_names = FALSE),
      bulk_table_item("action_ttest", c("Differential abundance", "t-test", "Summary Table"), "ttest_relative_frequency.csv", function() data_ttest()[[4]], row_names = TRUE),
      bulk_table_item("action_ttest", c("Differential abundance", "t-test", "Summary Table"), "total_counts_in_each_samples.csv", function() data_ttest()[[5]], row_names = FALSE),
      bulk_plot_pages_item("action_ttest", c("Differential abundance", "t-test", "Plot"), "ttest_plot", function() differential_plot_pages(data_ttest(), 6), function() input$Boxplot_ttest_output_width, function() input$Boxplot_ttest_output_height, function() input$Boxplot_ttest_output_dpi),

      bulk_table_item("action_metagenomeseq", c("Differential abundance", "metagenomeSeq", "Summary Table"), "metagenomeseq_result_significant.csv", function() data_metagenomeseq()[[2]], row_names = FALSE),
      bulk_table_item("action_metagenomeseq", c("Differential abundance", "metagenomeSeq", "Summary Table"), "metagenomeseq_result_all.csv", function() data_metagenomeseq()[[3]], row_names = FALSE),
      bulk_table_item("action_metagenomeseq", c("Differential abundance", "metagenomeSeq", "Summary Table"), "total_counts_in_each_samples.csv", function() data_metagenomeseq()[[4]], row_names = FALSE),
      bulk_plot_pages_item("action_metagenomeseq", c("Differential abundance", "metagenomeSeq", "Plot"), "metagenomeseq_plot", function() differential_plot_pages(data_metagenomeseq(), 5), function() input$Boxplot_metagenomeseq_output_width, function() input$Boxplot_metagenomeseq_output_height, function() input$Boxplot_metagenomeseq_output_dpi),

      bulk_table_item("action_deseq2", c("Differential abundance", "DESeq2", "Summary Table"), "deseq2_result_significant.csv", function() data_deseq2()[[2]], row_names = FALSE),
      bulk_table_item("action_deseq2", c("Differential abundance", "DESeq2", "Summary Table"), "deseq2_result_all.csv", function() data_deseq2()[[3]], row_names = FALSE),
      bulk_table_item("action_deseq2", c("Differential abundance", "DESeq2", "Summary Table"), "deseq2_normalized_count.csv", function() data_deseq2()[[4]], row_names = TRUE),
      bulk_table_item("action_deseq2", c("Differential abundance", "DESeq2", "Summary Table"), "total_counts_in_each_samples.csv", function() data_deseq2()[[5]], row_names = FALSE),
      bulk_plot_pages_item("action_deseq2", c("Differential abundance", "DESeq2", "Plot"), "deseq2_plot", function() differential_plot_pages(data_deseq2(), 6), function() input$Boxplot_deseq2_output_width, function() input$Boxplot_deseq2_output_height, function() input$Boxplot_deseq2_output_dpi),

      bulk_table_item("action_LEfSe", c("Differential abundance", "LEfSe", "Summary Table"), "LEfSe_result_significant.csv", function() data_LEfSe()[[2]], row_names = FALSE),
      bulk_table_item("action_LEfSe", c("Differential abundance", "LEfSe", "Summary Table"), "total_counts_in_each_samples.csv", function() data_LEfSe()[[3]], row_names = FALSE),
      bulk_plot_item("action_LEfSe", c("Differential abundance", "LEfSe", "Plot"), "LEfSe_plot", function() data_LEfSe()[[4]], function() input$Boxplot_LEfSe_output_width, function() input$Boxplot_LEfSe_output_height, function() input$Boxplot_LEfSe_output_dpi),

      bulk_maaslin3_item("action_MaAsLin3", c("Differential abundance", "MaAsLin3", "Results")),

      bulk_table_item("action_limma", c("Differential abundance", "Limma-Voom", "Summary Table"), "limma_result_significant.csv", function() data_limma()[[2]], row_names = FALSE),
      bulk_table_item("action_limma", c("Differential abundance", "Limma-Voom", "Summary Table"), "limma_result_all.csv", function() data_limma()[[3]], row_names = FALSE),
      bulk_table_item("action_limma", c("Differential abundance", "Limma-Voom", "Summary Table"), "total_counts_in_each_samples.csv", function() data_limma()[[4]], row_names = FALSE),
      bulk_plot_pages_item("action_limma", c("Differential abundance", "Limma-Voom", "Plot"), "limma_plot", function() differential_plot_pages(data_limma(), 5), function() input$Boxplot_limma_output_width, function() input$Boxplot_limma_output_height, function() input$Boxplot_limma_output_dpi),

      bulk_table_item("action_edger", c("Differential abundance", "edgeR", "Summary Table"), "edger_result_significant.csv", function() data_edger()[[2]], row_names = FALSE),
      bulk_table_item("action_edger", c("Differential abundance", "edgeR", "Summary Table"), "edger_result_all.csv", function() data_edger()[[3]], row_names = FALSE),
      bulk_table_item("action_edger", c("Differential abundance", "edgeR", "Summary Table"), "total_counts_in_each_samples.csv", function() data_edger()[[4]], row_names = FALSE),
      bulk_plot_pages_item("action_edger", c("Differential abundance", "edgeR", "Plot"), "edger_plot", function() differential_plot_pages(data_edger(), 5), function() input$Boxplot_edger_output_width, function() input$Boxplot_edger_output_height, function() input$Boxplot_edger_output_dpi),

      bulk_table_item("action_kruskal_wallis_test", c("Differential abundance", "Kruskal-Wallis test", "Summary Table"), "kruskal_wallis_test_result_significant.csv", function() data_kruskal_wallis_test()[[2]], row_names = FALSE),
      bulk_table_item("action_kruskal_wallis_test", c("Differential abundance", "Kruskal-Wallis test", "Summary Table"), "kruskal_wallis_test_result_all.csv", function() data_kruskal_wallis_test()[[3]], row_names = FALSE),
      bulk_table_item("action_kruskal_wallis_test", c("Differential abundance", "Kruskal-Wallis test", "Summary Table"), "kruskal_wallis_test_relative_frequency.csv", function() data_kruskal_wallis_test()[[4]], row_names = TRUE),
      bulk_table_item("action_kruskal_wallis_test", c("Differential abundance", "Kruskal-Wallis test", "Summary Table"), "total_counts_in_each_samples.csv", function() data_kruskal_wallis_test()[[5]], row_names = FALSE),
      bulk_plot_pages_item("action_kruskal_wallis_test", c("Differential abundance", "Kruskal-Wallis test", "Plot"), "kruskal_wallis_test_plot", function() differential_plot_pages(data_kruskal_wallis_test(), 6), function() input$Boxplot_kruskal_wallis_test_output_width, function() input$Boxplot_kruskal_wallis_test_output_height, function() input$Boxplot_kruskal_wallis_test_output_dpi),

      bulk_table_item("action_anova", c("Differential abundance", "ANOVA", "Summary Table"), "anova_result_significant.csv", function() data_anova()[[2]], row_names = FALSE),
      bulk_table_item("action_anova", c("Differential abundance", "ANOVA", "Summary Table"), "anova_result_all.csv", function() data_anova()[[3]], row_names = FALSE),
      bulk_table_item("action_anova", c("Differential abundance", "ANOVA", "Summary Table"), "anova_relative_frequency.csv", function() data_anova()[[4]], row_names = TRUE),
      bulk_table_item("action_anova", c("Differential abundance", "ANOVA", "Summary Table"), "total_counts_in_each_samples.csv", function() data_anova()[[5]], row_names = FALSE),
      bulk_plot_pages_item("action_anova", c("Differential abundance", "ANOVA", "Plot"), "anova_plot", function() differential_plot_pages(data_anova(), 6), function() input$Boxplot_anova_output_width, function() input$Boxplot_anova_output_height, function() input$Boxplot_anova_output_dpi)
    )
  }

  output$bulk_download_summary <- renderUI({
    actions <- completed_actions()

    if (!length(actions)) {
      return(div(class = "run-log-empty", "No completed analysis outputs are available yet."))
    }

    completed_labels <- vapply(
      sort(unique(actions)),
      function(action_id) {
        cfg <- tracked_actions[[action_id]]

        if (is.null(cfg)) {
          return(action_id)
        }

        format_scope(cfg$menu, cfg$tab)
      },
      character(1)
    )

    tagList(
      p(paste(length(completed_labels), "completed analysis section(s) will be included.")),
      tags$ul(lapply(completed_labels, tags$li))
    )
  })

  output$download_bulk_results <- metadavis_download("download_bulk_results", 
    filename = function() {
      paste0("MetaDAVis_bulk_results_", format(Sys.time(), "%Y-%m-%d_%H-%M-%S"), ".zip")
    },
    content = function(file) {
      image_ext <- default_if_missing(isolate(input$bulk_image_type), ".jpg")
      actions <- isolate(completed_actions())
      items <- bulk_result_items()
      selected_items <- Filter(function(item) item$action %in% actions, items)
      export_stamp <- format(Sys.time(), "%Y-%m-%d_%H-%M-%S")
      tmp_root <- file.path(tempdir(), paste0("MetaDAVis_bulk_", session$token, "_", as.integer(Sys.time())))
      archive_root <- file.path(tmp_root, paste0("MetaDAVis_bulk_results_", export_stamp))

      if (dir.exists(tmp_root)) {
        unlink(tmp_root, recursive = TRUE, force = TRUE)
      }

      dir.create(archive_root, recursive = TRUE, showWarnings = FALSE)
      on.exit(unlink(tmp_root, recursive = TRUE, force = TRUE), add = TRUE)

      progress <- shiny::Progress$new(session, min = 0, max = 1, style = "notification")
      on.exit(progress$close(), add = TRUE)

      total_steps <- length(selected_items) + 2L
      current_step <- 0L
      warnings <- character(0)

      set_bulk_progress <- function(detail, value = NULL) {
        if (is.null(value)) {
          current_step <<- current_step + 1L
          value <- min(0.95, current_step / max(total_steps, 1L))
        }

        progress$set(
          message = "Preparing bulk download",
          detail = detail,
          value = value
        )
      }

      set_bulk_progress("Adding sessionInfo.txt")
      writeLines(sess_txt(), con = file.path(archive_root, "sessionInfo.txt"), useBytes = TRUE)

      if (!length(selected_items)) {
        set_bulk_progress("No completed analysis outputs to extract")
      } else {
        for (item in selected_items) {
          output_label <- paste(c(item$path, item$type), collapse = " / ")
          set_bulk_progress(paste("Extracting", output_label))

          item_warnings <- tryCatch(
            save_bulk_item(item, archive_root, image_ext),
            error = function(err) {
              paste(output_label, "could not be exported:", conditionMessage(err))
            }
          )

          warnings <- c(warnings, item_warnings)
        }
      }

      warnings <- warnings[nzchar(warnings)]

      if (length(warnings)) {
        writeLines(
          warnings,
          con = file.path(archive_root, "bulkDownload_warnings.txt"),
          useBytes = TRUE
        )
      }

      set_bulk_progress("Compressing ZIP file", value = 0.98)

      if (file.exists(file)) {
        unlink(file)
      }

      write_bulk_zip(file, archive_root, tmp_root)
      progress$set(message = "Preparing bulk download", detail = "Download ready", value = 1)

      showNotification("Bulk download ZIP is ready.", type = "message", duration = 5)
    },
    contentType = "application/zip"
  )
  
  
  
 
  
}
