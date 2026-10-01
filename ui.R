library(shiny)
library(shinythemes)
library(shinyFiles)
library(DT)
library(shinyjs)
#library(bslib)
#options(warn=-1)
shinythemes::themeSelector()
timeoutSeconds <- 36000

inactivity <- sprintf("function idleTimer() {
var t = setTimeout(logout, %s);
window.onmousemove = resetTimer; // catches mouse movements
window.onmousedown = resetTimer; // catches mouse movements
window.onclick = resetTimer;     // catches mouse clicks
window.onscroll = resetTimer;    // catches scrolling
window.onkeypress = resetTimer;  //catches keyboard actions

function logout() {
Shiny.setInputValue('timeOut', '%ss')
}

function resetTimer() {
clearTimeout(t);
t = setTimeout(logout, %s);  // time is in milliseconds (1000 is 1 second)
}
}
idleTimer();", timeoutSeconds*1000, timeoutSeconds, timeoutSeconds*1000)
shinyUI(
    navbarPage(
    theme = shinytheme("cerulean"),
    "",
    id = "main_navbar",
    header = tagList(
      tags$head(
        tags$style(HTML("
          .run-status-badge {
            display: inline-block;
            padding: 4px 10px;
            margin-right: 10px;
            border-radius: 999px;
            font-size: 12px;
            font-weight: 700;
            letter-spacing: 0.02em;
            text-transform: uppercase;
          }
          .run-status-badge.idle,
          .run-status-badge.viewed {
            background: #e5e7eb;
            color: #374151;
          }
          .run-status-badge.loading,
          .run-status-badge.running {
            background: #fef3c7;
            color: #92400e;
          }
          .run-status-badge.completed-successfully {
            background: #dcfce7;
            color: #166534;
          }
          .run-status-badge.failed {
            background: #fee2e2;
            color: #991b1b;
          }
          .run-log-card {
            margin-bottom: 14px;
            padding: 14px 16px;
            border-radius: 8px;
            border: 1px solid #d9e2ef;
            border-left: 5px solid #94a3b8;
            background: #ffffff;
            box-shadow: 0 1px 3px rgba(15, 23, 42, 0.08);
          }
          .run-log-card.loading,
          .run-log-card.running {
            border-left-color: #f59e0b;
          }
          .run-log-card.completed-successfully {
            border-left-color: #16a34a;
          }
          .run-log-card.failed {
            border-left-color: #dc2626;
          }
          .run-log-card.viewed {
            border-left-color: #64748b;
          }
          .run-log-title {
            font-size: 16px;
            font-weight: 700;
            margin-bottom: 6px;
          }
          .run-log-meta {
            color: #64748b;
            font-size: 13px;
            margin-bottom: 8px;
          }
          .run-log-message {
            margin-bottom: 10px;
          }
          .run-log-params {
            margin: 0;
            padding: 10px 12px;
            border-radius: 6px;
            background: #f8fafc;
            white-space: pre-wrap;
            font-family: Consolas, 'Courier New', monospace;
            font-size: 12px;
          }
          .run-log-empty {
            padding: 14px 16px;
            border-radius: 8px;
            border: 1px dashed #cbd5e1;
            background: #f8fafc;
            color: #475569;
          }
          .shiny-notification-panel {
            top: auto !important;
            right: 18px !important;
            bottom: 18px !important;
            left: auto !important;
            width: 320px !important;
            max-width: calc(100vw - 24px);
            z-index: 2050;
          }
          .shiny-notification {
            width: 320px !important;
            max-width: calc(100vw - 24px) !important;
            padding: 10px 12px !important;
            border-radius: 8px !important;
            box-shadow: 0 10px 24px rgba(15, 23, 42, 0.18) !important;
          }
          .shiny-progress-notification {
            border-left: 4px solid #0ea5e9 !important;
          }
          .shiny-progress-notification .progress {
            height: 6px !important;
            margin-bottom: 8px !important;
            border-radius: 999px;
          }
          .shiny-progress-notification .progress-text .progress-message {
            display: block;
            font-size: 14px;
            font-weight: 700;
            color: #334155;
          }
          .shiny-progress-notification .progress-text .progress-detail {
            display: block;
            margin-top: 4px;
            font-size: 12px;
            color: #64748b;
          }
          .shiny-notification-message {
            border-left: 4px solid #16a34a !important;
          }
          .shiny-notification-warning {
            border-left: 4px solid #d97706 !important;
          }
          .shiny-notification-error {
            border-left: 4px solid #dc2626 !important;
          }
          @media (max-width: 767px) {
            .shiny-notification-panel {
              right: 10px !important;
              bottom: 10px !important;
              width: calc(100vw - 20px) !important;
            }
            .shiny-notification {
              width: calc(100vw - 20px) !important;
            }
          }
        "))
      )
    ),
    tabPanel(
      "MetaDAVis",
	  tags$script(inactivity),
      mainPanel(
        h1("MetaDAVis",align = "center"),
        hr(),
        h3("Introduction"),
        hr(),
        p(strong("MetaDAVis"), " (interactive Metagenome Data Analysis and Visualization) is a browser-based and user-friendly R Shiny application for researchers without programming proficiency to analyze and visualize metagenomics results from kingdom to species level. It comprises six functional analyses and one bulk download utility."),
        
        HTML("<B>The package includes the following:</B><br>
        <ul>
          <li>Data summary and abundance distribution</li>
          The data can be visualized in the stacked bar plot for abundance percentage, value, and relative frequency from 2 to 100 Taxa.<br>
            <ul>
            <li>Group: Samples are grouped by given conditions based on the metadata. </li>
            <li>Individual: Sample-based plots.</li>
            </ul>
          <li>Diversity analysis</li>
            <ul>
            <li>Alpha: Seven different methods was used from the phyloseq package. The results were displayed in a box and violin plot with a summary table.</li>
            <li>Beta: A total of 42 different diversity metrics were integrated from the phyloseq (unlist(distanceMethodList)) package with six selection methods. Results are visualized by bar and ordination with a summary table. Boxplot Wilcoxon p-values and the PERMANOVA p-value can be shown on the plot.</li>
            </ul>
          <li>Dimension reduction</li>
            <ul>
            <li>PCA: The ggfortify and plotly package was used to display plots in 2D(with and without labels and frame) and 3D with their summary table.</li>
            <li>t-SNE: Six different methods was used from the scater package. The samples were displayed in a t-SNE plot (2 and 3 dimensions) with a summary table.</li>
            <li>UMAP: Six different methods was used from the scater package. Displays the UMAP plot in sample and cluster-based with a summary table.</li>
            </ul>
          <li>Correlation analysis</li>
            <ul>
            <li>Taxa-based: The ggfortify package was used to display plot (with and without labels and frame) and their summary table.</li>
            <li>Sample-based: Six different methods was used from the scater package. The result was displayed in 2 and 3 dimensions t-SNE plots with a summary table.</li>
            </ul>
          <li>Heatmap: It was integrated with the ComplexHeatmap package. Display heatmap with and without row and column dendrograms and names.</li>
          <li>Differential abundance</li>
            <ul>
            <li>Two groups: Eight different analyses were provided using the Wilcoxon Rank Sum test, t-test, metagenomeSeq, DESeq2, Limma-Voom, edgeR, LEfSe, and MaAsLin3. Fold-change results are interpreted as condition2/condition1, and the condition selected in condition1 is removed from the condition2 choices.</li>
            <li>Individual box plots in differential analysis display up to 25 significant taxa per image using a 5 column by 5 row layout. When more than 25 significant taxa are available, each plot image can be selected and downloaded separately.</li>
            <li>Multiple groups comparison: Two different analyses, such as Kruskal-Wallis test and ANOVA was used for more than multiple group comparisons. These will perform statistical analysis and generate plots and summary tables based on the significant taxa.</li>
            </ul>
          <li>Bulk download</li>
            <ul>
            <li>The Bulk Download menu exports all completed analysis images and tables in one structured ZIP folder. The Completed Outputs panel lists the analyses that will be included, users can choose JPG, PNG, TIFF, PDF, SVG, BMP, EPS, or PS for plots, differential individual plot pages are exported as separate image files, tables are saved as CSV files, MaAsLin3 output ZIP is included when available, sessionInfo.txt is added for reproducibility, and bulkDownload_warnings.txt is added if any completed output cannot be exported.</li>
            </ul>
          </ul>
        <p>*It provides publication quality plots in multiple formats and summary tables (.csv format) to visualize, download individually, or export together through Bulk Download.</p>
        <hr>
        <h3> use MetaDAVis online</h3>
        <p>MetaDAVis is deployed at: <a href='https://www.gudalab-rtools.net/MetaDAVis'>https://www.gudalab-rtools.net/MetaDAVis</a></p>
        <hr>
        <h3> Launch MetaDAVis using R and GitHub</h3>
        <p> MetaDAVis were deposited under the GitHub repository: <a href='https://github.com/GudaLab/MetaDAVis'>https://github.com/GudaLab/MetaDAVis</a><br>
        Before running the app, the user must have R (>= 4.4.2), RStudio (>= 2024.12.0), Bioconductor (>= 3.20) and Shiny (>= 1.10.0) (Tested with this version).<br>
         If users use an older R version, they may encounter errors in installing packages, So the users are recommended to update their R version first.<br>
         Once the user opens the R in the command line or Rstudio, need to run the following command in R to install the shiny package.<br><br></p>
          
<pre>install.packages('shiny')<br>
library(shiny)</pre>
          <hr>
          <h3>Start the app</h3>
          Start the R session using RStudio and run these lines:<br><br>
<pre>shiny::runGitHub('MetaDAVis','GudaLab')</pre>
or
Alternatively, download the source code from GitHub and run the following command in the R session using RStudio:
<pre>
library(shiny)
runApp('/path/to/the/MetaDAVis-master', launch.browser=TRUE)</pre>
<hr>
<h3>Help manual for the usage of MetaDAVis <a href='manual/MetaDAVis_manual.pdf', target='_blank'>[Download]</a></h3>
<hr>
<h3>Cite: </h3>
<p> Jagadesan S, Guda C (2025) MetaDAVis: An R shiny application for metagenomic data analysis and visualization. PLoS ONE 20(4): e0319949. <a href='https://doi.org/10.1371/journal.pone.0319949'>https://doi.org/10.1371/journal.pone.0319949</a></p>
<hr>
<h3> Developed and maintained by</h3>
<p>This application was created by Sankarasubramanian Jagdesan and Babu Guda.  We share the passion about developing an user-friendly tool for all biologists, especially those who do not have access to bioinformaticians or programming efficency.
</p>
<hr>
"),
tags$head(
         tags$style(HTML("
    #view_count { color: #dc2626; font-weight: 700; }  /* red, bold number */
    .views-center { text-align: center; }
  "))
       ),
       tags$div(class = "views-center", tags$p("Total number of views: ",textOutput("view_count", inline = TRUE))),
       hr(),
        
    ),
   ),

       tabPanel(
      "Upload files",
      useShinyjs(),
      sidebarLayout(
        sidebarPanel(
          h3("Upload files"),
          selectInput("select_file_type", label = "Select Input format", choices = list("Qiime2" = "qiime_format", "Megan" ="Megan", "Taxa count file (prepare your own file based on example)" = "check", "Example data (To test our tool)" ="example", "Galaxy input (OTU table, taxonomy and metadata uploaded to Galaxy)" ="galaxy"), selected = "qiime_format"),
          h5("The file accepts .txt or .tsv (Megan and users own file) or .csv formats (Qiime2). Galaxy input accepts tab or comma separated tables, detected automatically."),
           fluidRow(
          box(id = "box1", width = 12,
          column(width = 8, fileInput("file1", "Upload count file ", accept = c(".tsv", ".txt", ".csv"),multiple = F)),
          column(width = 4, selectInput("file1_split", "Fields separated by", c("tab" = "\t", "Comma" =","), selected = "tab")),
          ),
           ),
          fluidRow(
          box(id = "box2", width = 12,
          column(width = 8, fileInput("metadata", "Upload meta-data ", accept = c(".tsv", ".txt", ".csv"),multiple = F)),
          column(width = 4, selectInput("metadata_split", "Fields separated by", c("tab" = "\t", "Comma" =","), selected = "tab")),
          ),
          ),
          radioButtons("select_RA_type", "Choose the level to display", choices = c("Kingdom" = 1, "Phylum" = 2, "Class" = 3, "Order" = 4, "Family" = 5,  "Genus" = 6, "Species" = 7), selected = 4),
          actionButton("action_level", "Submit")
          
        ),
        mainPanel(
          tabsetPanel(
            type = "tabs",
            tabPanel(
              "Summary",    
         h4("Download example data"),
         tags$b("Qiime2 format"),
         br(),
         a(href="example_data/Qiime2_Greengenes_output_level7.csv", "Qiime2 Greengenes Output format",download=NA, target="_blank"),
         br(),
         a(href="example_data/Qiime2_metadata_for_Greengenes.csv", "Qiime2 metadata for greengenes",download=NA, target="_blank"),
         br(),
         a(href="example_data/Qiime2_Silva_output_level7.csv", "Qiime2 Silva Outpus format",download=NA, target="_blank"),
         br(),
         a(href="example_data/Qiime2_metadata_for_Silva.csv", "Qiime2 metadata for Silva",download=NA, target="_blank"),
         br(),
         tags$b("MEGAN output format"),
         br(),
         a(href="example_data/Megan_WGS_output.tsv", "Megan WGS output format",download=NA, target="_blank"),
         br(),
         a(href="example_data/Megan_WGS_metadata.tsv", "Megan metadata",download=NA, target="_blank"),
         br(),
         tags$b("Taxa count file (Prepare your input accourding to our count and metadata format)",download=NA, target="_blank"),
         br(),
         a(href="example_data/Taxa_count_file.tsv", "Count files format",download=NA, target="_blank"),
         br(),
         a(href="example_data/Tax_metadata.tsv", "Metadata",download=NA, target="_blank"),
         #downloadButton("Qiime2_metadata_for_Silva.csv", "Metadata"),
         hr(),
         h4("After the data is uploaded and checked, it will be displayed in the table summary below."),
          hr(),
          h4("Number of OTUs"),
         withSpinner(verbatimTextOutput("text_level")),
          h4("Metadata"),
         withSpinner(verbatimTextOutput("text_metadata"))
            ),
          tabPanel(
            "Taxonomy table",
            h4("Display the taxonomy counts for each samples"),
            withSpinner(dataTableOutput("taxonomy_table")),
           downloadButton(outputId = "download_taxonomy_table", label = "Download as csv"),
          ),
            tabPanel(
              "Metadata table",
              h4("Display the metadata file"),
              fluidRow(
                column(
              withSpinner(dataTableOutput("metadata_table")),
              downloadButton(outputId = "download_metadata_table", label = "Download as csv"), width = 4),
          ),
        ),
          tabPanel(
            "No. of conditions",
            h4("Display the number of condition based on your metadata"),
              fluidRow(
              column(
                withSpinner(dataTableOutput("conditions_table")),
            downloadButton(outputId = "download_conditions_table", label = "Download as csv"), width = 4),
              ),
          ),
          tabPanel(
            "Counts in samples",
            h4("Display the total number of counts in each samples"),
            fluidRow(
              column(
                withSpinner(dataTableOutput("count_table")),
            downloadButton(outputId = "download_count_table", label = "Download as csv"), width = 4),
            ),
          ),
        ),
      ),
    ),
    ),
    #distribution
    navbarMenu(
      "Distribution",
      tabPanel(
        "Group",
        sidebarLayout(
          sidebarPanel(
            h3("Distribution of top bacterial taxa (groups)"), selectInput("input_RA_bar_plot_group", label = "Selected input", choices = "Example data selected"),
            radioButtons("select_plot_type_group", "Types of plot", choices = c("Abundance (%) - stacked bar" = 1,"Abundance value - stacked bar" = 2, "Relative frequency - stacked bar" = 3), selected = 1),
            selectInput("color_palette_group", label = "Colors", choices = list("RdYlBu"="RdYlBu", "RdYlGn"="RdYlGn", "BrBG"="BrBG", "PiYG"="PiYG", "PRGn"="PRGn", "PuOr"="PuOr", "RdBu"="RdBu", "RdGy"="RdGy", "Spectral"="Spectral", "Blues"="Blues", "Reds"="Reds", "Greens"="Greens", "Greys"="Greys", "Oranges"="Oranges", "Accent"="Accent", "Dark2"="Dark2","Paired"="Paired","Pastel1"="Pastel1","Pastel2"="Pastel2","Set1"="Set1","Set2"="Set2","Set3"="Set3"), selected = "RdYlBu"),
            numericInput("top_n_bar_plot_group", label = "Number of top bacterial taxa (Max = 100)", value = 15),
            selectInput("select_image_type_group", label = "Output image format", choices = list("JPG" = ".jpg", "TIFF" =".tiff", "PDF" = ".pdf",  "SVG" = ".svg", "BMP" = ".bmp", "EPS" = ".eps", "PS" = ".ps"), selected = ".jpg"),
            actionButton("action_m1_bar_plot_group", "Submit"),
          ),
          mainPanel(
            withSpinner(plotOutput("bar_plot_group", width = "50%", height = "600px")),
            fluidRow(
            column(3, numericInput("bar_plot_group_output_height", label = h5("Figure height (upto 49 inces)"), value = 8, width = "300px")),
            column(3, numericInput("bar_plot_group_output_width", label = h5("Figure width (upto 49 inces)"), value = 8, width = "300px")),
            column(3, numericInput("bar_plot_group_output_dpi", label = h5("Figure resolution (dpi:72 to 300)"), value = 300, width = "300px")),
            ),
            downloadButton(outputId = "download_bar_plot_group", label = "Download plot"),
          ),
        ),
      ),
      tabPanel(
        "Individual",
        sidebarLayout(
          sidebarPanel(
            h3("Distribution of top bacterial taxa (samples)"),
            selectInput("input_RA_bar_plot_individual", label = "Selected input", choices = "Example data selected"),
            radioButtons("select_plot_type_individual", "Types of plot", choices = c("Abundance (%) - stacked bar" = 1,"Abundance value - stacked bar" = 2, "Relative frequency - stacked bar" = 3), selected = 1),
            selectInput("color_palette_individual", label = "Colors", choices = list("RdYlBu"="RdYlBu", "RdYlGn"="RdYlGn", "BrBG"="BrBG", "PiYG"="PiYG", "PRGn"="PRGn", "PuOr"="PuOr", "RdBu"="RdBu", "RdGy"="RdGy", "Spectral"="Spectral", "Blues"="Blues", "Reds"="Reds", "Greens"="Greens", "Greys"="Greys", "Oranges"="Oranges", "Accent"="Accent", "Dark2"="Dark2","Paired"="Paired","Pastel1"="Pastel1","Pastel2"="Pastel2","Set1"="Set1","Set2"="Set2","Set3"="Set3"), selected = "RdYlBu"),
            numericInput("top_n_bar_plot_individual", label = "Number of top bacterial taxa (Max = 100)", value = 15),
            selectInput("select_image_type_individual", label = "Output image format", choices = list("JPG" = ".jpg", "TIFF" =".tiff", "PDF" = ".pdf",  "SVG" = ".svg", "BMP" = ".bmp", "EPS" = ".eps", "PS" = ".ps"), selected = ".jpg"),
            actionButton("action_m1_bar_plot_individual", "Submit"),
          ),
          mainPanel(
            withSpinner(plotOutput("bar_plot_individual", height = "600px")),
            fluidRow(
            column(3, numericInput("bar_plot_individual_output_height", label = h5("Figure height (upto 49 inces)"), value = 8, width = "300px")),
            column(3, numericInput("bar_plot_individual_output_width", label = h5("Figure width (upto 49 inces)"), value = 8, width = "300px")),
            column(3, numericInput("bar_plot_individual_output_dpi", label = h5("Figure resolution (dpi:72 to 300)"), value = 300, width = "300px")),
            ),
            downloadButton(outputId = "download_bar_plot_individual", label = "Download plot")
          ),
        ),
      ),
    ),   
    
    #Diversity
    navbarMenu("Diversity",
               tabPanel(
                 "Alpha",
                 sidebarLayout(
                   sidebarPanel(
                     h3("Alpha diversity"),
                     selectInput("input_RA_Alpha", label = "Selected input",choices = "Example data selected"),
                     selectInput("select_alpha", label = "Select Method", choices = list("Observed" = 1, "Chao1" = 2, "ACE" = 3, "Shannon" = 4, "Simpson" = 5, "InvSimpson" = 6, "Fisher"=7, "All_Combined"=8), selected = 1),
                     radioButtons("select_alpha_pvalue", label = "Wilcoxon test", choices = c("Yes (show's Pvalue)" = "Yes", "No" = "No", "Show *" = "star"), selected = "No"),
                     radioButtons("select_plot",label = "Types of plot", choices = c("Box plot" = 1, "Violin plot" = 2), selected = 1),
                     selectInput("select_alpha_color_palette", label = "Colors", choices = list("RdYlBu"="RdYlBu", "RdYlGn"="RdYlGn", "BrBG"="BrBG", "PiYG"="PiYG", "PRGn"="PRGn", "PuOr"="PuOr", "RdBu"="RdBu", "RdGy"="RdGy", "Spectral"="Spectral", "Blues"="Blues", "Reds"="Reds", "Greens"="Greens", "Greys"="Greys", "Oranges"="Oranges", "Accent"="Accent", "Dark2"="Dark2","Paired"="Paired","Pastel1"="Pastel1","Pastel2"="Pastel2","Set1"="Set1","Set2"="Set2","Set3"="Set3"), selected = "RdYlBu"),
                     selectInput("select_image_type_alpha", label = "Output image format", choices = list("JPG" = ".jpg", "TIFF" =".tiff", "PDF" = ".pdf",  "SVG" = ".svg", "BMP" = ".bmp", "EPS" = ".eps", "PS" = ".ps"), selected = ".jpg"),
                     actionButton("action_alpha_diversity", "Submit"),
                   ),
                   mainPanel(
                     tabsetPanel(
                       type = "tabs",
                       tabPanel(
                         "Alpha diversity plot",
                         h3("Boxplot"),
                         withSpinner(plotOutput("boxplot_Alpha_Div", height = "600px")),
                         br(),
                         br(),
                         br(),
                         br(),
                         fluidRow(
                         column(3, numericInput("Boxplot_alpha_div_output_height", label = h5("Figure height (upto 49 inces)"), value = 8, width = "300px")),
                         column(3, numericInput("Boxplot_alpha_div_output_width", label = h5("Figure width (upto 49 inces)"), value = 8, width = "300px")),
                         column(3, numericInput("Boxplot_alpha_div_output_dpi", label = h5("Figure resolution (dpi:72 to 300)"), value = 300, width = "300px")),
                         ),
                         downloadButton(outputId = "download_Boxplot_Alpha_Div", label = "Download plot"),
                       ),
                       tabPanel(
                         "Summary Table",
                         h3("Result - alpha diversity estimates for each metagenome"),
                         hr(),
                         withSpinner(dataTableOutput("alpha_table")),
                         downloadButton(outputId = "download_result_alpha", label = "Download as csv"),
                       ),
                     ),
                   ),
                 ),
               ),
               
               tabPanel(
                 "Beta",
                 sidebarLayout(
                   sidebarPanel(
                     h3("Beta diversity"),
                     selectInput("input_RA_Beta", label = "Selected input", choices = "Example data selected"),
                     h4("PERMANOVA Options"),
                     selectInput("select_beta", label = "Select diversity methods", choices = c("bray"="bray", "manhattan"="manhattan", "jaccard"="jaccard", "euclidean"="euclidean", "canberra"="canberra", "clark"="clark", "kulczynski"="kulczynski", "gower"="gower", "altGower"="altGower", "morisita"="morisita", "horn"="horn", "mountford"="mountford", "raup"="raup", "binomial"="binomial", "chao"="chao", "cao"="cao", "mahalanobis"="mahalanobis", "chisq"="chisq", "chord"="chord", "hellinger"="hellinger", "aitchison"="aitchison", "robust.aitchison"="robust.aitchison"), selected = "bray"),
                     numericInput("adonis_permutations", "Number of permutations", value = 99, min = 1, step = 1),
                     selectInput("adonis_dissimilarities", label = "Square root of dissimilarities", choices = c("Yes" = "TRUE", "No" = "FALSE"), selected = "FALSE"),
                     radioButtons("select_beta_boxplot_pvalue", label = "Boxplot Wilcoxon test", choices = c("Yes (show's Pvalue)" = "Yes", "No" = "No", "Show *" = "star"), selected = "No"),
                     radioButtons("select_beta_pvalue", label = "PERMANOVA p-value", choices = c("Yes (show's Pvalue)" = "Yes", "No" = "No", "Show *" = "star"), selected = "No"),
                     selectInput("select_method", "Select ordination based method", choices = c("PCoA" = "PCoA", "NMDS" = "NMDS", "DCA" = "DCA", "RDA" = "RDA", "MDS" = "MDS"), selected = "PCoA"),
                     selectInput("select_beta_color_palette", label = "Colors", choices = list("RdYlBu"="RdYlBu", "RdYlGn"="RdYlGn", "BrBG"="BrBG", "PiYG"="PiYG", "PRGn"="PRGn", "PuOr"="PuOr", "RdBu"="RdBu", "RdGy"="RdGy", "Spectral"="Spectral", "Blues"="Blues", "Reds"="Reds", "Greens"="Greens", "Greys"="Greys", "Oranges"="Oranges", "Accent"="Accent", "Dark2"="Dark2","Paired"="Paired","Pastel1"="Pastel1","Pastel2"="Pastel2","Set1"="Set1","Set2"="Set2","Set3"="Set3"), selected = "RdYlBu"),
                     selectInput("select_image_type_beta", label = "Output image format", choices = list("JPG" = ".jpg", "TIFF" =".tiff", "PDF" = ".pdf",  "SVG" = ".svg", "BMP" = ".bmp", "EPS" = ".eps", "PS" = ".ps"), selected = ".jpg"),
                     actionButton("action_beta_diversity", "Submit"),
                   ),
                   mainPanel(
                     tabsetPanel(
                       type = "tabs",
                       tabPanel(
                         "Beta diversity Plot",
                         h3("Beta diversity plot"),
                         withSpinner(plotOutput("boxplot_Beta_Div", height = "600px")),
                         fluidRow(
                         column(3, numericInput("Boxplot_beta_div_output_height", label = h5("Figure height (upto 49 inces)"), value = 8, width = "300px")),
                         column(3, numericInput("Boxplot_beta_div_output_width", label = h5("Figure width (upto 49 inces)"), value = 8, width = "300px")),
                         column(3, numericInput("Boxplot_beta_div_output_dpi", label = h5("Figure resolution (dpi:72 to 300)"), value = 300, width = "300px")),
                         ),
                         #numericInput("Boxplot_beta_div_output_dpi", label = h5("Figure resolution (dpi:72 to 300)"), value = 200, width = "300px"),
                         downloadButton(outputId = "download_Boxplot_beta_Div", label = "Download plot"),
                       ),
                       tabPanel(
                         "Summary Table",
                         h3("Result - distance between all the samples"),
                         hr(),
                         withSpinner(dataTableOutput("beta_table")),
                         downloadButton(outputId = "download_result_beta", label = "Download as csv"),
                       ),
                       tabPanel(
                         "Adonis Table",
                         h3("Result - Permutation test for adonis under reduced model"),
                         hr(),
                         withSpinner(dataTableOutput("beta_table2")),
                         downloadButton(outputId = "download_result_beta2", label = "Download as csv"),
                       ),
                   ),
                 ),
               ),


),
),
navbarMenu("Dimension reduction",
           tabPanel(
             "PCA-2D",
             sidebarLayout(
               sidebarPanel(
                 h3("PCA-2D"),
                 selectInput("input_RA_pca", label = "Selected input", choices = "Example data selected"),
                 selectInput("select_pca_label", "Label", choices = c("TRUE"="TRUE", "FALSE"="FALSE"), selected = "FALSE"),
                 numericInput("select_pca_label_size", label = "Label size", value = 3),
                 selectInput("select_pca_frame", "Frame", choices = c("TRUE"="TRUE", "FALSE"="FALSE"), selected = "FALSE"),
                 selectInput("select_pca_color_palette", label = "Colors", choices = list("RdYlBu"="RdYlBu", "RdYlGn"="RdYlGn", "BrBG"="BrBG", "PiYG"="PiYG", "PRGn"="PRGn", "PuOr"="PuOr", "RdBu"="RdBu", "RdGy"="RdGy", "Spectral"="Spectral", "Blues"="Blues", "Reds"="Reds", "Greens"="Greens", "Greys"="Greys", "Oranges"="Oranges", "Accent"="Accent", "Dark2"="Dark2","Paired"="Paired","Pastel1"="Pastel1","Pastel2"="Pastel2","Set1"="Set1","Set2"="Set2","Set3"="Set3"), selected = "RdYlBu"),
                 selectInput("select_image_type_pca", label = "Output image format", choices = list("JPG" = ".jpg", "TIFF" =".tiff", "PDF" = ".pdf",  "SVG" = ".svg", "BMP" = ".bmp", "EPS" = ".eps", "PS" = ".ps"), selected = ".jpg"),
                 actionButton("action_pca", "Submit"),
               ),
               mainPanel(
                 tabsetPanel(
                   type = "tabs",
                   tabPanel(
                     "PCA 2D Plot",
                     h3("Principal Component Analysis (PCA)"),
                     withSpinner(plotOutput("plot_pca", width = "70%", height = "500px")),
                     fluidRow(
                       column(3, numericInput("pca_plot_output_height", label = h5("Figure height (upto 49 inces)"), value = 8, width = "300px")),
                       column(3, numericInput("pca_plot_output_width", label = h5("Figure width (upto 49 inces)"), value = 8, width = "300px")),
                       column(3, numericInput("pca_plot_output_dpi", label = h5("Figure resolution (dpi:72 to 300)"), value = 300, width = "300px")),
                     ),
                     downloadButton(outputId = "download_plot_pca", label = "Download plot"),
                   ),
                   tabPanel(
                     "Summary Table",
                     fluidRow(
                       column(
                         withSpinner(dataTableOutput("pca_table")),
                     downloadButton(outputId = "download_result_pca", label = "Download as csv"), width = 6),
                     ),
                   ),
                 ),
               ),
             ),
           ),
           tabPanel(
             "PCA-3D",
             sidebarLayout(
               sidebarPanel(
                 h3("PCA-3D"),
                 selectInput("input_RA_pca3d", label = "Selected input", choices = "Example data selected"),
                 selectInput("select_pca3d_color_palette", label = "Colors", choices = list("RdYlBu"="RdYlBu", "RdYlGn"="RdYlGn", "BrBG"="BrBG", "PiYG"="PiYG", "PRGn"="PRGn", "PuOr"="PuOr", "RdBu"="RdBu", "RdGy"="RdGy", "Spectral"="Spectral", "Blues"="Blues", "Reds"="Reds", "Greens"="Greens", "Greys"="Greys", "Oranges"="Oranges", "Accent"="Accent", "Dark2"="Dark2","Paired"="Paired","Pastel1"="Pastel1","Pastel2"="Pastel2","Set1"="Set1","Set2"="Set2","Set3"="Set3"), selected = "RdYlBu"),
                 #selectInput("select_image_type_pca3d", label = "Output image format", choices = list("JPG" = ".jpg", "TIFF" =".tiff", "PDF" = ".pdf",  "SVG" = ".svg", "BMP" = ".bmp", "EPS" = ".eps", "PS" = ".ps"), selected = ".jpg"),
                 actionButton("action_pca3d", "Submit"),
               ),
               mainPanel(
                 tabsetPanel(
                   type = "tabs",
                   tabPanel(
                     "PCA 3D Plot",
                     h3("Principal Component Analysis (PCA)"),
                     withSpinner(plotlyOutput("plot_pca3d", height = "700px", width= "800px")),
                  
                   ),
                   tabPanel(
                     "Summary Table",
                     fluidRow(
                       column(
                         withSpinner(dataTableOutput("pca3d_table")),
                    downloadButton(outputId = "download_result_pca3d", label = "Download as csv"), width = 6),
                     ),
                   ),
                 ),
               ),
             ),
           ),
           tabPanel(
             "t-SNE",
             sidebarLayout(
               sidebarPanel(
                 h3("t-SNE"),
                 selectInput("input_tsne", label = "Selected input", choices = "Example data selected"),
                 selectInput("select_tsne_method", "Select method", choices = c("counts"="counts", "rclr"="rclr", "hellinger"="hellinger", "pa"="pa", "rank"="rank", "relabundance"="relabundance"), selected = "counts"),
                 selectInput("select_tsne_dimension", "Select dimension to display", choices = c("2" = 2, "3" = 3), selected = 2),
                 selectInput("select_tsne_color_palette", label = "Colors", choices = list("RdYlBu"="RdYlBu", "RdYlGn"="RdYlGn", "BrBG"="BrBG", "PiYG"="PiYG", "PRGn"="PRGn", "PuOr"="PuOr", "RdBu"="RdBu", "RdGy"="RdGy", "Spectral"="Spectral", "Blues"="Blues", "Reds"="Reds", "Greens"="Greens", "Greys"="Greys", "Oranges"="Oranges", "Accent"="Accent", "Dark2"="Dark2","Paired"="Paired","Pastel1"="Pastel1","Pastel2"="Pastel2","Set1"="Set1","Set2"="Set2","Set3"="Set3"), selected = "RdYlBu"),
                 selectInput("select_image_type_tsne", label = "Output image format", choices = list("JPG" = ".jpg", "TIFF" =".tiff", "PDF" = ".pdf",  "SVG" = ".svg", "BMP" = ".bmp", "EPS" = ".eps", "PS" = ".ps"), selected = ".jpg"),
                 actionButton("action_tsne", "Submit"),
               ),
               mainPanel(
                 tabsetPanel(
                   type = "tabs",
                   tabPanel(
                     "t-SNE Plot",
                     h3("t-distributed Stochastic Neighbor Embedding (t-SNE)"),
                     withSpinner(plotOutput("plot_tsne", width = "70%", height = "500px")),
                     fluidRow(
                       column(3, numericInput("tsne_plot_output_height", label = h5("Figure height (upto 49 inces)"), value = 8, width = "300px")),
                       column(3, numericInput("tsne_plot_output_width", label = h5("Figure width (upto 49 inces)"), value = 8, width = "300px")),
                       column(3, numericInput("tsne_plot_output_dpi", label = h5("Figure resolution (dpi:72 to 300)"), value = 300, width = "300px")),
                     ),
                     downloadButton(outputId = "download_plot_tsne", label = "Download plot"),
                   ),
                   tabPanel(
                     "Summary Table",
                     fluidRow(
                       column(
                         withSpinner(dataTableOutput("tsne_table")),
                     downloadButton(outputId = "download_result_tsne", label = "Download as csv"), width = 6),
                 ),
               ),
                 ),
               ),
             ),
           ),
           tabPanel(
             "UMAP",
             sidebarLayout(
               sidebarPanel(
                 h3("UMAP"),
                 selectInput("input_umap", label = "Selected input", choices = "Example data selected"),
                 selectInput("select_umap_method", "Select method", choices = c("counts"="counts", "rclr"="rclr", "hellinger"="hellinger", "pa"="pa", "rank"="rank", "relabundance"="relabundance"), selected = "counts"),
                 selectInput("select_umap_kvalue", "Select k value (for graph construction)", choices = c("2" = 2, "3" = 3, "4" = 4, "5" = 5, "6" = 6, "7" = 7, "8" = 8, "9" = 9, "10" = 10, "11" = 11, "12" = 12, "13" = 13, "14" = 14, "15" = 15), selected = 2),
                 selectInput("select_umap_color_palette", label = "Colors", choices = list("RdYlBu"="RdYlBu", "RdYlGn"="RdYlGn", "BrBG"="BrBG", "PiYG"="PiYG", "PRGn"="PRGn", "PuOr"="PuOr", "RdBu"="RdBu", "RdGy"="RdGy", "Spectral"="Spectral", "Blues"="Blues", "Reds"="Reds", "Greens"="Greens", "Greys"="Greys", "Oranges"="Oranges", "Accent"="Accent", "Dark2"="Dark2","Paired"="Paired","Pastel1"="Pastel1","Pastel2"="Pastel2","Set1"="Set1","Set2"="Set2","Set3"="Set3"), selected = "RdYlBu"),
                 selectInput("select_image_type_umap", label = "Output image format", choices = list("JPG" = ".jpg", "TIFF" =".tiff", "PDF" = ".pdf",  "SVG" = ".svg", "BMP" = ".bmp", "EPS" = ".eps", "PS" = ".ps"), selected = ".jpg"),
                 actionButton("action_umap", "Submit"),
               ),
               mainPanel(
                 tabsetPanel(
                   type = "tabs",
                   tabPanel(
                     "UMAP Plot",
                     h3("Uniform Manifold Approximation and Projection for Dimension Reduction (UMAP)"),
                     withSpinner(plotOutput("plot_umap", width = "100%", height = "500px")),
                     fluidRow(
                       column(3, numericInput("umap_plot_output_height", label = h5("Figure height (upto 49 inces)"), value = 8, width = "300px")),
                       column(3, numericInput("umap_plot_output_width", label = h5("Figure width (upto 49 inces)"), value = 8, width = "300px")),
                       column(3, numericInput("umap_plot_output_dpi", label = h5("Figure resolution (dpi:72 to 300)"), value = 300, width = "300px")),
                     ),
                     downloadButton(outputId = "download_plot_umap", label = "Download plot"),
                   ),
                   tabPanel(
                     "Summary Table based on condition",
                     fluidRow(
                       column(
                         withSpinner(dataTableOutput("umap_table")),
                     downloadButton(outputId = "download_result_umap", label = "Download as csv"), width = 6),
                 ),
               ),
                   tabPanel(
                     "Summary Table based on cluster",
                     fluidRow(
                       column(
                         withSpinner(dataTableOutput("umap_table1")),
                     downloadButton(outputId = "download_result_umap1", label = "Download as csv"), width = 6),
             ),
           ),
                 ),
               ),
             ),
           ),
),
#correlation
navbarMenu(
  "Correlation",
  tabPanel(
    "Taxa-based",
    sidebarLayout(
      sidebarPanel(
        h3("Compute correlation between taxa for selected condition(s)"), 
        selectInput("input_taxa_condition_based_correlation", label = "Selected input", choices = "Example data selected"),
        selectInput("input_taxa_condition_based_correlation2", label = "Select condition(s)", choices = "Please upload metadata in upload page", multiple = TRUE),
        radioButtons("select_taxa_condition_based_correlation_method", "Correlation methods", choices = c("pearson" = "pearson","kendall" = "kendall", "spearman" = "spearman"), selected = "pearson"),
        numericInput("select_taxa_condition_based_correlation_label_size", label = "Label size", value = 3),
        selectInput("select_taxa_condition_geom_shape", label = "Geom shapes", choices = list("circle"="circle", "tile"="tile", "text"="text"), selected = "circle"),
        selectInput("select_image_taxa_condition_based_correlation", label = "Output image format", choices = list("JPG" = ".jpg", "TIFF" =".tiff", "PDF" = ".pdf",  "SVG" = ".svg", "BMP" = ".bmp", "EPS" = ".eps", "PS" = ".ps"), selected = ".jpg"),
        actionButton("action_taxa_condition_based_correlation", "Submit"),
      ),
      mainPanel(
        tabsetPanel(
          type = "tabs",
          tabPanel(
            "Correlation plot",
            uiOutput("taxa_condition_based_correlation_plot_ui"),
            h4("Taxa based correlation plot."),
            fluidRow(
              column(3, numericInput("taxa_condition_based_correlation_output_height", label = h5("Figure height (upto 49 inces)"), value = 8, width = "300px")),
              column(3, numericInput("taxa_condition_based_correlation_output_width", label = h5("Figure width (upto 49 inces)"), value = 8, width = "300px")),
              column(3, numericInput("taxa_condition_based_correlation_output_dpi", label = h5("Figure resolution (dpi:72 to 300)"), value = 300, width = "300px")),
            ),
            downloadButton(outputId = "download_plot_taxa_condition_based_correlation", label = "Download plot"),
          ),
          tabPanel(
            "Summary Table",
            fluidRow(
              column(
                withSpinner(dataTableOutput("taxa_condition_based_correlation_table")),
                downloadButton(outputId = "download_result_taxa_condition_based_correlation", label = "Download as csv"), width = 8),
            ),
          ),
        ),
      ),
    ),
  ),
  tabPanel(
    "Sample-based",
    sidebarLayout(
      sidebarPanel(
        h3("Compute correlation between samples"), 
        selectInput("input_samples_based_correlation", label = "Selected input", choices = "Example data selected"),
        selectInput("input_samples_based_correlation2", label = "Select condition(s)", choices = "Please upload metadata in upload page", multiple = TRUE),
        radioButtons("select_samples_based_correlation_method", "Correlation methods", choices = c("pearson" = "pearson","kendall" = "kendall", "spearman" = "spearman"), selected = "pearson"),
        numericInput("select_samples_based_correlation_label_size", label = "Label size", value = 3),
        selectInput("select_sample_geom_shape", label = "Geom shapes", choices = list("circle"="circle", "tile"="tile", "text"="text"), selected = "circle"),
        selectInput("select_image_type_samples_based_correlation", label = "Output image format", choices = list("JPG" = ".jpg", "TIFF" =".tiff", "PDF" = ".pdf",  "SVG" = ".svg", "BMP" = ".bmp", "EPS" = ".eps", "PS" = ".ps"), selected = ".jpg"),
        actionButton("action_samples_based_correlation", "Submit"),
      ),
      mainPanel(
        tabsetPanel(
          type = "tabs",
          tabPanel(
            "Correlation plot",
            withSpinner(plotOutput("plot_samples_based_correlation", width = "100%", height = "1000px")),
            h4("Taxa Samples based correlation plot for selected condition(s)."),
            fluidRow(
              column(3, numericInput("samples_based_correlation_output_height", label = h5("Figure height (upto 49 inces)"), value = 8, width = "300px")),
              column(3, numericInput("samples_based_correlation_output_width", label = h5("Figure width (upto 49 inces)"), value = 8, width = "300px")),
              column(3, numericInput("samples_based_correlation_output_dpi", label = h5("Figure resolution (dpi:72 to 300)"), value = 300, width = "300px")),
            ),
            downloadButton(outputId = "download_plot_samples_based_correlation", label = "Download plot"),
          ),
          tabPanel(
            "Summary Table",
            fluidRow(
              column(
                withSpinner(dataTableOutput("samples_based_correlation_table")),
            downloadButton(outputId = "download_result_samples_based_correlation", label = "Download as csv"), width = 8),
        ),
      ),
        ),
      ),
    ),
  ),
  ),
#Heatmap
tabPanel(
  "Heatmap",
  sidebarLayout(
    sidebarPanel(
      h3("Heatmap - relative abundance"),
      selectInput("input_heatmap", label = "Selected input", choices = "Example data selected"),
      selectInput("heatmap_clustering_method_rows", label = "Clustering method rows", choices = list("complete" = "complete", "single"= "single", "average (UPGMA)" = "average", "mcquitty (WPGMA)" = "mcquitty", "median (WPGMC)" = "median", "centroid (UPGMC)" = "centroid"), selected = "complete"),
      selectInput("heatmap_clustering_method_columns", label = "Clustering method columns", choices = list("complete" = "complete", "single"= "single", "average (UPGMA)" = "average", "mcquitty (WPGMA)" = "mcquitty", "median (WPGMC)" = "median", "centroid (UPGMC)" = "centroid"), selected = "complete"),
      selectInput("heatmap_normalization", label = "Normalization method", choices = list("scale"="scale", "minmax"="minmax", "log"="log", "Row normalization"="row", "Column normalization"="column", "None"="none"), selected = "scale"),
      selectInput("heatmap_color_palette", label = "Colors", choices = list("RdYlBu"="RdYlBu", "RdYlGn"="RdYlGn", "BrBG"="BrBG", "PiYG"="PiYG", "PRGn"="PRGn", "PuOr"="PuOr", "RdBu"="RdBu", "RdGy"="RdGy", "Spectral"="Spectral", "Blues"="Blues", "Reds"="Reds", "Greens"="Greens", "Greys"="Greys", "Oranges"="Oranges", "Accent"="Accent", "Dark2"="Dark2","Paired"="Paired","Pastel1"="Pastel1","Pastel2"="Pastel2","Set1"="Set1","Set2"="Set2","Set3"="Set3"), selected = "RdYlBu"),
      selectInput("heatmap_row_names", "Show row names", choices = c("TRUE"="TRUE", "FALSE"="FALSE"), selected = TRUE),
      numericInput("heatmap_row_names_size", label = "Row name size", value = 7),
      selectInput("heatmap_column_names", "Show column names", choices = c("TRUE"="TRUE", "FALSE"="FALSE"), selected = TRUE),
      numericInput("heatmap_column_names_size", label = "Column name size", value = 7),
      selectInput("heatmap_row_dend", "Show row cladogram", choices = c("TRUE"="TRUE", "FALSE"="FALSE"), selected = TRUE),
      selectInput("heatmap_column_dend", "Show column cladogram", choices = c("TRUE"="TRUE", "FALSE"="FALSE"), selected = TRUE),
      
      selectInput("select_image_type_heatmap", label = "Output image format", choices = list("JPG" = ".jpg", "TIFF" =".tiff", "PDF" = ".pdf",  "SVG" = ".svg", "BMP" = ".bmp", "EPS" = ".eps", "PS" = ".ps"), selected = ".jpg"),
      actionButton("action_heatmap", "Submit"),
    ),
    mainPanel(
      h4("Heatmap using relative abundance"),
      withSpinner(plotOutput("plot_heatmap", width = "100%", height = "1000px")),
      fluidRow(
        column(3, numericInput("heatmap_output_height", label = h5("Figure height (upto 49 inces)"), value = 8, width = "300px")),
        column(3, numericInput("heatmap_output_width", label = h5("Figure width (upto 49 inces)"), value = 8, width = "300px")),
        column(3, numericInput("heatmap_output_dpi", label = h5("Figure resolution (dpi:72 to 300)"), value = 300, width = "300px")),
      ),
      downloadButton(outputId = "download_heatmap", label = "Download Heatmap"),
    ),
  ),
),

navbarMenu("Differential abundance",
           "Two groups",
           tabPanel(
             "Wilcoxon Rank Sum test",
             sidebarLayout(
               sidebarPanel(
                 h3("Wilcoxon Rank Sum test"),
                 selectInput("input_wilcoxtest", label = "Selected input", choices = "Example data selected"),
                 selectInput("group1_wilcoxtest", label = "Select condition1", choices = "Please upload metadata in upload page"),
                 selectInput("group2_wilcoxtest", label = "Select condition2", choices = "Please upload metadata in upload page"),
                 selectInput("select_wilcoxtest_pvalue", label = "Test correction", choices = list("Benjamini-Hochberg FDR" = "padj", "P-value" = "pvalue"), selected = "padj"),
                 numericInput("wilcoxtest_pvalue","FDR or Pvalue", value = "0.05"),
                 selectInput("wilcox_color_palette", label = "Colors", choices = list("RdYlBu"="RdYlBu", "RdYlGn"="RdYlGn", "BrBG"="BrBG", "PiYG"="PiYG", "PRGn"="PRGn", "PuOr"="PuOr", "RdBu"="RdBu", "RdGy"="RdGy", "Spectral"="Spectral", "Blues"="Blues", "Reds"="Reds", "Greens"="Greens", "Greys"="Greys", "Oranges"="Oranges", "Accent"="Accent", "Dark2"="Dark2","Paired"="Paired","Pastel1"="Pastel1","Pastel2"="Pastel2","Set1"="Set1","Set2"="Set2","Set3"="Set3"), selected = "RdYlBu"),
                 radioButtons("select_wilcoxtest_plot", "Types of plot", choices = c("Grouped box plot" = 1, "Individual box plot" = 2, "Volcano plot" = 3, "Heatmap" = 4), selected = 1),
                 radioButtons("wilcoxtest_plot_taxa_mode", "Significant taxa in plot", choices = c("All significant" = "all", "Selected number" = "selected"), selected = "all"),
                 conditionalPanel(
                   condition = "input.wilcoxtest_plot_taxa_mode == 'selected'",
                   numericInput("wilcoxtest_plot_top_n", "Number of significant taxa in plot", value = 25, min = 2, max = 100, step = 1)
                 ),
                 checkboxInput("show_wilcoxtest_plot_labels", "Show volcano/heatmap labels", value = TRUE),
                 selectInput("select_image_type_wilcoxtest", label = "Output image format", choices = list("JPG" = ".jpg", "TIFF" =".tiff", "PDF" = ".pdf",  "SVG" = ".svg", "BMP" = ".bmp", "EPS" = ".eps", "PS" = ".ps"), selected = ".jpg"),
                 actionButton("action_wilcoxtest", "Submit"),
               ),
               mainPanel(
                 tabsetPanel(
                   type = "tabs",
                   tabPanel(
                     "Summary Table",
                     h3("Result - OTUs that were significantly different between two groups"),
                     withSpinner(verbatimTextOutput("text_wilcoxtest_level")),
                     fluidRow(
                       column(
                         withSpinner(dataTableOutput("wilcoxtest_table")), width = 12),
                 ),
                     hr(),
                     fluidRow(
                     column(3, h4("Download significant"),
                     downloadButton(outputId = "download_result_wilcoxtest_1", label = "Download as csv")),
                     column(3, h4("Download all"),
                     downloadButton(outputId = "download_result_wilcoxtest_2", label = "Download as csv")),
                     column(3,h4("Download relative frequency"),
                     downloadButton(outputId = "download_result_wilcoxtest_3", label = "Download as csv")),
                     column(3,h4("Total counts in each samples"),
                     downloadButton(outputId = "download_result_wilcoxtest_4", label = "Download as csv")),
                     ),
                   ),
                   tabPanel(
                     "Plot",
                     h3("Plot"),
                     withSpinner(plotOutput("boxplot_wilcoxtest", width = "100%", height = "800px")),
                     uiOutput("wilcoxtest_individual_plot_page_ui"),
                     fluidRow(
                       column(3, numericInput("Boxplot_wilcoxtest_output_height", label = h5("Figure height (upto 49 inces)"), value = 8, width = "300px")),
                       column(3, numericInput("Boxplot_wilcoxtest_output_width", label = h5("Figure width (upto 49 inces)"), value = 8, width = "300px")),
                       column(3, numericInput("Boxplot_wilcoxtest_output_dpi", label = h5("Figure resolution (dpi:72 to 300)"), value = 300, width = "300px")),
                     ),
                     downloadButton(outputId = "download_Boxplot_wilcoxtest", label = "Download plot"),
                   ),
                 ),
               ),
             ),
           ),
           tabPanel(
             "t-test",
             sidebarLayout(
               sidebarPanel(
                 h3("t-test: Two sample t-test"),
                 selectInput("input_ttest", label = "Selected input", choices = "Example data selected"),
                 selectInput("group1_ttest", label = "Select condition1", choices = "Please upload metadata in upload page"),
                 selectInput("group2_ttest", label = "Select condition2", choices = "Please upload metadata in upload page"),
                 selectInput("select_ttest_pvalue", label = "Test correction", choices = list("Benjamini-Hochberg FDR" = "padj", "P-value" = "pvalue"), selected = "padj"),
                 numericInput("ttest_pvalue","FDR or Pvalue", value = "0.05"),
                 selectInput("ttest_color_palette", label = "Colors", choices = list("RdYlBu"="RdYlBu", "RdYlGn"="RdYlGn", "BrBG"="BrBG", "PiYG"="PiYG", "PRGn"="PRGn", "PuOr"="PuOr", "RdBu"="RdBu", "RdGy"="RdGy", "Spectral"="Spectral", "Blues"="Blues", "Reds"="Reds", "Greens"="Greens", "Greys"="Greys", "Oranges"="Oranges", "Accent"="Accent", "Dark2"="Dark2","Paired"="Paired","Pastel1"="Pastel1","Pastel2"="Pastel2","Set1"="Set1","Set2"="Set2","Set3"="Set3"), selected = "RdYlBu"),
                 radioButtons("select_ttest_plot", "Types of plot", choices = c("Grouped box plot" = 1, "Individual box plot" = 2, "Volcano plot" = 3, "Heatmap" = 4), selected = 1),
                 radioButtons("ttest_plot_taxa_mode", "Significant taxa in plot", choices = c("All significant" = "all", "Selected number" = "selected"), selected = "all"),
                 conditionalPanel(
                   condition = "input.ttest_plot_taxa_mode == 'selected'",
                   numericInput("ttest_plot_top_n", "Number of significant taxa in plot", value = 25, min = 2, max = 100, step = 1)
                 ),
                 checkboxInput("show_ttest_plot_labels", "Show volcano/heatmap labels", value = TRUE),
                 selectInput("select_image_type_ttest", label = "Output image format", choices = list("JPG" = ".jpg", "TIFF" =".tiff", "PDF" = ".pdf",  "SVG" = ".svg", "BMP" = ".bmp", "EPS" = ".eps", "PS" = ".ps"), selected = ".jpg"),
                 actionButton("action_ttest", "Submit"),
               ),
               mainPanel(
                 tabsetPanel(
                   type = "tabs",
                    tabPanel(
                     "Summary Table",
                     h3("Result - OTUs that were significantly different between two groups"),
                     withSpinner(verbatimTextOutput("text_ttest_level")),
                      fluidRow(
                       column(
                         withSpinner(dataTableOutput("ttest_table")), width = 12),
                     ),
                     hr(),
                     fluidRow(
                     column(3, h4("Download significant"),
                     downloadButton(outputId = "download_result_ttest_1", label = "Download as csv")),
                     column(3, h4("Download all"),
                     downloadButton(outputId = "download_result_ttest_2", label = "Download as csv")),
                     column(3, h4("Download relative frequency"),
                     downloadButton(outputId = "download_result_ttest_3", label = "Download as csv")),
                     column(3, h4("Total counts in each samples"),
                     downloadButton(outputId = "download_result_ttest_4", label = "Download as csv")),
                     ),
                   ),
                   tabPanel(
                     "Plot",
                     h3("Plot"),
                     withSpinner(plotOutput("boxplot_ttest", width = "100%", height = "800px")),
                     uiOutput("ttest_individual_plot_page_ui"),
                     fluidRow(
                       column(3, numericInput("Boxplot_ttest_output_height", label = h5("Figure height (upto 49 inces)"), value = 8, width = "300px")),
                       column(3, numericInput("Boxplot_ttest_output_width", label = h5("Figure width (upto 49 inces)"), value = 8, width = "300px")),
                       column(3, numericInput("Boxplot_ttest_output_dpi", label = h5("Figure resolution (dpi:72 to 300)"), value = 300, width = "300px")),
                     ),
                     downloadButton(outputId = "download_Boxplot_ttest", label = "Download plot"),
                   ),
                 ),
               ),
             ),
           ),
           tabPanel(
             "metagenomeSeq",
             sidebarLayout(
               sidebarPanel(
                 h3("metagenomeSeq"),
                 selectInput("input_RA_metagenomeseq", label = "Selected input", choices = "Example data selected"),
                 selectInput("group1_metagenomeseq", label = "Select condition1", choices = "Please upload metadata in upload page"),
                 selectInput("group2_metagenomeseq", label = "Select condition2", choices = "Please upload metadata in upload page"),
                 selectInput("select_metagenomeseq_pvalue", label = "Test correction", choices = list("Benjamini-Hochberg FDR" = "padj", "P-value" = "pvalue"), selected = "padj"),
                 numericInput("metagenomeseq_pvalue","FDR or Pvalue", value = "0.05"),
                 selectInput("metagenomeseq_color_palette", label = "Colors", choices = list("RdYlBu"="RdYlBu", "RdYlGn"="RdYlGn", "BrBG"="BrBG", "PiYG"="PiYG", "PRGn"="PRGn", "PuOr"="PuOr", "RdBu"="RdBu", "RdGy"="RdGy", "Spectral"="Spectral", "Blues"="Blues", "Reds"="Reds", "Greens"="Greens", "Greys"="Greys", "Oranges"="Oranges", "Accent"="Accent", "Dark2"="Dark2","Paired"="Paired","Pastel1"="Pastel1","Pastel2"="Pastel2","Set1"="Set1","Set2"="Set2","Set3"="Set3"), selected = "RdYlBu"),
                 radioButtons("select_metagenomeseq_plot", "Types of plot", choices = c("Grouped box plot" = 1, "Individual box plot" = 2, "Volcano plot" = 3, "Heatmap" = 4), selected = 1),
                 radioButtons("metagenomeseq_plot_taxa_mode", "Significant taxa in plot", choices = c("All significant" = "all", "Selected number" = "selected"), selected = "all"),
                 conditionalPanel(
                   condition = "input.metagenomeseq_plot_taxa_mode == 'selected'",
                   numericInput("metagenomeseq_plot_top_n", "Number of significant taxa in plot", value = 25, min = 2, max = 100, step = 1)
                 ),
                 checkboxInput("show_metagenomeseq_plot_labels", "Show volcano/heatmap labels", value = TRUE),
                 selectInput("select_image_type_metagenomeseq", label = "Output image format", choices = list("JPG" = ".jpg", "TIFF" =".tiff", "PDF" = ".pdf",  "SVG" = ".svg", "BMP" = ".bmp", "EPS" = ".eps", "PS" = ".ps"), selected = ".jpg"),
                 actionButton("action_metagenomeseq", "Submit"),
               ),
               mainPanel(
                 tabsetPanel(
                   type = "tabs",
                   tabPanel(
                     "Summary Table",
                     h3("Result - OTUs that were significantly different between two groups"),
                     withSpinner(verbatimTextOutput("text_metagenomeseq_level")),
                     fluidRow(
                       column(
                         withSpinner(dataTableOutput("metagenomeseq_table")), width = 12),
                     ),
                     hr(),
                     fluidRow(
                     column(3, h4("Download significant"),
                     downloadButton(outputId = "download_result_metagenomeseq_1", label = "Download as csv")),
                     column(3, h4("Download all"),
                     downloadButton(outputId = "download_result_metagenomeseq_2", label = "Download as csv")),
                     column(3, h4("Total counts in each samples"),
                     downloadButton(outputId = "download_result_metagenomeseq_3", label = "Download as csv")),
                     ),
                   ),
                   tabPanel(
                     "Plot",
                     h3("Plot"),
                     withSpinner(plotOutput("boxplot_metagenomeseq", width = "100%", height = "800px")),
                     uiOutput("metagenomeseq_individual_plot_page_ui"),
                     fluidRow(
                       column(3, numericInput("Boxplot_metagenomeseq_output_height", label = h5("Figure height (upto 49 inces)"), value = 8, width = "300px")),
                       column(3, numericInput("Boxplot_metagenomeseq_output_width", label = h5("Figure width (upto 49 inces)"), value = 8, width = "300px")),
                       column(3, numericInput("Boxplot_metagenomeseq_output_dpi", label = h5("Figure resolution (dpi:72 to 300)"), value = 300, width = "300px")),
                     ),
                     downloadButton(outputId = "download_Boxplot_metagenomeseq", label = "Download plot"),
                   ),
                   
                 ),
               ),
             ),
           ),
          
           tabPanel(
             "DESeq2",
             sidebarLayout(
               sidebarPanel(
                 h3("DESeq2"),
                 selectInput("input_RA_deseq2", label = "Selected input", choices = "Example data selected"),
                 selectInput("group1_deseq2", label = "Select condition1", choices = "Please upload metadata in upload page"),
                 selectInput("group2_deseq2", label = "Select condition2", choices = "Please upload metadata in upload page"),
                 selectInput("select_deseq2_pvalue", label = "Test correction", choices = list("Benjamini-Hochberg FDR" = "padj", "P-value" = "pvalue"), selected = "padj"),
                 numericInput("deseq2_pvalue","FDR or Pvalue", value = "0.05"),
                 selectInput("deseq2_color_palette", label = "Colors", choices = list("RdYlBu"="RdYlBu", "RdYlGn"="RdYlGn", "BrBG"="BrBG", "PiYG"="PiYG", "PRGn"="PRGn", "PuOr"="PuOr", "RdBu"="RdBu", "RdGy"="RdGy", "Spectral"="Spectral", "Blues"="Blues", "Reds"="Reds", "Greens"="Greens", "Greys"="Greys", "Oranges"="Oranges", "Accent"="Accent", "Dark2"="Dark2","Paired"="Paired","Pastel1"="Pastel1","Pastel2"="Pastel2","Set1"="Set1","Set2"="Set2","Set3"="Set3"), selected = "RdYlBu"),
                 radioButtons("select_deseq2_plot", "Types of plot", choices = c("Grouped box plot" = 1, "Individual box plot" = 2, "Volcano plot" = 3, "Heatmap" = 4), selected = 1),
                 radioButtons("deseq2_plot_taxa_mode", "Significant taxa in plot", choices = c("All significant" = "all", "Selected number" = "selected"), selected = "all"),
                 conditionalPanel(
                   condition = "input.deseq2_plot_taxa_mode == 'selected'",
                   numericInput("deseq2_plot_top_n", "Number of significant taxa in plot", value = 25, min = 2, max = 100, step = 1)
                 ),
                 checkboxInput("show_deseq2_plot_labels", "Show volcano/heatmap labels", value = TRUE),
                 selectInput("select_image_type_deseq2", label = "Output image format", choices = list("JPG" = ".jpg", "TIFF" =".tiff", "PDF" = ".pdf",  "SVG" = ".svg", "BMP" = ".bmp", "EPS" = ".eps", "PS" = ".ps"), selected = ".jpg"),
                 actionButton("action_deseq2", "Submit"),
               ),
               mainPanel(
                 tabsetPanel(
                   type = "tabs",
                   tabPanel(
                     "Summary Table",
                     h3("Result - OTUs that were significantly different between two groups"),
                     withSpinner(verbatimTextOutput("text_deseq2_level")),
                     fluidRow(
                       column(
                         withSpinner(dataTableOutput("deseq2_table")), width = 12),
                     ),
                     hr(),
                     fluidRow(
                     column(3, h4("Download significant"),
                     downloadButton(outputId = "download_result_deseq2_1", label = "Download as csv")),
                     column(3, h4("Download all"),
                     downloadButton(outputId = "download_result_deseq2_2", label = "Download as csv")),
                     column(3, h4("Download normalized count"),
                     downloadButton(outputId = "download_result_deseq2_3", label = "Download as csv")),
                     column(3, h4("Total counts in each samples"),
                     downloadButton(outputId = "download_result_deseq2_4", label = "Download as csv")),
                     ),
                   ),
                   tabPanel(
                     "Plot",
                     h3("Plot"),
                     withSpinner(plotOutput("boxplot_deseq2", width = "100%", height = "800px")),
                     uiOutput("deseq2_individual_plot_page_ui"),
                     fluidRow(
                       column(3, numericInput("Boxplot_deseq2_output_height", label = h5("Figure height (upto 49 inces)"), value = 8, width = "300px")),
                       column(3, numericInput("Boxplot_deseq2_output_width", label = h5("Figure width (upto 49 inces)"), value = 8, width = "300px")),
                       column(3, numericInput("Boxplot_deseq2_output_dpi", label = h5("Figure resolution (dpi:72 to 300)"), value = 300, width = "300px")),
                     ),
                     downloadButton(outputId = "download_Boxplot_deseq2", label = "Download plot"),
                   ),
                   
                 ),
               ),
             ),
           ),
           tabPanel(
             "LEfSe",
             sidebarLayout(
               sidebarPanel(
                 h3("Linear discriminant analysis (LDA) Effect Size (LEfSe)"),
                 selectInput("input_RA_LEfSe", label = "Selected input", choices = "Example data selected"),
                 selectInput("group1_LEfSe", label = "Select condition1", choices = "Please upload metadata in upload page"),
                 selectInput("group2_LEfSe", label = "Select condition2", choices = "Please upload metadata in upload page"),
                 selectInput("select_LEfSe_method", label = "Method", choices = list("BH"="BH", "holm"="holm", "hochberg"="hochberg", "hommel"="hommel", "bonferroni"="bonferroni", "BY"="BY", "none"="none"), selected = "BH"),
                 numericInput("select_LEfSe_pvalue","kruskal.threshold", value = "0.05"),
                 numericInput("select_LEfSe_threshold","lda.threshold", value = "2"),
                 selectInput("select_LEfSe_color_palette", label = "Colors", choices = list("RdYlBu"="RdYlBu", "RdYlGn"="RdYlGn", "BrBG"="BrBG", "PiYG"="PiYG", "PRGn"="PRGn", "PuOr"="PuOr", "RdBu"="RdBu", "RdGy"="RdGy", "Spectral"="Spectral", "Blues"="Blues", "Reds"="Reds", "Greens"="Greens", "Greys"="Greys", "Oranges"="Oranges", "Accent"="Accent", "Dark2"="Dark2","Paired"="Paired","Pastel1"="Pastel1","Pastel2"="Pastel2","Set1"="Set1","Set2"="Set2","Set3"="Set3"), selected = "RdYlBu"),
                 selectInput("select_image_type_LEfSe", label = "Output image format", choices = list("JPG" = ".jpg", "TIFF" =".tiff", "PDF" = ".pdf",  "SVG" = ".svg", "BMP" = ".bmp", "EPS" = ".eps", "PS" = ".ps"), selected = ".jpg"),
                 actionButton("action_LEfSe", "Submit"),
               ),
               mainPanel(
                 tabsetPanel(
                   type = "tabs",
                   tabPanel(
                     "Summary Table",
                     h3("Result - OTUs that were significantly different between two groups"),
                     withSpinner(verbatimTextOutput("text_LEfSe_level")),
                     fluidRow(
                       column(
                         withSpinner(dataTableOutput("LEfSe_table")), width = 12),
                     ),
                     hr(),
                     fluidRow(
                       column(3, h4("Download significant"),
                              downloadButton(outputId = "download_result_LEfSe_1", label = "Download as csv")),
                       column(3, h4("Total counts in each samples"),
                              downloadButton(outputId = "download_result_LEfSe_4", label = "Download as csv")),
                     ),
                   ),
                   tabPanel(
                     "Plot",
                     h3("Plot"),
                     withSpinner(plotOutput("boxplot_LEfSe", width = "100%", height = "800px")),
                     fluidRow(
                       column(3, numericInput("Boxplot_LEfSe_output_height", label = h5("Figure height (upto 49 inces)"), value = 8, width = "300px")),
                       column(3, numericInput("Boxplot_LEfSe_output_width", label = h5("Figure width (upto 49 inces)"), value = 8, width = "300px")),
                       column(3, numericInput("Boxplot_LEfSe_output_dpi", label = h5("Figure resolution (dpi:72 to 300)"), value = 300, width = "300px")),
                     ),
                     downloadButton(outputId = "download_Boxplot_LEfSe", label = "Download plot"),
                   ),
                   
                 ),
               ),
             ),
           ),
           
           
            tabPanel(
             "MaAsLin3",
             sidebarLayout(
               sidebarPanel(
                 h3("Microbiome Multivariable Associations with Linear Models"),
                 selectInput("input_RA_MaAsLin3", label = "Selected input", choices = "Example data selected"),
                 selectInput("group1_MaAsLin3", label = "Select condition1", choices = "Please upload metadata in upload page"),
                 selectInput("group2_MaAsLin3", label = "Select condition2", choices = "Please upload metadata in upload page"),
                 selectInput("select_MaAsLin3_normalization", label = "Normalization", choices = list("TSS - total sum scaling"="TSS", "CLR - centered log ratio"="CLR", "NONE"="NONE"), selected = "TSS"),
                 selectInput("select_MaAsLin3_transformation", label = "Transformation", choices = list("LOG"="LOG", "PLOG"="PLOG", "NONE"="NONE"), selected = "TSS"),
                 selectInput("select_MaAsLin3_correction", label = "Correction", choices = list("BH"="BH"), selected = "BH"),
                 numericInput("select_MaAsLin3_pvalue","FDR corrected q-value", value = "0.1"),
                 actionButton("action_MaAsLin3", "Submit"),
               ),
               mainPanel(
                 tabsetPanel(
                   type = "tabs",
                   tabPanel(
                     "Results",
                     h3("Results"),
                     withSpinner(verbatimTextOutput("text_MaAsLin3_level")),
                     h3("Download results"),
                     downloadButton(outputId = "download_zip_MaAsLin3", label = "Download Output as ZIP"),
                   ),
                   
                 ),
               ),
             ),
           ),
           
           tabPanel(
             "Limma-Voom",
             sidebarLayout(
               sidebarPanel(
                 h3("Limma-Voom"),
                 selectInput("input_RA_limma", label = "Selected input", choices = "Example data selected"),
                 selectInput("group1_limma", label = "Select condition1", choices = "Please upload metadata in upload page"),
                 selectInput("group2_limma", label = "Select condition2", choices = "Please upload metadata in upload page"),
                 selectInput("select_limma_pvalue", label = "Test correction", choices = list("Benjamini-Hochberg FDR" = "padj", "P-value" = "pvalue"), selected = "padj"),
                 numericInput("limma_pvalue","FDR or Pvalue", value = "0.05"),
                 selectInput("limma_color_palette", label = "Colors", choices = list("RdYlBu"="RdYlBu", "RdYlGn"="RdYlGn", "BrBG"="BrBG", "PiYG"="PiYG", "PRGn"="PRGn", "PuOr"="PuOr", "RdBu"="RdBu", "RdGy"="RdGy", "Spectral"="Spectral", "Blues"="Blues", "Reds"="Reds", "Greens"="Greens", "Greys"="Greys", "Oranges"="Oranges", "Accent"="Accent", "Dark2"="Dark2","Paired"="Paired","Pastel1"="Pastel1","Pastel2"="Pastel2","Set1"="Set1","Set2"="Set2","Set3"="Set3"), selected = "RdYlBu"),
                 radioButtons("select_limma_plot", "Types of plot", choices = c("Grouped box plot" = 1, "Individual box plot" = 2, "Volcano plot" = 3, "Heatmap" = 4), selected = 1),
                 radioButtons("limma_plot_taxa_mode", "Significant taxa in plot", choices = c("All significant" = "all", "Selected number" = "selected"), selected = "all"),
                 conditionalPanel(
                   condition = "input.limma_plot_taxa_mode == 'selected'",
                   numericInput("limma_plot_top_n", "Number of significant taxa in plot", value = 25, min = 2, max = 100, step = 1)
                 ),
                 checkboxInput("show_limma_plot_labels", "Show volcano/heatmap labels", value = TRUE),
                 selectInput("select_image_type_limma", label = "Output image format", choices = list("JPG" = ".jpg", "TIFF" =".tiff", "PDF" = ".pdf",  "SVG" = ".svg", "BMP" = ".bmp", "EPS" = ".eps", "PS" = ".ps"), selected = ".jpg"),
                 actionButton("action_limma", "Submit"),
               ),
               mainPanel(
                 tabsetPanel(
                   type = "tabs",
                   tabPanel(
                     "Summary Table",
                     h3("Result - OTUs that were significantly different between two groups"),
                     withSpinner(verbatimTextOutput("text_limma_level")),
                     fluidRow(
                       column(
                         withSpinner(dataTableOutput("limma_table")), width = 12),
                     ),
                     hr(),
                     fluidRow(
                     column(3, h4("Download significant"),
                     downloadButton(outputId = "download_result_limma_1", label = "Download as csv")),
                     column(3, h4("Download all"),
                     downloadButton(outputId = "download_result_limma_2", label = "Download as csv")),
                     column(3, h4("Total counts in each samples"),
                     downloadButton(outputId = "download_result_limma_3", label = "Download as csv")),
                     ),
                    ),
                   tabPanel(
                     "Plot",
                     h3("Plot"),
                     withSpinner(plotOutput("boxplot_limma", width = "100%", height = "800px")),
                     uiOutput("limma_individual_plot_page_ui"),
                     fluidRow(
                       column(3, numericInput("Boxplot_limma_output_height", label = h5("Figure height (upto 49 inces)"), value = 8, width = "300px")),
                       column(3, numericInput("Boxplot_limma_output_width", label = h5("Figure width (upto 49 inces)"), value = 8, width = "300px")),
                       column(3, numericInput("Boxplot_limma_output_dpi", label = h5("Figure resolution (dpi:72 to 300)"), value = 300, width = "300px")),
                     ),
                     downloadButton(outputId = "download_Boxplot_limma", label = "Download plot"),
                   ),
                 ),
               ),
             ),
           ),
           
           tabPanel(
             "edgeR",
             sidebarLayout(
               sidebarPanel(
                 h3("edgeR"),
                 selectInput("input_RA_edger", label = "Selected input", choices = "Example data selected"),
                 selectInput("group1_edger", label = "Select condition1", choices = "Please upload metadata in upload page"),
                 selectInput("group2_edger", label = "Select condition2", choices = "Please upload metadata in upload page"),
                 selectInput("select_edger_pvalue", label = "Test correction", choices = list("Benjamini-Hochberg FDR" = "padj", "P-value" = "pvalue"), selected = "padj"),
                 numericInput("edger_pvalue","FDR or Pvalue", value = "0.05"),
                 selectInput("edger_color_palette", label = "Colors", choices = list("RdYlBu"="RdYlBu", "RdYlGn"="RdYlGn", "BrBG"="BrBG", "PiYG"="PiYG", "PRGn"="PRGn", "PuOr"="PuOr", "RdBu"="RdBu", "RdGy"="RdGy", "Spectral"="Spectral", "Blues"="Blues", "Reds"="Reds", "Greens"="Greens", "Greys"="Greys", "Oranges"="Oranges", "Accent"="Accent", "Dark2"="Dark2","Paired"="Paired","Pastel1"="Pastel1","Pastel2"="Pastel2","Set1"="Set1","Set2"="Set2","Set3"="Set3"), selected = "RdYlBu"),
                 radioButtons("select_edger_plot", "Types of plot", choices = c("Grouped box plot" = 1, "Individual box plot" = 2, "Heatmap" = 3), selected = 1),
                 radioButtons("edger_plot_taxa_mode", "Significant taxa in plot", choices = c("All significant" = "all", "Selected number" = "selected"), selected = "all"),
                 conditionalPanel(
                   condition = "input.edger_plot_taxa_mode == 'selected'",
                   numericInput("edger_plot_top_n", "Number of significant taxa in plot", value = 25, min = 2, max = 100, step = 1)
                 ),
                 checkboxInput("show_edger_plot_labels", "Show heatmap labels", value = TRUE),
                 selectInput("select_image_type_edger", label = "Output image format", choices = list("JPG" = ".jpg", "TIFF" =".tiff", "PDF" = ".pdf",  "SVG" = ".svg", "BMP" = ".bmp", "EPS" = ".eps", "PS" = ".ps"), selected = ".jpg"),
                 actionButton("action_edger", "Submit"),
               ),
               mainPanel(
                 tabsetPanel(
                   type = "tabs",
                   tabPanel(
                     "Summary Table",
                     h3("Result - OTUs that were significantly different between two groups"),
                     withSpinner(verbatimTextOutput("text_edger_level")),
                     fluidRow(
                       column(
                         withSpinner(dataTableOutput("edger_table")), width = 12),
                     ),
                     hr(),
                     fluidRow(
                     column(3, h4("Download significant"),
                     downloadButton(outputId = "download_result_edger_1", label = "Download as csv")),
                     column(3, h4("Download all"),
                     downloadButton(outputId = "download_result_edger_2", label = "Download as csv")),
                     column(3, h4("Total counts in each samples"),
                     downloadButton(outputId = "download_result_edger_3", label = "Download as csv")),
                     ),
                     ),
                   tabPanel(
                     "Plot",
                     h3("Plot"),
                     withSpinner(plotOutput("boxplot_edger", width = "100%", height = "800px")),
                     uiOutput("edger_individual_plot_page_ui"),
                     fluidRow(
                       column(3, numericInput("Boxplot_edger_output_height", label = h5("Figure height (upto 49 inces)"), value = 8, width = "300px")),
                       column(3, numericInput("Boxplot_edger_output_width", label = h5("Figure width (upto 49 inces)"), value = 8, width = "300px")),
                       column(3, numericInput("Boxplot_edger_output_dpi", label = h5("Figure resolution (dpi:72 to 300)"), value = 300, width = "300px")),
                     ),
                     downloadButton(outputId = "download_Boxplot_edger", label = "Download plot"),
                   ),
                 ),
               ),
             ),
           ),
           "----",
           "Multiple groups",
           tabPanel(
             "Kruskal-Wallis test",
             sidebarLayout(
               sidebarPanel(
                 h3("Kruskal-Wallis test"),
                 selectInput("input_kruskal_wallis_test", label = "Selected input", choices = "Example data selected"),
                 selectInput("select_kruskal_wallis_test_pvalue", label = "Test correction", choices = list("Benjamini-Hochberg FDR" = "padj", "P-value" = "pvalue"), selected = "padj"),
                 numericInput("kruskal_wallis_test_pvalue","FDR or Pvalue", value = "0.05"),
                 radioButtons("kruskal_wallis_test_ad_hoc", "Post-hoc test", choices = c("Yes" = "Yes", "No" = "No"), selected = "No"),
                 selectInput("kruskal_wallis_test_color_palette", label = "Colors", choices = list("RdYlBu"="RdYlBu", "RdYlGn"="RdYlGn", "BrBG"="BrBG", "PiYG"="PiYG", "PRGn"="PRGn", "PuOr"="PuOr", "RdBu"="RdBu", "RdGy"="RdGy", "Spectral"="Spectral", "Blues"="Blues", "Reds"="Reds", "Greens"="Greens", "Greys"="Greys", "Oranges"="Oranges", "Accent"="Accent", "Dark2"="Dark2","Paired"="Paired","Pastel1"="Pastel1","Pastel2"="Pastel2","Set1"="Set1","Set2"="Set2","Set3"="Set3"), selected = "RdYlBu"),
                 radioButtons("kruskal_wallis_test_plot", "Types of plot", choices = c("Grouped box plot" = 1, "Individual box plot" = 2, "Heatmap" = 3), selected = 1),
                 radioButtons("kruskal_wallis_test_plot_taxa_mode", "Significant taxa in plot", choices = c("All significant" = "all", "Selected number" = "selected"), selected = "all"),
                 conditionalPanel(
                   condition = "input.kruskal_wallis_test_plot_taxa_mode == 'selected'",
                   numericInput("kruskal_wallis_test_plot_top_n", "Number of significant taxa in plot", value = 25, min = 2, max = 100, step = 1)
                 ),
                 checkboxInput("show_kruskal_wallis_test_plot_labels", "Show heatmap labels", value = TRUE),
                 selectInput("select_image_type_kruskal_wallis_test", label = "Output image format", choices = list("JPG" = ".jpg", "TIFF" =".tiff", "PDF" = ".pdf",  "SVG" = ".svg", "BMP" = ".bmp", "EPS" = ".eps", "PS" = ".ps"), selected = ".jpg"),
                 actionButton("action_kruskal_wallis_test", "Submit"),
               ),
               mainPanel(
                 tabsetPanel(
                   type = "tabs",
                  tabPanel(
                     "Summary Table",
                     h3("Result - OTUs that were significantly different between multiple groups"),
                     withSpinner(verbatimTextOutput("text_kruskal_wallis_test_level")),
                     fluidRow(
                       column(
                         withSpinner(dataTableOutput("kruskal_wallis_test_table")), width = 12),
                     ),
                     hr(),
                     fluidRow(
                     column(3, h4("Download significant"),
                     downloadButton(outputId = "download_result_kruskal_wallis_test_1", label = "Download as csv")),
                     column(3, h4("Download all"),
                     downloadButton(outputId = "download_result_kruskal_wallis_test_2", label = "Download as csv")),
                     column(3, h4("Download relative frequency"),
                     downloadButton(outputId = "download_result_kruskal_wallis_test_3", label = "Download as csv")),
                     column(3, h4("Total counts in each samples"),
                     downloadButton(outputId = "download_result_kruskal_wallis_test_4", label = "Download as csv")),
                     ),
                   ),
                  tabPanel(
                    "Plot",
                    h3("Plot"),
                    withSpinner(plotOutput("boxplot_kruskal_wallis_test", width = "100%", height = "1500px")),
                    uiOutput("kruskal_wallis_test_individual_plot_page_ui"),
                    fluidRow(
                      column(3, numericInput("Boxplot_kruskal_wallis_test_output_height", label = h5("Figure height (upto 49 inces)"), value = 8, width = "300px")),
                      column(3, numericInput("Boxplot_kruskal_wallis_test_output_width", label = h5("Figure width (upto 49 inces)"), value = 8, width = "300px")),
                      column(3, numericInput("Boxplot_kruskal_wallis_test_output_dpi", label = h5("Figure resolution (dpi:72 to 300)"), value = 300, width = "300px")),
                    ),
                    downloadButton(outputId = "download_Boxplot_kruskal_wallis_test", label = "Download plot"),
                  ),
                 ),
               ),
             ),
           ),
           tabPanel(
             "ANOVA",
             sidebarLayout(
               sidebarPanel(
                 h3("Analysis of variance: ANOVA"),
                 selectInput("input_anova", label = "Selected input", choices = "Example data selected"),
                 selectInput("select_anova_pvalue", label = "Test correction", choices = list("Benjamini-Hochberg FDR" = "padj", "P-value" = "pvalue"), selected = "padj"),
                 numericInput("anova_pvalue","FDR or Pvalue", value = "0.05"),
                 radioButtons("anova_ad_hoc", "Post-hoc test", choices = c("Yes" = "Yes", "No" = "No"), selected = "No"),
                 selectInput("anova_color_palette", label = "Colors", choices = list("RdYlBu"="RdYlBu", "RdYlGn"="RdYlGn", "BrBG"="BrBG", "PiYG"="PiYG", "PRGn"="PRGn", "PuOr"="PuOr", "RdBu"="RdBu", "RdGy"="RdGy", "Spectral"="Spectral", "Blues"="Blues", "Reds"="Reds", "Greens"="Greens", "Greys"="Greys", "Oranges"="Oranges", "Accent"="Accent", "Dark2"="Dark2","Paired"="Paired","Pastel1"="Pastel1","Pastel2"="Pastel2","Set1"="Set1","Set2"="Set2","Set3"="Set3"), selected = "RdYlBu"),
                 radioButtons("anova_plot", "Types of plot", choices = c("Grouped box plot" = 1, "Individual box plot" = 2, "Heatmap" = 3), selected = 1),
                 radioButtons("anova_plot_taxa_mode", "Significant taxa in plot", choices = c("All significant" = "all", "Selected number" = "selected"), selected = "all"),
                 conditionalPanel(
                   condition = "input.anova_plot_taxa_mode == 'selected'",
                   numericInput("anova_plot_top_n", "Number of significant taxa in plot", value = 25, min = 2, max = 100, step = 1)
                 ),
                 checkboxInput("show_anova_plot_labels", "Show heatmap labels", value = TRUE),
                 selectInput("select_image_type_anova", label = "Output image format", choices = list("JPG" = ".jpg", "TIFF" =".tiff", "PDF" = ".pdf",  "SVG" = ".svg", "BMP" = ".bmp", "EPS" = ".eps", "PS" = ".ps"), selected = ".jpg"),
                 actionButton("action_anova", "Submit"),
               ),
               mainPanel(
                 tabsetPanel(
                   type = "tabs",
                   tabPanel(
                     "Summary Table",
                     h3("Result - OTUs that were significantly different between multiple groups"),
                     withSpinner(verbatimTextOutput("text_anova_level")),
                     fluidRow(
                       column(
                         withSpinner(dataTableOutput("anova_table")), width = 12),
                     ),
                     hr(),
                     fluidRow(
                     column(3, h4("Download significant"),
                     downloadButton(outputId = "download_result_anova_1", label = "Download as csv")),
                     column(3, h4("Download all"),
                     downloadButton(outputId = "download_result_anova_2", label = "Download as csv")),
                     column(3, h4("Download relative frequency"),
                     downloadButton(outputId = "download_result_anova_3", label = "Download as csv")),
                     column(3, h4("Total counts in each samples"),
                     downloadButton(outputId = "download_result_anova_4", label = "Download as csv")),
                     ),
                   ),
                   tabPanel(
                     "Plot",
                     h3("Plot"),
                     withSpinner(plotOutput("boxplot_anova", width = "100%", height = "1500px")),
                     uiOutput("anova_individual_plot_page_ui"),
                     fluidRow(
                       column(3, numericInput("Boxplot_anova_output_height", label = h5("Figure height (upto 49 inces)"), value = 8, width = "300px")),
                       column(3, numericInput("Boxplot_anova_output_width", label = h5("Figure width (upto 49 inces)"), value = 8, width = "300px")),
                       column(3, numericInput("Boxplot_anova_output_dpi", label = h5("Figure resolution (dpi:72 to 300)"), value = 300, width = "300px")),
                     ),
                     downloadButton(outputId = "download_Boxplot_anova", label = "Download plot"),
                   ),
                   
                 ),
               ),
             ),
           ),
),
navbarMenu(
  "Bulk Download",
  tabPanel(
    "All completed outputs",
    sidebarLayout(
      sidebarPanel(
        h3("Bulk Download"),
        selectInput(
          "bulk_image_type",
          label = "Image format",
          choices = list(
            "JPG" = ".jpg",
            "PNG" = ".png",
            "TIFF" = ".tiff",
            "PDF" = ".pdf",
            "SVG" = ".svg",
            "BMP" = ".bmp",
            "EPS" = ".eps",
            "PS" = ".ps"
          ),
          selected = ".jpg"
        ),
        downloadButton(outputId = "download_bulk_results", label = "Download ZIP")
      ),
      mainPanel(
        h3("Completed Outputs"),
        uiOutput("bulk_download_summary")
      )
    )
  )
),
tabPanel(
  "Run Log",
  h3("Run Status"),
  uiOutput("run_status_panel"),
  hr(),
  h3("Run History"),
  p("Each entry records the application tab or menu, the run status, and the selected parameters using the labels defined in ui.R."),
  uiOutput("run_log_ui")
),
tabPanel(
  "Session Info",
  tags$h4("R Session"),
  downloadButton("download_sess", "Download session-info.txt"),
  br(),
  withSpinner(verbatimTextOutput("sess")),
  
),
)
)

