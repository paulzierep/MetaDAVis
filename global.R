## Check packages  and install##
if (!require('shiny')) install.packages("shiny", dependencies = TRUE)
if (!require('DT')) install.packages("DT")
if (!require('shinythemes')) install.packages("shinythemes")
if (!require('shinyFiles')) install.packages("shinyFiles")
if (!require('shinyjs')) install.packages("shinyjs")
if (!require("shinydashboard")) install.packages("shinydashboard")
if (!require('ggplot2')) install.packages("ggplot2", dependencies = TRUE)
if (!require('ggpubr')) install.packages("ggpubr")
if (!require('vegan')) install.packages("vegan")
if (!require('ggfortify')) install.packages("ggfortify")
if (!require('ggplotify')) install.packages("ggplotify")
if (!require('reshape2')) install.packages("reshape2")
if (!require('tibble')) install.packages("tibble")
if (!require('scales')) install.packages("scales")
if (!require('dunn.test')) install.packages("dunn.test")
if (!require('tidyr')) install.packages("tidyr")
if (!require('dplyr')) install.packages("dplyr")
if (!require('devtools')) install.packages("devtools")
if (!require('patchwork')) install.packages("patchwork")
if (!require('GGally')) install.packages("GGally")
if (!require('plotly'))install.packages("plotly")
if (!require('zip'))install.packages("zip")
if (!require('filelock')) install.packages("filelock")
if (!require("shinycssloaders")) install.packages("shinycssloaders")
if (!require("RColorBrewer")) install.packages("RColorBrewer")
if (!require('BiocManager')) install.packages("BiocManager", update = FALSE)
if (!require('phyloseq')) BiocManager::install("phyloseq", update = FALSE)
if (!require('microbiome')) BiocManager::install("microbiome", update = FALSE)
if (!require('ComplexHeatmap')) BiocManager::install("ComplexHeatmap", update = FALSE)
if (!require('qvalue')) BiocManager::install("qvalue", update = FALSE)
if (!require('scater')) BiocManager::install("scater", update = FALSE)
if (!require('DESeq2')) BiocManager::install("DESeq2", update = FALSE)
if (!require('limma')) BiocManager::install("limma", update = FALSE)
if (!require('edgeR')) BiocManager::install("edgeR", update = FALSE)
if (!require('metagenomeSeq')) BiocManager::install("metagenomeSeq", update = FALSE)
if (!require('bluster')) BiocManager::install("bluster", update = FALSE)
if (!require('mia')) BiocManager::install("mia", update = FALSE)
if (!require('microbiomeutilities')) BiocManager::install("microsud/microbiomeutilities", update = FALSE)
if (!require('maaslin3'))BiocManager::install("biobakery/maaslin3", update = FALSE)
if (!require('lefser'))BiocManager::install("lefser", update = FALSE)
source("scripts/data_input.R")
source("scripts/galaxy_downloads.R")
# Shared Galaxy history import. scripts/galaxy_ie.R is used here for the picker
# only: this application already ships its own "Send to Galaxy" buttons, so
# galaxy_ie_send_ui() is deliberately not called and no second button is injected.
source("scripts/galaxy_ie.R")
galaxy_ie_app("metadavis", "METADAVIS_OUTPUT_DIR", "METADAVIS_OTU_TABLE")
options(shiny.maxRequestSize=2000*1024^2)
options(future.globals.maxSize= 925289600000)
#views
count_file <- "view_counter.rds"
lock_file  <- "view_counter.lock"
if (!file.exists(count_file)) saveRDS(0L, count_file)

read_count <- function() {
  readRDS(count_file)
}
increment_count <- function() {
  lock <- lock(lock_file, timeout = 5000)   # wait up to 5s for the lock
  on.exit(unlock(lock), add = TRUE)
  n <- readRDS(count_file)
  n <- n + 1L
  saveRDS(n, count_file)
  n
}
