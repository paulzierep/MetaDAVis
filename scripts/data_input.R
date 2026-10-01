library("tidyr")
library("dplyr")

# Guess the field separator of a delimited file from its header line. Galaxy
# datasets have no meaningful extension, and the Galaxy Interactive Tool only
# ever stages three files, so asking the user to repeat the separator per file
# is not worth a form field. Anything that is neither tab nor comma separated is
# reported as tab separated, which is what read.delim() would assume anyway.
detect_sep <- function(path) {
  header <- tryCatch(readLines(path, n = 1L, warn = FALSE), error = function(e) character(0))
  if (length(header) != 1L || !grepl(",", header, fixed = TRUE)) {
    return("\t")
  }
  # Prefer the separator that occurs most often, so that a comma inside a single
  # sample name does not win over genuinely tab separated data.
  n_tabs <- lengths(regmatches(header, gregexpr("\t", header, fixed = TRUE)))
  n_commas <- lengths(regmatches(header, gregexpr(",", header, fixed = TRUE)))
  if (n_tabs >= n_commas) "\t" else ","
}

# Strip the Greengenes/SILVA rank prefixes ("d__", "p__", ...) and normalise
# missing values to "".
strip_rank_prefix <- function(x) {
  x <- as.character(x)
  x <- sub("^[[:alpha:]]__", "", x)
  x[is.na(x)] <- ""
  x
}

# Reduce a column name to something that can be compared: Galaxy hands the
# header over the way its own parser saw it, which can differ from what
# read.delim() produced - a comment marker, surrounding quotes, a stray
# carriage return or just a different case.
normalise_column_name <- function(x) {
  x <- trimws(as.character(x))
  x <- gsub("^\"|\"$", "", x)
  tolower(x)
}

# Pick the grouping condition column out of the metadata table. `picked` is
# either a column number (what Galaxy's data_column param and the in-app text
# field hand over) or a column name. Returns the name of the column to use, or
# NULL when nothing usable was asked for.
#
# An unusable choice is a warning rather than an error on purpose: the condition
# is a property of the analysis, and refusing to load the data because of it
# leaves the user with an empty summary table and no way to see the samples at
# all. The second column is the convention for every input format of this
# application, so that is what is used instead.
resolve_condition_column <- function(picked, metadata_names) {
  usable <- length(picked) == 1L && !is.na(picked) &&
    nzchar(trimws(as.character(picked)))
  if (!usable) return(NULL)

  want <- normalise_column_name(picked)
  if (grepl("^[0-9]+$", want)) {
    index <- as.integer(want)
    # Column 1 holds the sample ids, so it can never be the condition: every
    # downstream plot reads group_index[, 2] as the condition.
    if (index >= 2L && index <= length(metadata_names)) {
      return(metadata_names[[index]])
    }
    warning(
      "Column ", index, " is not a usable grouping condition column: the ",
      "metadata table has ", length(metadata_names) - 1L,
      " annotation column(s), so pick one of 2 to ", length(metadata_names),
      ". Using column 2 instead.", call. = FALSE
    )
    return(NULL)
  }

  hit <- which(normalise_column_name(metadata_names) == want)
  if (!length(hit)) {
    warning(
      "The metadata table has no column named '", as.character(picked),
      "'. Available columns: ", paste(metadata_names, collapse = ", "),
      ". Using column 2 instead.", call. = FALSE
    )
    return(NULL)
  }
  if (hit[[1]] == 1L) {
    warning(
      "'", as.character(picked), "' is the sample id column, not an ",
      "annotation. Using column 2 instead.", call. = FALSE
    )
    return(NULL)
  }
  metadata_names[[hit[[1]]]]
}

# Read the sample metadata table and reduce it to the two columns this
# application can use: the sample ids and the grouping condition. Every
# downstream plot reads the second column of the metadata as the condition, and
# the summary tables are built from it, so the remaining annotation columns of a
# wide table are dropped instead of being carried around unused.
#
# Returns the three shapes the rest of the application expects:
#
#   data_index   sample ids and condition, side by side, as Samples/Condition
#   data_index1  the condition on its own, keyed by sample id
#   data_index2  the distinct conditions, for the "No. of conditions" table
read_sample_metadata <- function(Index, sep = NULL, Condition = NULL, log_head = NULL) {
  if (is.null(sep)) sep <- detect_sep(Index)
  raw <- read.delim(Index, header = TRUE, sep = sep, check.names = FALSE)

  if (ncol(raw) < 2L) {
    stop("The metadata table needs a sample column plus at least one annotation column.")
  }
  if (nrow(raw) == 0L) {
    stop("The metadata table does not contain any samples.")
  }
  if (anyDuplicated(raw[[1L]])) {
    stop("The sample ids in the metadata table must be unique.")
  }

  picked <- resolve_condition_column(Condition, colnames(raw))
  if (is.null(picked)) picked <- colnames(raw)[[2]]

  data_index <- raw[, c(1L, match(picked, colnames(raw))), drop = FALSE]
  colnames(data_index) <- c("Samples", "Condition")
  if (!is.null(log_head)) log_head(data_index, "data_index (sample metadata)")

  # Same single annotation column as data_index, keyed by sample id, so the two
  # cannot drift apart. Built from data_index rather than read a second time: a
  # second read of the same file can disagree with the first one (quoting, line
  # endings), and the sample names have to line up with the count matrix.
  data_index1 <- data.frame(
    Condition = data_index[["Condition"]],
    row.names = data_index[["Samples"]]
  )
  if (!is.null(log_head)) log_head(data_index1, "data_index1 (condition, row.names=Samples)")

  data_index2 <- unique(data_index["Condition"])
  rownames(data_index2) <- NULL
  if (!is.null(log_head)) log_head(data_index2, "data_index2 (unique conditions)")

  list(
    data_index  = data_index,
    data_index1 = data_index1,
    data_index2 = data_index2,
    text        = paste("There are ", nrow(data_index), " Samples.", sep = "")
  )
}

# Build the seven-column taxonomy layout MetaDAVis aggregates on, from a
# phyloseq-style pair of tables:
#
#   Input     feature table, features in the rows (first column holds the
#             feature id), samples in the columns
#   Taxonomy  taxonomy table, features in the rows (first column holds the
#             feature id), ranks in the columns
#
# The result has the 7 taxonomy columns first and the sample count columns
# after them, which is exactly the shape the rank-collapsing code below
# expects, so both input styles share one aggregation path.
phyloseq_counts_taxonomy <- function(Input, Taxonomy, sep = NULL, show_head = FALSE) {
  taxa <- c("Kingdom", "Phylum", "Class", "Order", "Family", "Genus", "Species")

  if (is.null(sep)) sep <- detect_sep(Input)
  tax_sep <- if (is.null(sep)) detect_sep(Taxonomy) else sep

  otu <- read.delim(Input, header = TRUE, row.names = 1, sep = sep, check.names = FALSE)
  tax <- read.delim(Taxonomy, header = TRUE, row.names = 1, sep = tax_sep, check.names = FALSE)

  if (nrow(otu) == 0L) stop("The OTU table does not contain any features.")
  if (ncol(otu) == 0L) stop("The OTU table does not contain any sample columns.")
  if (nrow(tax) == 0L) stop("The taxonomy table does not contain any features.")
  if (anyDuplicated(colnames(otu))) stop("Sample names in the OTU table must be unique.")
  if (any(!nzchar(colnames(otu)))) stop("The OTU table has empty sample names.")

  # Map the taxonomy columns onto the seven ranks MetaDAVis knows. Matched
  # case-insensitively on the column name ("Domain" is accepted as "Kingdom"),
  # falling back to positional order for unnamed or abbreviated tables, which
  # is how most taxonomy exports with stripped headers look.
  tax_names <- tolower(trimws(names(tax)))
  wanted <- c("kingdom", "phylum", "class", "order", "family", "genus", "species")
  aliases <- list(c("kingdom", "domain"), "phylum", "class", "order", "family", "genus", "species")
  match_at <- vapply(seq_along(wanted), function(i) {
    hit <- which(tax_names %in% aliases[[i]])
    if (length(hit)) hit[[1]] else NA_integer_
  }, integer(1))

  if (all(is.na(match_at)) && ncol(tax) >= 7L) {
    match_at <- seq_len(7L)
  }
  if (anyNA(match_at)) {
    stop(paste0(
      "The taxonomy table needs columns for ", taxa[[1]],
      " to ", taxa[[7]], ". Found: ",
      paste(names(tax), collapse = ", "), "."
    ))
  }

  tax_m <- matrix("", nrow = nrow(otu), ncol = 7L)
  for (i in seq_len(7L)) {
    column <- match(rownames(otu), rownames(tax))
    tax_m[, i] <- strip_rank_prefix(tax[[match_at[[i]]]])[column]
  }
  colnames(tax_m) <- taxa
  tax_m <- as.data.frame(tax_m, stringsAsFactors = FALSE)

  # Features with no taxonomy row at all would otherwise come back as an empty
  # name at every rank: the lineage back-filling only fills blanks from a named
  # ancestor, and there is none. Label them explicitly so they show up in the
  # taxonomy table and in plot labels as "Unclassified" rather than as a blank.
  unclassified <- rowSums(is.na(tax_m) | tax_m == "") == 7L
  if (any(unclassified)) {
    tax_m[["Kingdom"]][unclassified] <- "Unclassified"
  }

  out <- cbind(tax_m, otu, stringsAsFactors = FALSE)
  rownames(out) <- NULL

  if (isTRUE(show_head)) {
    cat("\n--- data_tmp (phyloseq OTU x taxonomy) ---\n",
        paste(utils::capture.output(utils::head(out, 5L)), collapse = "\n"), "\n", sep = "")
  }
  out
}

# `sep`/`sep1` default to NULL so that the phyloseq path can detect the separator
# per file; the other formats are called with explicit separators by the UI.
data_input_RA <- function(file_type, Input, Index, Taxonomy = NULL, type, sep = NULL, sep1 = NULL, Condition = NULL, show_head = TRUE, head_n = 5){
  # Condition: which metadata column holds the grouping condition, either a
  # column number or a column name. NULL/empty means "use the second column".
  
  log_head <- function(x, label){
    if (show_head) {
      msg <- paste0(
        "\n--- ", label, " (head ", head_n, ") ---\n",
        paste(utils::capture.output(utils::head(x, head_n)), collapse = "\n"),
        "\n"
      )
      cat(msg, file = stderr())
      flush.console()
    }
  }

  
  file_type <- as.character(file_type)
  taxa <- c("Kingdom", "Phylum", "Class", "Order", "Family", "Genus", "Species")

  if (file_type == "phyloseq"){
    # The Galaxy input format: a phyloseq sample data table with the sample ids
    # in the first column and one annotation column per further column. Its
    # separator is detected from the file, because Galaxy datasets carry no
    # extension and the staged copies keep their plain names.
    metadata_sep <- NULL
  }
  else if (file_type == "example"){
    metadata_sep <- "\t"
  }
  else if (file_type == "qiime_format" | file_type == "Megan" | file_type == "check"){
    metadata_sep <- sep1
  }
  else {
    stop("Invalid file type. Please check your input.")
  }

  # Metadata file, reduced to the sample ids and the grouping condition. Every
  # input format shares this path, so they all end up with the same two-column
  # shape whatever the file they were given looks like.
  metadata <- read_sample_metadata(
    Index, sep = metadata_sep, Condition = Condition, log_head = log_head
  )
  data_index   <- metadata$data_index
  data_index1  <- metadata$data_index1
  data_index2  <- metadata$data_index2
  text_data_index <- metadata$text

  # counts / taxonomy input
  if (file_type == "phyloseq" && (is.null(Taxonomy) || !nzchar(Taxonomy) || !file.exists(Taxonomy))) {
    stop("The phyloseq input format needs an OTU table, a taxonomy table and a metadata table.")
  }
  if (file_type == "qiime_format"){
    data_tmp <- as.data.frame(t(read.delim(Input, header = FALSE, sep = sep)))
    names(data_tmp) <- as.character(data_tmp[1,])
    data_tmp <- data_tmp[-1,]
    colnames(data_tmp)[1] <- "index"
    data_tmp$index <- gsub("[;]__","", data_tmp$index)
    data_tmp <- suppressWarnings(
      data_tmp %>% separate(index, c("Kingdom","Phylum","Class","Order","Family","Genus","Species"), sep = "; |;")
    )
    rownames(data_tmp) <- NULL
    log_head(data_tmp, "data_tmp (QIIME format after split)")
  }
  else if (file_type == "Megan"){
    data_tmp <- read.delim(Input, header = TRUE, sep = sep)
    colnames(data_tmp)[1:7] <- c("Kingdom","Phylum","Class","Order","Family","Genus","Species")
    log_head(data_tmp, "data_tmp (MEGAN format)")
  }
  else if (file_type == "example"){
    data_tmp <- read.delim(Input, header = TRUE, sep = "\t")
    colnames(data_tmp)[1:7] <- c("Kingdom","Phylum","Class","Order","Family","Genus","Species")
    log_head(data_tmp, "data_tmp (example)")
  }
  else if (file_type == "check"){
    data_tmp <- read.delim(Input, header = TRUE, sep = sep)
    colnames(data_tmp)[1:7] <- c("Kingdom","Phylum","Class","Order","Family","Genus","Species")
    log_head(data_tmp, "data_tmp (check)")
  }
  else if (file_type == "phyloseq"){
    data_tmp <- phyloseq_counts_taxonomy(Input, Taxonomy, sep = sep, show_head = show_head)
  }
  else{
    print("Check your file format and separators. Also ensure sample names match between count and metadata.")
  }
  
  # phyloseq tables are features x samples, so the OTU column names are the
  # sample names and have to line up with the metadata. The other formats already
  # share the sample columns between count and metadata by construction.
  if (file_type == "phyloseq"){
    missing_samples <- setdiff(colnames(data_tmp)[-(1:7)], data_index[["Samples"]])
    extra_samples <- setdiff(data_index[["Samples"]], colnames(data_tmp)[-(1:7)])
    if (length(missing_samples)) {
      stop(paste0(
        "These samples are in the OTU table but not in the metadata: ",
        paste(utils::head(missing_samples, 10), collapse = ", "),
        if (length(missing_samples) > 10) ", ..." else "", ". ",
        "Sample names have to match between the OTU table and the metadata."
      ))
    }
    if (length(extra_samples)) {
      warning(paste0(
        "Ignoring ", length(extra_samples),
        " metadata row(s) without a matching sample in the OTU table."
      ))
      keep <- data_index[["Samples"]] %in% colnames(data_tmp)[-(1:7)]
      data_index  <- data_index[keep, , drop = FALSE]
      data_index1 <- data_index1[rownames(data_index1) %in% data_index[["Samples"]], , drop = FALSE]
      rownames(data_index) <- NULL
    }
    # Downstream code pairs the OTU columns with the metadata row by row, so
    # reorder the metadata to follow the OTU table's sample order.
    if (!identical(as.character(data_index[["Samples"]]), colnames(data_tmp)[-(1:7)])) {
      ord <- match(colnames(data_tmp)[-(1:7)], data_index[["Samples"]])
      data_index  <- data_index[ord, , drop = FALSE]
      data_index1 <- data_index1[rownames(data_index1)[ord], , drop = FALSE]
      rownames(data_index) <- NULL
    }
  }
  
  data_tmp[is.na(data_tmp)] <- ''
  data_tmp <- data_tmp %>%  mutate(Phylum = ifelse(Phylum == 'p__' | Phylum == '', Kingdom, Phylum))
  data_tmp <- data_tmp %>%  mutate(Class  = ifelse(Class  == 'c__' | Class  == '', Phylum,  Class))
  data_tmp <- data_tmp %>%  mutate(Order  = ifelse(Order  == 'o__' | Order  == '', Class,   Order))
  data_tmp <- data_tmp %>%  mutate(Family = ifelse(Family == 'f__' | Family == '', Order,   Family))
  data_tmp <- data_tmp %>%  mutate(Genus  = ifelse(Genus  == 'g__' | Genus  == '', Family,  Genus))
  data_tmp <- data_tmp %>%  mutate(Species= ifelse(Species== 's__' | Species== '', Genus,   Species))
  log_head(data_tmp, "data_tmp (filled lineage)")
  
  # collapse to requested rank
  {
    if(type == "1"){
      data_kingdom <- data_tmp[, c(1, 8:ncol(data_tmp))]
      data_kingdom[, 2:ncol(data_kingdom)] <- sapply(data_kingdom[, 2:ncol(data_kingdom)], as.numeric)
      log_head(data_kingdom, "data_kingdom (pre-aggregate)")
      data_kingdom_sum <- aggregate(. ~ Kingdom, data_kingdom, sum)
      data_kingdom_sum <- data_kingdom_sum[rowSums(data_kingdom_sum[, 2:ncol(data_kingdom_sum)]) > 0, ]
      log_head(data_kingdom_sum, "data_kingdom_sum (post-aggregate)")
      data_OTU <- data_kingdom_sum[, 2:ncol(data_kingdom_sum)]
      rownames(data_OTU) <- data_kingdom_sum[, 1]
      Tax_data <- as.data.frame(data_kingdom_sum[, 1])
    }
    else if(type == "2"){
      data_phylum <- data_tmp[, c(2, 8:ncol(data_tmp))]
      data_phylum[, 2:ncol(data_phylum)] <- sapply(data_phylum[, 2:ncol(data_phylum)], as.numeric)
      log_head(data_phylum, "data_phylum (pre-aggregate)")
      data_phylum_sum <- aggregate(. ~ Phylum, data_phylum, sum)
      data_phylum_sum <- data_phylum_sum[rowSums(data_phylum_sum[, 2:ncol(data_phylum_sum)]) > 0, ]
      log_head(data_phylum_sum, "data_phylum_sum (post-aggregate)")
      data_OTU <- data_phylum_sum[, 2:ncol(data_phylum_sum)]
      rownames(data_OTU) <- data_phylum_sum[, 1]
      Tax_data <- as.data.frame(data_phylum_sum[, 1])
    }
    else if(type == "3"){
      data_class <- data_tmp[, c(3, 8:ncol(data_tmp))]
      data_class[, 2:ncol(data_class)] <- sapply(data_class[, 2:ncol(data_class)], as.numeric)
      log_head(data_class, "data_class (pre-aggregate)")
      data_class_sum <- aggregate(. ~ Class, data_class, sum)
      data_class_sum <- data_class_sum[rowSums(data_class_sum[, 2:ncol(data_class_sum)]) > 0, ]
      log_head(data_class_sum, "data_class_sum (post-aggregate)")
      data_OTU <- data_class_sum[, 2:ncol(data_class_sum)]
      rownames(data_OTU) <- data_class_sum[, 1]
      Tax_data <- as.data.frame(data_class_sum[, 1])
    }
    else if(type == "4"){
      data_order <- data_tmp[, c(4, 8:ncol(data_tmp))]
      data_order[, 2:ncol(data_order)] <- sapply(data_order[, 2:ncol(data_order)], as.numeric)
      log_head(data_order, "data_order (pre-aggregate)")
      data_order_sum <- aggregate(. ~ Order, data_order, sum)
      data_order_sum <- data_order_sum[rowSums(data_order_sum[, 2:ncol(data_order_sum)]) > 0, ]
      log_head(data_order_sum, "data_order_sum (post-aggregate)")
      data_OTU <- data_order_sum[, 2:ncol(data_order_sum)]
      rownames(data_OTU) <- data_order_sum[, 1]
      Tax_data <- as.data.frame(data_order_sum[, 1])
    }
    else if(type == "5"){
      data_family <- data_tmp[, c(5, 8:ncol(data_tmp))]
      data_family[, 2:ncol(data_family)] <- sapply(data_family[, 2:ncol(data_family)], as.numeric)
      log_head(data_family, "data_family (pre-aggregate)")
      data_family_sum <- aggregate(. ~ Family, data_family, sum)
      data_family_sum <- data_family_sum[rowSums(data_family_sum[, 2:ncol(data_family_sum)]) > 0, ]
      log_head(data_family_sum, "data_family_sum (post-aggregate)")
      data_OTU <- data_family_sum[, 2:ncol(data_family_sum)]
      rownames(data_OTU) <- data_family_sum[, 1]
      Tax_data <- as.data.frame(data_family_sum[, 1])
    }
    else if(type == "6"){
      data_genus <- data_tmp[, c(6, 8:ncol(data_tmp))]
      data_genus[, 2:ncol(data_genus)] <- sapply(data_genus[, 2:ncol(data_genus)], as.numeric)
      log_head(data_genus, "data_genus (pre-aggregate)")
      data_genus_sum <- aggregate(. ~ Genus, data_genus, sum)
      data_genus_sum <- data_genus_sum[rowSums(data_genus_sum[, 2:ncol(data_genus_sum)]) > 0, ]
      log_head(data_genus_sum, "data_genus_sum (post-aggregate)")
      data_OTU <- data_genus_sum[, 2:ncol(data_genus_sum)]
      rownames(data_OTU) <- data_genus_sum[, 1]
      Tax_data <- as.data.frame(data_genus_sum[, 1])
    }
    else if(type == "7"){
      data_species <- data_tmp[, c(7, 8:ncol(data_tmp))]
      data_species[, 2:ncol(data_species)] <- sapply(data_species[, 2:ncol(data_species)], as.numeric)
      log_head(data_species, "data_species (pre-aggregate)")
      data_species_sum <- aggregate(. ~ Species, data_species, sum)
      data_species_sum <- data_species_sum[rowSums(data_species_sum[, 2:ncol(data_species_sum)]) > 0, ]
      log_head(data_species_sum, "data_species_sum (post-aggregate)")
      data_OTU <- data_species_sum[, 2:ncol(data_species_sum)]
      rownames(data_OTU) <- data_species_sum[, 1]
      Tax_data <- as.data.frame(data_species_sum[, 1])
    }
    else {
      print("Incorrect file")
    }
  }
  
  log_head(data_OTU, "data_OTU (final counts matrix)")
  
  parent_seq_count <- as.data.frame(colSums(data_OTU))
  parent_seq_count <- tibble::rownames_to_column(parent_seq_count, "Samples")
  colnames(parent_seq_count)[2] <- "Total_counts"
  log_head(parent_seq_count, "parent_seq_count (per-sample totals)")
  
  # summaries
  text_OTU_summary <- paste("There are ", nrow(data_OTU), " bacterial taxa at the ", taxa[as.numeric(type)], " level.", sep = "")
  text_metadata_summary <- paste("Number of ", names(table(data_index[,2])), ": ", table(data_index[,2]), collapse = ", ", sep = "")
  
  number_samples <- nrow(data_index)
  
  return(list(
    text_OTU_summary  = text_OTU_summary,
    metadata_summary  = text_metadata_summary,
    Number_of_samples = number_samples,
    Data_OTU          = data_OTU,
    Data_Index        = data_index[,2],
    Type              = taxa[as.numeric(type)],
    Data_Index1       = data_index1,
    Tax_data          = Tax_data,
    Data_Index2       = data_index,
    Data_Index3       = data_index2,
    total_counts      = parent_seq_count
  ))
}
