# Uploading a download to the Galaxy history of the running session.
#
# Every table and plot of this application has a download button. Inside Galaxy,
# each gets a companion that saves the same file in the tool's discovered output
# directory. Galaxy adds those files to the history when the interactive job
# ends. This avoids blocking Shiny on an API request from inside the container.
#
# The upload itself is galaxy_ie_helpers (https://github.com/bgruening/galaxy_ie_helpers),
# which knows how to reach the Galaxy instance from inside a tool container and
# which history the session belongs to. The tool XML exports what it needs:
#
#   HISTORY_ID   the history this interactive session belongs to
#   GALAXY_URL   the URL of the Galaxy instance (with $DOCKER_HOST substituted
#                in as well, for the docker bridge address of this container)
#   GALAXY_WEB_PORT  the port to fall back to when that URL does not answer
#   API_KEY      a key that may write into that history
#
# Nothing here changes what a download produces. metadavis_download() is
# downloadHandler() plus a note of the file name and content function, so
# "Send to Galaxy" writes the same file a click on the download button would.

# --- the command that carries the upload -------------------------------------

# The helper is installed into its own virtualenv in the container image. Use
# its supported console command; the Python package has no __main__ module.
metadavis_galaxy_put <- function() {
  candidates <- c(
    Sys.getenv("METADAVIS_GALAXY_PUT", unset = ""),
    "/opt/galaxy_ie_helpers/bin/put",
    "put"
  )
  for (candidate in candidates) {
    if (!nzchar(candidate)) next
    path <- if (grepl("/", candidate, fixed = TRUE)) candidate else Sys.which(candidate)
    if (nzchar(path) && file.access(path, mode = 1L) == 0L) {
      return(path)
    }
  }
  NA_character_
}

# What the tool XML handed over. Both are needed: the helper refuses to work
# without them, and a browser session (no API key, no history) has to be able to
# tell the user why the buttons are not there instead of failing silently.
metadavis_galaxy_history <- function() {
  Sys.getenv("HISTORY_ID", unset = "")
}

metadavis_galaxy_key <- function() {
  Sys.getenv("API_KEY", unset = "")
}

metadavis_galaxy_output_dir <- function() {
  Sys.getenv("METADAVIS_OUTPUT_DIR", unset = "")
}

metadavis_galaxy_output_path <- function(name) {
  output_dir <- metadavis_galaxy_output_dir()
  if (!nzchar(output_dir)) {
    return(NA_character_)
  }
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  file.path(output_dir, basename(name))
}

metadavis_galaxy_log <- function(...) {
  line <- paste0(format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z"), " ", paste0(..., collapse = ""))
  message(line)
  output_dir <- metadavis_galaxy_output_dir()
  if (nzchar(output_dir)) {
    dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
    cat(line, "\n", file = file.path(output_dir, "galaxy_upload.log"), append = TRUE)
  }
}

# TRUE when this session can upload back to Galaxy. Checked in the UI as well,
# so a plain `docker run` does not offer buttons that cannot work.
metadavis_galaxy_ready <- function() {
  nzchar(metadavis_galaxy_history()) &&
    nzchar(metadavis_galaxy_key()) &&
    !is.na(metadavis_galaxy_put())
}

# --- the upload --------------------------------------------------------------

# Write `path` into the current history under `name`. Returns a list with ok and
# message, so the caller can tell the user what happened instead of leaving a
# silent failure behind.
metadavis_send_to_galaxy <- function(path, name = basename(path), filetype = "auto") {
  if (!file.exists(path)) {
    return(list(ok = FALSE, message = paste0("there is no file to send at ", path)))
  }
  put <- metadavis_galaxy_put()
  if (is.na(put)) {
    return(list(
      ok = FALSE,
      message = "galaxy_ie_helpers is not installed, so nothing can be sent back to Galaxy"
    ))
  }
  if (!nzchar(metadavis_galaxy_history()) || !nzchar(metadavis_galaxy_key())) {
    return(list(
      ok = FALSE,
      message = paste0(
        "this session has no Galaxy history or API key, so it is not running ",
        "as a Galaxy interactive tool"
      )
    ))
  }

  galaxy_url <- Sys.getenv("GALAXY_URL", unset = "<unset>")
  galaxy_port <- Sys.getenv("GALAXY_WEB_PORT", unset = "<unset>")
  metadavis_galaxy_log(
    "upload start name=", name,
    " path=", normalizePath(path, mustWork = FALSE),
    " bytes=", file.info(path)$size,
    " filetype=", filetype,
    " history=", metadavis_galaxy_history(),
    " url=", galaxy_url,
    " fallback_port=", galaxy_port,
    " put=", put,
    " api_key_present=", nzchar(metadavis_galaxy_key())
  )

  # galaxy_ie_helpers has no output of its own on success, so the exit status is
  # what says whether the dataset made it into the history. Anything it does
  # print (with DEBUG=TRUE, the bioblend request log) is kept as the message.
  #
  # Invoke the same put command used by other Galaxy Shiny applications. The
  # helper reads GALAXY_URL, GALAXY_WEB_PORT and API_KEY from the environment.
  output <- tryCatch(
    system2(
      put,
      c(
        "-p", shQuote(path),
        "-t", shQuote(filetype),
        "--history-id", shQuote(metadavis_galaxy_history())
      ),
      stdout = TRUE,
      stderr = TRUE
    ),
    error = function(e) structure(character(), status = 1L, error = conditionMessage(e))
  )

  status <- attr(output, "status")
  if (is.null(status)) status <- 0L
  error_detail <- attr(output, "error")
  output_text <- paste(as.character(output), collapse = " | ")
  metadavis_galaxy_log(
    "upload finish name=", name,
    " exit_status=", status,
    if (!is.null(error_detail)) paste0(" error=", error_detail) else "",
    if (nzchar(output_text)) paste0(" output=", output_text) else " output=<empty>"
  )
  if (status != 0L) {
    detail <- paste(utils::tail(as.character(output), 20L), collapse = "\n")
    return(list(
      ok = FALSE,
      message = paste0(
        "the upload failed with exit code ", status,
        if (nzchar(detail)) paste0(":\n", detail)
      )
    ))
  }
  list(ok = TRUE, message = paste0(name, " was added to the Galaxy history"))
}

metadavis_send_to_galaxy_async <- function(path, name = basename(path), filetype = "auto") {
  rscript <- file.path(R.home("bin"), "Rscript")
  helper <- normalizePath("scripts/galaxy_downloads.R", mustWork = TRUE)
  expression <- paste0(
    "source(", encodeString(helper, quote = "\""), "); ",
    "metadavis_send_to_galaxy(", encodeString(normalizePath(path), quote = "\""), ", ",
    encodeString(name, quote = "\""), ", ", encodeString(filetype, quote = "\""), ")"
  )
  system2(
    rscript,
    c("--vanilla", "-e", shQuote(expression)),
    stdout = FALSE,
    stderr = FALSE,
    wait = FALSE
  )
}

# --- the download registry ---------------------------------------------------

# A download button cannot be reused for an upload: its content function is only
# ever called while a download is in flight. So every handler is registered on
# the way through, which is what lets the generic "Send to Galaxy" handler below
# produce the file again without a second implementation of any table or plot.
metadavis_download_registry <- function() {
  new.env(parent = emptyenv())
}

metadavis_download_register <- function(registry, id, filename, content, contentType = "application/octet-stream") {
  assign(
    id,
    list(filename = filename, content = content, contentType = contentType),
    envir = registry
  )
}

# downloadHandler() itself, plus the registration. Defined here rather than at
# the call sites so the ~70 download outputs of the application keep reading
# exactly as they did: the arguments and the return value are downloadHandler()'s.
metadavis_download_with <- function(registry, id, filename, content, contentType = "application/octet-stream") {
  metadavis_download_register(registry, id, filename, content, contentType)
  downloadHandler(filename = filename, content = content, contentType = contentType)
}

# The file name a download button would use, as a plain string. Galaxy names the
# dataset after it, so it has to survive a path and keep its extension.
metadavis_download_name <- function(handler, id) {
  name <- tryCatch(
    if (is.function(handler$filename)) handler$filename() else as.character(handler$filename),
    error = function(e) NULL
  )
  if (is.null(name) || length(name) != 1L || !nzchar(name)) {
    name <- paste0(id, ".txt")
  }
  name <- basename(as.character(name))
  # Galaxy uses the extension to guess the datatype, so a name without one
  # (writeLines() based downloads keep .txt, but be explicit about it).
  if (!grepl("\\.[A-Za-z0-9]+$", name)) {
    name <- paste0(name, ".txt")
  }
  name
}

# --- the UI ------------------------------------------------------------------

# One small piece of javascript, added to the page once, instead of a second
# button next to each of the ~70 download buttons: every download link shiny
# renders is an <a class="shiny-download-link" id="<output id>">, so the
# companion buttons can be injected wherever they appear - including the ones
# inside panels that are only rendered once an analysis has been run.
#
# The button asks the server for the file by output id
# (input$metadavis_galaxy_send), which regenerates it with the registered
# content function and uploads it.
metadavis_galaxy_send_ui <- function() {
  if (!metadavis_galaxy_ready()) {
    return(NULL)
  }
  # NULL outside Galaxy, so a plain `docker run` does not offer a button that
  # cannot work.
  tags$script(HTML(METADAVIS_GALAXY_SEND_JS))
}

METADAVIS_GALAXY_SEND_JS <- "
(function () {
  var inputName = 'metadavis_galaxy_send';
  var pending = {};

  function idOf(anchor) {
    if (anchor.id) return anchor.id;
    var href = anchor.getAttribute('href') || '';
    var match = /[?&]w2=([^&]+)/.exec(href);
    return match ? decodeURIComponent(match[1]) : null;
  }

  function buttonFor(anchor, id) {
    var button = document.createElement('button');
    button.id = 'metadavis_gx_send_' + id;
    button.type = 'button';
    button.className = 'btn btn-default btn-sm metadavis-gx-send';
    button.style.marginLeft = '6px';
    button.style.whiteSpace = 'nowrap';
    button.textContent = 'Send to Galaxy';
    button.title = 'Add this file to the Galaxy history of this session';
    button.addEventListener('click', function (event) {
      event.preventDefault();
      if (pending[id]) return;
      pending[id] = true;
      button.disabled = true;
      button.textContent = 'Sending...';
      Shiny.setInputValue(inputName, id, { priority: 'event' });
    });
    return button;
  }

  function release(id) {
    delete pending[id];
    var button = document.getElementById('metadavis_gx_send_' + id);
    if (!button) return;
    button.disabled = false;
    button.textContent = 'Send to Galaxy';
  }

  function inject() {
    var links = document.querySelectorAll('a.shiny-download-link');
    for (var i = 0; i < links.length; i++) {
      var anchor = links[i];
      var id = idOf(anchor);
      if (!id) continue;
      var key = 'metadavis_gx_send_' + id;
      if (document.getElementById(key)) continue;
      anchor.parentNode.insertBefore(buttonFor(anchor, id), anchor.nextSibling);
    }
  }

  Shiny.addCustomMessageHandler('metadavis_galaxy_send_done', function (id) {
    release(id);
  });

  $(document).on('shiny:connected', function () { window.setTimeout(inject, 250); });
  $(document).on('shiny:value', function () { window.setTimeout(inject, 0); });
  $(document).on('shiny:bound', function () { window.setTimeout(inject, 0); });

  // Download buttons are rendered and removed as panels are shown, completed
  // and reset, so new ones have to be picked up while the session runs.
  var observer = new MutationObserver(function () { window.setTimeout(inject, 0); });
  observer.observe(document.documentElement, { childList: true, subtree: true });
})();
"
