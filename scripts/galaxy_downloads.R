# Uploading a download to the Galaxy history of the running session.
#
# Every table and plot of this application has a download button. Inside Galaxy
# that button is only half the story: whatever a user produces is gone when the
# browser tab is closed, and downloading it into ~/Downloads of the machine the
# browser runs on is not what a Galaxy user wants. So each of those buttons gets
# a "Send to Galaxy" companion that puts the very same file into the history the
# interactive session belongs to, where it can be used by the next tool.
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

# --- the python that carries the upload --------------------------------------

# The helper is installed into its own virtualenv in the container image. A
# venv is used because Ubuntu marks the system python as externally managed, and
# it also keeps the R stack of this image from reaching it by accident.
metadavis_galaxy_python <- function() {
  candidates <- c(
    Sys.getenv("METADAVIS_GALAXIE_PYTHON", unset = ""),
    "/opt/galaxy_ie_helpers/bin/python",
    "python3"
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

# TRUE when this session can upload back to Galaxy. Checked in the UI as well,
# so a plain `docker run` does not offer buttons that cannot work.
metadavis_galaxy_ready <- function() {
  nzchar(metadavis_galaxy_history()) &&
    nzchar(metadavis_galaxy_key()) &&
    !is.na(metadavis_galaxy_python())
}

# --- the upload --------------------------------------------------------------

# Write `path` into the current history under `name`. Returns a list with ok and
# message, so the caller can tell the user what happened instead of leaving a
# silent failure behind.
metadavis_send_to_galaxy <- function(path, name = basename(path), filetype = "auto") {
  if (!file.exists(path)) {
    return(list(ok = FALSE, message = paste0("there is no file to send at ", path)))
  }
  python <- metadavis_galaxy_python()
  if (is.na(python)) {
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

  # galaxy_ie_helpers has no output of its own on success, so the exit status is
  # what says whether the dataset made it into the history. Anything it does
  # print (with DEBUG=TRUE, the bioblend request log) is kept as the message.
  #
  # Its put() is called directly instead of through `python -m` or the installed
  # `put` console script: the package has no __main__, and the console script's
  # flags (-p/--filepath) are one of two spellings the CLI has had.
  script <- paste(
    "import sys;",
    "from galaxy_ie_helpers import put;",
    "put([sys.argv[1]], file_type=sys.argv[2], history_id=sys.argv[3])"
  )
  output <- tryCatch(
    system2(
      python,
      c("-c", shQuote(script), shQuote(path), shQuote(filetype), shQuote(metadavis_galaxy_history())),
      stdout = TRUE,
      stderr = TRUE
    ),
    error = function(e) structure(character(), status = 1L, error = conditionMessage(e))
  )

  status <- attr(output, "status")
  if (is.null(status)) status <- 0L
  if (status != 0L) {
    detail <- paste(utils::tail(as.character(output), 5L), collapse = "\n")
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