# ─────────────────────────────────────────────────────────────────────
# fix-lessons.R
#
# Repairs the ONE thing that turns a deck into a giant scrolling HTML
# page instead of live slides: a `format:` block in the document's YAML
# header, which overrides live-revealjs from _quarto.yml.
#
# This script does NOT touch your code chunks. {r} stays {r}, {webr}
# stays {webr}. Mixing both in a deck is valid and intentional:
#   {webr}  -> interactive editor with a Run button
#   {r}     -> static code display (with eval: false in _quarto.yml)
#
# Usage:
#   source("fix-lessons.R")
#   scan_lessons()     # read-only report
#   fix_lessons()      # repair headers, with backup
# ─────────────────────────────────────────────────────────────────────

INCLUDE_LINE <- "{{< include ./_extensions/r-wasm/live/_knitr.qmd >}}"


# ── SCAN (read-only) ─────────────────────────────────────────────────
scan_lessons <- function() {
  files <- sort(list.files(pattern = "^Day.*\\.qmd$"))
  if (length(files) == 0) {
    cat("No Day*.qmd found. Working directory is:\n  ", getwd(), "\n")
    return(invisible(NULL))
  }

  res <- data.frame(file = files, format_blk = NA, include = NA,
                    webr = NA, r = NA, stringsAsFactors = FALSE)

  for (i in seq_along(files)) {
    x <- readLines(files[i], warn = FALSE)
    fences <- which(trimws(x) == "---")
    hdr <- if (length(fences) >= 2) x[fences[1]:fences[2]] else x[1:min(30, length(x))]

    res$format_blk[i] <- any(grepl("^format:", hdr))
    res$include[i]    <- any(grepl("_knitr\\.qmd", x))
    res$webr[i]       <- sum(grepl("^```\\{webr", x))
    res$r[i]          <- sum(grepl("^```\\{r[},]", x))
  }

  # A deck is broken only if it has a format block, or is missing the
  # include line while containing webr chunks.
  res$status <- ifelse(
    res$format_blk | (res$webr > 0 & !res$include),
    "NEEDS FIX", "OK"
  )

  print(res, row.names = FALSE)
  cat("\n")
  n <- sum(res$status == "NEEDS FIX")
  if (n > 0) cat(n, "file(s) need fixing. Run: fix_lessons()\n")
  else       cat("All headers look correct.\n")
  cat("\nNote: the r and webr columns are shown for information only.\n")
  cat("Neither this scan nor fix_lessons() will change your chunks.\n")
  invisible(res)
}


# ── FIX (headers only) ───────────────────────────────────────────────
fix_lessons <- function(files = NULL, backup = TRUE) {

  if (is.null(files)) files <- sort(list.files(pattern = "^Day.*\\.qmd$"))
  if (length(files) == 0) { cat("No files to process.\n"); return(invisible(NULL)) }

  if (backup) {
    bdir <- paste0("backup_qmd_", format(Sys.time(), "%Y%m%d_%H%M%S"))
    dir.create(bdir, showWarnings = FALSE)
    file.copy(files, file.path(bdir, basename(files)))
    cat("Backed up", length(files), "file(s) to", bdir, "\n\n")
  }

  for (f in files) {
    x <- readLines(f, warn = FALSE)
    changed <- character(0)

    fences <- which(trimws(x) == "---")
    if (length(fences) < 2) { cat("SKIP ", f, "- no YAML header\n"); next }

    hs <- fences[1]; he <- fences[2]
    hdr  <- x[(hs + 1):(he - 1)]
    body <- if (he < length(x)) x[(he + 1):length(x)] else character(0)

    # Remove `format:` and every indented line beneath it.
    keep <- rep(TRUE, length(hdr))
    skipping <- FALSE
    for (i in seq_along(hdr)) {
      if (grepl("^format:", hdr[i])) { skipping <- TRUE; keep[i] <- FALSE; next }
      if (skipping) {
        if (grepl("^[ \t]", hdr[i]) || trimws(hdr[i]) == "") { keep[i] <- FALSE; next }
        skipping <- FALSE
      }
    }
    if (any(!keep)) {
      hdr <- hdr[keep]
      changed <- c(changed, "removed format block")
    }

    # Add the include line only if it is genuinely absent.
    if (!any(grepl("_knitr\\.qmd", body))) {
      body <- c("", INCLUDE_LINE, body)
      changed <- c(changed, "added include line")
    }

    if (length(changed) > 0) {
      writeLines(c("---", hdr, "---", body), f)
      cat("FIXED", f, "-", paste(changed, collapse = "; "), "\n")
    } else {
      cat("ok   ", f, "\n")
    }
  }

  cat("\nChunks were not modified. Re-scan with scan_lessons(),",
      "then render.\n")
  invisible(NULL)
}


# ─────────────────────────────────────────────────────────────────────
# To fix only specific files, pass them explicitly:
#
#   fix_lessons(files = c("Day02_....qmd", "Day04_....qmd"))
#
# AFTER FIXING
#   1. scan_lessons()
#   2. quarto render          (Terminal)
#   3. check one deck locally
#   4. git add -A; git commit -m "Fix deck headers"; git push
#
# STILL MANUAL (needs your judgment)
#   - #| autorun: true   on chunks that define objects used later
#   - #| fig-height: 3.6 on plots that overflow the slide
#   - {.smaller} / {.scrollable} on dense slides
# ─────────────────────────────────────────────────────────────────────
