library(RefManageR)
library(stringr)
library(badger)

# --- 1. Initialize & Extract Helpers ---
custom <- RefManageR:::MakeBibLaTeX()

# Pulling internal RefManageR functions into scope
internal_funcs <- c("collapse", "fmtPrefix", "fmtBAuthor", "fmtJournDate", 
                    "cleanupLatex", "sentenceP", "emph", "fmtNote", 
                    "addPeriod", "fmtVersion", "fmtOrganization", 
                    "fmtAddendum", "fmtEprint")

for (f in internal_funcs) assign(f, get(f, envir = custom))

# --- 2. Helper Functions ---

# Centralized Badge Generator
make_badge <- function(label, value, color, url = NULL) {
  if (is.null(value) || length(value) == 0) return(NULL)
  clean_val <- str_replace_all(value, "[\\-\\s]", "--")
  
  if (!is.null(url)) {
    badger::badge_custom(x = label, y = clean_val, color = color, url = url)
  } else {
    badger::badge_doi(clean_val, color = color)
  }
}

# Author Highlighting
# `name` may be a single string or a vector of name variants; each is bolded in turn.
# Hyphens are normalised (Unicode U+2010 -> ASCII "-") so both hyphen styles used
# across the .bib match the same variant.
custom$bold_name <- function(text, name) {
  text <- str_replace_all(text, "‐", "-")
  for (nm in name) {
    nm <- str_replace_all(nm, "‐", "-")
    text <- str_replace_all(text, fixed(nm), paste0("<span class='me-highlight'>", nm, "</span>"))
  }
  text
}

custom$shortName <- function(pers) {
  name_raw <- custom$shortNameLF(pers)
  custom$bold_name(name_raw, c("Pacheco-Riaño, L. C.", "Pacheco-Riaño, L. Camila"))
}

# Collapse long author lists into a short summary plus an expandable full list.
# The highlighted (bold) author is always kept visible in the collapsed view.
.orig_fmtBAuthor <- custom$fmtBAuthor
custom$fmtBAuthor <- function(doc) {
  aut <- doc$author
  # Defer editor-only / duplicated entries to the original formatter.
  if (isTRUE(doc$.duplicated) || !length(aut)) return(.orig_fmtBAuthor(doc))

  names_fmt <- vapply(aut, custom$shortName, character(1))
  n <- length(names_fmt)

  join_and <- function(x) {
    m <- length(x)
    if (m == 1L) return(x)
    if (m == 2L) return(paste(x[1L], "and", x[2L]))
    paste0(paste(x[-m], collapse = ", "), ", and ", x[m])
  }

  head_n    <- 3L   # authors always shown up front
  threshold <- 8L   # only collapse when there are more than this many

  if (n <= threshold) return(join_and(names_fmt))

  full_str <- join_and(names_fmt)

  # Keep the first `head_n`, the bold author(s), and the last author visible.
  hl_idx <- which(str_detect(names_fmt, "me-highlight"))
  keep <- sort(unique(c(seq_len(head_n), hl_idx, n)))

  parts <- character(0)
  prev <- 0L
  for (k in keep) {
    if (k - prev > 1L) parts <- c(parts, "&hellip;")  # ellipsis marks a gap
    parts <- c(parts, names_fmt[k])
    prev <- k
  }
  collapsed_str <- paste(parts, collapse = ", ")
  collapsed_str <- str_replace_all(collapsed_str, ", &hellip;, ", " &hellip; ")

  toggle_show <- paste0(" <a class='authors-toggle' role='button' tabindex='0'>show other authors</a>")
  toggle_hide <- " <a class='authors-toggle' role='button' tabindex='0'>show fewer</a>"

  paste0(
    "<span class='authors-wrap'>",
    "<span class='authors-collapsed'>", collapsed_str, toggle_show, "</span>",
    "<span class='authors-full' style='display:none'>", full_str, toggle_hide, "</span>",
    "</span>"
  )
}
fmtBAuthor <- custom$fmtBAuthor

# --- 3. Component Formatters ---

custom$fmtDOI <- function(s) {
  if (!length(s)) return(NULL)
  doi_stripped <- str_remove(s, ".*\\.org/")
  make_badge("DOI", doi_stripped, "green")
}

custom$fmtURL <- function(paper) {
  if (!length(paper$url)) return(NULL)
  make_badge("URL", paper$url, "orange", url = paper$url)
}

custom$fmtCRAN <- function(paper) {
  if (!length(paper$url)) return(NULL)
  pkg <- str_extract(paper$url, "(?<=pkg=)[^&]+|(?<=package=)[^&]+")
  if (is.na(pkg)) return(NULL)
  badger::badge_cran_release(pkg, color = "green")
}

custom$fmtJournal <- function(s) {
  if (length(s$journaltitle)) {
    res <- emph(cleanupLatex(s$journaltitle))
    if (length(s$journalsubtitle)) {
      res <- paste(addPeriod(res), emph(cleanupLatex(s$journalsubtitle)))
    }
    return(res)
  } else if (!is.null(s$journal)) {
    emph(cleanupLatex(s$journal))
  }
}

# --- 4. Main Entry Type Definitions ---

custom$formatArticle <- function(paper) {
  collapse(c(
    fmtPrefix(paper),
    fmtBAuthor(paper),
    fmtJournDate(paper),
    paste0(cleanupLatex(paper$title), "."),
    sentenceP(paste0(c(
      custom$fmtJournal(paper),
      paper$volume 
    ), collapse = " ")),
    custom$fmtDOI(paper$doi),
    fmtEprint(paper),
    fmtAddendum(paper$addendum)
  ))
}

custom$formatManual <- function(paper) {
  collapse(c(
    fmtPrefix(paper), 
    fmtBAuthor(paper), 
    fmtJournDate(paper),
    emph(cleanupLatex(paper$title)), 
    fmtVersion(paper$version),
    fmtOrganization(paper$organization),
    custom$fmtCRAN(paper),
    custom$fmtDOI(paper$doi)
  ))
}

custom$formatUnpublished <- function(paper) {
  collapse(c(
    fmtPrefix(paper), 
    fmtBAuthor(paper), 
    fmtJournDate(paper), 
    emph(cleanupLatex(paper$title)),
    fmtNote(paper$note),
    custom$fmtURL(paper),
    custom$fmtDOI(paper$doi)
  ))
}

# --- 5. Register Style ---
tools::bibstyle("custom", custom)
BibOptions(
  bib.style = "custom", 
  max.names = 200, 
  style = "markdown",
  hyperlink = FALSE 
)