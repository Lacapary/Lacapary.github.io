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
custom$bold_name <- function(text, name) {
  str_replace(text, fixed(name), paste0("<span class='me-highlight'>", name, "</span>"))
}

custom$shortName <- function(pers) {
  name_raw <- custom$shortNameLF(pers)
  custom$bold_name(name_raw, "Pacheco‐Riaño, L. C.")
}

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