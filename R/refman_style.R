#Initialization and Function Definition
custom <- RefManageR:::MakeBibLaTeX()
# Custom Formatting for Articles ----
with(
  custom,
  custom$formatArticle <- function(paper) {
    collapse(c(
      fmtPrefix(paper),
      fmtBAuthor(paper),
      fmtJournDate(paper),
      fmtJTitle(paper$title),
      sentenceP(paste0(c(
        paste0(c(
          fmtJournal(paper),
          fmtSeries(paper$series)
        ),
        collapse = ""
        ),
        fmtVolume(paper$volume, paper$number)
      ), collapse = " "),
      fmtBTitle(paper$issuetitle, paper$issuesubtitle),
      fmtEditor(paper, suffix = NULL, prefix = ". "),
      fmtNote(paper$note, prefix = ". ", suffix = NULL),
      pgs = fmtPages(paper$pages, "none"),
      sep = ""
      ),
      fmtDOI(paper$doi),
      fmtEprint(paper),
      fmtAddendum(paper$addendum),
      fmtPubstate(paper$pubstate)
    ))
  }
)

## manual ----
with(custom, 
     custom$formatManual <- function (paper) {
       collapse(c(fmtPrefix(paper), fmtBAuthor(paper), fmtJournDate(paper),
                  fmtBTitle(paper$title,  paper$subtitle), fmtAddOn(paper$titleaddon), fmtLanguage(paper$language), 
                  fmtEditor(paper, !length(paper$author)), fmtEdition(paper$edition), 
                  sentence(cleanupLatex(paper$series), paper$number, sep = " "), 
                  addPeriod(fmtType(paper$type)), fmtVersion(paper$version), 
                  fmtNote(paper$note), fmtOrganization(paper$organization), 
                  sentenceP(fmtPublisher(paper$publisher, paper$location, 
                                         paper$address), 
                            fmtChapter(paper$chapter), pgs = fmtPages(paper$pages, 
                                                                      paper$bookpagination), tp = fmtTotalPages(paper$pagetotal, 
                                                                                                                paper$bookpagination), sep = ""), fmtISBN(paper$isbn), 
                  fmtDOI(paper$doi), fmtEprint(paper), fmtCRAN(paper), fmtAddendum(paper$addendum), 
                  fmtPubstate(paper$pubstate)))
     }
)

## unpublished ----
with(custom, 
     custom$formatUnpublished <- function (paper) {
       collapse(c(fmtPrefix(paper), fmtBAuthor(paper), fmtJournDate(paper), fmtIBTitle(paper$title, 
                                                                                       paper$subtitle, FALSE), fmtAddOn(paper$titleaddon), fmtLanguage(paper$language), 
                  fmtHowPublished(paper$howpublished), fmtNote(paper$note), 
                  sentence(fmtPublisher(NULL, paper$location, paper$address), sep = ""), fmtDOI(paper$doi), 
                  fmtURL(paper), fmtAddendum(paper$addendum), fmtPubstate(paper$pubstate)))
     }
)

# Helper Functions----
with(custom,
     custom$addPeriod <- function (string) {
       if (!.is_not_nonempty_text(string) && 
           grepl("([^.?!])$", string, useBytes = FALSE) &&
           !grepl(".\\*{1,2}$", string, useBytes = FALSE ))
         paste0(string, ".")
       else
         string
     })

## fragments----

custom$bold_name <- function(text, name) {
  text <- stringr::str_replace(string = text, pattern = name, replacement = paste0("**", name, "**"))
  text
}
with(
  custom,
  custom$shortName <- function(pers){
    custom$bold_name(custom$shortNameLF(pers), "Pacheco‐Riaño, L. C.")
  }
)
with(
  custom,
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
)
with(
  custom,
  custom$fmtVolume <- function(vol, num) {
    if (length(vol)) {
      res <- vol
      res
    }
  }
)
with(
  custom,
  custom$fmtDOI <- function (s) {
    if (length(s)) {
      s <- collapse(s)
      s <- gsub(".*\\.org/", "", s)
      badger::badge_doi(gsub("-", "--", s), color = "green")
    }
  }
)
with(
  custom,
  custom$fmtJTitle <- function (title) {
    if (!is.null(title)) 
      if (grepl("[.?!]$", title, useBytes = FALSE)) 
        collapse(cleanupLatex(title))
    else paste0(collapse(cleanupLatex(title)), ".")
  }
)

with(custom, 
     custom$fmtCRAN <- function (paper) {
       
       if (length(paper[["url"]])) {
         
         res <- paper$url
         package <- stringr::str_extract(res, "=.*$")
         package <- stringr::str_remove(package, "=")
         
         res <- switch(docstyle, html = paste0("URL: \\url{", 
                                               res, "}"), 
                       markdown = badger::badge_cran_release(package, color = "green"),
                       badger::badge_cran_release(package, color = "green")
         )
         res
         
       }
     }
)
with(custom,
     custom$fmtURL <- function (paper) {
       if (length(paper[["url"]])) {
         res <- paper$url
         res <- switch(
           docstyle,
           html = paste0("URL: \\url{", res, "}"),
           markdown = paste0("URL: [", res, "](", res, ")"),
           badger::badge_custom(x = "URL", y = stringr::str_replace(res, "-", "--"), color = "orange", url = res)
         )
         res
       }
       
     }
)
with(custom, 
     custom$fmtPublisher <- function (pub, loc, addr) {
       if (length(loc) || length(addr)) {
         res <- if (length(loc)) 
           plainclean(loc)
         else plainclean(addr)
         if (length(pub)) 
           res <- paste(res, plainclean(pub), sep = ": ")
         res
       }
       else if (length(pub)) {
         plainclean(pub)
       }
     }
)


tools::bibstyle("custom", custom)
BibOptions(bib.style = "custom", max.names=200)

