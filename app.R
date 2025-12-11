############################################################
# Mini-Shiny-App: Co-Citation & Bibliographic Coupling
############################################################

library(shiny)
library(httr)
library(jsonlite)
library(dplyr)
library(purrr)
library(tibble)
library(stringr)
library(readr)

openalex_base <- "https://api.openalex.org/works"
contact_email <- NULL

`%||%` <- function(a, b) if (!is.null(a)) a else b

clean_id <- function(x) {
  x <- trimws(x)
  # Alles, was mit "pmid", optionalem Doppelpunkt/Minus/Leerzeichen beginnt, entfernen
  x <- gsub("(?i)^pmid\\s*[:\\-]*\\s*", "", x, perl = TRUE)
  # Alles, was mit "doi", optionalem Doppelpunkt/Minus/Leerzeichen beginnt, entfernen
  x <- gsub("(?i)^doi\\s*[:\\-]*\\s*",  "", x, perl = TRUE)
  x
}


clean_pmid_value <- function(x) {
  x <- trimws(x)
  x <- gsub("^https?://pubmed\\.ncbi\\.nlm\\.nih\\.gov/", "", x)
  x <- gsub("/$", "", x)
  x
}

clean_doi_value <- function(x) {
  x <- trimws(x)
  x <- gsub("^https?://(dx\\.)?doi\\.org/", "", x)
  x
}


detect_id_type <- function(id) {
  id_clean <- clean_id(id)
  
  # Wenn explizit "pmid" irgendwo im Original steht → PMID
  if (grepl("(?i)pmid", id)) {
    return("pmid")
  }
  
  if (grepl("^[0-9]+$", id_clean)) {
    return("pmid")
  }
  if (grepl("(?i)^doi:", id)) {
    return("doi")
  }
  if (grepl("^10\\.[0-9]{4,9}/", id_clean)) {
    return("doi")
  }
  if (grepl("^10\\.", id_clean) && !grepl("/", id_clean)) {
    return("pmid")
  }
  if (grepl("/", id_clean)) {
    return("doi")
  }
  "pmid"
}


id_to_openalex_id <- function(id) {
  id_clean <- clean_id(id)
  id_type  <- detect_id_type(id)
  
  # Für PMIDs: zur Sicherheit alles außer Ziffern entfernen
  if (id_type == "pmid") {
    id_clean <- gsub("[^0-9]", "", id_clean)
  }
  
  if (id_type == "doi") {
    url <- paste0(openalex_base, "/doi:", id_clean)
  } else {
    url <- paste0(openalex_base, "/pmid:", id_clean)
  }
  
  query <- list()
  if (!is.null(contact_email) && nzchar(contact_email)) {
    query$mailto <- contact_email
  }
  
  message("Löse ID auf (", id_type, "): ", id_clean,
          "  -->  ", url)
  
  resp <- httr::GET(url, query = query)
  status <- httr::status_code(resp)
  
  if (status == 404) {
    warning("Nicht gefunden in OpenAlex (404): ", id,
            " (interpretiert als ", id_type, ")")
    return(NA_character_)
  }
  
  httr::stop_for_status(resp)
  
  txt <- httr::content(resp, as = "text", encoding = "UTF-8")
  if (!is.character(txt)) txt <- as.character(txt)
  
  res <- jsonlite::fromJSON(txt)
  Sys.sleep(0.2)
  res$id
}


get_work <- function(openalex_id) {
  id_clean <- openalex_id
  
  if (str_starts(id_clean, "http")) {
    id_clean <- sub(".*/", "", id_clean)
  }
  
  url <- paste0(openalex_base, "/", id_clean)
  
  query <- list()
  if (!is.null(contact_email) && nzchar(contact_email)) {
    query$mailto <- contact_email
  }
  
  resp <- httr::GET(url, query = query)
  status <- httr::status_code(resp)
  
  if (status == 404) {
    message("OpenAlex-Work nicht gefunden (404), wird übersprungen: ", id_clean)
    return(NULL)
  }
  
  if (status != 200) {
    txt_raw <- httr::content(resp, as = "text", encoding = "UTF-8")
    warning("OpenAlex-Antwort ist kein 200 OK (Status ", status, ").\n",
            "URL: ", url, "\n",
            "Antwort (Anfang): ", substr(txt_raw, 1, 200))
    return(NULL)
  }
  
  ctype <- httr::http_type(resp)
  if (!grepl("json", ctype, ignore.case = TRUE)) {
    txt_raw <- httr::content(resp, as = "text", encoding = "UTF-8")
    warning("OpenAlex hat keinen JSON-Content geliefert (Content-Type: ", ctype, ").\n",
            "URL: ", url, "\n",
            "Antwort (Anfang): ", substr(txt_raw, 1, 200))
    return(NULL)
  }
  
  txt <- httr::content(resp, as = "text", encoding = "UTF-8")
  if (!is.character(txt)) {
    txt <- as.character(txt)
  }
  
  res <- tryCatch(
    jsonlite::fromJSON(txt),
    error = function(e) {
      warning("JSON-Parsing-Fehler bei URL: ", url, "\n",
              "Fehler: ", conditionMessage(e), "\n",
              "Antwort (Anfang): ", substr(txt, 1, 200))
      return(NULL)
    }
  )
  
  Sys.sleep(0.2)
  res
}


get_referenced_works <- function(openalex_id) {
  w <- get_work(openalex_id)
  if (is.null(w)) return(character(0))
  refs <- w$referenced_works
  if (is.null(refs)) character(0) else unlist(refs)
}

get_citing_works <- function(openalex_id, max_works = 1000) {
  w <- get_work(openalex_id)
  if (is.null(w)) return(character(0))
  url <- w$cited_by_api_url
  if (is.null(url) || !nzchar(url)) return(character(0))
  per_page <- 200
  collected <- character(0)
  cursor <- "*"
  query_base <- list(`per-page` = per_page)
  if (!is.null(contact_email) && nzchar(contact_email)) {
    query_base$mailto <- contact_email
  }
  repeat {
    query <- c(query_base, list(cursor = cursor))
    resp <- httr::GET(url, query = query)
    status <- httr::status_code(resp)
    if (status == 404) break
    httr::stop_for_status(resp)
    txt <- httr::content(resp, as = "text", encoding = "UTF-8")
    if (!is.character(txt)) {
      txt <- as.character(txt)
    }
    data <- jsonlite::fromJSON(txt)
    Sys.sleep(0.2)
    res <- data$results
    if (length(res) == 0) break
    ids <- vapply(res, function(r) r$id, character(1))
    collected <- c(collected, ids)
    if (length(collected) >= max_works) break
    cursor <- data$meta$`next_cursor`
    if (is.null(cursor) || !nzchar(cursor)) break
  }
  unique(collected)
}

get_basic_metadata <- function(openalex_ids) {
  openalex_ids <- unique(openalex_ids)
  openalex_ids <- openalex_ids[!is.na(openalex_ids) & nzchar(openalex_ids)]
  message("Hole Metadaten für ", length(openalex_ids), " Arbeiten ...")
  map_df(openalex_ids, function(oid) {
    w <- get_work(oid)
    if (is.null(w) || length(w) == 0) {
      return(tibble(
        openalex_id      = oid,
        title            = NA_character_,
        publication_year = NA_integer_,
        doi              = NA_character_,
        pmid             = NA_character_,
        journal          = NA_character_,
        authors          = NA_character_
      ))
    }
    pmid   <- w$ids$pmid %||% NA_character_
    doi    <- w$doi %||% NA_character_
    title  <- w$title %||% NA_character_
    year   <- w$publication_year %||% NA_integer_
    journal <- w$host_venue$display_name %||% NA_character_
    tibble(
      openalex_id      = oid,
      title            = title,
      publication_year = year,
      doi              = doi,
      pmid             = pmid,
      journal          = journal
    )
  })
}

# Autorenstring: "A; B; C" -> RIS-Zeilen AU  -
authors_to_ris <- function(authors_str) {
  if (is.null(authors_str) || is.na(authors_str) || !nzchar(authors_str)) {
    return(character(0))
  }
  auths <- unlist(strsplit(authors_str, "\\s*;\\s*"))
  paste0("AU  - ", auths)
}

# sehr einfache RIS-Erzeugung aus einem Datenframe
df_to_ris <- function(df) {
  if (!nrow(df)) return("")
  
  blocks <- purrr::map(seq_len(nrow(df)), function(i) {
    x <- df[i, ]
    
    block <- c(
      "TY  - JOUR",
      if (!is.na(x$title)            && nzchar(x$title))            paste0("TI  - ", x$title)            else NULL,
      authors_to_ris(if ("authors" %in% names(x)) x$authors else NA_character_),
      if (!is.na(x$publication_year) && !is.na(x$publication_year)) paste0("PY  - ", x$publication_year) else NULL,
      if (!is.na(x$journal)          && nzchar(x$journal))          paste0("JO  - ", x$journal)          else NULL,
      if ("volume" %in% names(x) && !is.na(x$volume) && nzchar(x$volume)) paste0("VL  - ", x$volume)     else NULL,
      if ("issue"  %in% names(x) && !is.na(x$issue)  && nzchar(x$issue))  paste0("IS  - ", x$issue)      else NULL,
      if ("first_page" %in% names(x) && !is.na(x$first_page) && nzchar(x$first_page)) paste0("SP  - ", x$first_page) else NULL,
      if ("last_page"  %in% names(x) && !is.na(x$last_page)  && nzchar(x$last_page))  paste0("EP  - ", x$last_page)  else NULL,
      if (!is.na(x$doi)              && nzchar(x$doi))              paste0("DO  - ", x$doi)              else NULL,
      if (!is.na(x$pmid)             && nzchar(x$pmid))             paste0("ID  - PMID:", x$pmid)        else NULL,
      if ("openalex_id" %in% names(x)) paste0("ID  - OA:", x$openalex_id) else NULL,
      "ER  - "
    )
    
    paste(block, collapse = "\n")
  })
  
  paste(blocks, collapse = "\n\n")
}

# BibTeX-Key aus Zeile bauen
make_bibtex_key <- function(row) {
  if ("pmid" %in% names(row) && !is.na(row$pmid) && nzchar(row$pmid)) {
    key <- paste0("pmid", row$pmid)
  } else if ("doi" %in% names(row) && !is.na(row$doi) && nzchar(row$doi)) {
    key <- gsub("[^A-Za-z0-9]+", "_", row$doi)
  } else if ("openalex_id" %in% names(row) && !is.na(row$openalex_id)) {
    key <- gsub(".*/", "", row$openalex_id)
    key <- paste0("oa_", key)
  } else {
    key <- paste0("ref_", sample(1e6, 1))
  }
  key
}

# sehr einfache BibTeX-Erzeugung
df_to_bibtex <- function(df) {
  if (!nrow(df)) return("")
  
  entries <- purrr::map_chr(seq_len(nrow(df)), function(i) {
    x <- df[i, ]
    key <- make_bibtex_key(x)
    
    authors_field <- NULL
    if ("authors" %in% names(x) && !is.na(x$authors) && nzchar(x$authors)) {
      auths <- unlist(strsplit(x$authors, "\\s*;\\s*"))
      if (length(auths) > 0) {
        authors_field <- paste(auths, collapse = " and ")
      }
    }
    
    fields <- c(
      if (!is.na(x$title)            && nzchar(x$title))            paste0("  title = {", x$title, "}")           else NULL,
      if (!is.null(authors_field))                                   paste0("  author = {", authors_field, "}")   else NULL,
      if (!is.na(x$journal)          && nzchar(x$journal))          paste0("  journal = {", x$journal, "}")       else NULL,
      if (!is.na(x$publication_year) && !is.na(x$publication_year)) paste0("  year = {", x$publication_year, "}") else NULL,
      if ("volume" %in% names(x) && !is.na(x$volume) && nzchar(x$volume)) paste0("  volume = {", x$volume, "}")   else NULL,
      if ("issue"  %in% names(x) && !is.na(x$issue)  && nzchar(x$issue))  paste0("  number = {", x$issue, "}")    else NULL,
      if ("pages"  %in% names(x) && !is.na(x$pages)  && nzchar(x$pages))  paste0("  pages = {", x$pages, "}")     else NULL,
      if (!is.na(x$doi)              && nzchar(x$doi))              paste0("  doi = {", x$doi, "}")               else NULL,
      if (!is.na(x$pmid)             && nzchar(x$pmid))             paste0("  pmid = {", x$pmid, "}")             else NULL,
      if ("openalex_id" %in% names(x))                              paste0("  note = {OpenAlex ID: ", x$openalex_id, "}") else NULL
    )
    
    entry <- c(
      paste0("@article{", key, ","),
      paste(fields, collapse = ",\n"),
      "}"
    )
    
    paste(entry, collapse = "\n")
  })
  
  paste(entries, collapse = "\n\n")
}

ui <- fluidPage(
  titlePanel("Co-Citation & Bibliographic Coupling (OpenAlex)"),
  sidebarLayout(
    sidebarPanel(
      fileInput("seed_file", "Seed-Datei (eine DOI oder PMID pro Zeile)", accept = ".txt"),
      numericInput("min_shared_refs", "Min. Seed-Papers pro Referenz (BC)", value = 2, min = 1, step = 1),
      numericInput("min_shared_citers", "Min. Seed-Papers pro zitierende Arbeit (CC)", value = 2, min = 1, step = 1),
      numericInput("max_citing", "Max. zitierende Arbeiten pro Seed", value = 500, min = 50, step = 50),
      textInput("contact_email", "Kontakt-E-Mail für OpenAlex (optional)", value = ""),
      actionButton("run_btn", "Analyse starten")
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Seeds", 
                 h4("Aufgelöste Seed-Publikationen"),
                 tableOutput("seeds_table")
        ),
        tabPanel("Bibliographic Coupling", 
                 h4("Gemeinsam referenzierte Arbeiten"),
                 downloadButton("download_bc_csv",   "BC als CSV"),
                 downloadButton("download_bc_ris",   "BC als RIS"),
                 downloadButton("download_bc_bib",   "BC als BibTeX"),
                 downloadButton("download_bc_pmids", "BC-PMIDs (TXT)"),
                 downloadButton("download_bc_dois",  "BC-DOIs (TXT)"),
                 br(), br(),
                 tableOutput("bc_table")
        ),
        tabPanel("Co-Citation", 
                 h4("Arbeiten, die mehrere Seed-Papers zitieren"),
                 downloadButton("download_cc_csv",   "CC als CSV"),
                 downloadButton("download_cc_ris",   "CC als RIS"),
                 downloadButton("download_cc_bib",   "CC als BibTeX"),
                 downloadButton("download_cc_pmids", "CC-PMIDs (TXT)"),
                 downloadButton("download_cc_dois",  "CC-DOIs (TXT)"),
                 br(), br(),
                 tableOutput("cc_table")
        )
      )
    )
  )
)

server <- function(input, output, session) {
  
  results <- eventReactive(input$run_btn, {
    req(input$seed_file)
    contact_email <<- input$contact_email
    
    # IDs einlesen (ohne Pipe-Fallstrick)
    seed_ids_raw <- readr::read_lines(input$seed_file$datapath)
    seed_ids_raw <- trimws(seed_ids_raw)
    seed_ids_raw <- seed_ids_raw[nzchar(seed_ids_raw)]
    seed_ids_raw <- unique(seed_ids_raw)
    
    if (length(seed_ids_raw) == 0) {
      stop("Die Seed-Datei enthält keine IDs.")
    }
    
    # IDs bereinigen und Typ (DOI/PMID) erkennen
    seed_ids_clean <- unique(clean_id(seed_ids_raw))
    id_types <- vapply(seed_ids_clean, detect_id_type, character(1))
    
    # Auflösung in OpenAlex-IDs
    seed_oa_ids <- map_chr(seed_ids_clean, id_to_openalex_id)
    
    # Tabelle für die Seeds (inkl. evtl. NA bei nicht auflösbaren IDs)
    seeds_tbl <- tibble(
      input_id   = seed_ids_clean,
      id_type    = id_types,
      openalex_id = seed_oa_ids
    )
    
    # Nur erfolgreich aufgelöste Seeds für die weitere Analyse verwenden
    valid_seeds <- !is.na(seed_oa_ids)
    seeds_valid <- seeds_tbl[valid_seeds, ]
    
    if (nrow(seeds_valid) == 0) {
      stop("Keine der Seed-IDs konnte in OpenAlex aufgelöst werden.")
    }
    
    seed_oa_ids_valid <- seeds_valid$openalex_id
    
    # Metadaten für Seeds holen und an seeds_tbl anhängen
    meta_seeds <- get_basic_metadata(seed_oa_ids_valid)
    
    seeds_with_meta <- seeds_valid %>%
      left_join(meta_seeds, by = c("openalex_id" = "openalex_id")) %>%
      arrange(publication_year, input_id)
    
    if (length(seed_oa_ids) == 0) {
      stop("Keine der Seed-IDs konnte in OpenAlex aufgelöst werden.")
    }
    
    # --- Bibliographic Coupling ---
    refs_list <- map(seed_oa_ids_valid, get_referenced_works)
    names(refs_list) <- seed_oa_ids_valid
    
    refs_long <- imap_dfr(
      refs_list,
      ~ tibble(
        seed_openalex_id = .y,
        ref_openalex_id  = .x
      )
    )
    
    bc_df <- refs_long %>%
      distinct(seed_openalex_id, ref_openalex_id) %>%
      count(ref_openalex_id, name = "n_seed_papers_citing") %>%
      filter(n_seed_papers_citing >= input$min_shared_refs) %>%
      arrange(desc(n_seed_papers_citing))
    
    if (nrow(bc_df) > 0) {
      meta_bc <- get_basic_metadata(bc_df$ref_openalex_id)
      bc_result <- bc_df %>%
        rename(openalex_id = ref_openalex_id) %>%
        left_join(meta_bc, by = "openalex_id")
    } else {
      bc_result <- tibble(Hinweis = "Keine BC-Treffer mit der gewählten Schwelle.")
    }
    
    # --- Co-Citation ---
    citers_list <- map(seed_oa_ids_valid, ~ get_citing_works(.x, max_works = input$max_citing))
    names(citers_list) <- seed_oa_ids_valid
    
    citers_long <- imap_dfr(
      citers_list,
      ~ tibble(
        seed_openalex_id  = .y,
        citer_openalex_id = .x
      )
    )
    
    cc_df <- citers_long %>%
      distinct(seed_openalex_id, citer_openalex_id) %>%
      count(citer_openalex_id, name = "n_seed_papers_cited") %>%
      filter(n_seed_papers_cited >= input$min_shared_citers) %>%
      arrange(desc(n_seed_papers_cited))
    
    if (nrow(cc_df) > 0) {
      meta_cc <- get_basic_metadata(cc_df$citer_openalex_id)
      cc_result <- cc_df %>%
        rename(openalex_id = citer_openalex_id) %>%
        left_join(meta_cc, by = "openalex_id")
    } else {
      cc_result <- tibble(Hinweis = "Keine CC-Treffer mit der gewählten Schwelle.")
    }
    
    list(
      seeds = seeds_with_meta,
      bc    = bc_result,
      cc    = cc_result
    )
  })

  
  # --- Downloads für BC ---
  
  output$download_bc_csv <- downloadHandler(
    filename = function() {
      paste0("bc_results_", Sys.Date(), ".csv")
    },
    content = function(file) {
      res <- results()
      bc  <- res$bc
      # Falls nur Hinweis-Zeile existiert, trotzdem schreiben
      readr::write_csv(bc, file)
    }
  )
  
  output$download_bc_ris <- downloadHandler(
    filename = function() {
      paste0("bc_results_", Sys.Date(), ".ris")
    },
    content = function(file) {
      res <- results()
      bc  <- res$bc
      
      # Wenn es nur eine Hinweis-Spalte gibt, keine sinnvolle RIS-Ausgabe
      if ("Hinweis" %in% names(bc)) {
        writeLines("NO RESULTS", con = file)
      } else {
        ris_txt <- df_to_ris(bc)
        writeLines(ris_txt, con = file)
      }
    }
  )
  
  output$download_bc_bib <- downloadHandler(
    filename = function() {
      paste0("bc_results_", Sys.Date(), ".bib")
    },
    content = function(file) {
      res <- results()
      bc  <- res$bc
      
      if ("Hinweis" %in% names(bc)) {
        writeLines("% NO RESULTS", con = file)
      } else {
        bib_txt <- df_to_bibtex(bc)
        writeLines(bib_txt, con = file)
      }
    }
  )
  
  # --- Downloads für CC ---
  
  output$download_cc_csv <- downloadHandler(
    filename = function() {
      paste0("cc_results_", Sys.Date(), ".csv")
    },
    content = function(file) {
      res <- results()
      cc  <- res$cc
      readr::write_csv(cc, file)
    }
  )
  
  output$download_cc_ris <- downloadHandler(
    filename = function() {
      paste0("cc_results_", Sys.Date(), ".ris")
    },
    content = function(file) {
      res <- results()
      cc  <- res$cc
      
      if ("Hinweis" %in% names(cc)) {
        writeLines("NO RESULTS", con = file)
      } else {
        ris_txt <- df_to_ris(cc)
        writeLines(ris_txt, con = file)
      }
    }
  )
  
  output$download_cc_bib <- downloadHandler(
    filename = function() {
      paste0("cc_results_", Sys.Date(), ".bib")
    },
    content = function(file) {
      res <- results()
      cc  <- res$cc
      
      if ("Hinweis" %in% names(cc)) {
        writeLines("% NO RESULTS", con = file)
      } else {
        bib_txt <- df_to_bibtex(cc)
        writeLines(bib_txt, con = file)
      }
    }
  )
  
  output$download_bc_pmids <- downloadHandler(
    filename = function() {
      paste0("bc_pmids_", Sys.Date(), ".txt")
    },
    content = function(file) {
      res <- results()
      bc  <- res$bc
      
      # Wenn nur Hinweisspalte vorhanden ist, gibt es nichts Sinnvolles
      if ("Hinweis" %in% names(bc) || !"pmid" %in% names(bc)) {
        writeLines(character(0), con = file)
      } else {
        pmids <- bc$pmid
        pmids <- pmids[!is.na(pmids) & pmids != ""]
        if (length(pmids) > 0) {
          pmids <- clean_pmid_value(pmids)
          pmids <- unique(pmids)
        }
        writeLines(pmids, con = file)
      }
    }
  )
  
  output$download_bc_dois <- downloadHandler(
    filename = function() {
      paste0("bc_dois_", Sys.Date(), ".txt")
    },
    content = function(file) {
      res <- results()
      bc  <- res$bc
      
      if ("Hinweis" %in% names(bc) || !"doi" %in% names(bc)) {
        writeLines(character(0), con = file)
      } else {
        dois <- bc$doi
        dois <- dois[!is.na(dois) & dois != ""]
        if (length(dois) > 0) {
          dois <- clean_doi_value(dois)
          dois <- unique(dois)
        }
        writeLines(dois, con = file)
      }
    }
  )
  
  output$download_cc_pmids <- downloadHandler(
    filename = function() {
      paste0("cc_pmids_", Sys.Date(), ".txt")
    },
    content = function(file) {
      res <- results()
      cc  <- res$cc
      
      if ("Hinweis" %in% names(cc) || !"pmid" %in% names(cc)) {
        writeLines(character(0), con = file)
      } else {
        pmids <- cc$pmid
        pmids <- pmids[!is.na(pmids) & pmids != ""]
        if (length(pmids) > 0) {
          pmids <- clean_pmid_value(pmids)
          pmids <- unique(pmids)
        }
        writeLines(pmids, con = file)
      }
    }
  )
  
  
  output$download_cc_dois <- downloadHandler(
    filename = function() {
      paste0("cc_dois_", Sys.Date(), ".txt")
    },
    content = function(file) {
      res <- results()
      cc  <- res$cc
      
      if ("Hinweis" %in% names(cc) || !"doi" %in% names(cc)) {
        writeLines(character(0), con = file)
      } else {
        dois <- cc$doi
        dois <- dois[!is.na(dois) & dois != ""]
        if (length(dois) > 0) {
          dois <- clean_doi_value(dois)
          dois <- unique(dois)
        }
        writeLines(dois, con = file)
      }
    }
  )
  
    
  output$bc_table <- renderTable({
    res <- results()
    req(res$bc)
    head(res$bc, 100)
  })
  
  output$cc_table <- renderTable({
    res <- results()
    req(res$cc)
    head(res$cc, 100)
  })
  
  output$seeds_table <- renderTable({
    res <- results()
    req(res$seeds)
    # sinnvoll auf ein paar zentrale Spalten reduzieren
    head(
      res$seeds %>%
        select(
          input_id,
          id_type,
          openalex_id,
          title,
          publication_year,
          journal,
          doi,
          pmid
        ),
      100
    )
  })
}

shinyApp(ui, server)