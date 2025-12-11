############################################################
# Co-Citation & Bibliographic Coupling mit OpenAlex
# Eingabe: gemischte Liste aus DOIs und PMIDs
# Ausgabe:
#   - bibliographic_coupling_results.csv
#   - co_citation_results.csv
#   - new_reference_candidates.csv
#   - new_reference_ids_mixed.txt
#   - new_reference_pmids.txt
#   - new_reference_dois.txt
#   - new_references.ris
#   - new_references.bib
#
# Voraussetzungen:
# install.packages(c("httr", "jsonlite", "dplyr", "purrr", "tibble", "stringr", "readr"))
############################################################

library(httr)
library(jsonlite)
library(dplyr)
library(purrr)
library(tibble)
library(stringr)
library(readr)

# ----------------------------------------------------------
# 1. PARAMETER: HIER ANPASSEN
# ----------------------------------------------------------

###########################################################
# Seed-IDs aus Textdatei einlesen
###########################################################

# Pfad zur Eingabedatei (eine ID pro Zeile, DOIs und/oder PMIDs))
seed_file <- "seed_ids.txt"

# IDs lesen
seed_ids_raw <- readr::read_lines(seed_file) %>%
  trimws() %>%                     # Leerzeichen entfernen
  .[nzchar(.)] %>%                 # leere Zeilen entfernen
  unique()                         # ggf. doppelte entfernen


if (length(seed_ids_raw) == 0) {
  stop("Die Datei 'seed_ids.txt' enthält keine Seed-IDs.")
}

message("Gelesene Seed-IDs: ", length(seed_ids_raw))
print(seed_ids_raw)

# Mindestanzahl Seed-Papers, die eine Referenz teilen
min_shared_refs   <- 2   # für Bibliographic Coupling
min_shared_citers <- 2   # für Co-Citation

# Maximalanzahl zitierender Arbeiten pro Seed (begrenzt Laufzeit)
max_citing_per_seed <- 500

# Optional: E-Mail für OpenAlex (höflich, hilft bei Rate Limits)
contact_email <- "mail@example.com"  # ggf. anpassen

# OpenAlex API-Basis
openalex_base <- "https://api.openalex.org/works"

# ----------------------------------------------------------
# 2. Hilfsfunktionen
# ----------------------------------------------------------

`%||%` <- function(a, b) if (!is.null(a)) a else b

clean_id <- function(x) {
  x <- trimws(x)
  x <- gsub("(?i)^pmid:\\s*", "", x, perl = TRUE)  # "PMID:" entfernen
  x <- gsub("(?i)^doi:\\s*",  "", x, perl = TRUE)  # "DOI:" entfernen
  x
}

clean_pmid_value <- function(x) {
  x <- trimws(x)
  # typische PubMed-URL entfernen
  x <- gsub("^https?://pubmed\\.ncbi\\.nlm\\.nih\\.gov/", "", x)
  x <- gsub("/$", "", x)  # trailing Slash entfernen
  x
}

clean_doi_value <- function(x) {
  x <- trimws(x)
  # doi.org-URL-Präfix entfernen
  x <- gsub("^https?://(dx\\.)?doi\\.org/", "", x)
  x
}


# Erkennung, ob es sich um DOI oder PMID handelt
detect_id_type <- function(id) {
  id_clean <- clean_id(id)
  
  # 1) Rein numerisch -> sehr wahrscheinlich PMID
  if (grepl("^[0-9]+$", id_clean)) {
    return("pmid")
  }
  
  # 2) Explizit als DOI markiert?
  if (grepl("(?i)^doi:", id)) {
    return("doi")
  }
  
  # 3) Klassischer DOI: beginnt mit 10.xxxx/...
  if (grepl("^10\\.[0-9]{4,9}/", id_clean)) {
    return("doi")
  }
  
  # 4) Beginnt mit 10., aber kein Slash -> eher keine DOI -> als PMID behandeln
  if (grepl("^10\\.", id_clean) && !grepl("/", id_clean)) {
    return("pmid")
  }
  
  # 5) Hat einen Slash -> sehr wahrscheinlich DOI
  if (grepl("/", id_clean)) {
    return("doi")
  }
  
  # Fallback: eher PMID
  return("pmid")
}

# DOI oder PMID -> OpenAlex-ID (z.B. "https://openalex.org/W46385550")
id_to_openalex_id <- function(id) {
  id_clean <- clean_id(id)
  id_type  <- detect_id_type(id)
  
  if (id_type == "doi") {
    url <- paste0(openalex_base, "/doi:", id_clean)
  } else if (id_type == "pmid") {
    url <- paste0(openalex_base, "/pmid:", id_clean)
  } else {
    warning("Konnte Typ nicht eindeutig bestimmen, versuche als DOI: ", id_clean)
    url <- paste0(openalex_base, "/doi:", id_clean)
  }
  
  query <- list()
  if (!is.null(contact_email) && nzchar(contact_email)) {
    query$mailto <- contact_email
  }
  
  message("Löse ID auf (", id_type, "): ", id_clean, "  -->  ", url)
  
  resp <- httr::GET(url, query = query)
  
  status <- httr::status_code(resp)
  if (status == 404) {
    warning("Nicht gefunden in OpenAlex (404): ", id_clean, " (interpretiert als ", id_type, ")")
    return(NA_character_)
  }
  
  httr::stop_for_status(resp)
  
  res <- httr::content(resp, as = "text", encoding = "UTF-8") |> jsonlite::fromJSON()
  Sys.sleep(0.2)  # kleine Pause für Rate-Limits
  res$id  # z.B. "https://openalex.org/W46385550"
}

# OpenAlex-Work-Objekt laden (immer als API-URL)
get_work <- function(openalex_id) {
  # openalex_id kann z.B. sein:
  # - "https://openalex.org/W46385550"
  # - "https://api.openalex.org/works/W46385550"
  # - "W46385550"
  
  id_clean <- openalex_id
  
  if (str_starts(id_clean, "http")) {
    # letzten Pfadteil extrahieren (z.B. "W46385550")
    id_clean <- sub(".*/", "", id_clean)
  }
  
  url <- paste0(openalex_base, "/", id_clean)
  
  query <- list()
  if (!is.null(contact_email) && nzchar(contact_email)) {
    query$mailto <- contact_email
  }
  
  resp <- httr::GET(url, query = query)
  status <- httr::status_code(resp)
  
  # 404 explizit als "nicht vorhanden"
  if (status == 404) {
    message("OpenAlex-Work nicht gefunden (404), wird übersprungen: ", id_clean)
    return(NULL)
  }
  
  if (status != 200) {
    txt <- httr::content(resp, as = "text", encoding = "UTF-8")
    warning("OpenAlex-Antwort ist kein 200 OK (Status ", status, ").\n",
            "URL: ", url, "\n",
            "Antwort (Anfang): ", substr(txt, 1, 200))
    return(NULL)
  }
  
  ctype <- httr::http_type(resp)
  if (!grepl("json", ctype, ignore.case = TRUE)) {
    txt <- httr::content(resp, as = "text", encoding = "UTF-8")
    warning("OpenAlex hat keinen JSON-Content geliefert (Content-Type: ", ctype, ").\n",
            "URL: ", url, "\n",
            "Antwort (Anfang): ", substr(txt, 1, 200))
    return(NULL)
  }
  
  txt <- httr::content(resp, as = "text", encoding = "UTF-8")
  
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

# Liste der referenzierten Arbeiten eines Seeds
get_referenced_works <- function(openalex_id) {
  w <- get_work(openalex_id)
  if (is.null(w)) return(character(0))
  refs <- w$referenced_works
  if (is.null(refs)) character(0) else unlist(refs)
}

# Arbeiten, die ein Seed-Paper zitieren (Co-Citation-Basis)
get_citing_works <- function(openalex_id, max_works = 1000) {
  w <- get_work(openalex_id)
  if (is.null(w)) return(character(0))
  
  url <- w$cited_by_api_url
  if (is.null(url) || !nzchar(url)) {
    return(character(0))
  }
  
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
    if (status == 404) {
      message("cited_by-List nicht gefunden (404) für: ", openalex_id)
      break
    }
    httr::stop_for_status(resp)
    
    data <- httr::content(resp, as = "text", encoding = "UTF-8") |> jsonlite::fromJSON()
    Sys.sleep(0.2)
    
    res <- data$results
    if (length(res) == 0) break
    
    ids <- vapply(res, function(r) r$id, character(1))
    collected <- c(collected, ids)
    
    if (length(collected) >= max_works) {
      message("Erreichtes Limit max_works = ", max_works, " für Seed: ", openalex_id)
      break
    }
    
    cursor <- data$meta$`next_cursor`
    if (is.null(cursor) || !nzchar(cursor)) break
  }
  
  unique(collected)
}

# Metadaten (inkl. Autoren etc.) für eine Liste von OpenAlex-IDs
get_basic_metadata <- function(openalex_ids) {
  openalex_ids <- unique(openalex_ids)
  openalex_ids <- openalex_ids[!is.na(openalex_ids) & nzchar(openalex_ids)]
  message("Hole Metadaten für ", length(openalex_ids), " Arbeiten ...")
  
  map_df(openalex_ids, function(oid) {
    w <- get_work(oid)
    
    # Wenn get_work() NULL geliefert hat (404 etc.), NA-Zeile zurückgeben
    if (is.null(w) || length(w) == 0) {
      return(tibble(
        openalex_id      = oid,
        title            = NA_character_,
        publication_year = NA_integer_,
        doi              = NA_character_,
        pmid             = NA_character_,
        journal          = NA_character_,
        volume           = NA_character_,
        issue            = NA_character_,
        first_page       = NA_character_,
        last_page        = NA_character_,
        pages            = NA_character_,
        authors          = NA_character_
      ))
    }
    
    pmid   <- w$ids$pmid %||% NA_character_
    doi    <- w$doi %||% NA_character_
    title  <- w$title %||% NA_character_
    year   <- w$publication_year %||% NA_integer_
    
    journal <- w$host_venue$display_name %||% NA_character_
    
    volume <- w$biblio$volume %||% NA_character_
    issue  <- w$biblio$issue %||% NA_character_
    fp     <- w$biblio$first_page %||% NA_character_
    lp     <- w$biblio$last_page %||% NA_character_
    
    # ---------- Autoren robust extrahieren ----------
    authors_vec <- character(0)
    auths <- w$authorships
    
    if (!is.null(auths) && length(auths) > 0) {
      
      if (is.data.frame(auths)) {
        # Fall A: data.frame mit Spalte "author.display_name"
        if ("author.display_name" %in% names(auths)) {
          authors_vec <- auths$author.display_name
        }
        # Fall B: data.frame mit List-Spalte "author" (die selbst $display_name hat)
        else if ("author" %in% names(auths) && is.list(auths$author)) {
          authors_vec <- vapply(
            auths$author,
            function(a) {
              if (is.list(a) && !is.null(a$display_name) && !is.na(a$display_name)) {
                as.character(a$display_name)
              } else {
                NA_character_
              }
            },
            character(1)
          )
        }
      } else if (is.list(auths)) {
        # Fall C: Liste von Einträgen mit $author$display_name
        authors_vec <- vapply(
          auths,
          function(a) {
            if (is.list(a) && !is.null(a$author) && is.list(a$author) &&
                !is.null(a$author$display_name) && !is.na(a$author$display_name)) {
              as.character(a$author$display_name)
            } else if (is.list(a) && !is.null(a$author_display_name)) {
              as.character(a$author_display_name)
            } else {
              NA_character_
            }
          },
          character(1)
        )
      }
    }
    
    if (length(authors_vec) > 0) {
      authors_vec <- authors_vec[!is.na(authors_vec) & authors_vec != ""]
    }
    
    authors_str <- if (length(authors_vec) > 0) {
      paste(authors_vec, collapse = "; ")
    } else {
      NA_character_
    }
    
    pages <- if (!is.null(fp) && nzchar(fp) && !is.null(lp) && nzchar(lp)) {
      paste0(fp, "-", lp)
    } else {
      fp %||% NA_character_
    }
    
    tibble(
      openalex_id      = oid,
      title            = title,
      publication_year = year,
      doi              = doi,
      pmid             = pmid,
      journal          = journal,
      volume           = volume,
      issue            = issue,
      first_page       = fp,
      last_page        = lp,
      pages            = pages,
      authors          = authors_str
    )
  })
}

# ----------------------------------------------------------
# 3. IDs auflösen und OpenAlex-Seed-Menge bauen
# ----------------------------------------------------------

seed_ids_clean <- unique(clean_id(seed_ids_raw))

seed_oa_ids <- map_chr(seed_ids_clean, id_to_openalex_id)
names(seed_oa_ids) <- seed_ids_clean

seed_oa_ids <- seed_oa_ids[!is.na(seed_oa_ids)]

if (length(seed_oa_ids) == 0) {
  stop("Keine der Seed-IDs konnte in OpenAlex aufgelöst werden.")
}

message("Erfolgreich aufgelöste Seeds: ", length(seed_oa_ids))
print(seed_oa_ids)

# ----------------------------------------------------------
# 4. Bibliographic Coupling:
#    Arbeiten, die von mehreren Seed-Papers gemeinsam zitiert werden
# ----------------------------------------------------------

message("Starte Bibliographic-Coupling-Analyse ...")

refs_list <- map(seed_oa_ids, get_referenced_works)
names(refs_list) <- seed_oa_ids

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
  filter(n_seed_papers_citing >= min_shared_refs) %>%
  arrange(desc(n_seed_papers_citing))

if (nrow(bc_df) > 0) {
  # Liste der Seeds pro Referenz
  seed_by_ref <- refs_long %>%
    distinct(seed_openalex_id, ref_openalex_id) %>%
    group_by(ref_openalex_id) %>%
    summarise(
      seed_openalex_ids = paste(unique(seed_openalex_id), collapse = ";"),
      .groups = "drop"
    )
  
  bc_df <- bc_df %>%
    left_join(seed_by_ref, by = "ref_openalex_id")
  
  # Metadaten anreichern
  meta_bc <- get_basic_metadata(bc_df$ref_openalex_id)
  
  bc_result <- bc_df %>%
    rename(openalex_id = ref_openalex_id) %>%
    left_join(meta_bc, by = "openalex_id") %>%
    arrange(desc(n_seed_papers_citing), publication_year)
  
  write_csv(bc_result, "bibliographic_coupling_results.csv")
  message("Bibliographic Coupling: ", nrow(bc_result),
          " Ergebnisse in 'bibliographic_coupling_results.csv' gespeichert.")
} else {
  message("Keine gemeinsamen Referenzen mit mindestens ", min_shared_refs, " Seeds gefunden.")
}

# ----------------------------------------------------------
# 5. Co-Citation:
#    Arbeiten, die mehrere Seed-Papers zitieren
# ----------------------------------------------------------

message("Starte Co-Citation-Analyse ...")

citers_list <- map(seed_oa_ids, ~ get_citing_works(.x, max_works = max_citing_per_seed))
names(citers_list) <- seed_oa_ids

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
  filter(n_seed_papers_cited >= min_shared_citers) %>%
  arrange(desc(n_seed_papers_cited))

if (nrow(cc_df) > 0) {
  seeds_per_citer <- citers_long %>%
    distinct(seed_openalex_id, citer_openalex_id) %>%
    group_by(citer_openalex_id) %>%
    summarise(
      seed_openalex_ids = paste(unique(seed_openalex_id), collapse = ";"),
      .groups = "drop"
    )
  
  cc_df <- cc_df %>%
    left_join(seeds_per_citer, by = "citer_openalex_id")
  
  meta_cc <- get_basic_metadata(cc_df$citer_openalex_id)
  
  cc_result <- cc_df %>%
    rename(openalex_id = citer_openalex_id) %>%
    left_join(meta_cc, by = "openalex_id") %>%
    arrange(desc(n_seed_papers_cited), publication_year)
  
  write_csv(cc_result, "co_citation_results.csv")
  message("Co-Citation: ", nrow(cc_result),
          " Ergebnisse in 'co_citation_results.csv' gespeichert.")
} else {
  message("Keine Co-Citation-Knoten mit mindestens ", min_shared_citers, " Seeds gefunden.")
}

############################################################
# 6. Dedup-Liste neuer Referenzen (ohne Seed-Papers)
############################################################

message("Erzeuge deduplizierte Kandidatenliste neuer Referenzen ...")

all_results <- list()

if (exists("bc_result") && is.data.frame(bc_result) && nrow(bc_result) > 0) {
  all_results <- append(all_results, list(bc_result %>% mutate(source = "BC")))
}

if (exists("cc_result") && is.data.frame(cc_result) && nrow(cc_result) > 0) {
  all_results <- append(all_results, list(cc_result %>% mutate(source = "CC")))
}

if (length(all_results) == 0) {
  message("Keine Ergebnisse aus BC/CC vorhanden – es wird keine Kandidatenliste erzeugt.")
} else {
  combined <- bind_rows(all_results)
  
  combined_unique <- combined %>%
    group_by(openalex_id) %>%
    summarise(
      title            = first(title),
      publication_year = first(publication_year),
      doi              = first(doi),
      pmid             = first(pmid),
      journal          = first(journal),
      volume           = first(volume),
      issue            = first(issue),
      first_page       = first(first_page),
      last_page        = first(last_page),
      pages            = first(pages),
      authors          = first(authors),
      sources          = paste(sort(unique(source)), collapse = "+"),
      .groups = "drop"
    )
  
  candidates <- combined_unique %>%
    filter(!(openalex_id %in% seed_oa_ids))
  
  candidates <- candidates %>%
    # zuerst pmid/doi „säubern“ (URLs -> nackte IDs)
    mutate(
      pmid = ifelse(!is.na(pmid) & pmid != "", clean_pmid_value(pmid), pmid),
      doi  = ifelse(!is.na(doi)  & doi  != "", clean_doi_value(doi),  doi)
    ) %>%
    # dann bevorzugten Identifier setzen
    mutate(
      id_preferred = dplyr::case_when(
        !is.na(pmid) & pmid != "" ~ pmid,
        !is.na(doi)  & doi  != "" ~ doi,
        TRUE ~ NA_character_
      )
    ) %>%
    filter(!is.na(id_preferred)) %>%
    arrange(desc(publication_year))
  
  write_csv(candidates, "new_reference_candidates.csv")
  
  # Jetzt sind pmid / doi und id_preferred bereits „clean“
  unique_ids   <- unique(candidates$id_preferred)
  unique_pmids <- unique(na.omit(candidates$pmid))
  unique_dois  <- unique(na.omit(candidates$doi))
  
  write_lines(unique_ids,   "new_reference_ids_mixed.txt")
  write_lines(unique_pmids, "new_reference_pmids.txt")
  write_lines(unique_dois,  "new_reference_dois.txt")
  
  
  message("Zusätzliche ID-Listen gespeichert als:",
          "\n - new_reference_ids_mixed.txt",
          "\n - new_reference_pmids.txt",
          "\n - new_reference_dois.txt")
  
  ############################################################
  # 7. Export der neuen Referenzen als RIS und BibTeX
  ############################################################
  
  if (nrow(candidates) > 0) {
    
    message("Erzeuge RIS- und BibTeX-Dateien für neue Referenzen ...")
    
    authors_to_ris <- function(authors_str) {
      if (is.na(authors_str) || !nzchar(authors_str)) return(character(0))
      auths <- unlist(strsplit(authors_str, "\\s*;\\s*"))
      paste0("AU  - ", auths)
    }
    
    # ---------- RIS ----------
    ris_blocks <- purrr::map(seq_len(nrow(candidates)), function(i) {
      x <- candidates[i, ]
      
      block <- c(
        "TY  - JOUR",
        if (!is.na(x$title)            && nzchar(x$title))            paste0("TI  - ", x$title)            else NULL,
        authors_to_ris(x$authors),
        if (!is.na(x$publication_year) && !is.na(x$publication_year)) paste0("PY  - ", x$publication_year) else NULL,
        if (!is.na(x$journal)          && nzchar(x$journal))          paste0("JO  - ", x$journal)          else NULL,
        if (!is.na(x$volume)           && nzchar(x$volume))           paste0("VL  - ", x$volume)           else NULL,
        if (!is.na(x$issue)            && nzchar(x$issue))            paste0("IS  - ", x$issue)            else NULL,
        if (!is.na(x$first_page)       && nzchar(x$first_page))       paste0("SP  - ", x$first_page)       else NULL,
        if (!is.na(x$last_page)        && nzchar(x$last_page))        paste0("EP  - ", x$last_page)        else NULL,
        if (!is.na(x$doi)              && nzchar(x$doi))              paste0("DO  - ", x$doi)              else NULL,
        if (!is.na(x$pmid)             && nzchar(x$pmid))             paste0("ID  - PMID:", x$pmid)        else NULL,
        paste0("ID  - OA:", x$openalex_id),
        "ER  - "
      )
      
      paste(block, collapse = "\n")
    })
    
    ris_full <- paste(ris_blocks, collapse = "\n\n")
    readr::write_file(ris_full, "new_references.ris")
    message("RIS-Datei 'new_references.ris' geschrieben.")
    
    # ---------- BibTeX ----------
    make_bibtex_key <- function(row) {
      if (!is.na(row$pmid) && nzchar(row$pmid)) {
        key <- paste0("pmid", row$pmid)
      } else if (!is.na(row$doi) && nzchar(row$doi)) {
        key <- gsub("[^A-Za-z0-9]+", "_", row$doi)
      } else {
        key <- gsub(".*/", "", row$openalex_id)
        key <- paste0("oa_", key)
      }
      key
    }
    
    bibtex_entries <- purrr::map_chr(seq_len(nrow(candidates)), function(i) {
      x <- candidates[i, ]
      key <- make_bibtex_key(x)
      
      authors_field <- NULL
      if (!is.na(x$authors) && nzchar(x$authors)) {
        auths <- unlist(strsplit(x$authors, "\\s*;\\s*"))
        if (length(auths) > 0) {
          authors_field <- paste(auths, collapse = " and ")
        }
      }
      
      fields <- c(
        if (!is.na(x$title)            && nzchar(x$title))            paste0("  title = {", x$title, "}")            else NULL,
        if (!is.null(authors_field))                                   paste0("  author = {", authors_field, "}")    else NULL,
        if (!is.na(x$journal)          && nzchar(x$journal))          paste0("  journal = {", x$journal, "}")        else NULL,
        if (!is.na(x$publication_year) && !is.na(x$publication_year)) paste0("  year = {", x$publication_year, "}")  else NULL,
        if (!is.na(x$volume)           && nzchar(x$volume))           paste0("  volume = {", x$volume, "}")          else NULL,
        if (!is.na(x$issue)            && nzchar(x$issue))            paste0("  number = {", x$issue, "}")           else NULL,
        if (!is.na(x$pages)            && nzchar(x$pages))            paste0("  pages = {", x$pages, "}")            else NULL,
        if (!is.na(x$doi)              && nzchar(x$doi))              paste0("  doi = {", x$doi, "}")                else NULL,
        if (!is.na(x$pmid)             && nzchar(x$pmid))             paste0("  pmid = {", x$pmid, "}")              else NULL,
        paste0("  note = {OpenAlex ID: ", x$openalex_id, "}")
      )
      
      entry <- c(
        paste0("@article{", key, ","),
        paste(fields, collapse = ",\n"),
        "}"
      )
      
      paste(entry, collapse = "\n")
    })
    
    bibtex_full <- paste(bibtex_entries, collapse = "\n\n")
    readr::write_file(bibtex_full, "new_references.bib")
    message("BibTeX-Datei 'new_references.bib' geschrieben.")
  } else {
    message("Keine 'candidates'-Zeilen vorhanden – kein RIS/BibTeX-Export.")
  }
}

message("Fertig.")