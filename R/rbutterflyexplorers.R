# ----------------------------------------------------------------
# Internal: djb2 hash (matches Butterfly Explorers JS)
# ----------------------------------------------------------------
.djb2 <- function(url) {
  if (is.na(url) || url == "") return("00000000")
  chars <- utf8ToInt(url)
  hash <- 5381
  for (ch in chars) {
    hash <- ((hash * 33) + ch) %% (2^32)
  }
  n <- floor(hash)
  if (is.na(n) || n == 0) return("00000000")
  digits <- c(0:9, letters[1:26])
  result <- character(0)
  while (n > 0) {
    result <- c(digits[(n %% 36) + 1], result)
    n <- floor(n %/% 36)
  }
  id <- paste(result, collapse = "")
  id <- formatC(id, width = 8, flag = "-")
  toupper(substring(stringr::str_pad(id, 8, pad = "0"), 1, 8))
}

# ----------------------------------------------------------------
# Internal: family lookup by genus
# ----------------------------------------------------------------
.family <- function(genus) {
  fams <- list(
    Papilionidae = c("Papilio","Battus","Eurytides","Heraclides","Pterourus","Graphium","Parnassius"),
    Pieridae     = c("Pieris","Colias","Anteos","Aphrissa","Phoebis","Eurema","Pyrisitia","Nathalis","Pontia","Anthocharis","Euchloe"),
    Nymphalidae  = c("Danaus","Heliconius","Agraulis","Dryas","Euptoieta","Vanessa","Junonia","Limenitis","Adelpha","Marpesia","Siproeta","Anartia","Hypolimnas","Morpho","Caligo","Taygetis","Boloria","Euphydryas","Melitaea","Chlosyne","Phyciodes","Nymphalis","Polygonia","Aglais","Hamadryas","Memphis"),
    Lycaenidae   = c("Lycaena","Atlides","Callophrys","Satyrium","Strymon","Ministrymon","Calycopis","Rekoa","Parrhasius","Celastrina","Hemiargus","Leptotes","Cupido","Glaucopsyche","Plebejus","Icaricia","Lycaeides","Rapala"),
    Hesperiidae  = c("Epargyreus","Polygonus","Chioides","Urbanus","Astraptes","Autochton","Achalarus","Thorybes","Erynnis","Pyrgus","Heliopetes","Hesperia","Polites","Wallengrenia","Pompeius","Atalopedes","Atrytone","Poanes","Euphyes","Atrytonopsis","Amblyscirtes","Lerodea","Calpodes","Panoquina","Agathymus","Megathymus","Stallingsia","Perichares","Celaenorrhinus","Cogia","Staphylus","Aegiale"),
    Riodinidae   = c("Calephelis","Apodemia","Emesis","Lasaia","Amarynthis","Mesosemia","Eurybia","Nymphidium","Theope")
  )
  for (f in names(fams)) {
    if (genus %in% fams[[f]]) return(f)
  }
  NA_character_
}

# ----------------------------------------------------------------
# Internal: parse location string
# ----------------------------------------------------------------
.parse_loc <- function(loc) {
  if (is.na(loc) || nchar(trimws(loc)) < 2)
    return(list(country = NA_character_, state = NA_character_))

  s <- tolower(loc)
  s <- gsub("\u00f3","o", s); s <- gsub("\u00e9","e", s)
  s <- gsub("\u00ed","i", s); s <- gsub("\u00e1","a", s)
  s <- gsub("bagamati","bagmati", s)
  s <- gsub("province no\\.?\\s*1","koshi", s)
  s <- gsub("province no\\.?\\s*2","madhesh", s)
  s <- gsub("province no\\.?\\s*3","bagmati", s)
  s <- gsub("province no\\.?\\s*4","gandaki", s)
  s <- gsub("province no\\.?\\s*5","lumbini", s)
  s <- gsub("province no\\.?\\s*6","karnali", s)
  s <- gsub("province no\\.?\\s*7","sudurpashchim", s)

  countries <- c("costa rica","united states","ecuador","panama","nepal")
  country <- NA_character_
  for (c in countries) {
    if (grepl(paste0("\\b",c,"\\b"), s)) { country <- tools::toTitleCase(c); break }
  }

  us_states <- c("alabama","alaska","arizona","arkansas","california","colorado","connecticut",
    "delaware","florida","georgia","hawaii","idaho","illinois","indiana","iowa","kansas",
    "kentucky","louisiana","maine","maryland","massachusetts","michigan","minnesota",
    "mississippi","missouri","montana","nebraska","nevada","new hampshire","new jersey",
    "new mexico","new york","north carolina","north dakota","ohio","oklahoma","oregon",
    "pennsylvania","rhode island","south carolina","south dakota","tennessee","texas",
    "utah","vermont","virginia","washington","west virginia","wisconsin","wyoming","puerto rico")

  if (is.na(country)) {
    for (st in us_states) {
      if (grepl(paste0("\\b",st,"\\b"), s)) { country <- "United States"; break }
    }
  }

  state <- NA_character_
  if (!is.na(country)) {
    if (country == "United States") {
      for (st in us_states) {
        if (grepl(paste0("\\b",st,"\\b"), s)) { state <- tools::toTitleCase(st); break }
      }
    } else if (country == "Nepal") {
      for (p in c("koshi","madhesh","bagmati","gandaki","lumbini","karnali","sudurpashchim")) {
        if (grepl(paste0("\\b",p,"\\b"), s)) { state <- paste0(tools::toTitleCase(p)," Province"); break }
      }
    } else if (country == "Costa Rica") {
      for (p in c("san jose","alajuela","cartago","heredia","guanacaste","puntarenas","limon")) {
        if (grepl(paste0("\\b",p,"\\b"), s)) { state <- tools::toTitleCase(p); break }
      }
    } else if (country == "Panama") {
      for (p in c("bocas del toro","cocle","colon","chiriqui","darien","herrera","los santos","veraguas","guna yala")) {
        if (grepl(paste0("\\b",p,"\\b"), s)) { state <- tools::toTitleCase(p); break }
      }
    } else if (country == "Ecuador") {
      for (p in c("azuay","bolivar","canar","carchi","chimborazo","cotopaxi","el oro","esmeraldas",
                  "galapagos","guayas","imbabura","loja","los rios","manabi","napo","orellana",
                  "pastaza","pichincha","sucumbios","tungurahua")) {
        if (grepl(paste0("\\b",p,"\\b"), s)) { state <- tools::toTitleCase(p); break }
      }
    }
  }
  list(country = country, state = state)
}

# ----------------------------------------------------------------
# Internal: fetch and parse gallery
# ----------------------------------------------------------------
.fetch <- function(verbose = TRUE) {
  url <- "https://www.butterflyexplorers.com/p/new-butterflies.html"
  if (verbose) cli::cli_progress_step("Fetching Butterfly Explorers gallery...")

  resp <- tryCatch(
    httr2::request(url) |>
      httr2::req_headers("User-Agent" = "rbutterflyexplorers R package") |>
      httr2::req_perform(),
    error = function(e) cli::cli_abort("Failed to fetch: {conditionMessage(e)}")
  )

  if (verbose) cli::cli_progress_step("Parsing observations...")
  html  <- rvest::read_html(httr2::resp_body_string(resp))
  links <- html |> rvest::html_elements("a[data-lightbox]")

  hrefs  <- rvest::html_attr(links, "href")
  titles <- rvest::html_attr(links, "data-title")
  imgs   <- links |> rvest::html_elements("img")
  thumbs <- rvest::html_attr(imgs, "src")
  alts   <- rvest::html_attr(imgs, "alt")

  ok    <- !is.na(hrefs) & !is.na(thumbs)
  hrefs  <- hrefs[ok];  titles <- titles[ok]
  thumbs <- thumbs[ok]; alts   <- alts[ok]
  keep   <- !duplicated(hrefs)
  hrefs  <- hrefs[keep];  titles <- titles[keep]
  thumbs <- thumbs[keep]; alts   <- alts[keep]

  # Species
  sp_m  <- stringr::str_match(titles, "<i>([^<]+)</i>")
  sname <- stringr::str_trim(sp_m[,2])
  after <- stringr::str_replace(titles, ".*</i>", "")
  cn_m  <- stringr::str_match(after, "^\\s*[-\u2013\u2014]\\s*([^<]+?)(?:</|<br|$)")
  cname <- stringr::str_trim(cn_m[,2])
  cname <- stringr::str_remove(cname, "\\s+(?:Wildlife|Park|Reserve|County|State|National|Forest)\\b.*$")
  cname <- stringr::str_remove(cname, "\\s+\\d{4}/.*$")

  na_sp <- is.na(sname) & !is.na(alts)
  if (any(na_sp)) {
    am <- stringr::str_match(alts[na_sp], "^(.*?)\\s*[-\u2013]\\s*(.*?)$")
    sname[na_sp] <- stringr::str_trim(am[,2])
    cname[na_sp] <- stringr::str_trim(am[,3])
  }
  sname[is.na(sname) | nchar(sname) < 2] <- "Unknown"
  cname[is.na(cname) | nchar(cname) < 2] <- "Unknown"

  # Dates
  dm <- stringr::str_match(titles, "(\\d{4})/(\\d{2})/(\\d{2})")
  dates <- as.Date(ifelse(!is.na(dm[,1]),
    paste0(dm[,2],"-",dm[,3],"-",dm[,4]), NA_character_))

  # Coords
  p2    <- stringr::str_split_fixed(titles, "<br/>", 3)[,2]
  cm    <- stringr::str_match(p2, "\\(([^,]+),([^,)]+)(?:,([^)]+))?\\)")
  lats  <- suppressWarnings(as.numeric(stringr::str_trim(cm[,2])))
  lons  <- suppressWarnings(as.numeric(stringr::str_trim(cm[,3])))
  elevs <- stringr::str_trim(cm[,4])

  # Locality
  locs <- stringr::str_remove(p2, "\\([^)]*\\).*$")
  locs <- stringr::str_remove(locs, "\u00a9.*$")
  locs <- stringr::str_trim(locs)
  locs[nchar(locs) < 3] <- NA_character_

  # Family
  genus  <- stringr::str_extract(sname, "^\\S+")
  family <- vapply(genus, .family, character(1))

  # IDs
  ids <- vapply(hrefs, .djb2, character(1))

  # Locations
  if (verbose) cli::cli_alert_info("Parsing locations for {length(locs)} records...")
  lp       <- lapply(locs, .parse_loc)
  countries <- vapply(lp, `[[`, character(1), "country")
  states    <- vapply(lp, `[[`, character(1), "state")

  if (verbose) cli::cli_alert_success("Loaded {length(ids)} observations.")

  tibble::tibble(
    occurrenceID      = paste0("BE:ButterflyExplorers:", ids),
    obs_id            = ids,
    scientificName    = sname,
    vernacularName    = cname,
    family            = family,
    eventDate         = dates,
    country           = countries,
    stateProvince     = states,
    locality          = locs,
    decimalLatitude   = lats,
    decimalLongitude  = lons,
    verbatimElevation = elevs,
    associatedMedia   = hrefs,
    thumbnailURL      = thumbs,
    recordedBy        = "Sajan K.C. and Anisha Sapkota",
    basisOfRecord     = "HumanObservation",
    institutionCode   = "BE",
    collectionCode    = "ButterflyExplorers",
    datasetName       = "Butterfly Explorers v3.3",
    license           = "CC BY-NC 4.0",
    references        = paste0("https://www.butterflyexplorers.com/p/recently-added.html?obs=", ids)
  )
}

# ----------------------------------------------------------------
#' Get butterfly observations
#'
#' Fetches observations from Butterfly Explorers with optional filters.
#'
#' @param species Partial species name filter (case-insensitive)
#' @param country Country name filter
#' @param family Family name filter
#' @param state_province State or province filter
#' @param date_from Start date (YYYY-MM-DD)
#' @param date_to End date (YYYY-MM-DD)
#' @param as_sf Return as sf spatial object?
#' @param verbose Print progress?
#' @return A tibble or sf object
#' @export
be_observations <- function(species = NULL, country = NULL, family = NULL,
                             state_province = NULL, date_from = NULL,
                             date_to = NULL, as_sf = FALSE, verbose = TRUE) {
  obs <- .fetch(verbose)

  if (!is.null(species))
    obs <- dplyr::filter(obs, stringr::str_detect(scientificName, stringr::regex(species, ignore_case = TRUE)) |
                              stringr::str_detect(vernacularName,  stringr::regex(species, ignore_case = TRUE)))
  if (!is.null(country))
    obs <- dplyr::filter(obs, !is.na(country), stringr::str_detect(country, stringr::regex(country, ignore_case = TRUE)))
  if (!is.null(family))
    obs <- dplyr::filter(obs, !is.na(family),  stringr::str_detect(family,  stringr::regex(family,  ignore_case = TRUE)))
  if (!is.null(state_province))
    obs <- dplyr::filter(obs, !is.na(stateProvince), stringr::str_detect(stateProvince, stringr::regex(state_province, ignore_case = TRUE)))
  if (!is.null(date_from))
    obs <- dplyr::filter(obs, !is.na(eventDate), eventDate >= as.Date(date_from))
  if (!is.null(date_to))
    obs <- dplyr::filter(obs, !is.na(eventDate), eventDate <= as.Date(date_to))

  if (verbose) cli::cli_alert_success("Returned {nrow(obs)} observation{?s} ({dplyr::n_distinct(obs$scientificName)} species).")

  if (as_sf) {
    obs <- dplyr::filter(obs, !is.na(decimalLatitude), !is.na(decimalLongitude))
    return(sf::st_as_sf(obs, coords = c("decimalLongitude","decimalLatitude"), crs = 4326, remove = FALSE))
  }
  obs
}

# ----------------------------------------------------------------
#' Get species list
#'
#' Returns a summary of all species with observation counts.
#'
#' @param verbose Print progress?
#' @return A tibble
#' @export
be_species <- function(verbose = TRUE) {
  obs <- .fetch(verbose)
  obs |>
    dplyr::group_by(scientificName, vernacularName, family) |>
    dplyr::summarise(
      n = dplyr::n(),
      first = min(eventDate, na.rm = TRUE),
      last  = max(eventDate, na.rm = TRUE),
      n_countries = dplyr::n_distinct(country, na.rm = TRUE),
      .groups = "drop"
    ) |>
    dplyr::arrange(scientificName)
}

# ----------------------------------------------------------------
#' Get observation by ID
#'
#' @param id Observation ID hash (e.g. "A3BX9K2M")
#' @param verbose Print progress?
#' @return A single-row tibble or NULL
#' @export
be_observation_id <- function(id, verbose = TRUE) {
  id  <- stringr::str_remove(id, "^BE:ButterflyExplorers:")
  obs <- .fetch(verbose)
  out <- dplyr::filter(obs, obs_id == id)
  if (nrow(out) == 0) { cli::cli_warn("ID {id} not found."); return(NULL) }
  out
}
