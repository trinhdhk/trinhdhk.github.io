suppressPackageStartupMessages({
  library(rvest)
  library(xml2)
  library(httr2)
  library(yaml)
  library(stringr)
})

user_agent <- "Mozilla/5.0 (compatible; TrinhDongCrawl/1.0; +https://github.com/trinhdhk)"

fetch_html <- function(url) {
  req <- request(url) |> req_user_agent(user_agent) |> req_timeout(30)
  resp <- req_perform(req)
  resp_body_html(resp)
}

safe_text <- function(node) {
  if (length(node) == 0) return("")
  text <- tryCatch(
    html_text2(node, trim = TRUE),
    error = function(e) html_text(node, trim = TRUE)
  )
  if (length(text) == 0) return("")
  text
}

try_or_empty <- function(expr) {
  tryCatch(expr, error = function(e) "")
}

ncl_url <- "https://www.ncl.ac.uk/medical-sciences/people/profile/trinhdong.html"
scholar_url <- "https://scholar.google.com/citations?user=8VPRg4kAAAAJ&hl=en&oi=ao"
orcid_url <- "https://pub.orcid.org/v3.0/0000-0003-4281-4929/works"
orcid_employment_url <- "https://pub.orcid.org/v3.0/0000-0003-4281-4929/employments"

ncl_doc <- tryCatch(fetch_html(ncl_url), error = function(e) NULL)

ncl_name <- if (!is.null(ncl_doc)) {
  try_or_empty(html_element(ncl_doc, "h1")) |> safe_text()
} else ""

ncl_role <- if (!is.null(ncl_doc)) {
  nodes <- html_elements(ncl_doc, ".profile-title, .person-title, .profile__jobTitle, .profile__title")
  if (length(nodes) > 0) safe_text(nodes[[1]]) else ""
} else ""

ncl_email <- if (!is.null(ncl_doc)) {
  email_node <- html_element(ncl_doc, "a[href^='mailto:']")
  safe_text(email_node)
} else ""

ncl_affiliation <- "Newcastle University"

ncl_location <- if (!is.null(ncl_doc)) {
  loc_node <- html_element(ncl_doc, ".profile__location, .profile-location")
  safe_text(loc_node)
} else ""

first_non_empty <- function(values) {
  for (value in values) {
    if (!is.null(value) && nzchar(value)) {
      return(value)
    }
  }
  ""
}

collect_text <- function(doc, selectors) {
  for (selector in selectors) {
    node <- html_element(doc, selector)
    text <- safe_text(node)
    if (nzchar(text)) return(text)
  }
  ""
}

collect_list <- function(doc, selectors) {
  items <- character()
  for (selector in selectors) {
    nodes <- html_elements(doc, selector)
    if (length(nodes) > 0) {
      texts <- vapply(nodes, safe_text, character(1))
      items <- c(items, texts[nzchar(texts)])
    }
  }
  unique(items)
}

resolve_url <- function(url, base_url) {
  if (!nzchar(url)) return("")
  if (grepl("^https?://", url)) return(url)
  paste0(base_url, url)
}

collect_image <- function(doc, selectors, base_url) {
  for (selector in selectors) {
    node <- html_element(doc, selector)
    if (!is.null(node)) {
      src <- html_attr(node, "src")
      if (!is.na(src) && nzchar(src)) {
        return(resolve_url(src, base_url))
      }
    }
  }
  ""
}

ncl_bio <- if (!is.null(ncl_doc)) {
  collect_text(ncl_doc, c(
    ".profile__bio",
    ".profile__summary",
    ".profile__content .profile__intro",
    ".profile__content .profile__bio",
    "#biography",
    ".biography",
    ".profile-bio"
  ))
} else ""

ncl_interests <- if (!is.null(ncl_doc)) {
  collect_list(ncl_doc, c(
    ".profile__research-interests li",
    ".profile__research li",
    ".research-interests li",
    ".profile__interests li",
    ".research-areas li"
  ))
} else character()

ncl_photo <- "https://includes.ncl.ac.uk/cmswebservices/myimpact/2020ws/picture/picture.php?wk=newcastleuniversity&pk=trinh.dong"

sections_extra <- tryCatch(read_yaml("data/sections_extra.yml"), error = function(e) list())
default_qualifications <- list()
if (!is.null(sections_extra$sections)) {
  for (section in sections_extra$sections) {
    if (!is.null(section$id) && section$id == "qualification" && !is.null(section$items)) {
      default_qualifications <- section$items
    }
  }
}

scholar_doc <- tryCatch(fetch_html(scholar_url), error = function(e) NULL)

scholar_name <- if (!is.null(scholar_doc)) {
  safe_text(html_element(scholar_doc, "#gsc_prf_in"))
} else ""

scholar_metrics <- list(citations = "", h_index = "", i10_index = "")

if (!is.null(scholar_doc)) {
  rows <- html_elements(scholar_doc, "#gsc_rsb_st tr")
  for (row in rows) {
    label <- tolower(safe_text(html_element(row, ".gsc_rsb_f")))
    values <- html_elements(row, ".gsc_rsb_std")
    all_time <- if (length(values) >= 1) safe_text(values[[1]]) else ""

    if (label == "citations") {
      scholar_metrics$citations <- all_time
    } else if (label == "h-index") {
      scholar_metrics$h_index <- all_time
    } else if (label == "i10-index") {
      scholar_metrics$i10_index <- all_time
    }
  }
}

pub_items <- list()

if (!is.null(scholar_doc)) {
  rows <- html_elements(scholar_doc, "#gsc_a_b .gsc_a_tr")
  for (row in rows) {
    title <- safe_text(html_element(row, ".gsc_a_at"))
    authors <- safe_text(html_element(row, ".gs_gray"))
    venue_nodes <- html_elements(row, ".gs_gray")
    venue <- if (length(venue_nodes) >= 2) safe_text(venue_nodes[[2]]) else ""
    year <- safe_text(html_element(row, ".gsc_a_y span"))
    citations <- safe_text(html_element(row, ".gsc_a_c"))
    link <- html_attr(html_element(row, ".gsc_a_at"), "href")
    if (!is.na(link) && nzchar(link)) {
      link <- paste0("https://scholar.google.com", link)
    } else {
      link <- ""
    }

    pub_items[[length(pub_items) + 1]] <- list(
      title = title,
      authors = authors,
      venue = venue,
      year = year,
      citations = citations,
      url = link,
      source = "Scholar"
    )
  }
}

orcid_items <- list()

fetch_orcid_json <- function(url, raw_path) {
  tryCatch({
    req <- request(url) |> req_user_agent(user_agent) |> req_headers(Accept = "application/json")
    resp <- req_perform(req)
    raw <- list(
      url = url,
      status = resp_status(resp),
      content_type = resp_header(resp, "content-type"),
      body = resp_body_string(resp)
    )
    write_yaml(raw, raw_path)
    resp_body_json(resp, simplifyVector = TRUE)
  }, error = function(e) {
    write_yaml(list(url = url, error = as.character(e$message)), raw_path)
    NULL
  })
}

orcid_json <- fetch_orcid_json(orcid_url, "data/crawl/orcid_works_raw.yml")
orcid_employment_json <- fetch_orcid_json(orcid_employment_url, "data/crawl/orcid_employment_raw.yml")

as_group_list <- function(groups) {
  if (is.null(groups)) return(list())
  if (is.data.frame(groups)) return(split(groups, seq_len(nrow(groups))))
  if (is.list(groups)) return(groups)
  list()
}

if (!is.null(orcid_json) && !is.null(orcid_json$group)) {
  groups <- as_group_list(orcid_json$group)
  for (group in groups) {
    summaries <- group[["work-summary"]]
    if (is.null(summaries) && !is.null(group[["work-summary"]][[1]])) {
      summaries <- group[["work-summary"]][[1]]
    }
    if (is.null(summaries)) next

    if (is.data.frame(summaries)) {
      summaries <- split(summaries, seq_len(nrow(summaries)))
    }

    for (summary in summaries) {
      title <- summary$title$title$value
      year <- summary[["publication-date"]]$year$value
      journal <- summary[["journal-title"]]$value
      url <- summary[["url"]]$value

      orcid_items[[length(orcid_items) + 1]] <- list(
        title = ifelse(is.null(title), "", title),
        authors = "",
        venue = ifelse(is.null(journal), "", journal),
        year = ifelse(is.null(year), "", year),
        citations = "",
        url = ifelse(is.null(url), "", url),
        source = "orcid"
      )
    }
  }
}

normalize_title <- function(title) {
  if (is.null(title)) return("")
  key <- tolower(title)
  key <- stringr::str_replace_all(key, "[^a-z0-9]+", " ")
  key <- stringr::str_squish(key)
  key
}

merge_publications <- function(items) {
  merged <- list()
  index <- list()

  for (item in items) {
    key <- normalize_title(item$title)
    if (!nzchar(key)) next

    if (!is.null(index[[key]])) {
      idx <- index[[key]]
      existing <- merged[[idx]]

      existing$authors <- first_non_empty(c(existing$authors, item$authors))
      existing$venue <- first_non_empty(c(existing$venue, item$venue))
      existing$year <- first_non_empty(c(existing$year, item$year))
      existing$citations <- first_non_empty(c(existing$citations, item$citations))
      existing$url <- first_non_empty(c(existing$url, item$url))

      sources <- unique(c(existing$sources, item$source))
      existing$sources <- sources

      merged[[idx]] <- existing
    } else {
      merged[[length(merged) + 1]] <- list(
        title = item$title,
        authors = item$authors,
        venue = item$venue,
        year = item$year,
        citations = item$citations,
        url = item$url,
        sources = unique(c(item$source))
      )
      index[[key]] <- length(merged)
    }
  }

  merged
}

all_items <- merge_publications(c(pub_items, orcid_items))

format_orcid_date <- function(date_node) {
  if (is.null(date_node)) return("")
  if (!is.list(date_node)) {
    return(as.character(date_node))
  }
  year <- date_node$year$value
  month <- date_node$month$value
  day <- date_node$day$value
  parts <- c(year, month, day)
  parts <- parts[!is.null(parts) & !is.na(parts) & nzchar(parts)]
  if (length(parts) == 0) return("")
  paste(parts, collapse = "-")
}

work_history <- list()

dir.create("data/crawl", recursive = TRUE, showWarnings = FALSE)

extract_employment_summaries <- function(group) {
  if (!is.null(group[["employment-summary"]])) return(group[["employment-summary"]])
  if (!is.null(group[["summaries"]]) && !is.null(group[["summaries"]][["employment-summary"]])) {
    return(group[["summaries"]][["employment-summary"]])
  }
  if (!is.null(group[["summary"]])) return(group[["summary"]])
  list()
}

if (!is.null(orcid_employment_json)) {
  raw_groups <- orcid_employment_json[["affiliation-group"]]
  if (is.null(raw_groups)) {
    raw_groups <- orcid_employment_json$group
  }

  groups <- as_group_list(raw_groups)
  for (group in groups) {
    summaries <- extract_employment_summaries(group)
    if (length(summaries) == 0 && !is.null(group[["summaries"]])) {
      summaries <- group[["summaries"]]
    }
    if (is.data.frame(summaries)) {
      summaries <- split(summaries, seq_len(nrow(summaries)))
    }
    if (length(summaries) == 0) next

    for (summary in summaries) {
      payload <- summary
      if (!is.null(summary[["employment-summary"]])) {
        payload <- summary[["employment-summary"]]
      }
      role <- payload[["role-title"]]
      org <- payload$organization$name
      start_date <- format_orcid_date(payload[["start-date"]])
      end_date <- format_orcid_date(payload[["end-date"]])

      clean_value <- function(value) {
        if (is.null(value) || length(value) == 0 || is.na(value)) return("")
        value
      }

      work_history[[length(work_history) + 1]] <- list(
        role = clean_value(role),
        organization = clean_value(org),
        start_date = clean_value(start_date),
        end_date = clean_value(end_date)
      )
    }
  }
}

profile_out <- list(
  name = ifelse(nzchar(scholar_name), scholar_name, ncl_name),
  role = ncl_role,
  affiliation = ncl_affiliation,
  email = ncl_email,
  location = ncl_location,
  bio = ncl_bio,
  research_interests = ncl_interests,
  photo_url = ncl_photo,
  photo_alt = "Trinh Dong"
)

crawl_out <- list(
  generated_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
  profile = profile_out,
  metrics = scholar_metrics,
  work_history = work_history,
  qualifications = default_qualifications,
  sources = list(ncl_url, scholar_url, orcid_url, orcid_employment_url)
)

pub_out <- list(
  generated_at = crawl_out$generated_at,
  items = all_items
)

overrides <- tryCatch(read_yaml("data/overrides.yml"), error = function(e) list())

if (!is.null(overrides$profile)) {
  if (!is.null(overrides$profile$bio) && nzchar(overrides$profile$bio)) {
    crawl_out$profile$bio <- overrides$profile$bio
  }
  if (!is.null(overrides$profile$research_interests) && length(overrides$profile$research_interests) > 0) {
    crawl_out$profile$research_interests <- overrides$profile$research_interests
  }
  if (!is.null(overrides$profile$photo_url) && nzchar(overrides$profile$photo_url)) {
    crawl_out$profile$photo_url <- overrides$profile$photo_url
  }
  if (!is.null(overrides$profile$photo_alt) && nzchar(overrides$profile$photo_alt)) {
    crawl_out$profile$photo_alt <- overrides$profile$photo_alt
  }
}

if (!is.null(overrides$work_history) && length(overrides$work_history) > 0) {
  crawl_out$work_history <- overrides$work_history
}

if (!is.null(overrides$qualifications) && length(overrides$qualifications) > 0) {
  crawl_out$qualifications <- overrides$qualifications
}

write_yaml(crawl_out, "data/crawl/crawl.yml")
write_yaml(pub_out, "data/crawl/publications.yml")
