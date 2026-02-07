# Trinh Dong - Quarto Website

This repository contains a Quarto-based personal website powered by R/knitr. The site renders from `.qmd` files and loads live data at runtime from JSON that is periodically crawled via GitHub Actions.

## Structure

- `index.qmd` / `publications.qmd` / `projects.qmd`: Website pages
- `data/profile.yml`: Editable metadata (summary, keywords, etc.)
- `data/sections.yml`: External links and live data endpoints
- `data/crawl/*.yml`: Crawled data stored in-repo
- `scripts/crawl.R`: Crawler script
- `.github/workflows/crawl.yml`: Scheduled crawler

## Local Preview

1. Install Quarto and R.
2. Install R packages:

```r
install.packages(c("yaml", "jsonlite", "rvest", "xml2", "httr2", "stringr"))
```

3. Render:

```bash
quarto render
```
