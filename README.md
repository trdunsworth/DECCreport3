# DECC Weekly Reporting Site

This repository contains a Quarto website for weekly DECC reports, with reproducible posts built from per-week data snapshots.

## Quick Start

- Install Quarto and R packages used in your analyses.
- Render the site:

```pwsh
quarto render . --no-cache
```

## Structure

- `index.qmd`: Landing page.
- `reports.qmd`: List of weekly reports.
- `about.qmd`: Project overview.
- `posts/YYYY-MM-DD/index.qmd`: A weekly report post.
  - `posts/YYYY-MM-DD/data/`: Immutable snapshot files for that week (e.g., `cad.csv`, `phone.csv`, `notes.md`).
- `templates/post.qmd`: Parameterized post template used to create new weeks.
- `R/`: Reusable modules (`utils.R`, `loaders.R`, `metrics.R`, `plots.R`, `narratives.R`).
- `scripts/new_week.ps1`: Scaffolds a new post and copies snapshots from `data-archive/YYYY/WW` when available.
- `data-archive/YYYY/WW/`: Long-term storage of weekly snapshots by year and ISO week number.

## Weekly Workflow

1. Place weekly snapshots in `data-archive/YYYY/WW/` (e.g., `cad.csv`, `phone.csv`, `notes.md`).
2. Scaffold a new post (example for report date 2025-12-02):

```pwsh
pwsh -File scripts/new_week.ps1 -ReportDate "2025-12-02"
```

This creates `posts/2025-12-02/` and copies snapshots into `posts/2025-12-02/data/` when present.

3. Edit analyst notes in `posts/DATE/data/notes.md` and render the post:

```pwsh
quarto render posts/2025-12-02/index.qmd --no-cache
```

## Render Scope and Legacy Files

The site render is restricted to core pages and `posts/**` to avoid building legacy QMDs that depend on older datasets. Keep exploratory or archived analyses outside the site scope or move them into an `archive/` folder.

## Reproducibility Notes

- Posts load only local snapshot files in their `data/` folder.
- Parsing is hardened (e.g., priorities via `readr::parse_number`) to avoid NA coercion and ordering issues.
- Visuals use standardized sizes for consistent layout across posts.

## Troubleshooting

- If a post cannot source modules, verify relative paths (from `posts/DATE/index.qmd` to `../../R/*.R`).
- If site render tries to build unrelated QMDs, confirm `_quarto.yml` render list includes only `index.qmd`, `reports.qmd`, `about.qmd`, and `posts/**`.
# DECC Weekly Reports Site

This repository hosts a Quarto website for weekly operational reports.

## Structure
- `_quarto.yml`: Site configuration; renders only `index.qmd`, `reports.qmd`, `about.qmd`, and `posts/**`.
- `posts/YYYY-MM-DD/`: One folder per weekly report with `index.qmd` and local `data/` snapshots.
- `data-archive/YYYY/WW/`: Immutable master snapshots per week; copied into posts during scaffolding.
- `R/`: Reusable modules (`utils.R`, `loaders.R`, `metrics.R`, `plots.R`, `narratives.R`).
- `templates/post.qmd`: Parameterized weekly post template.
- `scripts/new_week.ps1`: Scaffolds a new weekly post.

## Create a New Weekly Post
```powershell
pwsh -File scripts/new_week.ps1 -ReportDate "2025-12-02"
# Add CAD/phone CSVs to posts/YYYY-MM-DD/data/
quarto render .
```

## Notes
- Legacy analysis files are excluded from the site build. Migrate functionality into `R/` modules and the post template.
