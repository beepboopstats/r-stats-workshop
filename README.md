# r-stats-workshop

A four-part workshop series teaching programming in R and statistics using
longitudinal study data, for research staff and consortium members in the ABCD
study.

**Site: <https://www.beepboopstats.com/r-stats-workshop/>**

| Module | Date | Topic |
|:--|:--|:--|
| 0 | self-paced | Getting set up |
| 1 | Sep 10, 2026 | Foundations of R programming |
| 2 | Sep 24, 2026 | Data exploration, visualization & descriptive statistics |
| 3 | Oct 29, 2026 | Inferential statistics |
| 4 | Nov 12, 2026 | Applied projects |

Materials teach tidyverse style R with the native `|>` pipe, using synthetic
ABCD-shaped longitudinal datasets that contain no real participant data. Module 1
uses a small CSV with plain English column names; Modules 2-4 use a fuller dataset
with real ABCD variable names, coded factor levels and structured missing data,
documented at [content/data.qmd](content/data.qmd).

## Working on the site locally

Requires [R](https://www.r-project.org/) 4.5.2 and
[Quarto](https://quarto.org/docs/get-started/).

```sh
Rscript -e 'renv::restore()'   # install the pinned package versions
quarto preview                 # live-reload while editing
quarto render                  # build the site into docs/
```

To regenerate the synthetic teaching datasets (both seeded, so output is
identical on every run):

```sh
Rscript R/generate-workshop-data.R      # data/abcd-synthetic.csv  (Module 1)
Rscript R/generate-abcd-shaped-data.R   # data/data.RDS           (Modules 2-4)
```

## Layout

```
index.qmd                 landing page (site root)
content/                  one page per module, plus Module 1 deep-dives
R/                        helper + data-generation scripts
scripts/                  live-coding skeletons handed to participants
data/                     synthetic teaching data + dictionaries (tracked)
output/                   where live-coding artifacts get saved (contents ignored)
assets/                   SCSS theme and images
docs/                     rendered site (gitignored; built and published by CI)
```

Pushing to `main` renders the site and deploys it to GitHub Pages via
`.github/workflows/render-publish.yml`.

## License

[GPL-3.0](LICENSE)
