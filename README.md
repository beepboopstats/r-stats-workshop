# r-stats-workshop

A four-part workshop series teaching programming in R and statistics using
longitudinal study data, for research staff and consortium members in the ABCD
study.

**Site: <https://beepboopstats.github.io/r-stats-workshop/>**

| Module | Date | Topic |
|:--|:--|:--|
| 0 | self-paced | Getting set up |
| 1 | Sep 10, 2026 | Foundations of R programming |
| 2 | Sep 24, 2026 | Data exploration, visualization & descriptive statistics |
| 3 | Oct 29, 2026 | Inferential statistics |
| 4 | Nov 12, 2026 | Applied projects |

Materials teach tidyverse style R with the native `|>` pipe, using a synthetic
ABCD-shaped longitudinal dataset that contains no real participant data.

## Working on the site locally

Requires [R](https://www.r-project.org/) 4.5.2 and
[Quarto](https://quarto.org/docs/get-started/).

```sh
Rscript -e 'renv::restore()'   # install the pinned package versions
quarto preview                 # live-reload while editing
quarto render                  # build the site into docs/
```

To regenerate the synthetic teaching dataset (seeded, so output is identical):

```sh
Rscript R/generate-workshop-data.R
```

## Layout

```
index.qmd                 landing page (site root)
content/                  one page per module, plus Module 1 deep-dives
R/                        helper + data-generation scripts
scripts/                  live-coding skeletons handed to participants
data/                     synthetic teaching data (tracked)
output/                   where live-coding artifacts get saved (contents ignored)
assets/                   SCSS theme and images
docs/                     rendered site (gitignored; built and published by CI)
```

Pushing to `main` renders the site and deploys it to GitHub Pages via
`.github/workflows/render-publish.yml`.

## License

[GPL-3.0](LICENSE)
