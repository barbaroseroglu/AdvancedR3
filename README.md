---
editor_options:
  markdown:
    wrap: 72
    canonical: true
---

# AdvancedR3: It's just a template

This project is about learning how to collaborate in R

# Brief description of folder and file contents

The following folders contain:

-   `data/`: lipidomics.csv file that contains lipidomics data
-   `doc/`: learning.qmd file which is a quarto file
-   `R/`: Readme.md file

# Installing project R package dependencies

If dependencies have been managed by using
`usethis::use_package("packagename")` through the `DESCRIPTION` file,
installing dependencies is as easy as opening the `AdvancedR3.Rproj`
file and running this command in the console:

```         
# install.packages("remotes")
remotes::install_deps()
```

You'll need to have remotes installed for this to work.

# Resource

For more information on this folder and file workflow and setup, check
out the [prodigenr](https://rostools.github.io/prodigenr) online
documentation.
