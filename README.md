![R-CMD-check](https://github.com/best-practice-and-impact/CARS-2/workflows/R-CMD-check/badge.svg)
![test-coverage](https://github.com/best-practice-and-impact/CARS-2/workflows/test-coverage/badge.svg)

# Coding in Analysis and Research Survey

The contents of the rmarkdown folder can be used to generate a template site to present results from the second wave of the Coding in Analysis and Research Survey (CARS). 

A preview can be seen at the [CARS-2 website](https://best-practice-and-impact.github.io/CARS-2/).

> Rscript build_site.r runs the project 

Before joining the project read the [Contributing Guidance](https://github.com/best-practice-and-impact/CARS-2/wiki/Contributing-Guidance)
# Summary
To build the site you will need to 

* Build the `carsurvey2` r package
* Run the build script

To build the package

```bash
git clone https://github.com/best-practice-and-impact/CARS-2.git
cd CARS-2
```
Then open the `CARS_wave_2.Rproj` in R Studio

Finally in an R console run:

```r
devtools::document("carsurvey2/")
devtools::install("carsurvey2/")
source("build_site.r")
```

> `build_site.r` is the entry point to the project. Running this will run the entire process.  

## API
To load the data you will need to access the Smart Survey API. 
You can read the [Smart Survey API Documentation](https://docs.smartsurvey.io/docs) on how to do this.

You will then need to set two environmental variables which are accessed for the data ingest. 

```shell
# API KEYS for Smart Survey
# Smart Survey API Key
export CARS_TOKEN=

# Smart Survey API Key Secret
export CARS_SECRET=
```

## Adding/modifying content

The project is separated into two sections. The `carsurvey2` R Package which contains the code to generate the outputs and rmarkdown files which are rendered to produce the site.

### Website Source Code

The files needed to render the site can be found in the `rmarkdown` folder. To amend pages, make modifications to the .rmd files. To add pages, create new .rmd files and place them in the rmarkdown/ folder. For example, to create a new page called "page 2" add the file page2.rmd. 

To make changes to the navigation bar, edit the _site.yml file. When adding new pages, make sure to refer to them in the .yml file. For example: 

```yaml 
navbar:
  title: "My Website"
  left:
    - text: "Home"
      href: index.html
    - text: "Page 1"
      href: page1.html
```

The text for each link does not have to be the exact title as the page. 

Style changes can be made by amending style.css. 

The website is outputted to the folder `docs`. You do not need to interact with this folder. 

> You can open the file `docs/index.html` in any web browser to see what the current build of the site looks like

### `carsurvey2` R Package

This R Package contains the code to generate the website. This follows the standard layout, see [R Packages](https://r-pkgs.org/) for more information. 



## Rendering the site

The site can be rendered from R or RStudio. Ensure you have RMarkdown and yaml (the R package) installed. 

To render the site, run the build_site.r script. The script will use the R project root as the working directory.
