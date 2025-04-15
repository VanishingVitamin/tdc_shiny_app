# VanishingVitamin Thiamin Deficiency Complex app

The code in this repository was mostly written in the [R](https://cloud.r-project.org/) programming language within the [RStudio](https://posit.co/download/rstudio-desktop/) integrated development environment (IDE).
It uses [git](https://git-scm.com/) for version control.
See [Happy Git with R](https://happygitwithr.com/) for an R-centric introduction to git.

## I'm a user (accessing the app)

Great, welcome!
There are a few options for accessing the application.

### I don't want to use R

If you are a USGS employee, then you can access the app at <https://rconnect.chs.usgs.gov/vanishing-vitamin-app/>.
At the moment, non-USGS employees must follow the "I can use R" instructions below to access the app.

### I can use R

If you can use R and want to run the app locally, then complete the following steps.

1. Ensure you have [R](https://cloud.r-project.org/) installed. We also recommend you use the [RStudio](https://posit.co/download/rstudio-desktop/) development environment.

2. Ensure you have [git](https://git-scm.com/) installed.

3. Clone the repository: 
    * `git clone https://github.com/VanishingVitamin/tdc_shiny_app.git` over HTTPS, 
    * `git clone git@github.com:VanishingVitamin/tdc_shiny_app.git` over SSH (requires SSH key), 
    * or whatever your favorite way is from the URL <https://github.com/VanishingVitamin/tdc_shiny_app>.

4. Open the `tdc_shiny_app.Rproj` file to open the Shiny R project in RStudio.

5. If you haven't done so already, install the `devtools` R package with `install.packages("devtools")`.

6. Run `devtools::install(".")` in your R Console to install the `vanishingVitamin` R package locally.

7. One the install is complete, run `vanishingVitamin::launch_app()` which should trigger the app to launch.

## I'm a reviewer (providing feedback)

Wonderful, thank you for your feedback!
Complete steps 1. through 4. in the "I can use R" section above to ensure you can run the app locally.

Major changes to the application undergo a peer review process using GitHub [pull requests](https://docs.github.com/en/pull-requests/collaborating-with-pull-requests/proposing-changes-to-your-work-with-pull-requests/about-pull-requests) (PRs).
A pull request is a proposal to merge a set of changes from one git [*branch*](https://git-scm.com/book/en/v2/Git-Branching-Branches-in-a-Nutshell) into another.
As a reviewer, your job is to look at recent changes made to the application and provide feedback within the Pull Request.

See [Reviewing proposed changes in a pull request](https://docs.github.com/en/pull-requests/collaborating-with-pull-requests/reviewing-changes-in-pull-requests/reviewing-proposed-changes-in-a-pull-request) for an overview of the Pull Request review process.
See [A practical guide for better, faster code reviews](https://github.com/mawrkus/pull-request-review-guide?tab=readme-ov-file#-for-the-reviewers) for helpful tips for writing good code reviews.


## I'm a developer (making changes)

Awesome, we appreciate your help!
Before we dive into the app structure, complete steps 1. through 4. in the "I can use R" section above to ensure you can run the app locally.

### I want to make changes to the app

#### How to update the application

The Shiny application is housed within an R package called `vanishingVitamin`.
The user-facing function for launching the application is `vanishingVitamin::launch_app()`.
There are a few internal-only helper functions called within `launch_app()` that define the necessary components of a Shiny app.
Refer to the matching filenames in the `R/` folder for the source code of these helper functions.

- `app_ui()` defines the User Interface (`ui`) elements of the app. The function is a thin wrapper around a `bs4Dash::dashboardPage()` definition.
- `app_server()` defines the `server` function used in the app. The value returned by this function is another function, which defines the app's server logic.
- `app_theme()` defines the app's dashboard theme. It is a thin wrapper around a `fresh::create_theme()` call.

Despite the R package infrastructure, you can make changes to the application as you would any Shiny app.
Treat the `app_ui.R` and `app_server.R` scripts as you would `ui.R` and `server.R` scripts in regular Shiny app development.
To view the app during development, you must load the package, by pressing `CTRL/CMD + SHIFT + L` in RStudio or running `devtools::load_all()` in the R Console, then run `launch_app()`.

See [Mastering Shiny - Packages](https://mastering-shiny.org/scaling-packaging.html) for an overview of developing Shiny app R packages.
See [R Packages](https://r-pkgs.org/), particularly [The Whole Game](https://r-pkgs.org/whole-game.html) and [Fundamental development workflows](https://r-pkgs.org/workflow101.html), for more information on developing R packages in-general.

##### How to update data

The `vanishingVitamin` R package has two exported data sets.

1. The `tdc_data` data set contains data from [this Excel document](https://docs.google.com/spreadsheets/d/1TX5lkpAsdurQlWQoNAmKWHv4WBoPwjmq/edit?usp=sharing&ouid=106506252335393186387&rtpof=true&sd=true) that have been cleaned and standardized. 
   See the `inst/construct_tdc_dataset.R` for the code used to create this data set. 
2. The `citations` data set contains metadata for the citations associated with the data available in `tdc_data`. 
   See `inst/construct_citations_metadata.R` for the code used to create this data set.

You can learn more about the contents of the data sets by their doc pages -- run `?vanishingVitamin::tdc_data` and `?vanishingVitamin::citations`.
If you'd like to change either of these data sets, for example new data have been added to the source Excel document, complete the following steps:

1. Change the `inst/construct_tdc_dataset.R` or `inst/construct_citations_metadata.R` script to reflect the changes you want to make.
2. Re-run the scripts to create `tdc_data` and `citations` objects in your R environment. 
   If the scripts run fully, these objects should be saved to `inst/misc_data/tdc_data.csv` and `inst/misc_data/citations.rds` files, respectively.
3. Inspect the `tdc_data` and `citations` objects to verify the desired changes.
4. Run `usethis::use_data(tdc_data, overwrite = TRUE)` and `usethis::use_data(citations, overwrite = TRUE)`. 
   This will overwrite the two data sets exported by the application.
5. Run `devtools::load_all()`, then `vanishingVitamin::launch_app()`, then interact with the app to ensure it runs successfully.
6. Update the data set documentation in the `R/data.R`, if significant changes were made that render that documentation outdated.

#### Creating Pull Requests

Major changes to the application undergo a peer review process using GitHub [pull requests](https://docs.github.com/en/pull-requests/collaborating-with-pull-requests/proposing-changes-to-your-work-with-pull-requests/about-pull-requests) (PRs).
A pull request is a proposal to merge a set of changes from one git [*branch*](https://git-scm.com/book/en/v2/Git-Branching-Branches-in-a-Nutshell) into another.
The `main` branch on this repository represents the canonical version of the application and serves as the "root" for the repository's branches.

As the developer, you are responsible for creating a pull request for any changes you make to the app.
If you don't have a ton of experience using git or GitHub, I recommend reading through [Happy Git with R](https://happygitwithr.com/) for a friendly, R-centric introduction.
GitHub pull requests support text formatting using Markdown syntax -- see [Basic Syntax](https://www.markdownguide.org/basic-syntax/) for an introduction.

The typical workflow for making changes goes:

1. [Create](https://git-scm.com/book/en/v2/Git-Branching-Basic-Branching-and-Merging) a new branch upon which changes will be made.
2. Implement desired changes.
3. [Stage](https://git-scm.com/docs/git-add) and [commit](https://git-scm.com/docs/git-commit) changes to the remote GitHub repository. 
   Note that you can (and should!) stage and commit frequently while making changes. 
   Think of this as `CTRL/CMD + S` to save your progress while developing. 
4. (Recommended if collaborating on a branch) [Pull](https://git-scm.com/docs/git-pull) any commits pushed to the remote branch by someone else since you last pulled.
5. [Push](https://git-scm.com/docs/git-push) your commits to the remote branch.
    * If you're pushing a branch for the first time, run `git push -u origin <branch>` (`-u` is short for `--set-upstream`).
6. [Open](https://docs.github.com/en/pull-requests/collaborating-with-pull-requests/proposing-changes-to-your-work-with-pull-requests/creating-a-pull-request) a Pull Request (PR) on GitHub, setting the branch upon which changes have been made as the "compare" branch and the desired target branch (likely the `main` branch) as the "base" branch.
7. Fill out the PR template provided in this repository.
8. Request a review from another person.
9. Address any comments/feedback brought up during the review. 
    * If the feedback is pertinent to the changes proposed in the PR (e.g., a new feature is bugged), you'll want to incorporate the feedback before merging the branch. 
    * If the feedback is unrelated to the current changes, then you can decide whether you want to incorporate it into the current PR or document the feedback in a [GitHub issue](https://docs.github.com/en/issues/tracking-your-work-with-issues/using-issues/creating-an-issue#creating-an-issue-from-a-comment) for a future PR.
10. Once the review is complete and feedback addressed, complete the pull request by [merging](https://docs.github.com/en/pull-requests/collaborating-with-pull-requests/incorporating-changes-from-a-pull-request/merging-a-pull-request).
11. On your computer, run `git pull` then `git checkout main` to update your local version of the repository to match the remote GitHub version.
12. Return to step 1. if making new changes.

Note that creating PRs from a forked version of a repository is another common way to propose/merge changes into `main`.
We will not be using this method, instead incorporating changes via branches.

### I want to create a new executable file

[UNDER CONSTRUCTION] These instructions are intended for advanced developers who want to create a new `.exe` (executable) file for the app, which should only really be necessary for larger, public releases of the application.

## Disclaimer


