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

7. One the install is complete, run `vanishingVitamin::launch_app()`, which should trigger the app to launch.

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

#### Updating renv

If you add/remove packages during development, run `renv::install("[pkg-name]")` to add it to the `renv.lock` file then run `renv::snapshot()` to update the `renv` package list.
This will ensure future users/developers will have the same R packages installed when they run the app.

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
3. [Stage](https://git-scm.com/docs/git-add) and [commit](https://git-scm.com/docs/git-commit) changes to the remote GitHub repository. Note that you can (and should!) stage and commit multiple while making changes. Think of this as `CTRL/CMD + S` to save your progress while developing. 
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


