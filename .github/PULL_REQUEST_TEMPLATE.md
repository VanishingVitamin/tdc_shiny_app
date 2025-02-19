**This template illustrates how pull requests should be structured when making changes to the repository. Delete this text and replace the text within each section with relevant information.**

See [A practical guide for better, faster code reviews](https://github.com/mawrkus/pull-request-review-guide) for helpful tips on requesting and writing reviews.

## Context

This section provides a concise, high-level overview of why changes are being made.
For example, "this pull request adds a scatterplot summarizing [x] and fixes a bug in functionality [y]."

## Summary of Changes

This section contains an itemized list of files that are changed as part of this PR and an explanation of what changes were made:

* `ui.R`: added select and plot UI components for functionality [z]
* `server.R`: added renderPlot logic for creating a scatterplot and fixed a bug in functionality [y]
* README.md: updated documentation

## Requested Review

This section contains a bulleted list of instructions for the reviewer.
At a minimum, a reviewer should verify that the changes work as-expected on their computer (see Verification of Results section below).
You can elicit feedback based on your reviewer's background (e.g., ask them for subject-related feedback if they're a subject expert or efficiency/coding best practices if they are an experienced programmer).
Requesting pointed feedback can be helpful for a reviewer (e.g., "This chunk of code is running slowly for me. Do you have any ideas on how to speed it up?" or "I'm not in love with how this plot looks/functions in the app. Do you recommend any changes?").

* Switch to the relevant branch with `git fetch` then `git switch [branch name]` (**NOTE for PR author:** replace `[branch name]` with relevant branch name).
* Launch the app and follow the instructions in Verification of Results to ensure the output matches.
* Provide feedback on the new app functionality -- would you recommend any changes to the aesthetics or functionality of the new plot?
* Read through the code changes and note places that are confusing, poorly commented/documented, or potentially buggy/erroneous.
* Feel free to provide any other feedback you think of.

## Verification of Results

This is an optional section to verify that the changes work as-expected on the reviewer's computer.
This verification may take different forms depending on the nature of the changes.
If adding new functionality to the app, you might change the app inputs to specific settings, take and paste a screenshot of the app output into the PR, then request that the reviewer perform the same steps and verify that their output matches yours.

## Next Steps

This is an optional section to explain your plans for the next Pull Request.
