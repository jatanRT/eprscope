# Contributing to `{eprscope}`

## Suggested Workflow for a New Onboarding Collaborator

Create a new blank github issue with the following title:
"`Asking for a new collaboration` " with the label of "`new-collab-request` ".
Please, write couple of sentences about you and add links to your ORCID and/or
homepage and/or any existing git-service profile e.g. like github, gitlab or
bitbucket. Alternatively, a [ResearchGate](https://www.researchgate.net/) profile
might be linked as well. Experiences with the `R` coding as well as with the
basics of magnetic resonance spectroscopy are greatly appreciated, however they
are not required.

## Suggested Workflow for a New Contributor

### Fixing Typos, Grammatical Errors and the Code/Documentation Bugs

There are two possibilities:

1.  Creating a new Pull Request (PR)

-   In case you may find an issue to be corrected and you're able to manage it by
    yourself please, **first of all open a new github issue with an appropriate
    title and description** **as well as labels covering your suggestions**. Add
    label like "`self-managed` " in order to inform that you will be working on
    those changes. Additional labels like those mentioned below (see
    "`2. Create a new Issue` ") may be used, as well.
-   Fork the repository and clone it onto your desired workstation/computer.
-   Afterwards, create a new branch with the name like "`br_…` " where the
    ellipsis stands for 2-3 short words separated by underscore "`_` " to describe
    the issue in short.
-   Make your changes, please follow the current code/documentation style and/or
    consult the [`{tidyverse}` styleguide](https://style.tidyverse.org/index.html)
    as well as that of the [`{roxygen2}`](https://r-pkgs.org/man.html).
-   Do git commit(s) push it(them) and finally create PR either within a
    specialized git IDE of your choice (e.g. like Github Desktop, Tower or
    GitKraken) or directly on the web, i.e. on the main github project page. If
    you want to correct any documentation part and/or any typos or grammatical
    errors please, consider to do it in a concise way, i.e. do not create a PR for
    each small individual change. The title of your PR should briefly describe the
    change(s) and the body of the PR must contain additional details about those
    changes. **Please, don't forget to reference the issue, that you have**
    **written in the first step**, by the `#issue-number` (where the `issue-number`
    corresponds to number like 1, 2, …10…of your issue) within the body of the PR.
-   Finally, please wait for the response of the project maintainer(s). Prior to
    acceptation of any change a conversation may be held, especially in the case
    of substantial code/documentation modification which may also require a
    detailed revision.
-   As an inspiration how to perform a github flow you may look at the following
    videos:
    -   [How to Pull Request in 3
        Minutes](https://www.youtube.com/watch?v=jRLGobWwA3Y)
    -   [What is a Pull Request, and How Do I Write Great Pull
        Requests?](https://www.youtube.com/watch?v=_HedItVFr5M)
    -   [Github Workflow for Issues and Pull
        Requests](https://www.youtube.com/watch?v=d3N2yeAaLkc)

2.  Creating a New Issue

-   In case you may find an issue to be corrected and you cannot or don't want to
    manage it by yourself (as already shown in case 1), please write your issue
    (on the main github project page) properly entitled with all the details and
    provide relevant labels like "`bug` ", "`question` ", "`enhancement` ",
    "`new-feature` ", "`help` ", "`documentation` " or "`typo` ". You may also use
    a specialized issue template like "`Bug Report` ", "`Feature Request` " or
    "`Report a security vulnerability` ". If none of those templates suits you,
    just use "`Open a blank issue` ".
-   Subsequently, the issue will be discussed and depending on the discussion
    outcomes the developer(s)/maintainer(s) will work on code updates, accordingly.
