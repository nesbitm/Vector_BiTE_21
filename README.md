# The Multilingual Quantitative Biologist VectorBiTE 2021 Edition

This repository contains the source code for the **The Multilingual Quantitative Biologist** online book.

All code in this project was written in and tested with R 3.xx and Python 3.xx, though older language versions (including R 2.xx and Python 2.7) should work in most cases.

## Usage

### Building the book

If you'd like to develop on and build the Multilingual Quantitative Biologist book, you should:

- Clone this repository and run
- Run `pip install -r requirements.txt` (it is recommended you do this within a virtual environment)
- (Recommended) Remove the existing `The Multilingual Quantitative Biologist/_build/` directory
- Run `jupyter-book build The Multilingual Quantitative Biologist/`

A fully-rendered HTML version of the book will be built in `_build/html/`.

### Hosting/Deploying the book online

The html version of the book is hosted on the `gh-pages` branch of this repo, which is then rendered on [https://mhasoba.github.io/TheMulQuaBio/](https://mhasoba.github.io/TheMulQuaBio). 

To deal with non-standard kernel dependencies (bash, R) we will not use an GitHub actions workflow to automatically build and push the book to this branch on a push or pull request to master (something to look into in the future).

However, the workflow for building the book manually is not too onerous:

- Make changes to the book's content on the master branch of this repository
- Re-build the book with `jupyter-book build content` (assumes you are running the command from root directory of the repository)
- Ensure that HTML has been built for each page of your book. There should be a collection of HTML files in the `content/_build/html` folder. Also load `_build/html/notebooks/index.html` and check that the book has been built (with navigation etc.) as expected. 
- Run `ghp-import -n -p -f content/_build/html` to deploy the book.
 
The last command will automatically push the latest build to the `gh-pages` branch. More information on this hosting process can be found [here](https://jupyterbook.org/publish/gh-pages.html#manually-host-your-book-with-github-pages).

Typically after a few minutes the site should be viewable online at [https://mhasoba.github.io/TheMulQuaBio](https://mhasoba.github.io/TheMulQuaBio). If not, check repository settings under Options -> GitHub Pages to ensure that the gh-pages branch is configured as the build source for GitHub Pages.

An example command  to push all new changes to the git repository **and** is:

`git add -A && git commit -m "Commit message" && git push -u origin master`

**Please do not push changes for every little edit you make to the book (e.g., after fixing some typos)**. Push only significant changes. Remember, you can deploy the book (by pushing to the `gh-pages` branch using `ghp-import` as explained above) without pushing changes to the `master` branch. 

## Contributors

We welcome and recognize all contributions. You can see a list of current contributors in the [contributors tab](https://github.com/mhasoba/TheMulQuaBio/graphs/contributors). 

Also, note that:

* The `master` branch of this repository is protected, so even users with write (push) access need to push changes on a branch and make a [pull request](https://docs.github.com/en/free-pro-team@latest/github/collaborating-with-issues-and-pull-requests/about-pull-requests) (also, see [this](https://docs.github.com/en/free-pro-team@latest/github/collaborating-with-issues-and-pull-requests/creating-a-pull-request)). New commits to a non-master branch *after* a pull request has been made will result in any pull requests from that non-master branch to be discrded. Please [read this](https://gist.github.com/digitaljhelms/4287848) for good practices for branching (and merging).     
* The solutions to the exercises in this book are in a separate, [private git repo](https://bitbucket.org/mhasoba/themulquabio_sols) that students do not have access to. Ask Samraat (mhasoba@gmail.com) if you need access to that repository. Students will be provided the solutions when the time comes.
* The `results` directory in `content` is populated when scripts are run, but these are not version controlled (all files in this directory under `.gitignore`).

## Credits

This project is created using the excellent open source [Jupyter Book project](https://jupyterbook.org/), initiated using [executablebooks/cookiecutter-jupyter-book template](https://github.com/executablebooks/cookiecutter-jupyter-book).

Most of the sections on Data Analysis and Statistics were originally written by David Orme <d.orme@imperial.ac.uk>.

