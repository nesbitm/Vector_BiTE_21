# Welcome to the VectorBiTE 2021 Training Workshop!

*Please visit the [new soon to be changed landing page](https://vectorbite.github.io/VBiTraining2) first if you have not already. It includes the workshop schedule and important instructions, including how to prepare for the workshop.*

## Introduction

These [jupyter](https://jupyter.org/) notebooks contain the content of the [VectorBiTE 2021 Training workshop](https://vectorbite.github.io/VBiTraining2/). All these materials  are available at a [git repository](https://github.com/mhasoba/TheMulQuaBio).

These notebooks are accompanied by data and code on which you can practice your skills during the training sessions and in your own time.  If you do not use git, you may download the code, data, these notes, and other course materials from the repository at one go by clicking on the green "Clone or download" button. You can then unzip the downloaded .zip and grab the files you need.

It is important that you work through the exercises and problems in each chapter/section. This document does not tell you every single thing you need to know to perform the exercises in it. In programming and computing, you learn faster by trying to solve problems (including computer crashes!) on your own, often by liberally googling the problem!

This training is specifically for the [VectorBiTE 2021 Training](https://vectorbite.github.io/VBiTraining2/). If you need any more in depth explanations about computational biology/ecology please visit the [MulQuaBio](https://mhasoba.github.io/TheMulQuaBio/intro.html) for the full course. This will have more examples and explanations of skills that are assumed as a preliminary to this training.

## Keeping your workflow organized

In the training sessions, you will practice many examples where you are required to write large blocks of code. Please get into the habit of writing code into text files with an appropriate extension (e.g., `.R` for `R` code, `.py` for `python` code, etc.). Furthermore, please keep all your code files organized in one or more directories (e.g., named `code`). Similarly, some of these scripts will take data files as inputs, and output some results in the form of text or graphics. Please keep these inputs and outputs organized as well, in separate directories (e.g., named `data` and `results`) respectively. 

Thus in general, 

> * *Create separate `code`, `data`, `results` directories* within your VBiTE Training week directory, and set your `R` working directory (using `setwd()`) to `code`. 
>
> * *Build separate R scripts for each of the examples given within various topics*.
 
We will help you get set up and abide by this project organization and "workflow". Examples of this are in the [R pre-work](https://mhasoba.github.io/TheMulQuaBio/notebooks/07-R.html). Specifically, [here](https://mhasoba.github.io/TheMulQuaBio/notebooks/07-R.html#practicals).

## Conventions used in this document

Throughout these sessions, directory paths will be specified in UNIX (Mac, Linux) style, using `/` instead of the `\` used in Windows. Also, in general, we will be using [relative paths](https://en.wikipedia.org/wiki/Path_(computing)) throughout the exercises and practicals (more on this later, but google it!).

You will find all command line/console arguments, code snippets and output in boxes like this:
You will type the commands/code that you see in such boxes into the relevant command line (don't copy-paste - you likely need all the command / syntax typing practice you can get !). 

Note that the commandline prompt you will see on your own terminal/console will vary with the programming language: `$` for UNIX, `>>>` for Python, `>` for R, etc. 

Also note that:

$\star$ Lines marked with a star like this will be specific instructions for you to follow


And there will be notes, tips and warnings that you should pay particular attention to, which will appear like this:  

<!-- #region -->
```{note}
This is a note
```

```{tip}
This is a tip
```

```{warning}
This is a warning
```

So here's your first (and perhaps most important) tip:


**Finding solutions online.**  This document does not tell you every single thing you need to know to perform the exercises in it. In programming and computing, you learn faster by trying to solve problems on your own. The contemporary way to do this is to google the problem! Some suggestions: 
* Your typical approach should be to serach online the main keywords along the programming language name (e.g., "unix mv vs cp" or "R create empty dataframe"). 
* Look for [stackoverflow](https://stackoverflow.com/) or [stackexchange](https://stackexchange.com/) based results in particular. Look for number of endorsements for both the question that has been asked, and for its answer.
* Be selective - there will often be multiple solutions to the same issue or problem - don't just blindly use the first one you find. 
* Also, every time a mysterious, geeky-sounding term like "relative path" or "version control" appears, please search (e.g., google) it online it as well!

## Other Preliminaries 

You will  learn multiple techniques here (NLLS, MLE and Bayesian methods, etc) through examples. These notes assume that you have already seen [lectures](https://github.com/vectorbite/VBiTraining2/tree/master/lectures) on these and related topics. 

Final note: you may use [RStudio](https://www.rstudio.com) or any other code editor you prefer.

Please proceded to the [next lesson](https://vectorbite.github.io/VBiTraining2/#schedule).


```python

```
<!-- #endregion -->

```python

```
