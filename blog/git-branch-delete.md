---
title: git branch --delete master
tags: hacking, internet
description: Renaming a git default branch is easy.
published: 12020-08-04
---

`git` version 2.28.0, [released one week ago], includes a simple but nice new feature:

> init: allow setting the default for the initial branch name via the config

What does this mean?

## Changing new repositories

When creating a git repository using `git init`, git will create a default branch for you.[^init] Traditionally, this branch is called "master", so git creates this branch and you can begin staging and committing files.

Should you find this name distasteful, you can change the name of the branch at any time. The git invocation to do so is

`git branch --move master whatever`

As of this newest release, git can do this for you. To set the default branch name to `main` for all repos your user creates, you will want to edit the so-called *global* git configuration:

`git config --global init.defaultBranch main`

Any new repository you initialize will now use the default branch `main`.

## Changing existing repositories

This setting only affects new repositories that you create in the future --- but changing an existing repo is not difficult.

From the existing repo, rename the branch:

`git branch --move master main`

Push your new branch (assuming the remote repository is named "origin"):

`git push origin main`

Finally, delete the remote's original branch[^websites]:

`git push origin --delete master`

In three steps you have renamed a git branch without making a big deal out of it, all while avoiding the wrath of internet reactionaries.

[//]: # links n footnotes

[released one week ago]: https://lore.kernel.org/git/xmqq5za8hpir.fsf@gitster.c.googlers.com/

[^websites]: Some hosts for remote repositories do not appreciate you trying to delete the branch named `master`. This is the case for both GitLab and GitHub. For these, you will need to use their respective web interfaces.

[^init]: This simplification is like Newtonian physics: useful and accurate but not exactly true.
