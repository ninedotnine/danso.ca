---
title: How to stop yourself from force-pushing to `master`
tags: internet, programming
description: Be a better team player with git hooks.
published: 12019-08-30
---

If you're like me, you probably use git, and if you're like me, you probably use git badly. Years of working on projects by myself have ingrained in me a few habits that don't transfer well to team environments -- perhaps most notably, pushing (even force-pushing) to the master branch.

This post documents the steps I've taken to catch myself before this happens.

## Git hooks, briefly

A git hook is a program that can be run by git at various points in your git workflow. Typical examples include `pre-commit` (run before making a commit) and `post-checkout` (run after switching branches).

I'm not bothered by making bad commits -- in fact, I often do this on purpose to rebase later. What I'm trying to do is prevent *pushing* these bad commits, so I make a `pre-push` hook.

## Complete program text[^bash]

```bash
#!/bin/bash
# don't allow --force-pushing to master branch

hook_name="hooks/$(basename $0)"
cur_branch=$(git name-rev --name-only --no-undefined --always HEAD)
push_cmd=$(ps --pid $PPID --format "command=")

protected_branches="^(master|dev|release-*|patch-*)"
forceful_flags="force|delete|-f"
affirmative="yes|y|Y"

# putting regexes in quotes makes them fail, because bash ¯\_(ツ)_/¯
if [[ "$cur_branch" =~ $protected_branches ]]; then
    if [[ "$push_cmd" =~ $forceful_flags ]]; then
        echo -e "${hook_name}: don't force-push to $cur_branch"
        exit 1
    else
        echo -ne "${hook_name}: are you aware that you are on branch ${cur_branch}? "
        read confirmation < /dev/tty
        if [[ ! "$confirmation" =~ $affirmative ]]; then
            exit 2
        fi
    fi
fi
exit 0
```

You can find the latest version of this file in my [dotfiles repository](https://github.com/ninedotnine/dotfiles/blob/master/git_hooks/pre-push).

## Explanation

The code is pretty short and straightforward, but there are a few things worth explaining:

### Getting the current branch name

`git name-rev` exists to make getting the symbolic names of branches easy.

### Reading from stdin

Git hooks are not intended to run interactively. This is a problem if you are trying to write a confirmation ("are you sure?") program.

To circumvent this, read directly from `/dev/tty`.

### Checking the command-line arguments

`pre-push` will be forked from the `git` command that you run. With this in mind, we can pass the parent process ID to `ps` and it will output the command that was run.[^proc]

### Exit status

If a hook exits with a non-zero exit status, git won't follow through with the operation. I exploit this by exiting with `1` when we don't want to push, and `0` when we do.

## To set up

To apply this hook globally, to all current and future repos:

```
> cd ~
> mkdir -p .config/global_git_hooks/
> git config --global core.hooksPath .config/global_git_hooks
```

You can name your hooks directory whatever you want; here, I chose the name `.config/global_git_hooks/`. Save the file above as `pre-push` in the appropriate directory.

Ensure the file is executable:

```
> chmod +x ~/.config/global_git_hooks/pre-push
```

That's all it takes -- the program will be run every time invoke `git push`.

## Restoring the default behaviour

Sometimes, you might want the default behaviour. No problem! `cd` into the repository and edit the local config with `git config core.hooksPath $GIT_DIR/hooks`. This will override your global and allow custom settings on a repo-per-repo basis.

## What if I know what I'm doing and I really want to anyway?

You can skip the execution of hooks with `git push --no-verify`. Best of luck with that.

[//]: # footnotes

[^bash]: I don't always use bash, but the regex match operator (`=~`) was too convenient to refuse.
[^proc]: My first instinct was to read from `/proc`, but this turned out to be more difficult than running `ps`.
