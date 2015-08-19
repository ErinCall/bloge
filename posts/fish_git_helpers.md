Title: Git Helpers For The Fish Shell
Posted: 2015-08-19T08:30:00-0700
Tags:
    git
    fish

I recently switched from Bash to the [Fish Shell](http://fishshell.com/). It's great, but I missed the git helpers I got from [SCM Breeze](http://github.com/ndbroadbent/scm_breeze). Rather than sit around moping, I thought I'd implement the core feature in Fish.

--------------------8<------------------

The best thing SCM Breeze offers is a collection of shortcuts for files with outstanding changes. The author realized that when you run `git status`, the next thing you're likely to want to do is perform some operation or another on the changed files. You might want to `git add` them, view the changes in `git diff`, open them in an editor, etc. SCM Breeze alters the `git status` output to make the file listing a numbered list. It also wraps `git` in a Bash function that translates those numbers when they're used as arguments. The upshot is that instead of having to type `git add some/long/path/to/a/filename`, you can just ype `git add 1`.

That behavior is what I implemented in Fish, as well. It looks like this:

![Terminal video](https://cdn.erincall.com/48a79a4f594d917c242f7cab6f9810ee137b82f9)

One improvement I've made over SCM Breeze is that my version handles renamed files correctly. Under SCM Breeze, the entire rename would get a single number pointing to "`oldname -> newname`". It was unhelpful. My version assigns indices to the old and new names.

If you're interested in trying this out, drop [`git.fish`](https://git.erincall.com/ErinCall/dotfiles/blob/master/.config/fish/functions/git.fish) and [`acquire_git_changes.fish`](https://git.erincall.com/ErinCall/dotfiles/blob/master/.config/fish/functions/acquire_git_changes.fish) in your `~/.config/fish/functions` directory. This is still an early version, so there may be problems--let me know if you run into any! If you do have trouble, you can always use `command git …` to bypass the git-wrapping function.
