Title: Managing Multiple Identities In Git
DisqusId: 18
Slug: managing-multiple-identities-in-git
Tags:
    git
Posted: 2012-12-31T00:02:00Z
When you use git at work and for personal projects, it's easy to mess up and make a commit using the wrong identity. You can end up with your work email attached to a commit for your personal work, or your personal email attached to your professional work. One solution is to simply isolate your code: only do your work programming on your work computer and only do your personal programming on your personal computer. That's not always practical, though. For example, you might find yourself wanting to do personal work while travelling with your work computer. In this post I'll show you how I manage that problem.

----------8<-----------------

When you first run `git commit` on a new computer, it asks you to run a global config command to set up your identity:

```
Committer: Erin Call <erincall@Slim.local>
Your name and email address were configured automatically based
on your username and hostname. Please check that they are accurate.
You can suppress this message by setting them explicitly:

    git config --global user.name "Your Name"
    git config --global user.email you@example.com

After doing this, you may fix the identity used for this commit with:

    git commit --amend --reset-author
```

That's fine if you only use one identity to commit, but it's no good at all for our purposes. Instead, we use environment variables:

```
export GIT_AUTHOR_NAME='Erin Call'
export GIT_AUTHOR_EMAIL='hello@erincall.com'
export GIT_COMMITTER_NAME=$GIT_AUTHOR_NAME
export GIT_COMMITTER_EMAIL=$GIT_AUTHOR_EMAIL
```

Set this up on your personal and work computers, using the appropriate identity on each (don't forget to clear the identity from your global git config).

Now for the clever bit: the `personal` and `work` aliases. On your work computer, you'll have a shell alias for making commits using your personal identity:

```
alias personal="GIT_AUTHOR_EMAIL='hello@erincall.com' GIT_COMMITTER_EMAIL='hello@erincall.com'"
```

And on your personal computer, an alias for committing as your work identity:

```
alias personal="GIT_AUTHOR_EMAIL='ecall@myjob.com' GIT_COMMITTER_EMAIL='ecall@myjob.com'"
```

Now you can easily switch to the other identity for a single commit:

```
personal git commit
```

I've framed this in terms of work vs. home, but of course it'll work any time you need to manage multiple identities in git. It's a simple system that I find is very effective.

