Title: I Have Beef With How Git Push Is Taught
DisqusId: 25
Posted: 2014-01-10T22:41:00Z
Tags:
    git
Slug: i-have-beef-with-how-git-push-is-taught
There's a particular detail in the workings of the @git push@ command that's almost always elided in tutorials. It's one of those details where you don't often need to exercise your knowledge of it, but you occasionally need to know it exists. If you don't know you don't know it, you can end up in a frustratingly confusing state.

Pretty much [every](http://try.github.io/levels/1/challenges/11) [git](http://gitready.com/beginner/2009/01/21/pushing-and-pulling.html) [tutorial](https://www.atlassian.com/git/tutorial/remote-repositories#!push) [I've](http://www.vogella.com/tutorials/Git/article.html#gitpushbranch) [looked](http://gitimmersion.com/lab_48.html) at makes a similar claim: "The syntax of this command is `git push <remote> <branch>`."

That's false! The syntax of the command is `git push <remote> <local-ref>:<remote-branch>`. You can omit some of the keywords, and their values will be inferred in various ways. And in fact, it's extremely common to omit the `local-ref` and colon, leaving the command looking the way tutorials claim it always looks.

The [official git docs](http://git-scm.com/docs/git-push.html) get it right, of course, but they're totally inaccessible to a beginner (or, well, anyone who isn't a git maintainer themselves). No help there.

You might wonder if it's all that important. After all, how often do you really need to use the fully-explicated form? You'd be right, to a point. The only time you really need to specify both the local ref and the remote branch is when you want to push a local branch to some remote branch with a different name. Problem is, when the tutorials don't explain that another form of the command even exists, then someone who needs the full form don't know to look for it. On several occasions, I've talked to people who did something like:

```
git checkout some-branch
git commit
git push origin some-other-branch
```

expecting it to push the HEAD of `some-branch` to origin's `some-other-branch`. In other words, they'd made some changes on one branch, and they wanted to push those changes up to another branch. That's a reasonable thing to want to do, and it was even reasonable to think it would work. The problem was, having never even heard that there was a longer form for `git push`, they didn't even know to look into it. They were just lost at sea.
