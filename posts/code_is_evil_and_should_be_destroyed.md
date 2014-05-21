Title: Code Is Evil And Should Be Destroyed
DisqusId: 1
Slug: code-is-evil-and-should-be-destroyed
Posted: 2012-01-03T03:13:00Z
Tags:
    bugs
    software deleter
    nih
    dry
Writing code, while sometimes necessary, is inescapably evil. Yes, deploying an empty directory to production will be ineffective. Yes, most version control systems are so obstinate that they won't even accept an empty directory, and, ok, all right, I'll admit that perhaps if we intend to have features, we'll need to write some code at some point. However, it is completely unavoidable that this code will have bugs! *Completely unavaidable*. All code has bugs. There's a reason second-language learners often practice verb conjugation with the sentence "Your code/my code/his/her/its code has bugs."

### So what can we do? It's simple: we kill as much code as we can possibly lose.

First of all, get acquainted with your Eastern European friend, [Yagni](http://www.globalnerdy.com/2008/12/05/it-looks-like-youre-trying-to-stank-up-your-code-would-you-like-some-help/). If you're adding code and you don't know, with absolute certainty, that you're going to need it, you're gambling on the chance of later ease with the certainty of bugs and lost development time.

Second, stop hand-rolling things that someone else has already done. Whatever you've been working lately, I'll bet you there's a library out there right now that can solve your problem already. It's better-tested than your code, it's more feature-complete, and the author came up with a cuter name for it than you did. Look, I know writing glue code isn't sexy. I know it's not the sort of exciting project that looks super-cool on a resume. But you know what else doesn't look good on a resume? "My project was notable for its cost overruns, missed deadlines, and constant bugs."

Now I hear you over there saying, "but Andrew, if all I write is glue code, how will my program stand out from the other ones that glued those libraries together?" Well, if you don't know that already, what exactly is it that you're doing? What value, exactly, do you intend to create? Answer that question, write that code, and knock off for the day, because the minute you go outside those bounds you're creating needless bugs.

Finally, never do the same thing three times. In fact, if you can get away with it, don't do it more than once. Every time you do it, there's a chance--a good chance, in fact--that you'll make a mistake. Put it in a utility function, put it in a little script, put it in a wiki where you can copy-paste it, *whatever*--write it down somewhere and use what you wrote down, so when you realize you made a mistake, you can fix it once and stop making it.

The single most important role a programmer has is software deleter. Get out there and make a red diff!
