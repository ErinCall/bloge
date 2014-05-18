Title: The Splinter Mystery
Slug: the-splinter-mystery
DisqusId: 20
Posted: 2013-08-10T17:42:00Z
Tags:
    python
I've been wrestling with a mystery lately. The [Catsnap](https://git.andrewlorente.com/AndrewLorente/catsnap) tests use a browser-interaction library called [Splinter](http://splinter.cobrateam.info/) to verify the JavaScript on the pages. After I added a few tests, I started to see some fairly bizarre performance problems.

While the browser was doing things, it was very quick indeed. But at what looked like random times, [it would just sit and think about philosophy for a while](http://screencast.com/t/MpEpfe5mm). I asked about this on the Splinter mailing list and got no responses (I'm not sure how active that mailing list is), so I finally did some investigation on my own.

I created [a repository that demonstrates the slow behavior](https://github.com/AndrewLorente/slow_splinter_demo). I've built a fairly minimal amount of behavior, so it's easy to read through. Unfortunately, even though it _demonstrates_ the problem, it still doesn't _explain_ it. I still have no idea why this particular set of steps leads to such a sudden drop-off in performance.

At this point I think I've exhausted my ability to debug this any farther. I'm hoping to take it to the Cobrateam, uh, team, and see if they're interested in helping track down the root cause.
