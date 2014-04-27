Title: Announcing Radlibs
Slug: announcing-radlibs
Posted: 2013-07-07T21:09:00Z
Tags:
    python
For the last few days I've been eating, breathing, and sleeping a new project. Code has flown from my fingers like sparks from a millstone. Today I'd like to announce the result of my fevered efforts: [Radlibs](http://www.radlibs.info)! At its core, Radlibs is a laguange for generating English text. It uses randomly-selected category-members to fill in the blanks in a given [phrasal template](http://en.wikipedia.org/wiki/Phrasal_template).

The "Rad" in "radlibs" comes from its recursive nature: category-members may also be phrasal templates. Thus, with the right libraries Radlibs can generate some good clean fun:

[![crate and barrel](https://d5hwde6hzncg6.cloudfront.net/439301aa34325e767a339d3edfa340e4528c78ff_medium)](https://d5hwde6hzncg6.cloudfront.net/439301aa34325e767a339d3edfa340e4528c78ff)

I'm very proud of what I've made, but I'd be remiss if I didn't mention its influences. Many years ago when I worked at Rentrak, a few developers there generated a post-test hook called game.pl that would simulate a random encounter in a dungeon crawl, with the outcome based on whether your tests had passed or failed. Game.pl was built from the most monstrous regular expression you've ever seen (or maybe not--there's always a regex more monstrous), using perl's `/e` regex to implement recursion and branching. Another developer later open-sourced game.pl as [Legendary Flavor](https://github.com/wickline/legendary_flavor), but it hasn't seen much development since then. You can think of Radlibs as an homage, a port, or a resurrection effort.

Anyway, I hope you enjoy it. Be sure to [open an issue](https://github.com/AndrewLorente/radlibs/issues) if you encounter any problems.
