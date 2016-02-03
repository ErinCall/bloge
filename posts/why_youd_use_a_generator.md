Title: Oh, So That's Why You'd Use A Generator
Posted: 2012-07-18T06:19:00Z
DisqusId: 15
Slug: oh-so-thats-why-youd-use-a-generator
Tags:
    python
So, I've been working on this [catsnap](https://github.com/ErinCall/catsnap) project of mine, improving the performance. The big problem is that it's spending an enormous amount of time on the wire, waiting to get information back from AWS. For example, when searching for images by tag, my first pass used a pretty na&#239;ve approach: loop through the images associated with a tag, asking DynamoDB about each one. Fortunately, the excellent [boto](http://boto.cloudhackers.com/en/latest/index.html) library for interacting with aws has a batched lookup mode that lets me grab all the images at once.

Only...not quite. DynamoDB has this thing going on where if you ask for some items, it'll give some or all of them back.[1] It's polite enough to tell you which keys it ignored, but you still have to ask for them again if you really wanted what you said you wanted.

-----8<-----------

The obvious, hamfisted solution is to check and see if there were any "unprocessed keys," and go fetch those, and then check again, until we've finally gotten everything we wanted in the first place, and finally return. That leaves the user twiddling their thumbs, waiting on all those requests.

So instead I built [a generator](https://github.com/ErinCall/catsnap/blob/batched-requests/catsnap/batch/image_batch.py#L26) that immediately starts yielding whatever it has, and then goes back for more if anything is missing. It makes for a nice, snappy-feeling app.

I knew python generators existed, but I'd never really had occasion to use one. I think I'd fallen victim to that situation where people demonstrate a feature with toy problems, and although their simplicity makes it clear _how_ to use the feature, their triviality makes it very vague _why_ you'd use the feature. I'm glad I finally see the point!

[1] The [API docs](http://docs.amazonwebservices.com/amazondynamodb/latest/developerguide/API_BatchGetItems.html) imply that this should only crop up for relatively large queries, but I was seeing it for queries on 20-or-so items from tables that were well under a MB. Probably I was hitting the ceiling on my relatively low read-throughput setting.
