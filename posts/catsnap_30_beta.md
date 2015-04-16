Title: Catsnap 3.0 Beta
DisqusId: 17
Posted: 2012-12-27T23:15:00Z
Tags:
    python
    catsnap
Slug: catsnap-3-0-beta
A beta version of Catsnap 3.0 is now available. 3.0 introduces a major architectural change: instead of accessing dynamodb directly, the command-line script is now a client to a postgres-backed web api. This has several advantages, including browser-based access, reduced operating costs (potentially all the way to $0.00), and improved security.

You'll need to run the server code somewhere. The instructions below assume you're using a VPS or EC2. However, you could also run catsnap on your personal computer, or a managed service like [Heroku](http://www.heroku.com/). In any case, you'll need to create a postgres database for Catsnap to use. First-time Catsnap users will also need to [create an S3 bucket](http://docs.amazonwebservices.com/AmazonS3/latest/gsg/CreatingABucket.html).

----------8<-----------------

Deploying the 3.0 server is fairly straightforward. You'll need to set several environment variables:
* CATSNAP_API_KEY is a secret key the client and server share for authentication. It can be any string of characters.
* CATSNAP_AWS_ACCESS_KEY_ID and CATSNAP_AWS_SECRET_ACCESS_KEY: your AWS credentials.
* CATSNAP_BUCKET: the S3 bucket where you want catsnap to store images.
* CATSNAP_OWNER_ID: an openid provider that uniquely identifies you as the owner of this catsnap installation.
* CATSNAP_SECRET_KEY: a secret key to use when generating session identifiers.
* DATABASE_URL: a url Catsnap can use to connect to its postgres database.
* PORT: the port on which you want catsnap to listen.

Once you have your environment set up, [clone catsnap](https://git.erincall.com/ErinCall/catsnap) into a directory of your choice. Change to the catsnap directory and install its dependencies:

```
python setup.py install
```

Use yoyo-migrate to build the database:

```
yoyo-migrate apply migrations $DATABASE_URL
```

If you have an existing catsnap installation backed by dynamodb, there's a script for migrating your data into postgres. The script isn't installed by setup.py so you'll need to run it directly from its path:

```
scripts/one-time/migrate_dynamo_to_pg.py
```

Start the server with gunicorn:

```
gunicorn catsnap.app:app -b 0.0.0.0:$PORT -w 3
```

And you should be up and running.

Installing the client is even easier:

```
pip install catsnap==3.0.0b1
catsnap config
```

I'm aware of a couple minor issues, but I expect to have them fixed within the next few days. Please [let me know](mailto:hello@erincall.com) about any problems you run across (or even [send me a merge request](https://git.erincall.com/ErinCall/catsnap)), and happy catsnapping!
