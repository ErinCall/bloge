Title: And The Asset Pipeline Is So Enticing!
DisqusId: 9
Posted: 2012-03-04T04:59:00Z
Tags:
    rails
    heroku
Slug: and-the-asset-pipeline-is-so-enticing
*UPDATE*: I fixed it! I had a directory `assets/` with some non-site-related assets in it--layered image files, etc.--and had therefore put "assets" in my `.slugignore`. Apparently, Heroku was applying this to `app/assets/` as well.

I'm having a bear of a time upgrading this site[1] from rails 3.0 to 3.2. The bulk of the engine work was done upstream (thanks upstream, upstream rules) but I can't get `application.css` to compile on Heroku. It's working locally, but on production no matter what I do, I get exceptions on pageload:

```
A ActionView::Template::Error occurred in posts#index:
 application.css isn't precompiled
```

Initially it wouldn't compile at all; per [Heroku's troubleshooting tips](http://devcenter.heroku.com/articles/rails3x-asset-pipeline-cedar#troubleshooting) I added `config.assets.initialize_on_precompile = false` to config/application.rb . Now when I push to heroku, the output log appears to be successfully precompiling my assets:

```
$ git push heroku master
Counting objects: 17, done.
Delta compression using up to 4 threads.
Compressing objects: 100% (9/9), done.
Writing objects: 100% (9/9), 1.23 KiB, done.
Total 9 (delta 7), reused 0 (delta 0)

-----> Heroku receiving push
-----> Ruby/Rails app detected
-----> Installing dependencies using Bundler version 1.1.rc.7
       Running: bundle install --without development:test --path vendor/bundle --binstubs bin/ --deployment
       Fetching gem metadata from https://rubygems.org/.......
{{snip bundle install output}}
       Your bundle is complete! It was installed into ./vendor/bundle
       Cleaning up the bundler cache.
-----> Writing config/database.yml to read from DATABASE_URL
-----> Preparing app for Rails asset pipeline
       Running: rake assets:precompile
-----> Rails plugin injection
       Injecting rails_log_stdout
       Injecting rails3_serve_static_assets
-----> Discovering process types
       Procfile declares types      -> web
       Default types for Ruby/Rails -> console, rake, worker
-----> Compiled slug size is 20.7MB
-----> Launching... done, v30
```

But I still get `application.css isn't precompiled`.

I've tried manually precompiling my assets:

```
$ RAILS_ENV=production bundle exec rake assets:precompile
/Users/andrewlorente/.rvm/rubies/ruby-1.9.2-p290/bin/ruby /Users/andrewlorente/.rvm/gems/ruby-1.9.2-p290@opinions/bin/rake assets:precompile:all RAILS_ENV=production RAILS_GROUPS=assets

$ git add public/assets
$ git commit -m "precompiled assets"
$ git push heroku master
```

But it has no effect. Perplexingly, I still see `Running: rake assets:precompile` in the Heroku slug compilation output, even though in this case I have a `public/assets/manifest.yml` and [the Heroku docs say I should see](http://devcenter.heroku.com/articles/rails3x-asset-pipeline-cedar) `Detected manifest.yml, assuming assets were compiled locally`.

It almost looks like Heroku is expecting/putting `manifest.yml` somewhere other than `public/assets`, but I don't have much evidence to support that. I hope it's that, though, 'cause that would be a super-easy fix.

Uh the upside of this is I'm not sure I would've looked into the asset pipeline if I hadn't run into this trouble, and it turns out it's super-awesome 'cause you can just write sass and coffeescript and the framework will compile and concatenate and minify them. <3

[1] At the time this entry was written, this blog was powered by Rails. This is no longer the case, but I've retained the entry for posterity.
