Title: Alternative Ways To Rescue In Ruby
Posted: 2014-09-11T11:00:00-0700
Tags:
    ruby
    software deleter
### Begin

If you've worked in Ruby much, you're probably familiar with using `begin`/`rescue`/`end` to catch exceptions. Did you know there are other ways to use rescue? It's true! You may be able to delete a bit of software by using inline or implicit rescues.

### Inline Rescue

You can catch exceptions from a single statement by putting a `rescue` at the end of the line:

```Ruby
def get_some_bread
  throw_out_stale_bread
  top_slice rescue nil
end
```

This doesn't create any new capabilities for you, it's just a terser syntax. This is exactly equivalent:

```Ruby
def get_some_bread
  throw_out_stale_bread
  begin
    top_slice
  rescue StandardError
    nil
  end
end
```

The big downside to an inline rescue is that there's no way to specify an exception class to rescue. Rescuing from everything is [almost always a bad idea](http://ischenko.blogspot.com/2005/01/exception-based-code-antipatterns.html); you're extremely likely to catch exceptions that you don't know how to handle and should've let bubble higher up. Remember, it's better for your program to fail and tell you why than for it to plow heedlessly ahead and do something incomprehensible.

### Rescuing whole methods

Another way to trim your code is by rescuing a whole method. It turns out methods have an implicit `begin`, so if all the logic in your method is wrapped in a begin/rescue, you can instead use the implicit begin:

```Ruby
def get_some_bread
  throw_out_stale_bread
  top_slice
rescue NoBreadLeft => e
  LOGGER.warn("buy more bread!")
  nil
end
```

Again, this is simply a shortened syntax. It is exactly equivalent to:

```Ruby
def get_some_bread
  begin
    throw_out_stale_bread
    top_slice
  rescue NoBreadLeft => e
    LOGGER.warn("buy more bread!")
    nil
  end
end
```

The caveat here is that you have to wrap the whole method in your catch. As with catching all exceptions, it's risky to catch exceptions from code you didn't know could raise them: your handler is likely to do the wrong thing, putting your program in a consistent state. Still, this can be useful for small methods.

### End

Inline rescues and implicit begins aren't gonna shake up the way you develop Ruby, but they can reduce clutter and save you a little space.
