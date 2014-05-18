Title: Ruby Blocks Are Way More Complicated Than You Think
DisqusId: 16
Posted: 2012-11-22T05:42:00Z
Tags:
    ruby
Slug: ruby-blocks-are-way-more-complicated-than-you-think
I've been working with Ruby a lot lately, and it's impossible to do that very much without noticing how feature-rich the language is. Today I journeyed deep into the strange and wonderful world of blocks. Blocks are used heavily in Ruby, and although they look like the simple anonymous functions I'm used to in JavaScript, their semantics are [significantly more sophisticated](http://yehudakatz.com/2010/02/07/the-building-blocks-of-ruby/). Most relevant to us today is that a function that receives a block doesn't take it as an explicit parameter. Instead, it checks `block_given?` to see if the caller provided a block. In JavaScript we might write:

``` JavaScript
var call_me_when_you_get_there = function (where_to_go, callback) {
  go_to(where_to_go)
  if (callback !== undefined) {
    callback()
  }
}
```

In Ruby this plays very differently, using `block_given?` and `yield`:

``` Ruby
def call_me_when_you_get_there where_to_go
  go_to where_to_go
  if block_given?
    yield
  end
end
```

These two examples may look fairly similar at first blush. However, it's important to notice what keyword ruby has provided for moving program execution to the provided block, because it isn't just moving the program execution--it's yielding flow control. If you yield to a block that has the `return` keyword, it'll return from more than just the block--it'll return from the method where that block is defined.

```Ruby
def call_me_back
  if block_given?
    yield
  end
  puts "this string will not print"
end

def return_in_a_block
  call_me_back do
    return
  end
  puts "this string will not print, either"
end
```

Indeed, neither string prints:

```
>> return_in_a_block
=> nil
```

That is _intense_. No other language I know has that sort of flow-control sophistication. It's especially remarkable because blocks have such a prominent place in real-world Ruby. They aren't an arcane feature hidden away in the guts of the docs, they're the primary looping construct. That primacy of place led me into a fairly confusing situation today.

Ruby has a method called `define_method` that takes a method name and a block and, as you might expect, defines a method with the given name that executes the given block. For example, we could write a class with an `alamarain` method:

``` Ruby
class Chula
  define_method :alamarain do
    puts 'Move along home!'
  end
end
```

This example is a little degenerate; we could as easily have written `def alamarain`. However, `define_method` allows us wide latitude for metaprogramming--if circumstances dictate, we could use `define_method` to name our new method `count_to_four` or `then_three_more`, or any number of things.

However, since the methods we define with `define_method` are written in terms of a block, their context is a little different than it would be if we were simply writing them out by hand. We already saw that `return` will apply to the function that encloses our block, not the block itself. It turns out this applies to `yield` and `block_given?` as well. We can illustrate this by defining an identical method twice, once with `def` and once with `define_method`:

``` Ruby
class TwoTimes
  def defined_normally
    puts "a block was #{ block_given? ? '' : 'not ' }given"
  end

  define_method :defined_by_block do
    puts "a block was #{ block_given? ? '' : 'not ' }given"
  end
end
```

And the results:

```
>> TwoTimes.new.defined_normally do end
a block was given
=> nil
>> TwoTimes.new.defined_by_block do end
a block was not given
=> nil
```

If you've been following along closely, you see what's happening here: the `block_given?` invocation counts for the scope that encloses the block--in this case, the `class TwoTimes` declaration--not the block itself. So how can we use `define_method` to define a method that accepts a block? Well, it turns out `define_method` helpfully transforms a block that's given to its defined method into a proc, and passes that proc as an argument.

``` Ruby
class SmarterBlock
  define_method :defined_by_block do | &block |
    puts "a block was #{ block.nil? ? 'not ' : '' }given"
    block.call()
  end
end
```

This works the way we want:

```
>> SmarterBlock.new.defined_by_block do puts 'hello!' end
a block was given
hello!
=> nil
```

Honestly, I think this is more confusing than it should be. It's good that Ruby provides a lightweight anonymous-function syntax, and it's exciting that it provides this advanced flow control mechanism, but I'm not convinced they should be the same thing.
