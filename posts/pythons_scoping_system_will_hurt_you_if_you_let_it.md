Title: Python's Scoping System Will Hurt You If You Let It
DisqusId: 2
Slug: pythons-scoping-system-will-hurt-you-if-you-let-it
Posted: 2012-01-10T04:57:00Z
Tags:
    python
    harm reduction
So, I ran into a bit of Python's behavior that upset me. Here's some code that is wrong:

```
if some_function():
    x = 5
print x
```

Depending on the return value of `some_function`, this code may or may not execute successfully. Whether or not it happens to succeed, though: it's wrong. You have this exception sitting there, waiting to jump up and bite you. It is a problem waiting to happen. Python doesn't care, though! It will happily accept that code and let you catch the error in production. But why? Why doesn't Python just define a new lexical scope when entering an `if` or `for` block? Then it could tell you during compile-time that your code is wrong, and you could fix it before it ever screwed you up.

So, as is my wont, I groused on IRC, and the nice people there taught me some surprising and upsetting things about Python's scoping. It turns out it's not nearly that simple. Here, let's look at some closures.

`closures.rb`:

```
methods = []

(1..5).each do |x|
    methods.push(lambda { puts x })
end

methods.each do |method|
    method.call()
end
```

```
$ ruby ./closures.rb
1
2
3
4
5
```

`closures.pl`

```
my @subs;

for my $i ( 1 .. 5 ) {
    push @subs, sub {
        print "$i\n";
    }
}

for my $sub (@subs) {
    $sub->();
}
```

```
$ perl ./closures.pl
1
2
3
4
5
```

Okay, you get the point: closures. Bear with me for one more go-around with Python, though. `closures.py`:

```
from __future__ import print_function

functions = []

for i in xrange(1, 6):
    functions.append(lambda: print(i))

for function in functions:
    function()
```

```
$ python ./closures.py
5
5
5
5
5
```

Wait, what just happened? Where'd my nice sequence go?

It turns out that Python doesn't really do closures, exactly, at least not as I've understood them in the past. In many languages, including Perl and Ruby, a function _closes around_ its surrounding scope. The variables in the surrounding scope are imported into the function's own scope, with the values they held when the function was declared. In Python, on the other hand, a function _has access to_ its surrounding scope. The Python interpreter can't check for declaredness until runtime, because until then there's no way to know if the variable might be declared in the surrounding scope. For example, Python is totally ok with `go_mad.py`:

```
def go_mad():
    print i

i = 'b-a-n-a-n-a-s'

go_mad()
```

```
python ./go_mad.py
b-a-n-a-n-a-s
```

Even though `i` isn't declared until well after `go_mad`, it's perfectly ok with Python if `go_mad` accesses it. Is that an invitation to bugs, or what?

Ok, so we've seen why Python doesn't use compile-time scope checks to check for variables that may or may not be declared: it doesn't check scope until runtime. And we've seen why it doesn't check scope until runtime: a function recieves its scope from its surroundings.

What I haven't seen is _why_ a function receives its scope from its surroundings. What benefits does a Python programmer get that offset the cost? What do we gain in exchange for losing so much? Until I have an answer to that, here are some practices I'll be following to offset the dangers imposed by Python's scoping:

* Never define anything inside an if-block.
```
    x = None
    if some_function():
        x = 5
    print x
```
* In fact, declare a function's variables at its beginning, as recommended in Javascript.
* Delegate closures to extremely trivial functions, to force an earlier scope resolution. Here's a fixed version of @closures.py@:

```
from __future__ import print_function

functions = []

def generate_appender(val):
  return lambda: print(val)

for i in range(1, 6):
  functions.append(generate_appender(i))

for function in functions:
  function()
```

```
$ python ./closures.py
1
2
3
4
5
```

* In fact, I'll probably avoid closures entirely, barring a compelling reason to use them.
* Write my own dang language, if I'm so smart

