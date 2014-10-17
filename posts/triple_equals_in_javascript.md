Title: Triple-Equals In Javascript
Posted: 2012-02-22T06:20:00Z
DisqusId: 7
Tags:
    javascript
    bugs
Slug: triple-equals-in-javascript
[Molly wants to know what `===` means in JavaScript](https://twitter.com/#!/mollyn/status/172187789637586945), so I thought I'd write it up, from as close to first principles as my brain can think about. Fun!

Most programming languages use the `=` operator for variable assignment [1]:

```  JavaScript
x = 5;
```

Now any time the variable `x` is referenced, the expression referencing it will operate on the value `5`. You'll recall this from algebra, of course:

```
x = 5
f(x) = 3 + x^2
```

That whole system evaluates to 28. If `x` were 3, or 17.2, it would evaluate to something else. Variables in software work the same way[2].

Of course, if the `=` sign means variable assignment, it could be ambiguous when we want to check if two variables are equal:

``` JavaScript
if ( x = 27 ) {
    do_a_thing();
}
```

Whuh-oh, did we mean to _assign 27 to x_, or did we mean to _check to see if x is already 27_? Most popular programming languages resolve the ambiguity by using a double-equals to check equality:

``` JavaScript
if ( x == 27 ) {
    do_a_thing();
}
```

So we have single-equals for variable assignments, and double-equals for equality comparison. How do we get up to triple-equals? Better put on your boots, we're heading into the mucky bits. Not the nice new sage ones--those are nice, Molly, by the way, I meant to mention--put on some old ones that you won't mind getting dirty.

Suppose you have a bit of text from a user. In JavaScript, and most languages, this bit of text is a _string_: a series of numbers, each of which represents a letter or other character. For example, if a user has typed "27" into a textbox, the computer will store it with three numbers: `50,55,0` [3]. The 50 is ASCII for "2"; the 55 makes 7, and the 0 means "that's all there is in this string."

Now, suppose you have this string that the user gave you, and you want to know if it's equal to the number 27. You might try comparing it:

``` JavaScript
if (string_from_user == 27) {
    do_a_thing();
}
```

This will, in fact, work just as you wanted it to. However, in many early languages, it would've been totally illegal. The compiler would yell at you that you can't compare strings to numbers, because what does that even mean? The string just is a bunch of numbers. How do you compare a bunch of numbers to a single number? I don't know.

JavaScript, however, has learned a few tricks since those early languages were all the rage. It knows how to perform _type coercion_. When you use `==` to check to see if a string is equal to a number, you're telling JavaScript that you want to know if the string's contents describe a number that is equal to your number. You're saying you want to compare your `27` to the user's "27", not to the machine's `50,55,0`.

Unfortunately, JavaScript's rules for type coercion are broken. As you recall from math class, one of [the very important rules for equality](http://en.wikipedia.org/wiki/Equivalence_relation) is that it needs to be _transitive_: if `a = b` and `b = c`, then `a = c`. However, because of the way JavaScript coerces types, this is not the case. Let's check it out, using the string `"0"` for our `a`, the number `0` for our `b`, and the string `""`--a string containing nothing at all--for our `c`.

```
"0" == 0
true

0 == ""
true

"0" == ""
false
```

Aaaaaa what just happened? Well, JavaScript gave us just enough rope to hang ourselves. A couple minutes ago, we earnestly wanted it to use the number represented by a string when comparing that string to a number. So when we compared `"0"` to `0`, it obligingly said "yep, they're equal."

Then we asked it to compare `0` to `""`. [4] "Sure," JavaScript told us, "those are both pretty no-ey. I'll say they're equal."

Finally, we asked it to compare `"0"` and `""`. Well, now there's no coercion necessary. These are both strings, so JavaScript compares their contents, without worrying about whether they're maybe representing the same thing. Their contents are totally not the same, so JavaScript tells us they're not equal.

So, for people who like their software to do what they expect, JavaScript also provides `===`. `===` performs no type coercion: if you try to compare any string to any number using `===`, it will tell you "not equal," no ifs ands or buts.

And that's why `===` is important, and why working with JavaScript makes Selena feel stabby.

[1] Some languages use other operators to avoid this whole froofaraw. Pascal, for example, uses `:=` for variable assignment.

[2] Except for all the ways they don't work the same way at all, which aren't interesting at this time.

[3] This is a filthy filthy lie. The internal representation is much more complicated. The complications are not interesting at this time, and the lie is at least one that points roughly toward the truth.

[4] Because of the lie I told you about the internal representation of strings, you might be tempted to think that JavaScript is comparing the number `0` to the 0 that is the only thing in the empty string. But remember, that was a lie; that's not what the string looks like inside at all. It might look like that, in some languages! It does not look like that in JavaScript. Good on you for thinking of it, though!
