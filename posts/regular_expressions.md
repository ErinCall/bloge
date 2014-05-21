Title: Regular Expressions
Posted: 2013-10-12T06:37:00Z
DisqusId: 21
Tags:
    ruby
    regular expressions
Slug: regular-expressions
My friend [Shawna Scott](http://shawnacscott.com/blog/) encountered a need for regular expressions for the first time recently. I went to link her to some information, but I couldn't find a single introduction online that satisfied me. That lack seems disastrous. Regular Expressions are so important that every major programming language provides an engine for executing them, and many embed that engine directly into the parent language's syntax. They're inescapable--yet no tutorial measures up. Let's see what I can do, shall we?

### What Are Regular Expressions?

Regular Expressions are a specialized programming language for describing some aspect of a body of text that is of interest to you. I want to emphasize that regular expressions are a whole and entire _programming language_. They are not Turing complete, and their abilities are limited to text processing. Nevertheless, they truly are a language in their own right.

Regular Expressions describe some body of text. It's true that they are often used for "matching" or "substituting" text, but those are functions of the engine that implements them. At its core, an expression describes a piece of text. This, I think, is an important concept! When you find yourself lost trying to read an expression, ask yourself, "What text does this describe?"

For example, consider one of the most ubiquitous and important expressions: `.*`. This expression describes a body of text that contains 0 or more characters--that is, it describes anything, from the collected works of Shakespeare to this very blog post. It might surprise you that "everything and anything" is a ubiquitous and important description! You might also feel a little confused about what those two nonalphanumeric characters have to do with your grocery list. Don't worry! All will become clear.

### How Do Regular Expressions Work?

Regular Expressions are composed of symbols that describe a piece of text in various ways. A symbol, in this case, might be a letter, a number, or something more exotic like our friend `.*` from before. Letters and numbers describe text very literally--they just describe text that contains themselves. The expression `A1` describes the text "A1", and that's it.

Most nonalphanumeric characters have special meanings. The symbol `.` describes any single character--"A", or "b", or maybe a literal ".". The symbol `*` modifies the symbol that preceded it: taken together, the two describe 0 or more of the preceding symbol. Can you see now why our friend `.*` from before describes everything? The `.` symbol describes any single character, and the `*` symbol modifies it to describe any number of repetitions of that.

Be sure you understand the difference between "any number of repetitions of _any_ single character" and "any number of repetitions of _a_ single character." The latter concept would describe "aaa" or "bbb" but not "abc". `.*` describes all three of those pieces of text. When the `*` symbol modifies another symbol, the modified symbol gets a chance to describe whatever it can, each time that it's able to.

There are many other symbols with special meaning in a Regular Expression, and I will not attempt to explain all of them here. However, I do want to briefly cover a handful more symbols that can significantly increase the power of your expressions.

* `(` and `)` enclose a group of symbols and allow you to apply modifiers to that whole sub-expression. Thus the expression `(ab)*` describes the text "ab", and the text "ababababab", but not "abba".
* `[` and `]` enclose a group of symbols and create a sub-expression that describes any one of those symbols at a time. These __character classes__ work like a more restrictive form of the symbol `.`.
* `?` modifies the preceding symbol--or, as you now know, sub-expression--so that it describes 0 or 1 occurrence of whatever it would normally describe. Thus the expression `under_?score` describes the text "under_score" or "underscore", but not "under____score".

There are many other symbols that do exciting and useful things, and I encourage you to consult the reference in your preferred programming language's documentation to learn about them.

### How Do I Use Regular Expressions?

As discussed in the introduction, every reputable programming language provides an engine for executing regular expressions, either in the standard library or in the syntax of the language itself. In addition, the Unix utility `grep` is a tool for using regular expressions on streams of text at the command line.

There are three tasks for which you'll typically use a Regular Expression while programming: checking whether a body of text _matches_ an expression, _extracting_ the subset of a body of text that is described by an expression, and _transforming_ or _substituting_ the portions of a body of text that are described by an expression.

### Checking Whether A Body Of Text Matches An Expression

When dealing with user input, you'll commonly want to make sure that the input conforms to some format. For example, here is some Ruby that verifies that some input seems to contain an American phone number:

``` Ruby
if user_input =~ /\(?\d{3}\)?[.\- ]?\d{3}[.\- ]?\d{4}/
  puts "Thanks for telling me your phone number!"
else
  puts "Hey, that doesn't look quite right..."
end
```

This regular expression uses some symbols that I didn't discuss earlier. Can you guess what they do? Don't worry if you can't, or you don't want to spend your time on it--the important thing is the Ruby code surrounding it. It takes an expression and uses it to check whether a body of text contains some substring described by that expression. Up until now, we've been dealing with this fairly abstract notion of "describing" some text. The Regular Expression engine provided by Ruby applies that abstract concept to a real-world problem.

### Extracting Part Of A Body Of Text

In the example above, we checked whether a user provided us with their phone number inside a larger body of text. That's great, but it would probably be better if we could actually extract that phone number from its surroundings. We don't have to change the expression we used--it still describes the text we want. Instead, we just need to change the way we're interacting with the Regular Expression engine:

``` Ruby
phone_number = /\(?\d{3}\)?[.\- ]?\d{3}[.\- ]?\d{4}/.match(user_input).to_s
nefarious_telemarketing_database.store_new_phone_number(phone_number)
```

Now we've gone beyond simply verifying that the text our expression describes is present. We've actually extracted it from its surroundings, so we can store it for later use.

### Transforming Or Substituting A Body Of Text

Suppose that instead of operating a soulless call center, we're cautious custodians of our users' data. We've decided that, in order to help safeguard them, we're going to X out any phone numbers from our log files. That way, those jerks from the last example can't get ahold of it and steal our users' data...

```
sanitized_line = log_line.gsub(/\(?\d{3}\)?[.\- ]?\d{3}[.\- ]?\d{4}/, 'XXX-XXX-XXXX')
log_file.write(sanitized_line)
```

Once again we've used the same expression to describe the text we're interested in. The only difference is how we've used the Regular Expression engine to interact with that description.

### Wrapping Up

The examples above only cover Regular Expressions in a very shallow way. There are many aspects of their syntax and use that I didn't even attempt to cover here. However, I hope that I've given you the tools you need to start getting value from intermediate tutorials or your language's reference guide.

If you'd like to tell me about errors, points of confusion, or other objections, please dive right on into the comments. Of course, if you think I've done a good job, I'd love to hear that too.
