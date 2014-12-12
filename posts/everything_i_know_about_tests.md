Title: Everything I Know About Testing Computer Programs
Posted: 2014-06-10T15:30:00-0700
Tags:
    testing
In an email, Rachel King asked a fairly open-ended question about writing tests:

> I'm looking for guides on how to write tests. Trying to get better at debugging...

Since everything I know about testing is stuff I just picked up along the way, I don't know of any good guides off the top of my head. I started writing up what I do know, but when I realized I had a pretty sizeable document on my hands, I thought I'd make it a blog post instead. Let's get to it!

----------8<-----------------

### Unit tests and integration tests

I think of these two things as the ends of a spectrum. A _unit test_ acts on a single logical unit--ideally a [pure function](https://en.wikipedia.org/wiki/Pure_function)--and shows that it works the way it's supposed to, in isolation. An _integration test_ acts on a large swathe of the code at once, showing that all the pieces work together harmoniously. In general, moving toward the unit end of the spectrum will make a test easier to understand and debug, while moving toward the integration end will provide stronger assurances about the system's overall reliability.

A lot of people will shout about how integration testing is better than unit testing, or vice versa. They're wrong. Both have benefits, and a well-tested codebase will include tests that lie all along the spectrum.

#### Behavioral Testing, Functional Testing, Acceptance Testing, etc.

There are various categories or families of testing that've found homes in people's hearts. The way I've defined unit/integration testing above (which I guess I should emphasize are _not_ really the standard definitions), all these families fall at one point or another on the unit/integration spectrum.

#### Strength and precision of a test

The _strength_ of a test is a qualitative measure of its assurance that the codebase works correctly. The _precision_ of a test is a qualitative measure of the insight it provides when it fails. In general, a test's strength is inversely proportionate to its precision. A test that hits one unit of code with one input is very precise: a failure in that test says a lot about what's wrong with the code. However, it will only fail under very specific circumstances. A test that exercises a whole integrated stack of code with a variety of data is very strong: if there are any problems at all, it's likely to find them. However, a failure doesn't reveal much about where to start debugging.

Given a choice between only very strong tests and only very precise tests, I'd take the strong tests. The ease with which tests can be debugged is only relevant if they can actually reveal a problem. In non-hypothetical situations, though, I want both.

### Mocks, stubs, spies, and their ilk

The easiest code to unit-test is code that takes inputs and returns outputs and has no side-effects. Sometimes, though, one must write code composed mostly, or entirely, of side-effects. One way to test such code is by letting it run its side-effects and then check that they happened the way they should. This can be clunky, though, so another solution is to use a mocking library. Mocks lift out parts of the tested system and replace them with objects that behave in predefined ways, to isolate the interesting logic. They are super neat! There is a risk, though: if the mock objects don't behave the way their real counterparts do, the result will be a passing test suite on a broken codebase.

Stubs and spies are particular types of mock object. I think spies, in particular, are under-emphasized by mocking frameworks. A spy acts just like the real object it replaces--which dodges the inaccurate-interface problem--but also records everything that was done to it. Then the test can make assertions about what happened without having to spend time duplicating the interface it's spying on.

### The test suite should pass cleanly every time

The point of the test suite is to show that the code works right; consequently the ideal is to have an ironclad correspondence between "test suite does/doesn't pass" and "code does/doesn't work." It isn't possible to get 100% of this ideal on a nontrivial system, but it makes sense to strive toward it. The closer the test suite gets to the ideal, the more it can become an integral part of the development process. Some developers even use tools that watch their working directory for changes to files and automatically run the corresponding tests. If the test suite is unreliable, though, running it all the time will quickly become more annoying than helpful.

If a programming team is exceptionally diligent, the full test suite will succeed every time it's run, on every commit in the repo. This lets tools like [git-bisect](http://git-scm.com/book/en/Git-Tools-Debugging-with-Git) quickly track down regressions.

If the team is just regular diligent, the full test suite will succeed every time it's run on master. Tests that fail intermittently (because of race conditions, say) mean that every test failure requires careful examination to see if it's legitimate before normal debugging can even start.

If the normal state of the test suite is to have failing tests, it's terribly easy to overlook a "legitimate" failure. Failing tests on master are only slightly less alarming than errors on production, because the former can so easily turn into the latter.

#### The test suite should be fast

Although opinions differ on how fast comprises "fast," every development strategy that embraces testing agrees that the test suite must run quickly. This dovetails with the requirement that the suite passes reliably: the faster a suite runs, the more often one can run it; the more often one runs one's tests, the more utility they're providing.

### Altering The Implementation To Make It Easier To Test

This is a controversial topic. Programmers I respect have argued that the implementation is the important thing, so if accommodations must be made, they should be made by the tests. Rails creator David Heinemeier Hansson famously [came out against test-driven design](http://david.heinemeierhansson.com/2014/test-induced-design-damage.html) not long ago. I disagree, though: in my experience, practices that make code easier to test also make it easier to maintain and extend in general. Take DHH's example of the Rails controller architecture being decoupled from ActiveRecord: to me that sounds great, because now if a maintainer has cause to use some database-interactor other than ActiveRecord, it'll be easy.

### How to write a test

Ok, so I set out to write a post on writing tests, and wrote nearly a thousand words just on background. Let's get to the meat!

#### Write the tests first: "Test-Driven Development"

Most of the time, I follow the TDD maxim: "write a test, watch it fail, make it pass." Assuming I know how I want the system to work, I take that knowledge and encode it in a test. Now there's a test that can run over and over to see if the system is working yet. Hopefully, the test is invokable with a single command that executes quickly (again, for vague values of "quickly"), and shows clearly how the system falls short.

#### Write the implementation first: "Spiking"

Purists will claim that the tests must always be written first. In real life, I often know generally what I want the system to do, but don't know precisely how it should happen. At that point, writing some implementation can help get my thoughts in order and show me where the pitfalls and difficulties will lie, which gives me clues about how the code will need to work. Once I have that, I can go back and fill in the blank spaces in the tests. Often, those tests will show flaws in the early work, and it has to be rewritten. [Plan To Throw One Away](http://c2.com/cgi/wiki?PlanToThrowOneAway) comes to the fore when spiking.

There's a pitfall here that "write a test and watch it fail" is designed to avoid. If you never actually see a test fail, you don't know for sure that it _can_ fail. Tests are just code, and code can have bugs. Make sure you see your new tests fail at least once after a spike. I like to delete or drastically neuter the spiked code for a single run of the suite.

The most common strength-reducing bug is failing to write your test in such a way that the test framework can find it. The naming conventions are one of the first things you should learn about a new test framework.

#### Start with integration, then add unit tests

This is a mere personal preference, but it works for me. I write an integration test that encodes my ideas about my eventual goal (or the goal laid out in the task system). As I try to make it pass, I get ideas about how to break my solution into smaller units. I write those units one at a time, with unit tests driving that process. Once they look right, I come back to my initial integration test to show that each piece connects correctly to its neighbors.

### How to add tests to untested code

Testing legacy code is a real challenge. If it doesn't have tests it probably wasn't written in ways that make it easy to test--at the least, ease-of-testing certainly won't have been on the author's mind. Depending on how old the code is, it might not even be clear how it's supposed to work; I've dealt with legacy systems whose only specification was the current behavior of the code.

One rule of thumb that's worked out all right for me has been to not worry about unit testing legacy code. Since it presumably interacts with as much of the environment as the author felt like using, trying to break the code into unit-testable chunks exposes you to all sorts of subtle regressions. Get some integration tests in place for the most obvious aspects of its design before changing anything.

If the code is _buggy_ legacy code, adding integration tests will probably mean adding tests that fail if the bugs are fixed. Don't worry about it: this doesn't constitute adding bugs or compounding them. Counter-intuitively, it actually makes them easier to fix, because the test already exists; only the assertions need to be changed. Backing away in fear of an erroneous test just leaves future maintainers with the same quandary.

### Tests are your friends

In closing, I want to reiterate that tests aren't just a good idea in the long run, they're the fastest way to develop in the short run too. Yes, like any tool, they take some time to learn to use effectively. But the investment pays off. A good unit test provides an extremely fast "write code/see if it works" cycle, and optimizations in that cycle pay off more than anywhere else.
