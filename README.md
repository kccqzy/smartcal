# Smart Calendar

A smart calendar app that runs completely in your browser with no server needed.

There are already way too many Calendar apps that are free to use. On macOS and
iOS the Calendar app is built-in. On the web, Google Calendar is also free. Why
this?

*   One reason is that I want to build a calendar app inspired by the command
    line. Sure you could copy and rename and delete files using Finder or
    Explorer or Nautilus, but you can also do that just by typing commands into
    a terminal emulator. Many people would find that preferable over using the
    GUI. I want to experiment with the UI of a calendar app in that context,
    trying to see whether exposing a "command language" for calendar operation
    makes sense.

    This command language is not natural language even though at times it feels
    like natural language. The best way to describe it is that it prefers to use
    many keywords instead of symbols; it reads like SQL rather than Perl. Just
    like SQL, it has a definite syntax instead of just trying to reinterpret
    fuzzy natural language, because this is inherently ambiguous. As an analogy,
    again consider SQL; the SQL statements `select age from animals;` and
    `select cats from animals;` are both valid syntactically and feel like
    natural language. But anyone with a modicum of understanding of SQL knows
    that the natural language meaning of the second statement does not match
    that of the semantics in SQL; `select cats from animals;` should instead be
    `select * from animals where species = 'cat';`. And just like this, I intend
    to prioritize precision of meaning over naturalness of the language.

    Just like SQL, although the input is plain text, there is no need for the
    output to be restricted to plain text, like the majority of command-line
    programs. A `select` statement in SQL is plain text but it produces tabular
    results that are well suited for a GUI. Here, I also intend to make this
    calendar command language return results visible in a GUI. That's why this
    runs inside your browser, not your terminal emulator.

*   It purposefully does not support any granularity finer than day. Any
    calendar that manages my day in finer blocks just feels like
    micromanagement. I do not like that. If a calendar is for handling meetings,
    then yes, time granularity would be essential. But this calendar has no
    collaborative features of any kind. It is meant for you to manage your time
    and tasks.

*   In the future, I intend to have support for automatically deciding the exact
    dates for events. For example, you could tell the calendar to schedule a
    "Refactor this code" event *some time in the next week* and it will
    automatically figure out a day in the next week to place that event subject
    to constraints.

## Developing

### Development mode

Start the development server by `clojure -M:develop`.

(The recommended way of using `npx shadow-cljs` to use shadow-cljs unfortunately
didn't work. This alternative way of running shadow-cljs is documented in
[section 3.2.2 of the shadow-cljs
manual](https://shadow-cljs.github.io/docs/UsersGuide.html#deps-edn). It does
not involve downloading shadow-cljs from node.)

Afterwards navigate to localhost:3000 in a browser.

### Unit Tests

Unit tests are run in the browser. It has hot reload just as the main code. When
running `clojure -M:develop`, the unit tests are compiled to JS already. Just
navigate to localhost:3001 in a browser. The test results are then displayed in
the browser.

### Code Formatting

We use `zprint` in its default configuration. Run `clojure -M:fix` to run it on
the two (hard-coded) source files.

### Release Build

To produce a release build, run `clojure -M:release`.
