# Smart Calendar

A smart calendar app that runs in your browser.

## Developing

### Development mode

First get React by running `npm install`. The `package.json` file is very
simple: it only has React stuff.

Then start the development server by `clj -M:shadow-cljs watch app`.

(The recommended way of using `npx shadow-cljs watch app` unfortunately didn't
work. This alternative way of running shadow-cljs is documented in [section
3.2.2 of the shadow-cljs manual](https://shadow-cljs.github.io/docs/UsersGuide.html#deps-edn).
It does not involve downloading shadow-cljs from node.)

Afterwards navigate to localhost:3000 in a browser.

### Code Formatting

Just use `zprint` in its default configuration. Run `clj -M:zprint -w
src/smartcal/core.cljs`.

### Unit Tests

Unit tests are run in the browser. It has hot reload just as the main code. Run
`clj -M:shadow-cljs watch test` and then navigate to localhost:3001 in a
browser. The test results are then displayed in the browser. One can also use
`clj -M:shadow-cljs watch app test` to do both.

### Release Build

To produce a release build, run `clj -M:shadow-cljs release app`.
