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

### Code Formatting

Run `clj -M:zprint -w src/smartcal/core.cljs`.

### Release Build

To produce a release build, run `clj -M:shadow-cljs release app`.
