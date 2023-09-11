# SDFx
A Signed Distance Field art tool built with Clojure, WebGL, and HTMX.

Signed Distance Fields and the operations you can perform on them allow you to create complex and cool shapes which make for really cool art pieces; particularly if you can plot them with a pen plotter (I'm partial to AxiDraw). To that end, I've made this tool to make it fun and easy to build SDFs.

![](./examples/screen-recording.mov)
![](./examples/smooth-cube.svg)

## Getting Started
Since this is a tool for me, I'm not too fussed about distribution. However, it's actually pretty easy to get started.

### Build from Source
If you're comfortable with Clojure, you can do the following:
 - `git clone https://github.com/adam-james-v/sdfx.git`
 - `cd sdfx`
 - `clojure -T:build uber` which should build the uberjar `target/SDFx-0.0.1.jar`
 - run SDFx with `java -jar target/SDFx-0.0.1.jar`. This command will start the server and print the port it's using.
 - Head to `localhost:THE_PORT_SPECIFIED` and have fun!

### Run the Jar
Otherwise, you can
 - [download the jar](https://github.com/adam-james-v/sdfx/releases/download/001/SDFx-0.0.1.jar).
 - `cd` into that directory and
 - run the build with `java -jar SDFx-0.0.1.jar`. This will start the server and print the port it's using.
 - Head to `localhost:THE_PORT_SPECIFIED` and get creative!
