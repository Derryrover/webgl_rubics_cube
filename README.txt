This readme has 3 parts:

1. run a simple build by executing command in file elm_make_as_js.bat
This assumes you have elm 0.18 installed (if not go to step 2)

2. This step runs elm build tools and package manager in docker
This step is optional if step one was succesfull (if you have globally installed elm 0.18)
To install the docker follow instructions in docker_readme

3. The webgl library used is adapted slightly
Execute the description below if
- the colorpicker (when you drag on the cube to make a move) is not working
- the webgl context crashes because of bufferoverflow




Update
elm-stuff\packages\elm-community\webgl\2.0.3\src\Native\WebGL.js

line 576
preserveDrawingBuffer: true

because next logic is needed for readPixels
canvas.getContext("experimental-webgl", {preserveDrawingBuffer: true});


commented out lines that contain: cache.buffers[entity.buffer.guid
lines:
449
453

var buffer; //= model.cache.buffers[entity.buffer.guid];

      if (!buffer) {
        buffer = doBindSetup(gl, entityType, entity.buffer);
        //model.cache.buffers[entity.buffer.guid] = buffer;
      }


these lines cached the triangle buffers
anyway these buffers are going to be recreated each time
so there will be infinitively alot cached,
causing a memory leak

https://groups.google.com/forum/#!topic/elm-discuss/NzUX9AEulxg
//*/