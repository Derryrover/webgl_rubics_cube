Update
elm-stuff\packages\elm-community\webgl\2.0.3\src\Native\WebGL.js

line 576
preserveDrawingBuffer: true

because next logic is needed for readPixels
canvas.getContext("experimental-webgl", {preserveDrawingBuffer: true});