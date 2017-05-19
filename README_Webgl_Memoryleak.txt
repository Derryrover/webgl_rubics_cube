In the resulting file called: rubics_cube.js
comment out lines that contain: cache.buffers[entity.buffer.guid
lines:
7785
7789

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

