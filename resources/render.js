const sdfSrcContainer = document.querySelector("#sdf-src-container");

let canvas;
let gl;
let ext;

// Mouse and Keyboard Events for controlling the SDF render
let mouseDown = false;
let ctrlDown = false;
let lastMouseX = null;
let lastMouseY = null;

function handleKeyDown(event) {
  if (event.key == "Shift") {
    ctrlDown = true;
  }
}

function handleKeyUp(event) {
  if (event.key == "Shift") {
    ctrlDown = false;
  }
}

window.addEventListener('keydown', handleKeyDown);
window.addEventListener('keyup', handleKeyUp);

let rotationAngleZ = 0.0; // in radians
let rotationAngleScreenX = 0.0
let translationX = 0.0;
let translationY = 0.0;
let zoom = 1.0;

// Named event listeners

function handleWebGLContextLost(event) {
    event.preventDefault();
}

function handleWheelEvent(event) {
    event.preventDefault();
}

function handleMouseDownEvent(e) {
    mouseDown = true;
    lastMouseX = e.clientX;
    lastMouseY = e.clientY;
}

function handleMouseUpEvent() {
    mouseDown = false;
}

function handleWheelZoom(e) {
    const zoomAmount = e.deltaY * 0.01;
    zoom += zoomAmount;
}

function handleMouseMoveEvent(e) {
    if (!mouseDown) {
        return;
    }

    const deltaX = e.clientX - lastMouseX;
    const deltaY = e.clientY - lastMouseY;

    if (ctrlDown) {
        rotationAngleZ += deltaX * 0.5;
        rotationAngleScreenX += deltaY * 0.5;
    }

    if (!ctrlDown) {
        translationX += deltaX * 0.005;
        translationY += deltaY * 0.005;
    }

    lastMouseX = e.clientX;
    lastMouseY = e.clientY;
}

// Init context function

function initContext() {
    canvas = document.querySelector("#canvas");
    gl = canvas.getContext("webgl2", {
      preserveDrawingBuffer: true
    });

    ext = gl.getExtension("WEBGL_lose_context");

    if (!gl) {
        alert("WebGL is not available on your browser.");
    }

    // Add event listeners
    canvas.addEventListener("webglcontextlost", handleWebGLContextLost, false);
    canvas.addEventListener("webglcontextrestored", initContext, false);
    canvas.addEventListener('wheel', handleWheelEvent, false);
    canvas.addEventListener('mousedown', handleMouseDownEvent, false);
    canvas.addEventListener('mouseup', handleMouseUpEvent, false);
    canvas.addEventListener('wheel', handleWheelZoom, false);
    canvas.addEventListener('mousemove', handleMouseMoveEvent, false);
}

// Define vertices for a quad. This quad is what the fragment shader result is rendered to
const quadVertices = new Float32Array([
  -1.0,  1.0,  // Vertex 1
  -1.0, -1.0,  // Vertex 2
   1.0,  1.0,  // Vertex 3
   1.0,  1.0,  // Vertex 4 (repeat Vertex 3)
  -1.0, -1.0,  // Vertex 5 (repeat Vertex 2)
   1.0, -1.0   // Vertex 6
]);

// The first chunk of the fragment shader code.
// This contains the implementations for the SDF functions. That is, the primitives and transformations
// it is not complete on its own and must be combined with:
// 1. the SDF code (taken from a hidden div, populated by the backend)
// 2. the second part of the fragment shader code which has the raymarcher, sets the scene, and renders it with the main() function

// SDF function name conventions used:
// - any primitive starts with the prefix 'sd' and is CamelCased eg. sdBox
// - any transform starts with the prefix 'op' and is CamelCased eg. opUnion, opTranslate
// this convention just comes directly from Inigo Quilez's code on his website: https://iquilezles.org/articles/distfunctions/
// note that not everything is a 1:1 copy from that site, for example the transforms are implemented differently in this code.

const fragCodeA = `#version 300 es

precision highp float;

uniform vec2 iResolution;
uniform float u_rotationAngleZ;
uniform float u_rotationAngleScreenX;
uniform float u_translationX;
uniform float u_translationY;
uniform float u_zoom;
out vec4 outputColor;

const int MAX_STEPS = 100;

vec3 lightDir = normalize(vec3(-0.3, -1.0, -0.4));

float sdBox( vec3 p, vec3 b ) {
  vec3 q = abs(p) - b;
  return length(max(q,0.0)) + min(max(q.x,max(q.y,q.z)),0.0);
}

float sdSphere(vec3 p, float r) {
  return length(p) - r;
}

vec2 opRevolve( vec3 p ) {
  return vec2( length(p.xz), p.y );
}

float sdPlane(vec3 p, vec3 n, float h) {
    // n must be normalized
    return dot(p, n) - h;
}

float sdWedge(vec3 p, float deg) {
    float theta = atan(p.z, p.x); // angle in radians
    if (theta < 0.0) theta += 2.0 * 3.14159265; // map theta to [0, 2PI]

    float rad = radians(deg); // convert deg to radians
    float r = 1000.0;
    vec3 p1 = vec3(r*cos(rad), 0.0, r*sin(rad));
    vec3 n1 = vec3(r*-sin(rad), 0.0, r*cos(rad));
    vec3 p2 = vec3(r, 0.0, 0.0);
    vec3 n2 = vec3(0.0, 0.0, -1.0);

    float d1 = sdPlane(p, n1, dot(n1, p1));
    float d2 = sdPlane(p, n2, dot(n2, p2));
    float dcap = max(-p.y, p.y - 1000.0); // assuming unit height cylinder

    if (theta > rad) {
        return length(max(vec2(d1, d2), 0.0)) - length(min(vec2(d1, d2), 0.0));
    }
    return dcap;
}

//vec2 opExtrudeAlong( vec3 p ) {
//  return vec2( length(p.xz), p.y );/
//}

float opExtrude( vec3 p, float sdf, float h ) {
  vec2 w = vec2( sdf, abs(p.z) - h);
  return min(max(w.x,w.y), 0.0) + length(max(w,0.0));
}

// https://iquilezles.org/articles/distfunctions2d/
// create 2D shape fns that take a 3D pt so that they can be viewed as-is

vec3 opSlice( vec2 p, float h ) {
  return vec3( p, h );
}

//vec3 opSlice( vec3 p, float h ) {
//  return vec3( p.xy, h );
//}

float sdCircle( vec2 p, float r ) {
  return length(p.xy) - r;
}

float sdCircle( vec3 p, float r ) {
  return opExtrude(p, sdCircle(p.xy, r), 0.001);
}

float sdPolygon( vec2 p, vec2 v[200], int num ) {
    float d = dot(p-v[0],p-v[0]);
    float s = 1.0;
    for( int i=0, j=num-1; i<num; j=i, i++ ) {
        // distance
        vec2 e = v[j] - v[i];
        vec2 w =    p - v[i];
        vec2 b = w - e*clamp( dot(w,e)/dot(e,e), 0.0, 1.0 );
        d = min( d, dot(b,b) );
        // winding number from http://geomalgorithms.com/a03-_inclusion.html
        bvec3 cond = bvec3( p.y>=v[i].y,
                            p.y <v[j].y,
                            e.x*w.y>e.y*w.x );
        if( all(cond) || all(not(cond)) ) s=-s;
    }
    return s*sqrt(d);
}

float sdPolygon( vec3 p, vec2 v[200], int num ) { return opExtrude(p, sdPolygon(p.xy, v, num), 1.0); }

// WIP: can I 'extrude' by just subtracting the dist of a 2D sdf?
float sdLine( in vec3 p, in vec3 a, in vec3 b, float sdf ) {
    vec3 pa = p-a, ba = b-a;
    float h = clamp( dot(pa,ba)/dot(ba,ba), 0.0, 1.0 );
    return length( pa - ba*h ) - sdf;
}

vec3 opTranslate( vec3 p, vec3 d) { return(p - d); }
vec2 opTranslate( vec2 p, vec3 d) { return(p - d.xy); }
vec2 opTranslate( vec2 p, vec2 d) { return(p - d); }

vec3 opRotateX( vec3 p, float theta) {
    theta = radians(theta);
    // Rotation about X-axis
    mat3 Rx = mat3(
        1.0, 0.0, 0.0,
        0.0, cos(theta), -sin(theta),
        0.0, sin(theta),  cos(theta)
    );
    return Rx * p;
}

vec3 opRotateY( vec3 p, float theta) {
    theta = radians(theta);
    // Rotation about Y-axis
    mat3 Ry = mat3(
        cos(theta), 0.0, sin(theta),
        0.0, 1.0, 0.0,
        -sin(theta), 0.0, cos(theta)
    );
    return Ry * p;
}

vec3 opRotateZ( vec3 p, float theta) {
    theta = radians(theta);
    // Rotation about Z-axis
    mat3 Rz = mat3(
        cos(theta), -sin(theta), 0.0,
        sin(theta), cos(theta), 0.0,
        0.0, 0.0, 1.0
    );
    return Rz * p;
}

vec3 opRotate( vec3 p, vec3 rs) {
    float thetaX = rs.x;
    float thetaY = rs.y;
    float thetaZ = rs.z;

    p = opRotateZ(p, thetaZ);
    p = opRotateY(p, thetaY);
    p = opRotateX(p, thetaX);
    return p;
}
vec2 opRotate( vec2 p, vec3 rs) {
  vec3 pp = opRotateZ( vec3(p, 0.0), rs.z);
  return pp.xy;
}
vec2 opRotate( vec2 p, float theta) {
  vec3 pp = opRotateZ( vec3(p, 0.0), theta);
  return pp.xy;
}

float opOnion( in float d, in float t ) {
  return abs(d)-t;
}

vec3 opRepetition( in vec3 p, in vec3 s ) {
  vec3 q = p - s*round(p/s);
  return q;
}

float opUnion( float d1, float d2 ) { return min(d1,d2); }

float opDifference( float d1, float d2 ) { return max(-d1,d2); }

float opIntersection( float d1, float d2 ) { return max(d1,d2); }

float opSmoothUnion( float d1, float d2, float k ) {
    float h = clamp( 0.5 + 0.5*(d2-d1)/k, 0.0, 1.0 );
    return mix( d2, d1, h ) - k*h*(1.0-h); }

float opSmoothDifference( float d1, float d2, float k ) {
    float h = clamp( 0.5 - 0.5*(d2+d1)/k, 0.0, 1.0 );
    return mix( d2, -d1, h ) + k*h*(1.0-h); }

float opSmoothIntersection( float d1, float d2, float k ) {
    float h = clamp( 0.5 - 0.5*(d2-d1)/k, 0.0, 1.0 );
    return mix( d2, d1, h ) + k*h*(1.0-h); }

// utils

float dot2( in vec2 v ) { return dot(v,v); }
float dot2( in vec3 v ) { return dot(v,v); }
float ndot( in vec2 a, in vec2 b ) { return a.x*b.x - a.y*b.y; }

mat3 rotationMatrix(vec3 axis, float angle) {
    float s = sin(angle);
    float c = cos(angle);
    float oc = 1.0 - c;

    return mat3(oc * axis.x * axis.x + c,           oc * axis.x * axis.y - axis.z * s,  oc * axis.z * axis.x + axis.y * s,
                oc * axis.x * axis.y + axis.z * s,  oc * axis.y * axis.y + c,           oc * axis.y * axis.z - axis.x * s,
                oc * axis.z * axis.x - axis.y * s,  oc * axis.y * axis.z + axis.x * s,  oc * axis.z * axis.z + c);
}

`;

const fragCodeB = `
vec3 normal(vec3 p) {
    vec2 e = vec2(0.0001, 0.0);
    return normalize(vec3(mySdf(p + e.xyy) - mySdf(p - e.xyy),
                          mySdf(p + e.yxy) - mySdf(p - e.yxy),
                          mySdf(p + e.yyx) - mySdf(p - e.yyx)));
}

struct OrthoCamera {
    vec3 position;
    vec3 forward;
    vec3 up;
    vec3 rightv;
    float left;
    float right;
    float bottom;
    float top;
};

OrthoCamera createOrthoCamera(vec3 pos, vec3 lookAt, float width, float height) {
    OrthoCamera cam;
    cam.position = pos;
    cam.forward = normalize(lookAt - pos);
    cam.up = vec3(0, 0, 1);  // assuming Z is up
    cam.rightv = normalize(cross(cam.up, cam.forward));

    // how to avoid gimbal lock and other flipping problems?
    cam.position = rotationMatrix(cam.rightv, clamp(u_rotationAngleScreenX, -87.0, 38.0) * -0.025) * pos;
    cam.forward = normalize(lookAt - cam.position);
    cam.up = vec3(0, 0, 1);  // assuming Z is up
    cam.rightv = normalize(cross(cam.up, cam.forward));

    cam.up = cross(cam.forward, cam.rightv);  // Re-orthogonalize up vector to ensure it's perpendicular

    cam.left = -width / 2.0;
    cam.right = width / 2.0;
    cam.bottom = -height / 2.0;
    cam.top = height / 2.0;

    return cam;
}

struct Ray {
    vec3 origin;
    vec3 direction;
};

const float panCoefficient = 0.4;
Ray getRay(OrthoCamera cam, vec2 uv) {
    // Use the adjusted UV and the camera's parameters to calculate the ray's origin.
    vec2 adjustedUV = vec2((uv.x - u_translationX * panCoefficient),
                            (uv.y + u_translationY * panCoefficient));
    vec3 rayOrigin = cam.position +
                     adjustedUV.x * (cam.right - cam.left) * cam.rightv +
                     adjustedUV.y * (cam.top - cam.bottom) * cam.up;

    return Ray(rayOrigin, cam.forward);
}


const float max_t = 1000.0;
const float min_d = 0.0001;

vec4 raymarch_simple_normal_colors(Ray ray) {
    float t = 0.0;
    for (int i = 0; i < MAX_STEPS; i++) {
        vec3 p = ray.origin + ray.direction * t;
        float d = mySdf(p);
        if (d < min_d) {
            vec3 n = normal(p);
            vec3 col = n*0.5+0.5;
            float gradLen = length(vec3(d - mySdf(p - ray.direction * 0.001)));
            float alpha = 1.0 - smoothstep(0.0, 0.2, gradLen);
            return vec4(col * alpha, alpha);
        }
        t += d;
        if (t > max_t) break;
    }
    return vec4(0.0);
}

// basic raymarcher that renders the SDF's edges
vec4 raymarch_edges(Ray ray) {
    vec4 col = vec4(0.0); // default color
    float t = 0.0;
    for(int i = 0; i < MAX_STEPS; i++) {
        vec3 p = ray.origin + ray.direction * t;
        float d = mySdf(p);

        if (d < min_d) { // we hit the surface
            vec3 n = normal(p);

            // Sample normals in neighboring points around p
            vec3 n1 = normal(p + 0.0075 * n);
            vec3 n2 = normal(p - 0.0075 * n);

            // Compare the normals using dot product
            float dotProd = dot(n1, n2);

            if(dotProd < 0.1) {  // threshold should be adjusted based on the specifics of the SDF
                col = vec4(1.0, 1.0, 1.0, 1.0); // edge color
            }

            break;
        }
        t += d;
        if(t > max_t) break;
    }
    return vec4(col);
}

// Helper function to convert HSV to RGB
vec3 hsv2rgb(vec3 c) {
    vec4 K = vec4(1.0, 2.0 / 3.0, 1.0 / 3.0, 3.0);
    vec3 p = abs(fract(c.xxx + K.xyz) * 6.0 - K.www);
    return c.z * mix(K.xxx, clamp(p - K.xxx, 0.0, 1.0), c.y);
}


vec4 raymarch_zColor(Ray ray) {
    vec4 col = vec4(0.0); // default transparent color

    float t = 0.0;
    for(int i = 0; i < MAX_STEPS; i++) {
        vec3 p = ray.origin + ray.direction * t;

        float d = mySdf(p);

        if (d < 0.00001) {
            vec3 n = normal(p);

            // Check if the normal is approximately Z-facing
            if(abs(n.z) > 0.95) { // 0.95 is a threshold, you can adjust it
                // Use the z value of the point to derive a color, for example:
                float hue = mod(p.z * 0.125, 360.0); // you can adjust scaling and modulation
                col = vec4(hsv2rgb(vec3(hue, 1.0, 1.0)), 1.0); // Assuming you have a function to convert HSV to RGB
            } else {
                col = vec4(0.0, 0.0, 0.0, 0.0); // Some other color for non Z-facing surfaces
            }

            return col;
        }

        t += d;
        if(t > max_t) break;
    }

    return col;
}

// Main function
void main() {
    vec2 uv = (gl_FragCoord.xy - iResolution * 0.5) / min(iResolution.y, iResolution.x);

    // Define and construct your camera
    vec3 camPos = vec3(50.0, 50.0, 50.0);

    camPos = opRotate(camPos, vec3(0.0, 0.0, -u_rotationAngleZ));
    vec3 camLookAt = vec3(0.0, 0.0, 0.0);
    float width = 8.0/u_zoom;
    OrthoCamera cam = createOrthoCamera(camPos, camLookAt, width, width);

    // Get ray for current pixel
    Ray ray = getRay(cam, uv);

    // Raymarch and fetch color
    vec4 col = vec4(0.0);
    if (normalCols == true) col += raymarch_simple_normal_colors(ray);
    if (contourCols == true) col += raymarch_zColor(ray);
    if (outlines == true) col += raymarch_edges(ray);

    outputColor = col;
}


`;

let animation;
let shaderProgram;
let fragmentShader;
let vertexShader;
let vertexBuffer;

function createShader(type, source) {
  const shader = gl.createShader(type);
  gl.shaderSource(shader, source);
  gl.compileShader(shader);

  if (!gl.getShaderParameter(shader, gl.COMPILE_STATUS)) {
    console.error('An error occurred compiling the shaders:', gl.getShaderInfoLog(shader));
    gl.deleteShader(shader);
    return null;
  }

  return shader;
}

function initialize() {
  // If shaderProgram already exists, delete the old program and shaders
  if (shaderProgram) {
    gl.deleteProgram(shaderProgram);
    shaderProgram = null;
  }

  // Create shader program
  shaderProgram = gl.createProgram();

  // Create a new vertex buffer if it doesn't exist
  if (!vertexBuffer) {
    vertexBuffer = gl.createBuffer();
    gl.bindBuffer(gl.ARRAY_BUFFER, vertexBuffer);
    gl.bufferData(gl.ARRAY_BUFFER, quadVertices, gl.STATIC_DRAW);
  }

  // Define vertex shader with aspect ratio adjustment
  const vertexShaderSource = `#version 300 es
    in vec2 coordinates;
    uniform float aspectRatio;
    void main() {
      gl_Position = vec4(coordinates.x, coordinates.y * aspectRatio, 0.0, 1.0);
    }`;
  const vertexShader = createShader(gl.VERTEX_SHADER, vertexShaderSource);

  // Define fragment shader using source from div
  let sdfSrcDiv = document.querySelector("#sdf-src");
  let sdfCode = sdfSrcDiv.innerHTML;
  let fragCode = `${fragCodeA}${sdfCode}${fragCodeB}`;
  const fragmentShader = createShader(gl.FRAGMENT_SHADER, fragCode);

  // Attach and link shaders to the program
  gl.attachShader(shaderProgram, vertexShader);
  gl.attachShader(shaderProgram, fragmentShader);
  gl.linkProgram(shaderProgram);

  if (!gl.getProgramParameter(shaderProgram, gl.LINK_STATUS)) {
    console.error('Unable to initialize the shader program:', gl.getProgramInfoLog(shaderProgram));
    return;
  }

  // After linking the shaders to the program, they can be safely deleted.
  gl.deleteShader(vertexShader);
  gl.deleteShader(fragmentShader);

  gl.useProgram(shaderProgram);
}

initContext();
initialize();

function setUniforms() {
  const iResolutionUniformLocation = gl.getUniformLocation(shaderProgram, 'iResolution');
  gl.uniform2f(iResolutionUniformLocation, canvas.width, canvas.height);

  // Get the position attribute location
  const position = gl.getAttribLocation(shaderProgram, 'coordinates');
  gl.enableVertexAttribArray(position);
  gl.vertexAttribPointer(position, 2, gl.FLOAT, false, 0, 0);

  // Get the rotation, translation, and zoom uniforms location and set their values
  const rotationAngleZUniformLocation = gl.getUniformLocation(shaderProgram, "u_rotationAngleZ");
  gl.uniform1f(rotationAngleZUniformLocation, rotationAngleZ);
  const rotationAngleScreenXUniformLocation = gl.getUniformLocation(shaderProgram, "u_rotationAngleScreenX");
  gl.uniform1f(rotationAngleScreenXUniformLocation, rotationAngleScreenX);
  const translationXUniformLocation = gl.getUniformLocation(shaderProgram, "u_translationX");
  gl.uniform1f(translationXUniformLocation, translationX);
  const translationYUniformLocation = gl.getUniformLocation(shaderProgram, "u_translationY");
  gl.uniform1f(translationYUniformLocation, translationY);
  const zoomUniformLocation = gl.getUniformLocation(shaderProgram, "u_zoom");
  gl.uniform1f(zoomUniformLocation, zoom);

  // Get the aspect ratio uniform location and set its value
  const aspectRatioUniform = gl.getUniformLocation(shaderProgram, 'aspectRatio');
  gl.uniform1f(aspectRatioUniform, canvas.width / canvas.height);
}

function draw() {
  gl.viewport(0, 0, canvas.width, canvas.height);
  gl.clearColor(0.0, 0.0, 0.0, 0.0);
  gl.clear(gl.COLOR_BUFFER_BIT | gl.DEPTH_BUFFER_BIT);

  gl.useProgram(shaderProgram);
  gl.bindBuffer(gl.ARRAY_BUFFER, vertexBuffer);

  const position = gl.getAttribLocation(shaderProgram, 'coordinates');
  gl.enableVertexAttribArray(position);
  gl.vertexAttribPointer(position, 2, gl.FLOAT, false, 0, 0);

  setUniforms();

  gl.drawArrays(gl.TRIANGLES, 0, 6);

  animation = requestAnimationFrame(draw);
}

// call initContext to grab the canvas and set up the GL context
initContext();
// call draw to render the SDF
draw();

function redraw() {
  cancelAnimationFrame(animation);
  initialize();
  draw();
}

// Redraw the triangle every time the window size changes
//window.addEventListener('resize', redraw);

// Watch for changes to the shader source code and redraw when there's a change
const observer = new MutationObserver(redraw);
observer.observe(sdfSrcContainer, { characterData: true, childList: true, subtree: true });

const sdfSrc = document.getElementById("sdf-src");
let sdfSrcEventListener = htmx.on("#sdf-src", "contentChanged");

function dispatchContentChanged() {
  const contentChangedEvent = new Event('contentChanged', {
    'bubbles': true,
    'cancelable': true
  });

  sdfSrc.dispatchEvent(contentChangedEvent);
}

// Editor
let editor;
function initEditor() {
  // Initialize CodeMirror
  const ed = document.getElementById('editor');
  editor = CodeMirror.fromTextArea(ed, {
    mode: 'clojure',
    smartIndent: true,
    electricChars: true,
    lineNumbers: true,  // show line numbers
    theme: 'nord',   // optional theme; you can choose others
    matchBrackets: true, // parentheses matching
    autoCloseBrackets: true
  });

  editor.setSize(ed.style.width, ed.style.height);

  // Editor Can inc/dec numbers at cursor
  editor.setOption("extraKeys", {
    "Ctrl-Up": function(cm) {
      incrementOrDecrement(cm, 1);  // increment
    },
    "Ctrl-Down": function(cm) {
      incrementOrDecrement(cm, -1); // decrement
    },
    "Shift-Ctrl-Up": function(cm) {
      incrementOrDecrement(cm, 10);  // increment
    },
    "Shift-Ctrl-Down": function(cm) {
      incrementOrDecrement(cm, -10); // decrement
    }
  });

  function incrementOrDecrement(cm, change) {
    // Get cursor position
    var cursor = cm.getCursor();

    // Find the start and end of the number around the cursor
    var start = cursor.ch;
    var end = cursor.ch;
    var lineContent = cm.getLine(cursor.line);

    while (start > 0 && isNumericOrNegativeSign(lineContent.charAt(start - 1), lineContent.charAt(start - 2))) {
      start--;
    }

    while (end < lineContent.length && isNumeric(lineContent.charAt(end))) {
      end++;
    }

    // Get the number
    var number = lineContent.slice(start, end);

    if (number) {
      var changedNumber = (parseInt(number) + change).toString();
      cm.replaceRange(changedNumber, CodeMirror.Pos(cursor.line, start), CodeMirror.Pos(cursor.line, end));

      // Keep the cursor at its position
      var cursorShift = changedNumber.length - number.length;
      cm.setCursor(CodeMirror.Pos(cursor.line, cursor.ch + cursorShift));
    }
  }

  // Helper function to check if a character is numeric
  function isNumeric(char) {
    return !isNaN(parseInt(char));
  }

  // Helper function to check if a character is numeric or a negative sign
  function isNumericOrNegativeSign(char, prevChar) {
    // Check if it's a numeric character
    if (isNumeric(char)) {
      return true;
    }

    // Check if it's a negative sign that's not preceded by another number (to ensure it's not a subtraction operation)
    return char === '-' && !isNumeric(prevChar);
  }

  const form = document.getElementById('render-settings');
  editor.on("change", dispatchContentChanged);
  form.addEventListener('change', dispatchContentChanged);


}

document.addEventListener("DOMContentLoaded", initEditor);

window.addEventListener('unload', function() {
    if (shaderProgram) {
        gl.deleteProgram(shaderProgram);
        shaderProgram = null;
    }

    if (shaderProgram2) {
        gl.deleteProgram(shaderProgram2);
        shaderProgram2 = null;
    }

    if (vertexBuffer) {
        gl.deleteBuffer(vertexBuffer);
        vertexBuffer = null;
    }

    if (vertexBuffer2) {
        gl.deleteBuffer(vertexBuffer2);
        vertexBuffer2 = null;
    }


    document.removeEventListener("DOMContentLoaded", initEditor);
    window.removeEventListener('keydown', handleKeyDown);
    window.removeEventListener('keyup', handleKeyUp);
    canvas.removeEventListener("webglcontextlost", handleWebGLContextLost);
    canvas.removeEventListener("webglcontextrestored", initContext);
    canvas.removeEventListener('wheel', handleWheelEvent);
    canvas.removeEventListener('mousedown', handleMouseDownEvent);
    canvas.removeEventListener('mouseup', handleMouseUpEvent);
    canvas.removeEventListener('wheel', handleWheelZoom);
    canvas.removeEventListener('mousemove', handleMouseMoveEvent);

    canvas = null;
    gl = null;
    canvas2 = null;
    gl2 = null;
});


function distance(a, b) {
    return Math.sqrt((a.x - b.x) * (a.x - b.x) + (a.y - b.y) * (a.y - b.y));
}

function chainPoints(points) {
  points = points.filter((_, index) => (index + 1) % 1 === 0);
  if (points.length === 0) return [];

  // Start with the first point
  let currentPt = points[0];
  let chainedPts = [currentPt];
  let remainingPts = [...points.slice(1)]; // Copy all the other points to the remainingPts array

  // This loop continues until there are no points left in remainingPts
  while (remainingPts.length > 0) {
    // Sort the remaining points based on their distance to the current point
    remainingPts.sort((a, b) => distance(a, currentPt) - distance(b, currentPt));

    // The closest point becomes the new current point
    currentPt = remainingPts[0];
    chainedPts.push(currentPt);

    // Remove the new current point from remainingPts
    remainingPts = remainingPts.slice(1);
  }

  //return simplify(chainedPts, 1.5, false);
  return chainedPts;
}

// Edge tracing algorithm for concave hull
function edgeTracing(points) {
  if (points.length < 3) {
    // Cannot form a polygon with less than 3 points
    return null;
  }

  let hull = [];
  let usedPoints = new Set();

  // Starting point (can be any point)
  let currentPoint = points[0];
  let startPoint = currentPoint;

  // Add start point to the hull and mark as used
  hull.push(startPoint);
  usedPoints.add(JSON.stringify(startPoint));

  while (true) {
    let closestPoint = null;
    let closestDistance = Infinity;

    // Find the closest point to the current point
    for (let point of points) {
      if (usedPoints.has(JSON.stringify(point))) {
        // Skip if the point has already been used
        continue;
      }

      let dist = distance(currentPoint, point);
      if (dist < closestDistance) {
        closestDistance = dist;
        closestPoint = point;
      }
    }

    if (closestPoint === null) {
      // No available points to connect, thus end
      break;
    }

    // Add the closest point to the hull and mark as used
    hull.push(closestPoint);
    usedPoints.add(JSON.stringify(closestPoint));

    // If we have returned to the start point, break
    //if (closestPoint.x === startPoint.x && closestPoint.y === startPoint.y) {
    //  break;
    //}

    // Update current point
    currentPoint = closestPoint;
  }

  return hull;
}


// Function to calculate the Euclidean distance between two points
function distance(A, B) {
  let dx = A.x - B.x;
  let dy = A.y - B.y;
  return Math.sqrt(dx * dx + dy * dy);
}

// DBSCAN algorithm
function dbscan(points, epsilon, minPts) {
  let clusters = [];
  let visited = new Set();
  let noise = new Set();

  function regionQuery(P) {
    return points.filter(Q => distance(P, Q) <= epsilon);
  }

  function expandCluster(P, neighbors, C) {
    C.push(P);
    visited.add(JSON.stringify(P));

    let index = 0;
    while (index < neighbors.length) {
      let point = neighbors[index];
      let pointStr = JSON.stringify(point);

      if (!visited.has(pointStr)) {
        visited.add(pointStr);
        let pointNeighbors = regionQuery(point);

        if (pointNeighbors.length >= minPts) {
          neighbors = neighbors.concat(pointNeighbors);
        }
      }

      if (!clusters.some(cluster => cluster.includes(point)) && !noise.has(pointStr)) {
        C.push(point);
      }

      index++;
    }
  }

  for (let point of points) {
    let pointStr = JSON.stringify(point);

    if (visited.has(pointStr)) continue;

    let neighbors = regionQuery(point);

    if (neighbors.length < minPts) {
      noise.add(pointStr);
    } else {
      let C = [];
      expandCluster(point, neighbors, C);
      clusters.push(C);
    }
  }

  return clusters;
}

function getPixelColor(x, y, width, data) {
  const index = (y * width + x) * 4;
  const r = data[index];
  const g = data[index + 1];
  const b = data[index + 2];
  const a = data[index + 3];

  if (a === 0 || (r === 0 && g === 0 && b === 0)) {
    return null;
  }

  return {r, g, b};
}

function getAdjacentPixels(x, y) {
  return [
    {x: x + 1, y},
    {x: x - 1, y},
    {x, y: y + 1},
    {x, y: y - 1},
  ];
}

function colorDistance(color1, color2) {
  return Math.sqrt(
    Math.pow(color1.r - color2.r, 2) +
    Math.pow(color1.g - color2.g, 2) +
    Math.pow(color1.b - color2.b, 2)
  );
}


function findContours(width, height, pixelData) {
  const contours = {};
  const visited = new Set();

  for (let y = 0; y < height; y++) {
    for (let x = 0; x < width; x++) {
      const key = `${x}_${y}`;

      if (visited.has(key)) continue;

      const color = getPixelColor(x, y, width, pixelData);

      // Skip if the color is not interesting
      if (color === null) continue;

      if (!contours[color]) {
        contours[color] = [];
      }

      let boundary = [];
      let stack = [{x, y}];

      while (stack.length > 0) {
        const point = stack.pop();
        const {x, y} = point;
        const pointKey = `${x}_${y}`;

        if (visited.has(pointKey)) continue;

        visited.add(pointKey);

        const adjacentPixels = getAdjacentPixels(x, y);
        let isBoundary = false;

        const SIMILARITY_THRESHOLD = 12;
        for (const adj of adjacentPixels) {
          const adjKey = `${adj.x}_${adj.y}`;
          if (adj.x < 0 || adj.x >= width || adj.y < 0 || adj.y >= height) {
            isBoundary = true;
            continue;
          }

          const adjColor = getPixelColor(adj.x, adj.y, width, pixelData);

          if (adjColor === null) {
            isBoundary = true;  // Add this line to mark it as boundary if neighbor is null
            continue;            // and continue to the next adjacent pixel
          }

          if (colorDistance(color, adjColor) < SIMILARITY_THRESHOLD) {
            stack.push(adj);
          } else {
            isBoundary = true;
          }
        }

        if (isBoundary) {
          boundary.push(point);
        }
      }

      if (boundary.length > 2) {
        boundary = edgeTracing(boundary);
        //boundary = simplify(boundary, 0.6, false);
        contours[color].push(boundary);
      }
    }
  }

  return contours;
}

function grabPixels() {
  ctx = canvas.getContext("webgl2");
  const width = canvas.width;
  const height = canvas.height;

  const pixels = new Uint8Array(width * height * 4);

  gl.readPixels(0, 0, width, height, gl.RGBA, gl.UNSIGNED_BYTE, pixels);
  const contours = findContours(width, height, pixels);

  const url = '/contours';
  htmx.ajax('POST', '/contours', {
    source: '#canvas',
    target: '#contour',
    event: "contour",
    swap: 'innerHTML',
    values: { body: { contours: JSON.stringify(contours),
                      sdfSrc: editor.getValue() }}
  });

  return contours;
}
