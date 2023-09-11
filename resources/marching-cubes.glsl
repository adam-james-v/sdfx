const int GRID_SIZE = 256;
const float CELL_SIZE = 1.0 / float(GRID_SIZE);
vec2 segments[GRID_SIZE * GRID_SIZE * 2]; // space for 2 segments per cell
int segmentCount = 0;

// values will be a 1D array with access helpers to make it effectively 2D
const int TOTAL_SIZE = GRID_SIZE * GRID_SIZE;
float values[TOTAL_SIZE];

int getIndex(int x, int y) {
    return y * GRID_SIZE + x;
}

void setValue(int x, int y, float value) {
    values[getIndex(x, y)] = value;
}

float getValue(int x, int y) {
    return values[getIndex(x, y)];
}

float sdf2D(vec2 p) {
  vec3 pp = vec3(p.x, p.y, 0.0);
  return mySdf(pp);
}

int getConfiguration(float tl, float tr, float br, float bl) {
  int config = 0;
  if(tl < 0.0) config |= 1;
  if(tr < 0.0) config |= 2;
  if(br < 0.0) config |= 4;
  if(bl < 0.0) config |= 8;
  return config;
}

vec2 interpolate(vec2 p1, vec2 p2, float v1, float v2) {
  float alpha = (0.0 - v1) / (v2 - v1);
  return mix(p1, p2, alpha);
}

void main() {
  // 1. Discretization and SDF Evaluation
  for(int i = 0; i <= GRID_SIZE; i++) {
    for(int j = 0; j <= GRID_SIZE; j++) {
      vec2 pos = vec2(float(i) * CELL_SIZE, float(j) * CELL_SIZE);
      setValue(i, j, sdf2D(pos));
    }
  }

  // 2. Extract contour segments
  for(int i = 0; i < GRID_SIZE; i++) {
    for(int j = 0; j < GRID_SIZE; j++) {
      //int config = getConfiguration(values[i][j], values[i+1][j], values[i+1][j+1], values[i][j+1]);
      int config = getConfiguration(getValue(i,j), getValue(i+1,j), getValue(i+1,j+1), getValue(i,j+1));

      vec2 topLeft = vec2(float(i) * CELL_SIZE, float(j) * CELL_SIZE);
      vec2 topRight = vec2(float(i + 1) * CELL_SIZE, float(j) * CELL_SIZE);
      vec2 bottomRight = vec2(float(i + 1) * CELL_SIZE, float(j + 1) * CELL_SIZE);
      vec2 bottomLeft = vec2(float(i) * CELL_SIZE, float(j + 1) * CELL_SIZE);

      // For simplicity, handling a subset of configurations here. Expand as needed.
      if (config == 1 || config == 14) {
        segments[segmentCount++] = interpolate(topLeft, bottomLeft, getValue(i,j), getValue(i,j+1));
        segments[segmentCount++] = interpolate(topLeft, topRight, getValue(i,j), getValue(i+1,j));
      } else if (config == 2 || config == 13) {
        segments[segmentCount++] = interpolate(topRight, bottomRight, getValue(i+1,j), getValue(i+1,j+1));
        segments[segmentCount++] = interpolate(topLeft, topRight, getValue(i,j), getValue(i+1,j));
      } else if(config == 3 || config == 12) {
        segments[segmentCount++] = interpolate(topLeft, bottomLeft, getValue(i,j), getValue(i,j+1));
        segments[segmentCount++] = interpolate(topRight, bottomRight, getValue(i+1,j), getValue(i+1,j+1));
      } else if (config == 4 || config == 11) {
        segments[segmentCount++] = interpolate(bottomRight, topRight, getValue(i+1,j+1), getValue(i+1,j));
        segments[segmentCount++] = interpolate(bottomRight, bottomLeft, getValue(i+1,j+1), getValue(i,j+1));
      } else if (config == 5) {
        segments[segmentCount++] = interpolate(topLeft, bottomLeft, getValue(i,j), getValue(i,j+1));
        segments[segmentCount++] = interpolate(topLeft, topRight, getValue(i,j), getValue(i+1,j));
        segments[segmentCount++] = interpolate(bottomRight, topRight, getValue(i+1,j+1), getValue(i+1,j));
        segments[segmentCount++] = interpolate(bottomRight, bottomLeft, getValue(i+1,j+1), getValue(i,j+1));
      } else if (config == 6 || config == 9) {
        segments[segmentCount++] = interpolate(topLeft, bottomLeft, getValue(i,j), getValue(i,j+1));
        segments[segmentCount++] = interpolate(bottomRight, bottomLeft, getValue(i+1,j+1), getValue(i,j+1));
      } else if (config == 7 || config == 8) {
        segments[segmentCount++] = interpolate(topLeft, bottomLeft, getValue(i,j), getValue(i,j+1));
        segments[segmentCount++] = interpolate(bottomRight, bottomLeft, getValue(i+1,j+1), getValue(i,j+1));
        segments[segmentCount++] = interpolate(bottomRight, topRight, getValue(i+1,j+1), getValue(i+1,j));
      } else if (config == 10) {
        segments[segmentCount++] = interpolate(topLeft, topRight, getValue(i,j), getValue(i+1,j));
        segments[segmentCount++] = interpolate(bottomRight, topRight, getValue(i+1,j+1), getValue(i+1,j));
        segments[segmentCount++] = interpolate(bottomRight, bottomLeft, getValue(i+1,j+1), getValue(i,j+1));
      }
    }
  }

  // You now have all segments. You'd typically sort or chain these segments to get a proper contour.

  // In a rendering scenario, you'd then decide how to visualize these segments.
  // For the purpose of sending data to JavaScript, you'd typically leverage a buffer or texture to
  // pass this data out, then read this buffer in JavaScript.
}
