import type { Mat2D, Vec2 } from "./types";

export const identity = (): Mat2D => ({ a: 1, b: 0, c: 0, d: 1, e: 0, f: 0 });

export function degToRad(degrees: number) {
  return (degrees * Math.PI) / 180;
}

export function radToDeg(radians: number) {
  return (radians * 180) / Math.PI;
}

export function normalizeDegrees(degrees: number) {
  let value = degrees % 360;
  if (value > 180) value -= 360;
  if (value < -180) value += 360;
  return value;
}

export function multiply(left: Mat2D, right: Mat2D): Mat2D {
  return {
    a: left.a * right.a + left.c * right.b,
    b: left.b * right.a + left.d * right.b,
    c: left.a * right.c + left.c * right.d,
    d: left.b * right.c + left.d * right.d,
    e: left.a * right.e + left.c * right.f + left.e,
    f: left.b * right.e + left.d * right.f + left.f
  };
}

export function translation(x: number, y: number): Mat2D {
  return { a: 1, b: 0, c: 0, d: 1, e: x, f: y };
}

export function rotation(degrees: number): Mat2D {
  const radians = degToRad(degrees);
  const cos = Math.cos(radians);
  const sin = Math.sin(radians);
  return { a: cos, b: sin, c: -sin, d: cos, e: 0, f: 0 };
}

export function scale(scaleX: number, scaleY: number): Mat2D {
  return { a: scaleX, b: 0, c: 0, d: scaleY, e: 0, f: 0 };
}

export function composeTransform(x: number, y: number, degrees: number, scaleX = 1, scaleY = 1): Mat2D {
  return multiply(multiply(translation(x, y), rotation(degrees)), scale(scaleX, scaleY));
}

export function applyToPoint(matrix: Mat2D, point: Vec2): Vec2 {
  return {
    x: matrix.a * point.x + matrix.c * point.y + matrix.e,
    y: matrix.b * point.x + matrix.d * point.y + matrix.f
  };
}

export function invert(matrix: Mat2D): Mat2D {
  const det = matrix.a * matrix.d - matrix.b * matrix.c;
  if (Math.abs(det) < 1e-8) return identity();
  const inv = 1 / det;
  return {
    a: matrix.d * inv,
    b: -matrix.b * inv,
    c: -matrix.c * inv,
    d: matrix.a * inv,
    e: (matrix.c * matrix.f - matrix.d * matrix.e) * inv,
    f: (matrix.b * matrix.e - matrix.a * matrix.f) * inv
  };
}

export function distance(a: Vec2, b: Vec2) {
  return Math.hypot(a.x - b.x, a.y - b.y);
}

export function angleBetween(a: Vec2, b: Vec2) {
  return radToDeg(Math.atan2(b.y - a.y, b.x - a.x));
}

export function matrixRotationDegrees(matrix: Mat2D) {
  return radToDeg(Math.atan2(matrix.b, matrix.a));
}

export function lerp(a: number, b: number, t: number) {
  return a + (b - a) * t;
}

export function interpolateAngle(a: number, b: number, t: number) {
  return normalizeDegrees(a + normalizeDegrees(b - a) * t);
}

export function screenToRig(point: Vec2, scaleValue: number, offsetX: number, offsetY: number): Vec2 {
  return {
    x: (point.x - offsetX) / scaleValue,
    y: (point.y - offsetY) / scaleValue
  };
}
