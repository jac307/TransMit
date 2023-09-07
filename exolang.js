import * as T from './index.js';

export function TransMit(canvas) {
  this.tm = T.launch(canvas)();
}

TransMit.prototype.evaluate = function(zone, text) {
  return T.evaluate(this.tm)(txt)();
}

TransMit.prototype.animate = function(zone) {
  return T.animate(this.tm)();
}

export function exoLang(canvas) {
  return new TransMit(canvas);
}
