import * as T from './index.js';

export function TransMit(canvas) {
  this.tm = T.launch(canvas)();
}

TransMit.prototype.evaluate = function(zone, text) {
  return T.evaluate(this.tm)(text)();
}

TransMit.prototype.clearZone = function(zone) {}

TransMit.prototype.setTempo = function(foreignTempo) {}

TransMit.prototype.preAnimate = function() {}

TransMit.prototype.animateZone = function(zone) {
  return T.animate(this.tm)();
}

TransMit.prototype.postAnimate = function() {}

export function exoLang(canvas) {
  return new TransMit(canvas);
}
