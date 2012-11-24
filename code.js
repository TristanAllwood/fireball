var idx = 0;

var initialize = function () {
  console.log(ticks.length);
  render()
  window.setInterval(function() { step(); }, 100);
}

function render() {
  var cpos = ticks[idx];

  cpos.name;
  cpos.tixs;

  for(var i = 0 ; i < cpos.tixs.length ; i++) {
    var elem = $('#source-pos-'+ cpos.name + '-' + i);
    elem.toggleClass("source-pos-active", cpos.tixs[i] == 1);
  }
}

function step() {
  idx = (idx + 1) % ticks.length;
  render();
}


$(document).ready(initialize);
