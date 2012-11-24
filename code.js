var idx = 0;

var initialize = function () {
  addStatusControls();
  render();
  window.setInterval(function() { step(); }, 100);
}

function addStatusControls() {
  $("#source-tabs").append('<div id="controls"></div>');

  controls = $("#controls");

  controls.append('<div id="progress"><div id="progress-marker"></div></div>');
}

function render() {
  var previousPos = ticks[ idx == 0 ? 0 : idx - 1]
  var cpos = ticks[idx];

  if (previousPos.name != cpos.name) {
    $(".source").toggle(false);
    $(".source-tab").toggleClass("source-tab-active", false);
    $("#source-" + cpos.name).toggle(true);
    $("#source-tab-" + cpos.name).toggleClass("source-tab-active",true);
  }

  $("#progress-marker").css('margin-left', (idx * 100 / ticks.length) + '%');

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
