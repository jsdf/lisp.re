var input = document.getElementById("input");
var inputrow = document.getElementById("inputrow");
var screen = document.getElementById("screen");

var keysdown = new Set();
document.addEventListener("keydown", function(event) {
  keysdown.add(event.key);
  if (keysdown.has("Control") || keysdown.has("Meta")) {
    return;
  }
  input.focus();
});

document.addEventListener("keyup", function(event) {
  keysdown.delete(event.key);
});

var history = [];
var history_pos = 0;
var history_current_input = "";

function get_history() {
  var to_get = history.length - history_pos;
  var history_text = history[to_get];
  return history_text || history_current_input;
}

function write_out(text) {
  var textEl = document.createElement("pre");
  textEl.textContent = text + "\n";
  screen.insertBefore(textEl, inputrow);
  screen.scrollTop = screen.scrollHeight;
}

function read_in() {
  var in_text = input.value;
  input.value = "";
  history.push(in_text);
  write_out(in_text);
  return in_text;
}

function set_line_text(text) {
  input.value = text;
}

function clamp(val, min, max) {
  return Math.max(Math.min(val, max), min);
}

function await_input(handle_input) {
  input.addEventListener("keydown", function(event) {
    switch (event.key) {
      case "Enter":
        history_pos = 0;
        write_out(handle_input(read_in()));
        break;
      case "ArrowUp":
        if (history_pos == 0) {
          history_current_input = input.value;
        }
        history_pos = clamp(history_pos + 1, 0, history.length);
        set_line_text(get_history());
        event.preventDefault();
        break;
      case "ArrowDown":
        history_pos = clamp(history_pos - 1, 0, history.length);
        set_line_text(get_history());
        event.preventDefault();
        break;
    }
  });
}

// LOL
console.log = function(text) {
  write_out(text);
};

exports.write_out = write_out;
exports.await_input = await_input;
