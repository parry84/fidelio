import { Elm } from "./Main.elm";

require("material-components-web-elm/dist/material-components-web-elm.js");
require("material-components-web-elm/dist/material-components-web-elm.css");

function getFlags(data) {
  return data ? JSON.parse(data) : null;
}

function initializeWidgets() {
  const elmNodes = document.querySelectorAll(".elm");
  elmNodes.forEach((node) => {
    const app = Elm.Main.init({
      node,
      flags: getFlags(node.dataset.flags),
    });
    
    app.ports.copyLink.subscribe(copySubscription('#link'));
    app.ports.copySecret.subscribe(copySubscription('#secret'));
  });
}

const copySubscription = (selector) => () => {
  const copyText = document.querySelector(selector).textContent;
  const textArea = document.createElement('textarea');
  textArea.textContent = copyText;
  document.body.append(textArea);
  textArea.select();
  document.execCommand("copy");
  textArea.remove();
}

window.addEventListener("load", (event) => {
  initializeWidgets();
});

// Initializes Elm on Turbolinks transition
document.addEventListener("turbolinks:load", (e) => {
  initializeWidgets();
});
