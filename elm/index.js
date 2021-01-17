import { Elm } from "./Main.elm";

// Get all elm nodes
const elmNodes = document.querySelectorAll(".elm");

// Initialize Elm on each elmNode
elmNodes.forEach((node) => {
  const app = Elm.Main.init({
    node,
    flags: getFlags(node.dataset.flags),
  });

  app.ports.copyLink.subscribe(() => {
    const copyText = document.querySelector('#link').textContent;
    const textArea = document.createElement('textarea');
    textArea.textContent = copyText;
    document.body.append(textArea);
    textArea.select();
    document.execCommand("copy");
    textArea.remove();
  });

  app.ports.copySecret.subscribe(() => {
    const copyText = document.querySelector('#secret').textContent;
    const textArea = document.createElement('textarea');
    textArea.textContent = copyText;
    document.body.append(textArea);
    textArea.select();
    document.execCommand("copy");
    textArea.remove();
  });
});

// Parse the JSON from IHP or return null if there is none
function getFlags(data) {
  return data ? JSON.parse(data) : null;
}