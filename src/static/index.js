require("./styles/main.scss");

// inject bundled Elm app into div#main
var Elm = require("../elm/Main");
var elmApp = Elm.Main.embed(document.getElementById("main"));

elmApp.ports.sendToken.subscribe(function(token) {
  localStorage.setItem("token", token);
  sendTokenToElm(token);
});

function init() {
  const token = localStorage.getItem("token");
  sendTokenToElm(token);
}

function sendTokenToElm(token) {
  if (token) {
    elmApp.ports.receiveToken.send(token);
  }
}

init();
