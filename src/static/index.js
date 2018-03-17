require("./styles/main.scss");

// inject bundled Elm app into div#main
var Elm = require("../elm/Main");
var elmApp = Elm.Main.embed(document.getElementById("main"));

elmApp.ports.sendToken.subscribe(token => {
  localStorage.setItem("token", token);
  sendTokenToElm(token);
});

const init = () => {
  const token = localStorage.getItem("token");
  sendTokenToElm(token);
};

const sendTokenToElm = token => {
  if (token) {
    elmApp.ports.receiveToken.send(token);
  }
};

init();
