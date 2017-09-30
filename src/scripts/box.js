;!(function(self) {
  "use strict";
  
  self.box = function(config) {
    Elm.Main.embed(document.querySelector(config.root || 'body'), Object.assign({
      id: 'search-box',
      label: "Label",
      placeholder: 'Click here and start typing'
    }, config || {}));
  };
  
}(window));