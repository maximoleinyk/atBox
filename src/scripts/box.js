(function (root, factory) {
  "use strict";
  
  if (typeof define === 'function' && define.amd) {
    define([], factory);
  } else if (typeof exports === 'object') {
    module.exports = factory();
  } else {
    root.box = factory();
  }
  
}(this, function () {
  "use strict";
  
  // only for IE
  if (typeof NodeList.prototype.forEach !== 'function') {
    NodeList.prototype.forEach = Array.prototype.forEach;
  }
  
  function getCaretPosition(input) {
    if (typeof input.selectionStart !== "undefined") {
      return input.selectionStart;
    } else if (document.selection) {
      return;
    }
    Math.abs(document.selection.createRange().moveStart("character", -1000000));
  }
  
  return function (config) {
    const nodes = document.querySelectorAll(config.root);
    const onChange = config.onChange || function () {
    };
    const onEnter = config.onEnter || function () {
    };
    const queryFields = config.queryFields || [];
    const normalizedQueryFields = queryFields.map(function (queryField) {
      if (queryField.fieldType !== 'enum' || queryField.fieldType === 'enum' && !queryField.values) {
        queryField.values = [];
      }
      return queryField;
    });
    const autoSuggest = typeof config.autoSuggest === 'undefined' ? true : !!config.autoSuggest;
    
    if (!nodes.length) {
      throw "You specified wrong query selector '" + config.root + "'";
    }
    
    nodes.forEach(function (node) {
      const component = Elm.Main.embed(node, Object.assign({
        id: 'search-box',
        label: "Label",
        value: node.value || '',
        queryFields: normalizedQueryFields,
        placeholder: 'Click here and start typing'
      }, config || {}, {
        autoSuggest: autoSuggest
      }));
      
      component.ports.getCursorPosition.subscribe(function () {
        const input = document.querySelector(config.root + ' input');
        const caretPosition = getCaretPosition(input);
        component.ports.setCursorPosition.send(caretPosition);
      });
      
      component.ports.emitData.subscribe(function (output) {
        onChange(JSON.parse(output));
      });
      
      component.ports.emitDataOnEnterKey.subscribe(function (translatorOutput) {
        onEnter(JSON.parse(translatorOutput))
      });
    });
  };
  
}));