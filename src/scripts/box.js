;!(function(self) {
  "use strict";
  
  // only for IE
  if (typeof NodeList.prototype.forEach !== 'function') {
    NodeList.prototype.forEach = Array.prototype.forEach;
  }
  
  function selectText(elm) {
    elm.select();
  }
  
  function getSelectedText(elm) {
    // this one is used especially for a XUL environment
    if (!elm && document.commandDispatcher &&
      document.commandDispatcher.focusedElement) {
      elm = document.commandDispatcher.focusedElement;
    }
    if (elm && elm.setSelectionRange)
      return elm.value.substring(elm.selectionStart, elm.selectionEnd);
    else if (document.selection)
      return document.selection.createRange().text;
  }
  
  function setSelectedTextRange(elm, selectionStart, selectionEnd) {
    if (elm.setSelectionRange) {
      elm.focus();
      elm.setSelectionRange(selectionStart, selectionEnd);
    }
    else if (elm.createTextRange) {
      var range = elm.createTextRange();
      range.collapse(true);
      range.moveEnd('character', selectionEnd);
      range.moveStart('character', selectionStart);
      range.select();
    }
  }
  
  function setCaretToEnd(elm) {
    setSelectedTextRange(elm, elm.value.length, elm.value.length);
  }
  
  function setCaretToStart(elm) {
    setSelectedTextRange(elm, 0, 0);
  }
  
  function setCaretToPos(elm, pos) {
    setSelectedTextRange(elm, pos, pos);
  }
  
  function getCaretPosition(elm) {
    if (typeof elm.selectionStart != "undefined")
      return elm.selectionStart;
    else if (document.selection)
      return
    Math.abs(document.selection.createRange().moveStart("character", -1000000));
  }
  
  function selectString(elm, string) {
    var match = new RegExp(string, "i").exec(elm.value);
    if (match) {
      setSelectedTextRange(elm, match.index, match.index + match[0].length);
    }
  }
  
  function replaceSelectedText(elm, replaceString) {
    var isInput = isInList(elm.nodeName.toLowerCase(), "input,textarea");
    if (isInput && elm.setSelectionRange) {
      var selectionStart = elm.selectionStart;
      var selectionEnd = elm.selectionEnd;
      elm.value = elm.value.substring(0, selectionStart) + replaceString
        + elm.value.substring(selectionEnd);
      if (selectionStart != selectionEnd) // has there been a selection
        setSelectedTextRange(elm, selectionStart, selectionStart +
          replaceString.length);
      else // set caret
        setCaretToPos(elm, selectionStart + replaceString.length);
    }
//  else if (!isInput && elm.ownerDocument) {
//          elm.ownerDocument.defaultView.getSelection().deleteFromDocument() {
//  }
    else if (document.selection) {
      var range = document.selection.createRange();
      if (range.parentElement() == elm) {
        var isCollapsed = range.text == '';
        range.text = replaceString;
        if (!isCollapsed) { // there has been a selection
          //it appears range.select() should select the newly
          //inserted text but that fails with IE
          range.moveStart('character', -replaceString.length);
          range.select();
        }
      }
    }
  }
  function GetCaretPosition(ctrl) {
    var CaretPos = 0;   // IE Support
    if (document.selection) {
      ctrl.focus();
      var Sel = document.selection.createRange();
      Sel.moveStart('character', -ctrl.value.length);
      CaretPos = Sel.text.length;
    }
    // Firefox support
    else if (ctrl.selectionStart || ctrl.selectionStart == '0')
      CaretPos = ctrl.selectionStart;
    return (CaretPos);
  }
  
  function ReturnWord(text, caretPos) {
    var index = text.indexOf(caretPos);
    var preText = text.substring(0, caretPos);
    if (preText.indexOf(" ") > 0) {
      var words = preText.split(" ");
      return words[words.length - 1]; //return last word
    }
    else {
      return preText;
    }
  }
  
  function AlertPrevWord() {
    var text = document.getElementById("textArea");
    var caretPos = GetCaretPosition(text)
    var word = ReturnWord(text.value, caretPos);
    if (word != null) {
      alert(word);
    }
  }
  
  self.box = function(config) {
    const nodes = document.querySelectorAll(config.root);
    if (!nodes.length) {
      throw "You specified wrong query selector '" + config.root + "'";
    }
    nodes.forEach(function (node) {
      Elm.Main.embed(node, Object.assign({
        id: 'search-box',
        label: "Label",
        queryFields: [
          {
            field: 'name',
            label: 'Name',
            queryType: 'string'
          },
          {
            field: 'forename',
            label: 'Forename',
            queryType: 'string'
          },
          {
            field: 'surname',
            label: 'Surname',
            queryType: 'string'
          }
        ],
        placeholder: 'Click here and start typing'
      }, config || {}));
    });
  };
  
}(window));