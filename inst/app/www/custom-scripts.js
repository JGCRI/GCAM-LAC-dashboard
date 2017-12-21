// This recieves messages of type "disable-element" from the server.
Shiny.addCustomMessageHandler("disable-element",
  function(elementID) {
    element = document.getElementById(elementID);
    if (element.classList.contains('selectized')) {
      element.selectize.disable();
    } else {
    element.disabled = true;
    }
  }
);

Shiny.addCustomMessageHandler("enable-element",
  function(elementID) {
    element = document.getElementById(elementID);
    if (element.classList.contains('selectized')) {
      element.selectize.enable();
    } else {
      element.disabled = false;
    }
  }
);
