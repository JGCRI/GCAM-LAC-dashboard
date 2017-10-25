// This recieves messages of type "disable-element" from the server.
Shiny.addCustomMessageHandler("disable-element",
  function(elementID) {
    document.getElementById(elementID).disabled = true;
  }
);

Shiny.addCustomMessageHandler("enable-element",
  function(elementID) {
    document.getElementById(elementID).disabled = false;
  }
);
