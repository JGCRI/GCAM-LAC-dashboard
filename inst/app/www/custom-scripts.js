// Sends a message to show landing page when all plots are loaded
//
// It would be really great if the plots could be loaded while the loading
// screen is up, but that does not seem easily done. Basically, we would need
// to know the size of the container to render them in initially, and then
// somehow after that set their update renderPlot() call to have the width and
// height be auto.
// See here for more: https://github.com/rstudio/shiny/issues/1409
$(document).on('shiny:sessioninitialized', function(event) {
  Shiny.onInputChange("setupComplete", false);
  console.log('init');

  var t = 0;

  $('#dashboard-waterTabset').on('DOMSubtreeModified', function() {
    // Check if the map image has loaded
    img = $('#dashboard-waterTabset').find('img');
    msg = "image has length: ";
    if(img.length) {
      msg += img.length;
      if(img[0].currentSrc) {
        msg += " and a source of length: ";
        msg += img[0].currentSrc.length;
      }
    }
    // console.log(msg);
    if(img.length) {// && img[0].currentSrc) {
      t += 1;

      if(t == 2) {
        // send message to Shiny
        Shiny.onInputChange("setupComplete", true);
        console.log("sent message");
      }
    }

    // Because I don't know how to do this with Shiny:
    $('#dashboard-waterTabset').children().first().attr('class', 'col-sm-');
  });

});


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
