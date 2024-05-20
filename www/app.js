  // update the count when user types characters
  $(document).on('shiny:inputchanged', function(event) {
    if (event.name === 'describe_changes') {
      var maxLength = 200;
      var textArea = document.getElementById('describe_changes');
      if (textArea.value.length > maxLength) {
        textArea.value = textArea.value.substring(0, maxLength);
      }
      var typed = textArea.value.length;
      var remaining = maxLength - typed;
      document.getElementById('char_count').innerText = typed + '/' + maxLength + ' words';
    }
  });

$(document).ready(function() {
  const observer = new IntersectionObserver(function(entries) {
    if (entries[0].intersectionRatio > 0) {
      Shiny.setInputValue("screen_end_reached", true, { priority: "event" })
    }
  });
  
  observer.observe(document.querySelector("#end"));

  // textAreaInput characters control
  // Do not exceed 200 characters
  var maxLength = 200;
      var textArea = document.getElementById('describe_changes');
      textArea.setAttribute('maxlength', maxLength);
      var typed = textArea.value.length;
      var remaining = maxLength - typed;
      document.getElementById('char_count').innerText = typed + '/' + maxLength + ' words';
    });
