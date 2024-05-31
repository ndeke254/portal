$(document).ready(function() {
  const observer = new IntersectionObserver(function(entries) {
    if (entries[0].intersectionRatio > 0) {
      Shiny.setInputValue("screen_end_reached", true, { priority: "event" })
    }
  });
  observer.observe(document.querySelector("#end"));
  
  // check the number of words typed
  $(document).on('input', '#describe_changes', function() { 
    var maxLength = 50;
    var textArea = document.getElementById('describe_changes');
    var words = textArea.value.split(/\\s+/).filter(function(word) {
      return word.length > 0;
    });

    // If the word count exceeds the maximum, block further typing
    if (words.length > maxLength) {
      // Get the current position of the cursor
      var cursorPosition = textArea.selectionStart;

      // Trim the value to the first 50 words
      textArea.value = words.slice(0, maxLength).join(' ');

      // Reset the cursor position
      textArea.selectionStart = textArea.selectionEnd = cursorPosition;
    }
  });
});