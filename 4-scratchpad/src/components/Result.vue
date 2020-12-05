<template>
  <div class="box">
    <div class="row flex-fixed">
      <p class="sub-title">Output</p>
    </div>
    <div class="row flex-floating">
      <code id="out-text" class="language-lisp code-text match-braces" contenteditable readonly>bla bla blasbla </code>
    </div>
  </div>
</template>

<script>

const output = function() { return document.getElementById("out-text"); }

const handleResponseText = function(responseText) {
  output().innerText = responseText
}

const handleError = function() {
  output().innerText = "Some internal error occured, Sorry! (server unreachable, unsupported characters, ...)"
}

const exportedObject = {
  name: 'Result',
  handleResponseText: handleResponseText,
  handleError: handleError
}

window.ResultComponent = exportedObject

export default exportedObject

</script>
