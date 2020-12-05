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

// how to integate ajax: https://vuejsdevelopers.com/2017/08/28/vue-js-ajax-recipes/

window.out_text = "<output>"

export default {
  name: 'Result',
  /*data() {
    return {
      value: window.out_text
    }
  },
  methods: {
    asyncDisplayResult() {
      return;
      //const str = (document.getElementById("edit-text") || {value:""}).value
      //this.value = this.value + " " + str.length
    }
  },
  mounted() {
    setInterval(() => {
      //this.asyncDisplayResult()
    }, 500)
  }*/
}

</script>
