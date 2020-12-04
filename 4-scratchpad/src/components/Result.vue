<template>
  <div class="box">
    <div class="row flex-fixed">
      <p class="sub-title">Output</p>
    </div>
    <div class="row flex-floating">
      <textarea id="out-text" readonly v-model="value"/>
    </div>
  </div>
</template>

<script>

// how to integate ajax: https://vuejsdevelopers.com/2017/08/28/vue-js-ajax-recipes/

export default {
  name: 'Result',
  el: "#out-text",
  data() {
    return {
      value: "<output>"
    }
  },
  methods: {
    asyncDisplayResult() {
      this.value = this.value + " *"
    }
  },
  mounted() {
    setInterval(() => {
      this.asyncDisplayResult()
    }, 400)
  }
}

</script>
