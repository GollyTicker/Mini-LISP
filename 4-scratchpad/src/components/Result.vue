<template>
  <p class="sub-title">Output</p>
  <textarea id="out-text" readonly v-model="value"/>
</template>

<script>
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

<style scoped>
#out-text {
  border: 2px white solid;
}
</style>
