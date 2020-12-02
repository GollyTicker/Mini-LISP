<template>
  <p class="sub-title">Output</p>
  <textarea id="out-text" v-model="value"/>
</template>

<script>
export default {
  name: 'Result',
  el: "#out-text",
  data: () => {
    return {
      value: "<output>"
    }
  }
}
</script>

<style scoped>
#out-text {
  border: 2px white solid;
}
</style>
