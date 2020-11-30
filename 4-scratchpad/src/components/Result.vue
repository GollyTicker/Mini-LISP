<template>
  <p id="#out-text">{{ value }}</p>
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
