<template>
  <textarea id="#edit-text" v-model="value"/>
</template>

<script>
export default {
  name: 'Editor',
  data: () => {
    return {
      value: "blabla"
    }
  }
}
</script>

<style scoped>
#edit-text {
}
</style>
