<template>
  <h2>MiniLISP Scratchpad</h2>
  <Splitpanes
    class="default-theme"
    style="height: 80%; width: 95%; color: inherit;"
    vertical>

    <Pane min-size="20" max-size="80">
      EDITOR
      <Editor />
    </Pane>
    <Pane min-size="20" max-size="80">
      RESULT
      <Result />
    </Pane>

  </Splitpanes>
</template>

<script>
import Editor from './components/Editor.vue'
import Result from './components/Result.vue'
import { Splitpanes, Pane } from 'splitpanes'
import 'splitpanes/dist/splitpanes.css'


export default {
  name: 'App',
  components: {
    Editor,
    Result,
    Splitpanes, Pane
  }
}
</script>

<style>
#app {
  font-family: Avenir, Helvetica, Arial, sans-serif;
  -webkit-font-smoothing: antialiased;
  -moz-osx-font-smoothing: grayscale;
  text-align: center;
  margin-top: 60px;
  height: 100%;
}

body {
  color: #ecf0f2;
  background-color: #122617;
  border: 1px solid white;
}

html, body {
  height: 100%;
  width: 100%;
  margin: 0px;
}

</style>
