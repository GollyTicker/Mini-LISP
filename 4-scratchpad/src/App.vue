<template>
  <div id="top-bar">
    <h1>MiniLISP Scratchpad</h1>
  </div>

  <div id="#top-bar">
    <Splitpanes
      class="default-theme"
      style="height: 100%; width: 100%;"
      vertical>

      <Pane min-size="20" max-size="80">
        <Editor />
      </Pane>

      <Pane min-size="20" max-size="80">
        <Result />
      </Pane>

    </Splitpanes>
  </div>
</template>

<script>
import Editor from './components/Editor.vue'
import Result from './components/Result.vue'
import { Splitpanes, Pane } from 'splitpanes'
import 'splitpanes/dist/splitpanes.css'
// import VueSplit from 'vue-split-panel' // added in main.js

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
  margin: 0px;
  /*margin-top: 60px;*/
  height: 100%;
}

html, body {
  height: calc(100% - 10px);
  width: calc(100% - 10px);
  margin: 0px;
  padding: 0px;
  border: 0px;
  font-size: 1.1em;
}

p.sub-title {
  font-size: 1.05em;
  font-weight: bolder;
  text-align: left;
}

body {
  color: #ecf0f2;
  background-color: #122617;
}

#content-bar {
  height:100%;
}

/* splitpanes */
.splitpanes.default-theme .splitpanes__pane {
  color: inherit;
  background-color: inherit; /* perhaps use sass here?? with Vue loader*/
  border: 2px black solid;
  padding: 5px 10px 5px 10px;
}

.default-theme.splitpanes--vertical>.splitpanes__splitter{
  width: 7px;
  background-color: #828687;
}

textarea {
  resize: none;
  color: inherit;
  background-color: inherit;
  font-size: 1.1em;
  width: calc(100% - 20px);
  height: calc(100% - 10px);
  float: left;
}

</style>
