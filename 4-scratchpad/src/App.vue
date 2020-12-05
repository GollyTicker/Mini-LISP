<template>
  <div class="box">
    <div id="top-bar" class="row flex-fixed">
      <h1>MiniLISP Scratchpad</h1>
    </div>

    <div id="content-bar" class="row flex-floating">
      <!-- make content-bar take up rest of the screen: #fillup
        https://stackoverflow.com/questions/90178/make-a-div-fill-the-height-of-the-remaining-screen-space
      -->
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

    <div id="footer-bar" class="row flex-fixed" style="height: 5px"> </div>

  </div>
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
  margin: 0px;
  height: 100%;
}

html, body {
  height: calc(100% - 10px);
  width: calc(100% - 10px);
  margin: 0px;
  padding: 0px;
  border: 0px;
  font-size: 1.05em;
}

body {
  color: #ecf0f2;
  background-color: #122617;
}

p.sub-title, span.sub-title {
  font-weight: bolder;
  text-align: left;
}

/*  #fillup */
.box {
  display: flex;
  flex-flow: column;
  height: 100%;
}

/*  #fillup DEBUG */
.box > .row {
  /* border: 1px dotted red;
  margin: 0.5px; */
}

/*  #fillup */
.box .row.flex-fixed {
  flex: 0 1 auto;
}

/*  #fillup */
.box .row.flex-floating {
  flex: 1 1 auto;
  height: auto;
}

/* splitpanes */
.splitpanes.default-theme .splitpanes__pane {
  color: inherit;
  background-color: inherit; /* perhaps use sass here?? with Vue loader*/
  border: 2px black solid;
  padding: 5px 10px 5px 10px;
}

.default-theme.splitpanes--vertical>.splitpanes__splitter {
  width: 6px;
  background-color: #000;
  border: 2px #828687 double;
}

.code-text {
  display: block;
  resize: none;
  width: calc(100% - 20px);
  height: calc(100% - 10px);
  float: left;
  font-size: 0.9em !important;
  border: 1.5px solid white;
  white-space: pre-wrap !important; /* override prism syntax highlighting */
}

</style>
