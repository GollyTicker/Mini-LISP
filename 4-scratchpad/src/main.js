import { createApp } from 'vue'
import App from './App.vue'

const app = createApp(App)
app.mount('#app')

// for debug purposes
window._app = app
