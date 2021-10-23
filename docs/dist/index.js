import Elm from './Main.js'

var isVisited = localStorage.getItem('__visited') !== null

var app = Elm.Main.init({
  node: document.getElementById('root'),
  flags: { innerHeight, innerWidth, isVisited },
})

app.ports.scrollToID.subscribe(function (id) {
  document.getElementById(id).scrollIntoView({ behavior: 'smooth' })
})

app.ports.toggleScrollLock.subscribe(function (isLocked) {
  document.documentElement.style.overflowY = isLocked ? 'hidden' : ''
})

app.ports.setVisited.subscribe(function () {
  isVisited = true
  localStorage.setItem('__visited', true)
})

addEventListener('unload', () => {if (!isVisited) scrollTo(0, 0)})
