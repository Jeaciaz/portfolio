import Elm from './Main.elm'

var app = Elm.Main.init({
  node: document.getElementById('root'),
  flags: innerHeight,
})

app.ports.scrollToID.subscribe(function (id) {
  document.getElementById(id).scrollIntoView({ behavior: 'smooth' })
})

app.ports.toggleScrollLock.subscribe(function (isLocked) {
  document.documentElement.style.overflowY = isLocked ? 'hidden' : ''
})

addEventListener('unload', () => scrollTo(0, 0))
