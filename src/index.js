import Elm from './Main.elm'

var app = Elm.Main.init({
  node: document.getElementById('root'),
  flags: innerHeight
});

app.ports.scrollToID.subscribe(function (id) {
  document.getElementById(id).scrollIntoView({ behavior: 'smooth' })
})