import './main.css';
import { Main } from './Main.elm';

let windowSize = {
  width: window.innerWidth,
  height: window.innerHeight
};

Main.embed(document.getElementById('root'), {
  windowSize: windowSize
});
