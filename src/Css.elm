module Css exposing (..)

import String exposing (concat)


-----------------------------------------------------------------------------
-- CSS STYLES
-----------------------------------------------------------------------------


cssStyle : String
cssStyle =
    """
.card-outer {
  border: 1px solid grey;
  border-radius: 7px;
  display: inline-block;
  margin: 5px;
  padding: 3px 5px;
}
.card-outer:hover {
  border-color: blue;
  background-color: #777;
  cursor: pointer;
}
.selected-True {
  background-color: #555;
  border-color: red;
}
.proper-combination .selected-True {
  border-color: green;
}
.suit-mark {
}
.suit-mark:after {
  margin-left: 0.3em;
}
.spades:after {
  content: '♠';
}
.hearts:after {
  content: '♥';
  color: green;
}
.diamonds:after {
  content: '♦';
  color: red;
}
.clubs:after {
  content: '♣';
  color: blue;
}
.table-main {
    position: relative;
}
@media (min-width: 1200px) {
    .table-main {
        zoom: 120%;
    }
}
.mahjong-demand {
    position: absolute;
    top: 60px;
}
/*
.player-left {
    position: absolute;
    display: inline-block;
    left: 10px;
    top: 40%;
}
.player-right {
    position: absolute;
    display: inline-block;
    right: 10px;
    top: 40%;
}
.player-top {
    position: absolute;
    display: inline-block;
    top: 10px;
    left: 45%;
}
.player-bottom {
    position: absolute;
    display: inline-block;
    bottom: 10px;
    left: 45%;
}
.card-exchange {
    position: absolute;
    display: inline-block;
    top: 45%;
    left: 35%;
}
.card-exchange > div {
    display: inline-block;
    padding: 20px;
    margin: 5px;
    border: 1px solid black;
}
.card-exchange > div:hover {
    background-color: #ddd;
}
.cards-on-table {
    position: absolute;
    display: inline-block;
    top: 45%;
    left: 35%;
}
*/
.actual-player {
    border: 2px solid blue;
    padding: 5px;
}
.table-hand-owner {
    border: 2px solid yellow;
    padding: 5px;
}
"""



generateCss config =
    concat [ """

@import url(//fonts.googleapis.com/css?family=Lato);
@import url(https://fonts.googleapis.com/css?family=Roboto);

body, * {
  font-family: 'Lato', 'Roboto', sans-serif;
}


@media (min-width: 768px) {
  #side-menu {
     height: """, toString (config.windowSize.height - 51), """px;
  }

  #page-wrapper {
     min-height: """, toString (config.windowSize.height - 51), """px;
  }
}

@media (min-width: 768px) {
  .anonymous #page-wrapper {
    margin: 0;
  }
}



@media (max-width: 768px) {
  .navbar-collapse.collapsed {
    display: none;
  }
}


.browsehappy {
  margin: 0.2em 0;
  background: #ccc;
  color: #000;
  padding: 0.2em 0;
}

.btn + .btn {
  margin-left: 5px;
}

a:hover,
.anchor:hover {
  cursor: pointer;
}

tr.anchor:hover * {
  background-color: #eee;
}

nav.navbar-default {
  margin-bottom: 0;
}

.navbar-default a.navbar-brand:hover {
  color: #00417d;
}


.highlighted {
  background: yellow;
}

.fa.arrow {
  -webkit-transition: 0.5s ease all;
          transition: 0.5s ease all;
  -webkit-transform: rotate(90deg);
      -ms-transform: rotate(90deg);
          transform: rotate(90deg);
}

.fa.arrow.down {
  -webkit-transform: rotate(-90deg);
      -ms-transform: rotate(-90deg);
          transform: rotate(-90deg);
}


.row.header {
  padding-left: 30px;
  border-bottom: 1px solid #e7e7e7;
  margin-bottom: 15px;
  background: repeating-linear-gradient(45deg, white, white 10px, #f8f8f8 10px, #f8f8f8 20px);
}


@media(min-width:768px) {
  .row.header {
    margin-left: -30px;
    margin-right: -30px;
  }
}


.row.header h2 {
  font-weight: bold;
  margin-top: 10px;
  margin-bottom: 8px;
  font-size: 24px;
}


.nav > li > a, .nav > li > b {
  padding: 6px 12px;
}

.nav > li > b {
  color: #777;
  display: block;
}

.nav > li > a {
  padding-left: 20px;
  padding-right: 20px;
}

@media(min-width:768px) {
  .sidebar-small .nav > li > a {
    padding-left: 15px;
    padding-right: 15px;
  }
}

.sidebar ul li a.active {
  border-right: 3px solid;
}

.nav > li.last-in-group {
  border-bottom-color: #f8f8f8;
}

@media(min-width:768px) {
  .sidebar-small .nav > li {
    text-align: center;
  }

  .sidebar-small .nav > li.last-in-group {
    border-bottom-color: #e7e7e7;
  }

  .side-menu-toggler {
    border-top: 1px solid #ddd;
    border-bottom-width: 0;
  }
}

.nav > li.menu-header {
  padding-top: 12px;
}

.nav-pills > li > a {
  border-radius: 0;
}


@media (max-width: 992px) {
  .nav-pills > li {
    float: none;
  }

  .nav-pills > li + li {
    margin-top: 2px;
    margin-left: 0;
  }
}


h1, .h1, h2, .h2, h3, .h3 {
  color: #00417d;
}

.centered {
  position: fixed;
  top: 50%;
  left: 50%;
  /* bring your own prefixes */
  -webkit-transform: translate(-50%, -50%);
      -ms-transform: translate(-50%, -50%);
          transform: translate(-50%, -50%);
}

.centered-title {
  position: fixed;
  left: 50%;
  /* bring your own prefixes */
  -webkit-transform: translate(-50%, 0%);
      -ms-transform: translate(-50%, 0%);
          transform: translate(-50%, 0%);
}

.sidebar-nav {
  position: relative;
}

.form-inline > * {
  margin-right: 5px;
}
.form-inline label {
  padding-right: 5px;
}

.sidebar-nav .menu-collapse {
  position: absolute;
  left: 0;
  right: 0;
  bottom: 0;
  background-color: #eee;
}

.sidebar-nav ul li span {
  padding-left: 5px;
}

@media(min-width:768px) {
  .sidebar-small .sidebar {
    width: 50px;
  }

  .sidebar-small .menu-header,
  .sidebar-small a span {
    display: none;
  }

  .sidebar-small #page-wrapper {
    margin: 0 0 0 50px;
  }
}

.header {
  color: #333;
}

.title {
  font-weight: bold;
  text-align: right;
}


.chat-header ,
.table-options-header {
  font-weight: bold;
  padding: 10px;
  border-bottom: 1px solid #e7e7e7;
  margin-bottom: 10px;
}

.table-chat div {
  padding-bottom: 10px;
  padding-left: 10px;
  padding-right: 10px;
}

.table-main {
  padding: 10px;
  border: 1px solid green;
  background-color: #ccff99;
  border-radius: 10px;
  min-height: 400px;
}

""" ]


coverCss config =
    concat [ """
/*
 * Globals
 */

/* Links */
a,
a:focus,
a:hover {
  color: #fff;
}

/* Custom default button */
.btn-secondary,
.btn-secondary:hover,
.btn-secondary:focus {
  color: #333;
  text-shadow: none; /* Prevent inheritance from `body` */
  background-color: #fff;
  border: .05rem solid #fff;
}


/*
 * Base structure
 */

html,
body {
  height: 100%;
  background-color: #333;
}
body {
  color: #fff;
  text-align: center;
  text-shadow: 0 .05rem .1rem rgba(0,0,0,.5);
}

/* Extra markup and styles for table-esque vertical and horizontal centering */
.site-wrapper {
  display: table;
  width: 100%;
  height: 100%; /* For at least Firefox */
  min-height: 100%;
}
.site-wrapper > .site-wrapper {
  -webkit-box-shadow: inset 0 0 5rem rgba(0,0,0,.5);
          box-shadow: inset 0 0 5rem rgba(0,0,0,.5);
}
.site-wrapper-inner {
  display: table-cell;
  vertical-align: top;
}
.cover-container {
  margin-right: auto;
  margin-left: auto;
}

/* Padding for spacing */
.inner {
  padding: 2rem;
}


/*
 * Header
 */

.masthead {
  margin-bottom: 2rem;
}

.masthead-brand {
  margin-bottom: 0;
}

.nav-masthead .nav-link {
  padding: .25rem 0;
  font-weight: bold;
  color: rgba(255,255,255,.5);
  background-color: transparent;
  border-bottom: .25rem solid transparent;
}

.nav-masthead .nav-link:hover,
.nav-masthead .nav-link:focus {
  border-bottom-color: rgba(255,255,255,.25);
}

.nav-masthead .nav-link + .nav-link {
  margin-left: 1rem;
}

.nav-masthead .active {
  color: #fff;
  border-bottom-color: #fff;
}

@media (min-width: 48em) {
  .masthead-brand {
    float: left;
  }
  .nav-masthead {
    float: right;
  }
}


/*
 * Cover
 */

.cover {
  padding: 0 1.5rem;
}
.cover .btn-lg {
  padding: .75rem 1.25rem;
  font-weight: bold;
}


/*
 * Footer
 */

.mastfoot {
  color: rgba(255,255,255,.5);
}


/*
 * Affix and center
 */

@media (min-width: 40em) {
  /* Pull out the header and footer */
  .masthead {
    position: fixed;
    top: 0;
  }
  .navbar .masthead {
    position: initial;
  }
  .mastfoot {
    position: fixed;
    bottom: 0;
  }
  /* Start the vertical centering */
  .site-wrapper-inner {
    vertical-align: middle;
  }
  /* Handle the widths */
  .masthead,
  .mastfoot,
  .cover-container {
    width: 100%; /* Must be percentage or pixels for horizontal alignment */
  }
}

@media (min-width: 62em) {
  .masthead,
  .mastfoot,
  .cover-container {
    width: 42rem;
  }
}


/* custom */

.login-username {
    margin: auto;
}


.game-card {
    margin: 20px 10px;
    padding: 30px;
    min-height: 200px;
    border: 1px solid #666;
    border-radius: 5px;
    transition: all 0.5s;
}

.game-card:hover {
    cursor: pointer;
    border: 1px solid white;
    background-color: #222;
}

.add-table-button {
    padding: 20px;
    font-size: 48px;
}

.default-footer {
    padding: 32px;
    color: rgba(255,255,255,.5);
}

.thin {
    font-weight: 300;
    margin-bottom: 40px;
}

input.table-name {
    max-width: 300px;
    margin: auto;
}

.blur {
    position: fixed;
    top: 0;
    left: 0;
    z-index: 2;
    filter: blur(2px);
    background-color: #000;
    width: 100%;
    height: 100%;
}


.chat-header ,
.table-options-header {
  font-weight: bold;
  padding: 10px;
  border-bottom: 1px solid #e7e7e7;
  margin-bottom: 10px;
}

.table-chat div {
  padding-bottom: 10px;
  padding-left: 10px;
  padding-right: 10px;
}

.table-main {
  padding: 10px;
  border: 1px solid green;
  border-radius: 10px;
  width: 100%;
}

.game-table {
    display: table;
    width: 100%;
}

.game-table > div {
    display: table-row;
}

.game-table > div  > div {
    display: table-cell;
}

.full-game-row {
    column-span: all;
    width: 100%;
}

.game-buttons {
    margin-top: 10px;
    float: left;
}

.middle-table-row > div {
    vertical-align: middle;
    height: 200px;
}

.middle-table-row > .player-box {
    width: 25%;
}

.game-table .left-player,
.game-table .right-player {
    width: 25%;
}

""" ]
