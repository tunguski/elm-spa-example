module Css exposing (generateCss)

import String exposing (concat)


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
