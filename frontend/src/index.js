'use strict';

require('ace-css/css/ace.css');
require('font-awesome/css/font-awesome.css');

var {Elm} = require('./Main.elm');
var mountNode = document.getElementById('main');

var app = Elm.Main.init({node: mountNode});
