
// pull in desired CSS/SASS files
require( './style.css' );

// inject bundled Elm app into div#main

var Elm = require( './Sandbox' );
Elm.Sandbox.embed( document.getElementById( 'main' ) );
