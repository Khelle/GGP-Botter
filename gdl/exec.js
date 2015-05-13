var exec = require('child_process').exec;

var inPath  = process.argv[2];
var outPath = process.argv[3];

exec('node gdl.js ' + inPath + ' > ' + outPath, function(error, stdout, stderr) {
    if (error !== null) {
    	console.log('exec error: ' + error);
    	return;
    }

   	console.log('DONE');
});
