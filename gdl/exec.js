var exec = require('child_process').exec;

var cwd     = process.argv[2];
var inPath  = process.argv[3];
var outPath = process.argv[4];

exec('node gdl.js ' + inPath + ' > ' + outPath, {cwd: cwd}, function(error, stdout, stderr) {
    if (error !== null) {
    	console.log('exec error: ' + error);
    	return;
    }
});
