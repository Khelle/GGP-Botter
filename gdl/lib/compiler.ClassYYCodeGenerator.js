/* Declare CodeGenerator */
ClassYYCodeGenerator = function() {
  /* Generate Code */
  this.GetPattern = function(data) {
    var s;  
  
    if (typeof data === 'undefined' || data === null) {
      return '';
    } else if (data === false) {
      return '%cs';
    }

    switch (data.type) {
      case 'ATOM':            s = this.ParseAtom(data); break; 
      case 'FACT':            s = this.ParseFact(data); break; 
      case 'RULE':            s = this.ParseRule(data); break;  
      default:                s = '';      
    }
    return s;
  };
  
  /* Parsers */
  this.ParseAtom = function(data) {
    return '%d[0]';
  };

  this.ParseFact = function(data) {
    return '%name(%cs)';
  };

  this.ParseRule = function(data) {
    if (data.name === null) {
      return '%hs :- %cs';
    } else {
      return '%name :- %cs'
    }
  };
        
  return this;
};

exports.getInstance = ClassYYCodeGenerator;