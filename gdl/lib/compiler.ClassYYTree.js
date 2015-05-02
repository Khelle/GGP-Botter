ClassYYTree = function(rootInstance) {
  this.root = rootInstance;
                                   
  /* Add Node */
  this.SetRoot = function(node) {
    this.root = node;
    return this;
  }; 
  
  /* Generate output code */
  this.GenerateCode = function() {
    code = this.root.GenerateCode(0);
    return code.substring(0, code.length-1);
  };
  
  return this; 
};

exports.getInstance = ClassYYTree;