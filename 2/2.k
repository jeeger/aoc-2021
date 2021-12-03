data: 0:"test-input"
argument: { 0$(((x ? " ") - #x) + 1)#x }
command: {x[0]}
processed: {(command/:x) ,' (argument/:x)}
process: {a: x@1; c: *x; :[c~"f"; 0, a; c~"d"; a, 0; c~"u"; -a, 0; 0, 0]}
*/ +/ process/: processed data