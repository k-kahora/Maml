type byte = char

type compiler = {instructions: byte list; constants: Object.Obj.item list}

type bytecode = {instructions': byte list; constants': Object.Obj.item list}

let bytecode _compiler = {instructions'= []; constants'= []}

let new_compiler = {instructions= []; constants= []}

let compile _compiler _statements = Ok 10