%token EDIT VIEW
%token HELP
%token QUIT
%token <string> TEXT
%token EOF

%start <Command.t> main

%%

let command :=
  | EDIT; addr = addr; <Command.Edit>
  | VIEW; addr = addr; <Command.View>
  | QUIT; { Command.Quit }
  | HELP; { Command.Help }

let addr :=
  | id = text; { User_addr id }

let text :=
  | x = TEXT; { x }

let main :=
  | ~ = command; EOF; <>
