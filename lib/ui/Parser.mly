%token EDIT VIEW
%token HELP
%token QUIT
%token LS
%token UPDATE
%token <string> TEXT
%token EOF

%start <Command.t> main

%%

let command :=
  | EDIT; addr = addr; <Command.Edit>
  | VIEW; addr = addr; <Command.View>
  | LS; { Command.Ls }
  | QUIT; { Command.Quit }
  | HELP; { Command.Help }
  | UPDATE; { Command.Update }

let addr :=
  | id = text; { User_addr id }

let text :=
  | x = TEXT; { x }

let main :=
  | ~ = command; EOF; <>
