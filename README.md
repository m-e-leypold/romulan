Romulan
=======

*Romulan* is a declarative interface for the Common Lisp command line
parser [clingon](https://github.com/dnaeon/clingon).

Romulan is currently *work in progress*. Defining an application with
subcommands already works (see example below). For more planned
development (= features that do not exist yet, but will some time in
the future), see the section *[Future development](#future-development)* below.

Motivation
----------

[Clingon](https://github.com/dnaeon/clingon) is a very versatile
command line parser for Common Lisp. With *clingon* a command line
interface is typically defined procedurally by plugging it together
from elements that again are produced by functions:

	  (defun greet/options ()
		"Returns the options for the `greet' command"
		(list
		 (clingon:make-option
		  :string
		  :description "Person to greet"
		  :long-name "user"
		  :key :user)))

	  (defun greet/handler (cmd)
		"Handler for the `greet' command"
		(let ((who (clingon:getopt cmd :user)))
		  (format t "Hello, ~A!~%" who)))

	  (defun greet/command ()
		"A command to greet someone"
		(clingon:make-command
		 :name "greet"
		 :description "greets people"
		 :options (greet/options)
		 :handler #'greet/handler))

	  (defun main ()
		"The main entrypoint of our CLI program"
		(let ((app (greet/command)))
		  (clingon:run app)))

And this example is only the definition of a single command application, not
one with subcommands.

Doing it this way is very general and allows a developer to put
together a wide range of possible interfaces, but it has the
disadvantage that the structure of the command line interface is
difficult to get from the code. Also I do not like the explicit
extraction of options in the handler(s):

      (let ((who (clingon:getopt cmd :user))) ...)

So I wrote *Romulan* to be a declarative interface to *clingon*, with
semantics better visible in the code and some sensible defaults that
keep the definitions short. This comes admittedly at the price of less
flexibility than *clingon*.

Example
-------

As already mentioned above only subcommand interfaces (also called
*git* or *svn* style) can currently be defined in *Romulan*. Those are
interfaces where the command line looks like:

	  $ ./example-script-2 --user=Jim hello
	  Hello, Jim!
	  $ .//example-script-2 shout go for it
	  GO FOR IT, rak1912!
	
The words *hello* and *shout* are called subcommands here. 

You will find the following *Romulan* example in file
[```example-script-2```](./example-script-2) in the *Romulan* source, a
slightly larger example in [```example-script```](./example-script).

In *Romulan* an interface with subcommands is declared with
```commandline-subcommand-interface```. The declaration needs to
contain at least a usage string (that is later used to construct help
text) and the definition of global options:

      (commandline-subcommand-interface romulan-test "shout some words or say hello"

        :usage   "[-v] [-u <user>] <command> [options ...]"
        :options (:user (:description "user to greet"
						 :short-name #\u
						 :env-vars ("USER"))))

Invoking the command without any options will display a help text that
describes usage and global options from the definition above and lists
the subcommands (all this courtesy of *clingon*, *Romulan* just adds
defaults and reformats the definitions in a slightly different format
for *clingon*).

	  $ ./example-script-2 
	  NAME:
		romulan-test - shout some words or say hello

	  USAGE:
		romulan-test [-v] [-u <user>] <command> [options ...]

	  OPTIONS:
			--help          display usage information and exit
			--version       display version and exit
		-u, --user <VALUE>  user to greet [env: $USER]

	  COMMANDS:
		shout  shouts back anything you write
		hello  just says hello

Subcommands are defined with ```define-subcommands``` and look mostly
like function definitions with additional arguments that specify
usage, one-line help text and the subcommand options.

	  (define-subcommand hello (&key user)
		  (:description "just says hello")

		(format t "Hello, ~A!~%" user))

	  (define-subcommand shout (words &key user)

		  (:description "shouts back anything you write"
		   :usage       "[options ...] [arguments ...]"
		   :varargs     t)
		   
		(format t "~{~A~^ ~}, ~A!~%" (mapcar #'string-upcase words) user))

All subcommands are indeed procedures and where the ```&key```
parameters correspond to options which will be automatically bound
when invoking the subcommand. The positional parameters will be bound
from the positional arguments of the POSIX argument vector.

When all sub-commands are defined, ```end-subcommand-interface``` must
be invoked. This actually builds the interfaces declared with
```commandline-subcommand-interface```.

	  (end-subcommand-interface)

The command line interface thus defined is bound as a procedure to the
symbol given to ```commandline-subcommand-interface``` and can be
invoked as a procedure to process the command-line arguments and invoke
the subcommands.

	  (romulan-test)

The subcommands are themselves procedures and can simply be
called from Lisp (e.g. for testing) like any procedure:

      (hello :user "Jim)
	  Hello, Jim!


License
-------

	Romulan - Declarative interface to the clingon command line argument parser.
	Copyright (C) 2023  M E Leypold

	This program is free software: you can redistribute it and/or modify
	it under the terms of the GNU General Public License as published by
	the Free Software Foundation, either version 3 of the License, or
	(at your option) any later version.

	This program is distributed in the hope that it will be useful,
	but WITHOUT ANY WARRANTY; without even the implied warranty of
	MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
	GNU General Public License for more details.

	You should have received a copy of the GNU General Public License
	along with this program.  If not, see <https://www.gnu.org/licenses/>.

For the full license text see [LICENSE](./LICENSE.md).


Future development
------------------

- Free text help pages, stand-alone and associated to commands.
- Systematic tests are currently missing. There are only the example scripts.


<!--  LocalWords:  clingon Romulan
 -->
