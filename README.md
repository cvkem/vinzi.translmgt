# vinzi.transmgt

Tool for managing the pentaho language packs by 
language pack translators.

This tool generates a translation.xls file containing all 
language-pack property files. This file can be editted and
subsequently be expanded to the full language pack again.

The main benefit of the tool arises during maintenance, as the
translation.xls file also keeps track of the original property-value
in the base-language. So when running a check for updates it will
mark properties according to their status.

## Usage

Build the big-jar from code, or download it from:
   www.vinzi.eu:890/translmgt
Put the translMgt.properties in the current folder and adjust it
to your local setup.

Run the main program via
java -cp vinzi.transmgt-standalone.jar clojure.main -m vinzi.transmgt.core help

This will compile and run the program and show a short note about usage.
The other functions of the translmgt tool are activated by using the
same command, but replacing 'help' by an other command, followed by 
arguments.

### Building a new language from existing property-files
For example, to build the translation.xls for the dutch language-packe
(suffix nl) from scratch you run:

java -cp vinzi.transmgt-standalone.jar clojure.main -m vinzi.transmgt.core new-translation nl

The translation will be sorted based on the key names in the 
property-files, so when the same property-name (like Save) appears 
multiple times in different files, the keys will below one another
in the translation.xls. So translate the first entry and copy it
to all other entries having the same property-name. This speeds up
the translation, and increases the consistency of translations across
the different pentaho-tools.

### Check an existing pack for updates
Again for the nl-language-pack:

java -cp vinzi.transmgt-standalone.jar clojure.main -m vinzi.transmgt.core check-updates nl

Here main benefit of the tool arises during maintenance, as the
translation.xls file also keeps track of the original property-value
in the base-language. So when running a check for updates it will
mark add new properties with tag "TRANSLATE" and marked properties 
that changed value in the base-language with "UPDATE". Properties that
couldn't be found anymore in the base-language are marked "REMOVED".
Al remaining properties (like more than 95% of the pack will be 
marked "ok", so these do not require your attention.

### Expand a translation to property-files

java -cp vinzi.transmgt-standalone.jar clojure.main -m vinzi.transmgt.core expand-translation nl

This will expand the translation to property-files in the language
folder under the data-folder (as defined in the properties-file).
Existing property-files will be overwritten. 

The lines of a property-file will be in the same order as the orginal
line, and each line will be prefix with the original comments in
front of the line (as defined in the translation.xls).

### Building a new language that does not yet have properties
Prepare the folder, and add a copy of the translation.xls of
the base language to this folder.

## To Do
We still need to prepare a small wrapper shell-script 'translmgt.sh'
that does all of the above. 

Furthermore we need to think which parts of the process should
be handled by the central coordinator (webdetails) and what part should
be performed by the individual language-pack maintainers.

## License

Copyright © 2013 Vinzi

Distributed under the Eclipse Public License either version 1.0 
