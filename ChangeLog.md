# Revision history for uniqueness-periods-vector-general

## 0.1.0.0 -- 2020-08-31

* First version. Released on an unsuspecting world.

## 0.2.0.0 -- 2020-09-01

* Second version. Added the possibility to define what output to be printed (but not its order) using the new imported dependency module Data.Print.Info
from print-info module. Some documentation improvements.

## 0.2.0.1 -- 2020-09-07

* Second version revised A. Added more documentation.

## 0.2.0.2 -- 2020-09-09

* Second version revised B. Some documentation improvements.

## 0.2.1.0 -- 2020-09-09

* Second version revised C. Fixed issues with wrong arguments order in the uniquenessVariants2GNP function application inside the uniqNPoetical2GN and uniqNPoetical2VGN
functions.

## 0.2.2.0 -- 2020-09-09

* Second version revised D. Changed dependency bounds of uniqueness-periods-vector-common package.

## 0.2.3.0 -- 2020-09-09

* Second version revised E. Improved code readability.

## 0.3.0.0 -- 2020-09-11

* Third version. Fixed issues with being not ordered printed information. Changed the names of the functions. Conversion tables of the changed names is
now given in the ConversionTable.txt file. Some documentation improvements.

## 0.4.0.0 -- 2020-09-12

* Fourth version. Changed dependency boundaries. Library is rewritten to avoid the redundant complexity of the algorithm realization, to make
the functions basically pure, to fix the issue with printing subsystem in case of the file output. Changes break the previous behaviour, so please, check the code.

## 0.4.1.0 -- 2020-09-12

* Fourth version revised A. Changed the order of the elements in the list in the Languages.UniquenessPeriods.Vector.General.Debug.maximumElByVec function so that
being afterwards applied naturally Languages.UniquenessPeriods.Vector.General.Debug.printUniquenessG1List prints elements starting from the maximum one.

## 0.4.2.0 -- 2020-09-13

* Fourth version revised B. Fixed issue with toFile function in being not written output for the complex second argument.

## 0.4.3.0 -- 2020-09-28

* Fourth version revised C. Fixed issue with maximumElByVec function for situations with repeated distant groups of sounds (and probably other similar ones). Changed
the behaviour of the maximumElBy function so that it become strict in the inner maximum element.

## 0.4.4.0 -- 2020-09-28

* Fourth version revised D. Now uses new uniquenessPeriods2GNP variant from the renewed uniqueness-periods-vector-common package. Added newLineEnding function to the exported ones by the module. Some minor code improvements. Changed the maximumElByVec behaviour to more efficient one.

## 0.4.5.0 -- 2020-10-06

* Fourth version revised E. Fixed issues related to the dependency of uniqueness-periods-vector-common package prior to 0.4.1 versions.

## 0.4.6.0 -- 2020-10-07

* Fourth version revised F. Fixed issue with incomplete information being printed in functions printUniquenessG1List and printUniquenessG1.
Added a new function printUniquenessG1ListStr with prettier printing for the Char-based arguments (strings).

## 0.5.0.0 -- 2020-10-09

* Fifth version. Instead of two composable function application switched to the usage of the more flexible variant with FuncRep a b c data type from
renewed Languages.UniquenessPeriods.Vector.Data module. Updated the functions and documentation as needed.

## 0.5.1.0 -- 2020-10-12

* Fifth version revised A. Fixed issue with the incorrectly working imported dependency function uniquenessVariants2GNP that influenced all the related
functionality.

## 0.5.2.0 -- 2020-10-30

* Fifth version revised B. Fixed inlining policies so that ghc -dcore-lint produces now no warnings. Some minor code improvements.

## 0.5.3.0 -- 2020-11-12

* Fifth version revised C. Fixed issues with empty second element in the tuple for maximumElBy function.
