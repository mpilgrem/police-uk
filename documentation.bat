@Echo OFF
REM A batch file to help with using standalone-haddock on Windows 10 with stack.

REM Usage: run in the folder containing the .cabal file for the package. The
REM output will be placed in the folder specified as the first parameter when
REM invoking the batch file or docs by default.

setlocal

If [%1]==[] (Set dest-dir=docs) Else Set dest-dir=%1
Echo The output folder is set to: %dest-dir%
Echo:

For /F %%I in ('stack path --compiler-exe') Do Set compiler-exe=%%I
For /F %%I in ('stack path --dist-dir') Do Set dist-dir=%%I
For /F %%I in ('stack path --snapshot-pkg-db') Do Set snapshot-pkg-db=%%I
For /F %%I in ('stack path --local-pkg-db') Do Set local-pkg-db=%%I
Echo The following locations will be used:
Echo compiler-exe:    %compiler-exe%
Echo dist-dir:        %dist-dir%
Echo snapshot-pkg-db: %snapshot-pkg-db%
Echo local-pkg-db:    %local-pkg-db%
Echo:

standalone-haddock ^
-o%dest-dir% ^
--hyperlink-source ^
--quickjump ^
-v3 ^
--compiler-exe=%compiler-exe% ^
--dist-dir=%dist-dir% ^
--package-db=%snapshot-pkg-db% ^
--package-db=%local-pkg-db% .

endlocal
