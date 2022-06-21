# lake-parser-combinators

## Setup project
1. Install [The Haskell Tool Stack](https://docs.haskellstack.org/)
2. ```
   stack setup
   ```

## GHCI

### Command
```
stack build
stack ghci src/[VERSION]/TrivialLakeParser.hs
```

### Example using V4
```
stack build
stack ghci src/V4/TrivialLakeParser.hs
parseProgram "./programs/trivial.js" program
```

## Execute Build
```
stack build
stack exec lake-parser-combinators-exe
```
