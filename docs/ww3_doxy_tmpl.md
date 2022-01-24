# WAVEWATCH III Doxygen Header Reference
<br>

_Doxygen markup headers to be placed directly above the respective Fortran code units._

### FILE
```
!> @file  <file name>
!> @brief  <one sentence description>
!> 
!> @author <author name> @date <dd-Mon-yyyy>
```

### MODULE
```
!> @brief <one sentence description>   # contents of _1. Purpose_ from original header
!> 
!> @details <extended description>
!>
!> @param <module var 1>               # ALL module variables documented
!> ...
!> @param <module var N>
!> @author <author name> @date <dd-Mon-yyyy>
```

### PROGRAM
```
!> @brief  <one sentence description>      # contents of _1. Purpose_ from original header
!>
!> @details <extended description>          # contents of _2. Method_ from original header
!> 
!> @author <author name>  @date <dd-Mon-yyyy>
```

### SUBROUTINE
```
!> @brief  <one sentence description>       # contents of _1. Purpose_ from original header
!>
!> @details <extended description>           # contents of _2. Method_ from original header
!>
!> @param[in/out] <param name> <very short description> # specify:  [in], [out], [inout], or just `@param`.
!> @author <author name>  @date <dd-Mon-yyyy>
```

### FUNCTION
```
!> @brief  <one sentence description>
!>
!> @details <extended description>
!>
!> @param <param name>
!> @returns <return variable name>
!>
!> @author <author name> @date <dd-Mon-yyyy>
```


### Inline documentation (variables)
```
!<  <inline variable description>
```


### Optional tags
* Some of the more relevant tags that might be included:  `@todo`, `@remark`, `@attention`, `@warning`, `@bug`, `@example`,
  `@code`, `@cite`, `@copyright`.
* The complete list of tags: [Doxygen Special Commands](https://www.doxygen.nl/manual/commands.html). 

