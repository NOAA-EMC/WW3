# WAVEWATCH III Doxygen Header Reference
<br>

_Doxygen markup headers to be placed **directly above** the respective Fortran code units._

### FILE
```
!> @file
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
!> @author <author name> @date <dd-Mon-yyyy>
                                                    # ALL module variables documented
module_var_1       !<  <module_var_1 description>
 ...
!>  <module_var_i description>                      # multiples lines can be used
!>  <module_var_i description cont.>                # if needed.
module_var_i
 ...
module_var_N       !<  <module_var_N description>
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
!<  <inline variable description>                 # '!>' is equally valid here as well
```


### Optional tags
* Some of the more relevant tags that might be included:  `@remark`, `@attention`, `@warning`, `@example`,
  `@code`, `@cite`, `@copyright`.
* The complete list of tags: [Doxygen Special Commands](https://www.doxygen.nl/manual/commands.html). 
