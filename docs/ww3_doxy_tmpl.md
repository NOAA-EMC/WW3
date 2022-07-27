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
Module has two parts: a **header**, and **inline documentation** for module variables.
###### header
```
!> @brief <one sentence description>   # contents of '1. Purpose' from original header
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

###### inline documentation
*ALL* module variables need to be documented. To accomodate this, put each variable on it own line.
The documentation can go directly above or to the right of the variable declaration.  Multiple lines
can be used.
```
var_1       !<  <var_1 description>
 .
 .
            !<  <var_N description>
var_N       !<  <var_N description continued>
```

### PROGRAM
```
!> @brief  <one sentence description>      # contents of '1. Purpose' from original header
!>
!> @details <extended description>          # contents of '2. Method' from original header
!> 
!> @author <author name>  @date <dd-Mon-yyyy>
```

### SUBROUTINE
```
!> @brief  <one sentence description>       # contents of '1. Purpose' from original header
!>
!> @details <extended description>           # contents of '2. Method' from original header
!>
!> @param[inout] <param name> <very short description> # specify:  [in], [out], [inout], or just `@param`.
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


### Optional tags
* Some of the more relevant tags that might be included:  `@remark`, `@attention`, `@warning`, `@example`,
  `@code`, `@cite`, `@copyright`.
* The complete list of tags: [Doxygen Special Commands](https://www.doxygen.nl/manual/commands.html). 
