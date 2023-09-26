function(check_switches switches switch_files)
  # Read JSON file
  file(READ ${CMAKE_CURRENT_SOURCE_DIR}/cmake/switches.json json_str)
  # Get length of top-level array of all switch categories
  string(JSON len LENGTH ${json_str})
  # CMake's foreach RANGE is inclusive, so subtract 1 when looping
  math(EXPR len "${len} - 1")

  # Loop over switch categories
  set(files "")
  foreach(i_category RANGE ${len})
    string(JSON category GET ${json_str} ${i_category})
    string(JSON num_options LENGTH ${category} valid-options)

    # Loop over valid options
    math(EXPR num_options "${num_options} - 1")
    set(n_switches_in_category 0)
    foreach(j_options RANGE ${num_options})
      string(JSON valid_opt GET ${category} valid-options ${j_options} name)

      # This option is in current switch file
      if(valid_opt IN_LIST switches)
        math(EXPR n_switches_in_category "${n_switches_in_category} + 1")
        string(JSON n_files ERROR_VARIABLE err LENGTH ${category} valid-options ${j_options} build_files)

        # Check for conflicting switches
        string(JSON n_conflicts ERROR_VARIABLE err LENGTH ${category} valid-options ${j_options} conflicts)
        if(n_conflicts)
          math(EXPR n_conflicts "${n_conflicts} -1")
          # Loop over conflcits
          foreach(i_conflict RANGE ${n_conflicts})
            string(JSON conflict GET ${category} valid-options ${j_options} conflicts ${i_conflict})
            if(conflict IN_LIST switches)
              message(FATAL_ERROR "Switch '${valid_opt}' and '${conflict}' conflict")
            endif()
          endforeach()
        endif()

        # Check for required dependent switches
        string(JSON n_requires ERROR_VARIABLE err LENGTH ${category} valid-options ${j_options} requires)
        if(n_requires)
          math(EXPR n_requires "${n_requires} - 1")
          # Loop over required switches
          foreach(i_requires RANGE ${n_requires})

            string(JSON json_type TYPE ${category} valid-options ${j_options} requires ${i_requires})

            # Can be a string or an array. String values or directly required, while if an array one of the values is required.
            if(json_type STREQUAL "STRING")
              string(JSON required_switch GET ${category} valid-options ${j_options} requires ${i_requires})
              if(NOT required_switch IN_LIST switches)
                message(FATAL_ERROR "Switch '${valid_opt}' requires '${required_switch}' to be set")
              endif()
            elseif(json_type STREQUAL "ARRAY")
              string(JSON n_requires_any LENGTH ${category} valid-options ${j_options} requires ${i_requires})
              math(EXPR n_requires_any "${n_requires_any} - 1")

              # Loop over array and check that one of the switches is present
              set(found false)
              set(possible_values "")
              foreach(i_requires_any RANGE ${n_requires_any})
                string(JSON required_switch GET ${category} valid-options ${j_options} requires ${i_requires} ${i_requires_any})
                list(APPEND possible_values "${required_switch}")

                if(required_switch IN_LIST switches)
                  set(found true)
                endif()
              endforeach()

              if(NOT found)
                message(FATAL_ERROR "Switch ${valid_opt} requires one of ${possible_values} to be set")
              endif()

            endif()
          endforeach()
        endif()

        if(n_files)
          # Loop over files associated with switch and add them to build
          math(EXPR n_files "${n_files} - 1")
          foreach(i_files RANGE ${n_files})
            string(JSON file GET ${category} valid-options ${j_options} build_files ${i_files})
            list(APPEND files "${file}")
          endforeach()
        endif()
      endif()

    endforeach()

    # Check for the correct number of switches per category
    string(JSON num_switches GET ${category} num_switches)
    string(JSON category_name GET ${category} name)

    if(num_switches STREQUAL "one" AND NOT n_switches_in_category EQUAL 1)
      message(FATAL_ERROR "No valid ${category_name} switches found, but one is required")
    elseif(num_switches STREQUAL "upto1" AND n_switches_in_category GREATER 1)
      message(FATAL_ERROR "Too many ${category_name} switches found (max 1)")
    elseif(num_switches STREQUAL "upto2" AND n_switches_in_category GREATER 2)
      message(FATAL_ERROR "Too many ${category_name} switches found (max 2)")
    endif()

  endforeach()

  set(${switch_files} ${files} PARENT_SCOPE)
endfunction()
